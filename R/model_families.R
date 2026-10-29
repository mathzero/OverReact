# Model families for the modelling wrappers -----------------------------------
#
# A family specification says how a predictor's models are fitted and reported: the
# fitting engine, the glm family, the effect measure, whether standard errors are robust
# (sandwich), and whether the outcome is binary, which allows marginal risk differences
# and ratios.

.react_families <- c("auto", "binomial", "gaussian", "poisson", "quasipoisson",
                     "log_binomial", "modified_poisson", "ordinal", "multinomial")

.react_family_spec <- function(name) {
  spec <- switch(name,
    binomial = list(engine = "glm", glm_family = stats::binomial(), measure = "OR",
                    binary = TRUE, label = "binomial (logit)"),
    gaussian = list(engine = "glm", glm_family = stats::gaussian(), measure = "Beta",
                    binary = FALSE, label = "Gaussian (identity)"),
    poisson = list(engine = "glm", glm_family = stats::poisson(), measure = "IRR",
                   binary = FALSE, label = "Poisson (log)"),
    quasipoisson = list(engine = "glm", glm_family = stats::quasipoisson(), measure = "IRR",
                        binary = FALSE, label = "quasi-Poisson (log)"),
    log_binomial = list(engine = "glm", glm_family = stats::binomial(link = "log"),
                        measure = "RR", binary = TRUE, label = "log-binomial"),
    modified_poisson = list(engine = "glm", glm_family = stats::poisson(), measure = "RR",
                            binary = TRUE, robust = TRUE,
                            label = "modified Poisson (robust SE)"),
    ordinal = list(engine = "polr", measure = "OR", binary = FALSE,
                   label = "ordinal (proportional odds)"),
    multinomial = list(engine = "multinom", measure = "OR", binary = FALSE,
                       label = "multinomial (logit)")
  )
  utils::modifyList(list(name = name, glm_family = NULL, robust = FALSE), spec)
}

# Choose and validate a family for outcome y. "auto" picks binomial for two values,
# ordinal or multinomial for ordered or unordered categories, and Gaussian otherwise.
.react_resolve_family <- function(y, family = "auto", outcome = "outcome") {
  family <- match.arg(family, .react_families)
  values <- unique(stats::na.omit(y))
  if (length(values) < 2L) {
    stop("Less than two unique non-missing values found for outcome '", outcome, "'.")
  }
  categorical <- is.factor(y) || is.character(y) || is.logical(y)
  if (identical(family, "auto")) {
    family <- if (length(values) == 2L) {
      "binomial"
    } else if (categorical) {
      if (is.ordered(y)) "ordinal" else "multinomial"
    } else {
      "gaussian"
    }
  }
  spec <- .react_family_spec(family)

  if (isTRUE(spec$binary)) {
    # glm() needs 0/1, logical or two-level factor coding; anything else (such as 1/2)
    # would fail inside every model and leave empty results.
    if (length(values) != 2L) {
      stop("The ", family, " family needs a binary outcome, but '", outcome, "' has ",
           length(values), " distinct values.")
    }
    if (!categorical && !all(values %in% c(0, 1))) {
      stop("Binary outcome '", outcome, "' is coded ", paste(sort(values), collapse = "/"),
           "; recode it as 0/1, logical or a two-level factor.")
    }
  }
  if (family %in% c("gaussian", "poisson", "quasipoisson") && !is.numeric(y)) {
    stop("The ", family, " family needs a numeric outcome; '", outcome, "' is ", class(y)[1], ".")
  }
  if (family %in% c("poisson", "quasipoisson") && any(values < 0)) {
    stop("The ", family, " family needs non-negative counts in '", outcome, "'.")
  }
  if (identical(family, "ordinal") && length(values) < 3L) {
    stop("Ordinal models need at least three outcome categories; '", outcome, "' has ",
         length(values), ".")
  }
  spec
}

# Recode the outcome once, on the full data, so every stage shares its coding: binary
# outcomes become 0/1 (the second level is 1, as glm() treats factors), ordinal outcomes
# ordered factors, and multinomial outcomes factors.
.react_code_outcome <- function(y, spec) {
  if (isTRUE(spec$binary)) {
    if (is.logical(y)) return(as.integer(y))
    if (is.factor(y) || is.character(y)) {
      y <- droplevels(factor(y))
      return(as.integer(y == levels(y)[2]))
    }
    return(y)
  }
  if (spec$engine %in% c("polr", "multinom")) {
    lvls <- if (is.factor(y)) levels(droplevels(y)) else sort(unique(stats::na.omit(y)))
    return(factor(y, levels = lvls, ordered = identical(spec$engine, "polr")))
  }
  y
}

# HC0 sandwich covariance of a GLM's coefficients, as sandwich::sandwich() computes it.
.react_robust_vcov <- function(fit) {
  keep <- !is.na(stats::coef(fit))
  X <- stats::model.matrix(fit)[, keep, drop = FALSE]
  scores <- X * (stats::residuals(fit, type = "working") * fit$weights)
  bread <- summary(fit)$cov.unscaled
  out <- bread %*% crossprod(scores) %*% bread
  dimnames(out) <- dimnames(bread)
  out
}

.react_vcov <- function(mod) {
  if (!is.null(mod$robust_vcov)) mod$robust_vcov else stats::vcov(mod)
}

# Effect measure reported for a fitted model: exponentiated coefficients are odds ratios
# (logit link), risk ratios (log-link binomial) or incidence rate ratios (log-link
# Poisson); identity links give differences ("Beta"); other links stay as coefficients.
.react_model_measure <- function(mod) {
  if (inherits(mod, c("polr", "multinom"))) return("OR")
  fam <- tryCatch(mod$family, error = function(e) NULL)
  if (is.null(fam$link)) return(if (inherits(mod, "lm")) "Beta" else "Coef")
  if (identical(fam$link, "logit")) return("OR")
  if (identical(fam$link, "log")) {
    if (fam$family %in% c("poisson", "quasipoisson")) return("IRR")
    if (grepl("binomial", fam$family)) return("RR")
    return("Ratio")
  }
  if (identical(fam$link, "identity")) return("Beta")
  "Coef"
}

.react_ratio_measures <- c("OR", "RR", "IRR", "Ratio")

# Fit one stage's model, capturing warnings and errors for the diagnostics table.
.react_fit_model_captured <- function(formula, data, spec, control) {
  fit_once <- function() {
    if (identical(spec$engine, "polr")) {
      return(MASS::polr(formula, data = data, Hess = TRUE))
    }
    if (identical(spec$engine, "multinom")) {
      if (!requireNamespace("nnet", quietly = TRUE)) stop("Multinomial models need the nnet package.")
      # model = TRUE keeps the model frame, which the coefficient extraction needs.
      fit <- nnet::multinom(formula, data = data, trace = FALSE, Hess = TRUE, model = TRUE,
                            maxit = 1000)
      # nnet stops at its iteration limit without a warning; record one, as glm() would.
      if (!identical(as.integer(fit$convergence), 0L)) {
        warning("multinom() did not converge in 1000 iterations.")
      }
      return(fit)
    }
    start <- NULL
    if (identical(spec$name, "log_binomial")) {
      # Start inside the parameter space (every fitted risk below 1), or glm() often fails.
      y <- stats::model.response(stats::model.frame(formula, data))
      start <- c(log(mean(y)), rep(0, ncol(stats::model.matrix(formula, data)) - 1L))
    }
    fit <- stats::glm(formula, data = data, family = spec$glm_family, control = control,
                      start = start)
    if (isTRUE(spec$robust)) fit$robust_vcov <- .react_robust_vcov(fit)
    fit
  }

  warn_store <- character()
  fit <- tryCatch(
    withCallingHandlers(
      fit_once(),
      warning = function(w) {
        warn_store <<- c(warn_store, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) structure(list(`__error__` = conditionMessage(e)), class = "glm_error")
  )
  if (inherits(fit, "glm_error")) {
    return(list(fit = NULL, warnings = warn_store, error = fit$`__error__`))
  }
  list(fit = fit, warnings = warn_store, error = NULL)
}

# Offsets (log exposure times) apply to Poisson rate models only.
.react_check_offset <- function(offset, spec, data) {
  if (is.null(offset)) return(invisible(NULL))
  if (!spec$name %in% c("poisson", "quasipoisson")) {
    stop("`offset` is only used with Poisson and quasi-Poisson models.")
  }
  if (!offset %in% names(data)) stop("Offset variable '", offset, "' not found in data.")
  invisible(NULL)
}

# Give estimate columns the effect measure's name: "OR" in long tables, and the "_OR"
# suffix of formatted stage columns in wide tables.
.react_label_measure <- function(df, measure, wide = FALSE) {
  if (identical(measure, "OR") || is.null(df) || !nrow(df)) return(df)
  if (isTRUE(wide)) {
    names(df) <- sub("_OR$", paste0("_", measure), names(df))
  } else {
    names(df)[names(df) == "OR"] <- measure
  }
  df
}

# Multiple-testing adjustment: q-values (Benjamini-Hochberg by default, or any
# stats::p.adjust() method) within each adjustment stage, across every predictor and
# level. Reference and intercept rows are not tests. NULL or "none" adds no column.
.react_add_q_values <- function(df, method = "BH") {
  if (is.null(method) || identical(method, "none")) return(df)
  method <- match.arg(method, stats::p.adjust.methods)
  if (is.null(df) || !nrow(df) || !"P_value" %in% names(df)) return(df)
  is_test <- !is.na(df$P_value)
  if ("is_reference" %in% names(df)) is_test <- is_test & !(df$is_reference %in% TRUE)
  if ("is_intercept" %in% names(df)) is_test <- is_test & !(df$is_intercept %in% TRUE)
  stage <- if ("adjustment" %in% names(df)) as.character(df$adjustment) else rep("1", nrow(df))
  q <- rep(NA_real_, nrow(df))
  for (s in unique(stage[is_test])) {
    idx <- which(is_test & stage %in% s)
    q[idx] <- stats::p.adjust(df$P_value[idx], method = method)
  }
  df$Q_value <- q
  df[, append(setdiff(names(df), "Q_value"), "Q_value", after = match("P_value", names(df))), drop = FALSE]
}
