# Install OverReact from this offline bundle. Run it inside the enclave:
#
#   Rscript path/to/bundle/install.R [library folder] [--all]
#
# It checks every file against the bundle's checksums, installs OverReact and each
# bundled package that is missing or older than the bundled version into the library
# folder (default: the first entry of .libPaths()), then runs a short smoke test on
# invented data. --all installs every bundled package, for a self-contained project
# library. Nothing is downloaded.

args <- commandArgs(trailingOnly = TRUE)
install_all <- "--all" %in% args
lib <- args[!startsWith(args, "--")]
lib <- if (length(lib)) lib[[1]] else .libPaths()[[1]]
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
lib <- normalizePath(lib, winslash = "/")
.libPaths(c(lib, .libPaths()))

script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
bundle <- normalizePath(if (length(script)) dirname(script[[1]]) else ".", winslash = "/")

# 1. Every file must match its checksum: SHA-256 where this R has it, MD5 otherwise.
use_sha <- exists("sha256sum", asNamespace("tools")) &&
  file.exists(file.path(bundle, "SHA256SUMS"))
sums <- readLines(file.path(bundle, if (use_sha) "SHA256SUMS" else "MD5SUMS"))
files <- sub("^[0-9a-f]+  ", "", sums)
hash <- if (use_sha) get("sha256sum", asNamespace("tools")) else tools::md5sum
actual <- unname(hash(file.path(bundle, files)))
bad <- files[is.na(actual) | actual != sub("  .*$", "", sums)]
if (length(bad)) {
  stop("These bundle files are missing or changed: ", paste(bad, collapse = ", "), call. = FALSE)
}
message("All ", length(files), " files match the bundle's ",
        if (use_sha) "SHA-256" else "MD5", " checksums.")

# 2. OverReact, and any bundled package that is missing or older than the bundled one.
repo <- if (startsWith(bundle, "//")) {
  paste0("file:", bundle)
} else {
  paste0(if (.Platform$OS.type == "windows") "file:///" else "file://", bundle)
}
db <- utils::available.packages(repos = repo, type = "source")
have <- utils::installed.packages()[, c("Package", "Version"), drop = FALSE]
newest <- tapply(have[, "Version"], have[, "Package"],
                 function(v) as.character(max(package_version(v))))
current <- function(pkg) {
  !is.na(newest[pkg]) && package_version(newest[[pkg]]) >= package_version(db[pkg, "Version"])
}
todo <- if (install_all) rownames(db) else rownames(db)[!vapply(rownames(db), current, logical(1))]
todo <- union(todo, "OverReact")

r_minor <- paste(R.version$major, sub("\\..*$", "", R.version$minor), sep = ".")
binaries <- .Platform$OS.type == "windows" &&
  file.exists(file.path(bundle, "bin", "windows", "contrib", r_minor, "PACKAGES"))
if (.Platform$OS.type == "windows" && !binaries) {
  compiled <- todo[db[todo, "NeedsCompilation"] %in% "yes"]
  if (length(compiled) && !nzchar(Sys.which("make"))) {
    stop("These packages need compiling, which needs Rtools: ", paste(compiled, collapse = ", "),
         ". Ask for a bundle made with --windows-binaries --r-version=", r_minor, ".",
         call. = FALSE)
  }
}
message("Installing into ", lib, ": ", paste(todo, collapse = ", "))
old_compile_opt <- options(
  install.packages.compile.from.source = if (nzchar(Sys.which("make"))) "always" else "never"
)
on.exit(options(old_compile_opt), add = TRUE)
utils::install.packages(todo, lib = lib, repos = repo, type = if (binaries) "both" else "source")
missing <- todo[!nzchar(vapply(todo, function(pkg) system.file(package = pkg, lib.loc = lib),
                                character(1)))]
if (length(missing)) stop("Not installed: ", paste(missing, collapse = ", "), call. = FALSE)

# 3. Smoke test on invented data: a model, a forest plot and a CSV export.
suppressPackageStartupMessages(library(OverReact))
set.seed(1)
toy <- data.frame(y = stats::rbinom(300, 1, 0.3),
                  group = factor(sample(c("a", "b", "c"), 300, TRUE)),
                  sex = factor(sample(c("F", "M"), 300, TRUE)))
fit <- suppressMessages(ModelMakerMulti(toy, "group", outcome = "y", joint_adjustment_vars = "sex"))
stopifnot(nrow(fit$plot_output) > 0)
invisible(ggplot2::ggplot_build(plotReactForest(fit)))
out_dir <- tempfile("overreact-smoke-")
dir.create(out_dir)
saveREACTtable(fit$df_output, outpath = out_dir, filename = "smoke")
stopifnot(file.exists(file.path(out_dir, "smoke.csv")))
unlink(out_dir, recursive = TRUE)

message("OverReact ", utils::packageVersion("OverReact"), " is installed and passed its smoke test.\n",
        "Start each session with:\n",
        "  .libPaths(c(\"", lib, "\", .libPaths()))\n",
        "  library(OverReact)")
