# Build an offline bundle that installs OverReact in the REACT enclave.
#
# Run it outside the enclave, with internet access, from the root of an OverReact git
# checkout:
#
#   Rscript inst/enclave/make_bundle.R <new bundle folder> [options]
#
# Options:
#   --r-version=4.4      the enclave's R version (default: the R running this script)
#   --windows-binaries   also add Windows binaries for that R version, which install
#                        without Rtools
#   --extra=openxlsx,future,future.apply
#                        optional packages to add, with their dependencies
#   --no-vignettes       skip building the vignette (which needs knitr, rmarkdown, pandoc)
#   --repo=URL           CRAN mirror (default: https://cloud.r-project.org)
#
# The bundle is a local package repository. OverReact is built from the committed code
# (git HEAD), so untracked local files can never be included. Its dependencies come from
# CRAN, except base and recommended packages, which ship with R. install.R and
# ENCLAVE_INSTALL.md are copied in, and SHA256SUMS and MD5SUMS cover every file.

args <- commandArgs(trailingOnly = TRUE)
option <- function(name, default) {
  hit <- grep(paste0("^--", name, "(=|$)"), args, value = TRUE)
  if (!length(hit)) return(default)
  value <- sub(paste0("^--", name, "=?"), "", hit[[1]])
  if (nzchar(value)) value else TRUE
}
bundle <- args[!startsWith(args, "--")]
if (length(bundle) != 1L) {
  stop("Usage: Rscript inst/enclave/make_bundle.R <new bundle folder> [options]", call. = FALSE)
}
r_version <- option("r-version", paste(R.version$major, R.version$minor, sep = "."))
bin_version <- sub("^([0-9]+\\.[0-9]+).*$", "\\1", r_version)
binaries <- isTRUE(option("windows-binaries", FALSE))
extra <- option("extra", "")
extra <- if (isTRUE(extra)) character() else strsplit(extra, ",", fixed = TRUE)[[1]]
repo <- option("repo", "https://cloud.r-project.org")

if (!file.exists("DESCRIPTION") || read.dcf("DESCRIPTION", "Package")[[1]] != "OverReact") {
  stop("Run this from the root of an OverReact checkout.", call. = FALSE)
}
if (length(system2("git", c("status", "--porcelain", "--untracked-files=no"), stdout = TRUE))) {
  warning("Uncommitted changes are not included: the bundle is built from git HEAD.",
          call. = FALSE)
}
if (dir.exists(bundle) && length(list.files(bundle, all.files = TRUE, no.. = TRUE))) {
  stop("'", bundle, "' is not empty; name a new folder.", call. = FALSE)
}
contrib <- file.path(bundle, "src", "contrib")
dir.create(contrib, recursive = TRUE, showWarnings = FALSE)
bundle <- normalizePath(bundle)
contrib <- normalizePath(contrib)

# OverReact, built from the committed code.
src <- tempfile("overreact-src-")
dir.create(src)
archive <- tempfile(fileext = ".tar")
if (system2("git", c("archive", "--format=tar", paste0("--output=", archive), "HEAD")) != 0) {
  stop("git archive failed.", call. = FALSE)
}
utils::untar(archive, exdir = src)
build_dir <- tempfile("overreact-build-")
dir.create(build_dir)
old_wd <- setwd(build_dir)
build_args <- c("CMD", "build", if (isTRUE(option("no-vignettes", FALSE))) "--no-build-vignettes",
                shQuote(src))
status <- system2(file.path(R.home("bin"), "R"), build_args)
setwd(old_wd)
if (status != 0) stop("R CMD build failed.", call. = FALSE)
invisible(file.copy(list.files(build_dir, pattern = "^OverReact_.*\\.tar\\.gz$", full.names = TRUE),
                    contrib))

# Everything OverReact needs to load, plus any extras, leaving out the packages that
# ship with R.
desc <- read.dcf(file.path(src, "DESCRIPTION"))
fields <- intersect(c("Depends", "Imports", "LinkingTo"), colnames(desc))
roots <- trimws(sub("\\(.*", "", unlist(strsplit(desc[1, fields], ","))))
roots <- setdiff(c(roots, extra), c("R", ""))
standard <- rownames(utils::installed.packages(priority = c("base", "recommended")))
db <- utils::available.packages(repos = repo, type = "source")
deps <- unlist(tools::package_dependencies(roots, db = db,
                                           which = c("Depends", "Imports", "LinkingTo"),
                                           recursive = TRUE))
deps <- sort(setdiff(unique(c(roots, deps)), standard))
unknown <- setdiff(deps, rownames(db))
if (length(unknown)) {
  stop("Not available from ", repo, ": ", paste(unknown, collapse = ", "), call. = FALSE)
}
message("Downloading ", length(deps), " source packages ...")
invisible(utils::download.packages(deps, destdir = contrib, available = db, repos = repo,
                                   type = "source", quiet = TRUE))
tools::write_PACKAGES(contrib, type = "source")

# CRAN serves the newest versions, which may need a newer R than the enclave has.
target <- package_version(if (grepl("^[0-9]+\\.[0-9]+$", r_version)) paste0(r_version, ".0") else r_version)
needs_r <- vapply(deps, function(pkg) {
  depends <- db[pkg, "Depends"]
  req <- if (is.na(depends)) character() else regmatches(depends, regexpr("R *\\(>= *[0-9.]+\\)", depends))
  if (length(req)) sub("^R *\\(>= *([0-9.]+)\\)$", "\\1", req) else "0.0"
}, character(1))
too_new <- deps[package_version(needs_r) > target]
if (length(too_new)) {
  warning("These need a newer R than ", r_version, ": ",
          paste0(too_new, " (R >= ", needs_r[too_new], ")", collapse = ", "), call. = FALSE)
}

if (binaries) {
  bin <- file.path(bundle, "bin", "windows", "contrib", bin_version)
  dir.create(bin, recursive = TRUE, showWarnings = FALSE)
  bin_url <- paste0(repo, "/bin/windows/contrib/", bin_version)
  bin_db <- utils::available.packages(contriburl = bin_url, type = "win.binary")
  have <- intersect(deps, rownames(bin_db))
  message("Downloading ", length(have), " Windows binaries for R ", bin_version, " ...")
  invisible(utils::download.packages(have, destdir = bin, available = bin_db, contriburl = bin_url,
                                     type = "win.binary", quiet = TRUE))
  tools::write_PACKAGES(bin, type = "win.binary")
  if (length(setdiff(deps, have))) {
    message("No Windows binary for R ", bin_version, ": ",
            paste(setdiff(deps, have), collapse = ", "), "; the source package is used.")
  }
}

invisible(file.copy(file.path(src, "inst", c(file.path("enclave", "install.R"), "ENCLAVE_INSTALL.md")),
                    bundle))

files <- list.files(bundle, recursive = TRUE)
write_sums <- function(name, hash) {
  writeLines(paste0(unname(hash(file.path(bundle, files))), "  ", files), file.path(bundle, name))
}
write_sums("MD5SUMS", tools::md5sum)
if (exists("sha256sum", asNamespace("tools"))) write_sums("SHA256SUMS", tools::sha256sum)

message(sprintf("\nBundle ready: %s\n%d source packages%s, %.1f MB in all.\n",
                bundle, length(deps) + 1L,
                if (binaries) paste0(" and Windows binaries for R ", bin_version) else "",
                sum(file.size(file.path(bundle, files))) / 1e6),
        "Copy the whole folder into the enclave and run: Rscript install.R <library folder>")
