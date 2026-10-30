# Installing OverReact in the enclave

The enclave has no internet, so OverReact and the packages it needs arrive as an
offline bundle: a folder that works as a local package repository. Build it
outside the enclave, transfer it, and install it inside with one command.

## 1. Build the bundle (outside the enclave)

From the root of an OverReact checkout, with internet access:

```sh
Rscript inst/enclave/make_bundle.R ../overreact-bundle --r-version=4.4 --windows-binaries
```

| Option | Effect |
|---|---|
| `--r-version` | The enclave's R version (still to confirm). Defaults to the R running the script. |
| `--windows-binaries` | Adds Windows binaries for that R version. They install without Rtools; without them, about a dozen dependencies must be compiled from source. |
| `--extra=openxlsx,future,future.apply` | Adds optional packages. openxlsx writes formatted Excel workbooks (`savePrettyExcelWorkbook()`). future and future.apply run models in parallel (`ncores > 1`); without them, runs are sequential. |
| `--no-vignettes` | Skips building the vignette, which needs knitr, rmarkdown and pandoc. |

OverReact is built from the committed code (`git HEAD`), so uncommitted changes
and untracked local files never enter the bundle. Packages that ship with R
(base and recommended packages, such as MASS and mgcv) are not included. The
script warns if a package needs a newer R than `--r-version`.

The bundle holds:

| File | Contents |
|---|---|
| `src/contrib/` | OverReact and its dependencies as source packages, with a `PACKAGES` index |
| `bin/windows/contrib/<R version>/` | Windows binaries, with `--windows-binaries` |
| `install.R` | The installer |
| `ENCLAVE_INSTALL.md` | This guide |
| `SHA256SUMS`, `MD5SUMS` | Checksums for every file |

## 2. Transfer it

Copy the whole folder in through the agreed transfer route. Don't change its
contents: the installer checks every file.

## 3. Install it (inside the enclave)

```sh
Rscript path/to/overreact-bundle/install.R D:/projects/<project>/Rlib
```

The installer:

1. checks every file against `SHA256SUMS` (on R 4.5 or later) or `MD5SUMS`, and
   stops if any file is missing or changed;
2. installs OverReact, plus each bundled package that is missing or older than
   the bundled version, into the library folder you name (default: the first
   library on `.libPaths()`). Add `--all` to install every bundled package, for
   a self-contained project library;
3. loads OverReact and runs a smoke test on invented data: a model, a forest
   plot and a CSV export.

Nothing is downloaded.

## 4. Use it

```r
.libPaths(c("D:/projects/<project>/Rlib", .libPaths()))
library(OverReact)
```

Putting the library first matters when the enclave also has older copies of the
same packages, such as a ggplot2 before 4.0.0.

Tables and plots made in the enclave leave only after output checking, like any
other output.
