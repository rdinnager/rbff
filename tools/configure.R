OS <- commandArgs(TRUE)

rtools_home <- Sys.getenv("RTOOLS40_HOME")
r_arch <- switch(Sys.getenv("R_ARCH"),
                 `/i386` = "32",
                 `/x64` = "64")

R_BIN <- file.path(R.home("bin"), "R.exe")

# Find compiler
CC <- system2(R_BIN, c("CMD", "config", "CC"), stdout = TRUE)
CFLAGS <- system2(R_BIN, c("CMD", "config", "CFLAGS"), stdout = TRUE)
CPPFLAGS <- system2(R_BIN, c("CMD", "config", "CPPFLAGS"), stdout = TRUE)


win_other_places <- suppressWarnings(normalizePath(c(paste0("C:/msys", r_arch, c("/mingw", "/clang"), r_arch, "/include"),
                      paste0(rtools_home, c("/mingw", "/clang"), r_arch, "/include")),
                      winslash = "/"))

find_header <- function(header, subdir) {
  if(Sys.which("cpp") != "") {

    cpp_search <- switch(OS,
                         win = system2("cpp", c("-v", "nul"), TRUE, TRUE, timeout = 5),
                         nowin = system2("cpp", c("-v", "/dev/null"), TRUE, TRUE, timeout = 5))
    start <- which(cpp_search == "#include <...> search starts here:")
    end <- which(cpp_search == "End of search list.")

    if((end - start) > 0) {
      paths <- cpp_search[(start + 1L):
                            (end - 1L)]
    } else {
      paths <- ""
    }
    if(paths[1] != "") {
      paths <- gsub(" ", "", paths)
    }
  } else {
    paths <- ""
  }

  PATH <- switch(OS,
                 win = c(strsplit(Sys.getenv("PATH"), ";")[[1]], win_other_places),
                 nowin = strsplit(Sys.getenv("PATH"), ":")[[1]])
  paths <- c(paths, PATH)
  paths <- suppressWarnings(normalizePath(paths, winslash = "/"))
  paths <- c(file.path(paths, subdir, header),
             file.path(paths, header))

  here <- sapply(paths, file.exists)
  if(any(here)) {
    INCLUDE <- dirname(paths[here][1])
  } else {
    INCLUDE <- ""
  }
  INCLUDE

}

SUITESPARSE_DIR <- Sys.getenv("SUITESPARSE_DIR")
SUITESPARSE_INCLUDE <- Sys.getenv("SUITESPARSE_INCLUDE")
SUITESPARSE_LIB <- Sys.getenv("SUITESPARSE_LIB")

OPENBLAS_DIR <- Sys.getenv("OPENBLAS_DIR")
OPENBLAS_INCLUDE <- Sys.getenv("OPENBLAS_INCLUDE")
OPENBLAS_LIB <- Sys.getenv("OPENBLAS_LIB")

if(SUITESPARSE_DIR != "") {
  message("SUITESPARSE_DIR found, setting up directories...")
  if(SUITESPARSE_LIB == "") {
    SUITESPARSE_LIB <- file.path(SUITESPARSE_DIR, "lib")
  }
  if(SUITESPARSE_INCLUDE == "") {
    SUITESPARSE_LIB <- file.path(SUITESPARSE_DIR, "include")
  }
} else {
  if(SUITESPARSE_INCLUDE == "") {
    message("Searching for SuiteSparse...")
    SUITESPARSE_INCLUDE <- find_header("cholmod.h", "suitesparse")
    if(SUITESPARSE_INCLUDE != "") {
        message(paste0("SuiteSparse found at ", SUITESPARSE_INCLUDE))
    }
  }
}

if(OS == "win") {
  if(OPENBLAS_DIR != "") {
    message("OPENBLAS_DIR found, setting up directories...")
    if(OPENBLAS_LIB == "") {
      OPENBLAS_LIB <- file.path(OPENBLAS_DIR, "lib")
    }
    if(OPENBLAS_INCLUDE == "") {
      OPENBLAS_LIB <- file.path(OPENBLAS_DIR, "include")
    }
  } else {
    if(OPENBLAS_INCLUDE == "") {
      message("Searching for OpenBLAS...")
      OPENBLAS_INCLUDE <- find_header("cblas.h", "OpenBLAS")
        if(OPENBLAS_INCLUDE != "") {
          message(paste0("OpenBLAS found at ", OPENBLAS_INCLUDE))
        }
    }
  }
  pkg_l <- c("-lcholmod",
             "-lopenblas",
             "-lsuitesparseconfig",
             "-lcolamd",
             "-lamd",
             "-fopenmp")
} else {
  pkg_l <- "-lcholmod"
}

## Test configuration

CPPFLAGS <- paste0(CPPFLAGS, " -I", OPENBLAS_INCLUDE, " -I", SUITESPARSE_INCLUDE)

writeLines("#include <cholmod.h>", suitesparse_test <- tempfile())
test_suitesparse <- system2(CC, c(CPPFLAGS, CFLAGS,
                                  "-E", "-xc", suitesparse_test),
                            stderr = TRUE)
writeLines("#include <cblas.h>", openblas_test <- tempfile())
test_openblas <- system2(CC, c(CPPFLAGS, CFLAGS,
                               "-E", "-xc", openblas_test),
                         stderr = TRUE)

if(!is.null(attr(suitesparse_test, "status"))) {
  cat("
------------------------- [ANTIANTICONF] ------------------------------
* Failed to find the suitesparse system library required for rbff:    *
* Try installing:                                                     *
*     - deb: libsuitesparse-dev (Ubuntu, Debian, etc.)                *
*     - rpm: suitesparse-devel (Fedora, EPEL)                         *
*     - brew: suite-sparse                                            *
*     - pacman (Windows): mingw-w64-{x86_64,i686}-suitesparse         *
-----------------------------------------------------------------------
    - If it still doesn't work you may have to specify the locations
      of the 'include' and 'lib' directories manually using the
      environmental variables SUITESPARSE_INCLUDE and SUITESPARSE_LIB.")
}

if(!is.null(attr(test_openblas, "status"))) {
  cat("
------------------------- [ANTIANTICONF] ------------------------------
* Failed to find the OpenBLAS system library required for rbff:       *
* Try installing:                                                     *
*     - deb: libopenblas-dev (Ubuntu, Debian, etc.)                   *
*     - rpm: openblas-devel (Fedora, EPEL)                            *
*     - brew: openblas                                                *
*     - pacman (Windows): mingw-w64-{x86_64,i686}-openblas            *
-----------------------------------------------------------------------
    - If it still doesn't work you may have to specify the locations
      of the 'include' and 'lib' directories manually using the
      environmental variables OPENBLAS_INCLUDE and OPENBLAS_LIB.")
}

PKG_INCLUDES <- CPPFLAGS
PKG_LIBS <- paste(pkg_l, collapse = " ")

if(OPENBLAS_LIB != "") {
  PKG_LIBS <- paste0(PKG_LIBS, " -L", OPENBLAS_LIB)
}

if(SUITESPARSE_LIB != "") {
  PKG_LIBS <- paste0(PKG_LIBS, " -L", SUITESPARSE_LIB)
}

Makevars_in <- readLines("src/Makevars.in")
Makevars <- gsub("@PKG_INCLUDES@", PKG_INCLUDES, Makevars_in)
Makevars <- gsub("@PKG_LIBS@", PKG_LIBS, Makevars)
if(OS == "nowin") {
  writeLines(Makevars, "src/Makevars")
} else {
  writeLines(Makevars, "src/Makevars.win")
}
