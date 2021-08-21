OS <- commandArgs(TRUE)

win_other_places <- c("C:/msys64/mingw64/include",
                      "C:/msys64/mingw32/include",
                      "C:/msys64/clang64/include",
                      "C:/msys64/clang32/include")

find_suitesparse <- function() {
  if(Sys.which("cpp") != "") {
    cpp_search <- system2("cpp", "-v", TRUE, TRUE, timeout = 5)
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
      paths <- c(file.path(paths, "suitesparse", "cholmod.h"),
                 file.path(paths, "cholmod.h"))
    }
  } else {
    paths <- ""
  }

  PATH <- switch(OS,
                 win = c(strsplit(Sys.getenv("PATH"), ";")[[1]], win_other_places),
                 nowin = strsplit(Sys.getenv("PATH"), ":")[[1]])
  paths <- c(paths, file.path(PATH, "cholmod.h"))

  here <- sapply(paths, file.exists)
  if(any(here)) {
    SUITESPARSE_INCLUDE <- dirname(paths[here][1])
  } else {
    SUITESPARSE_INCLUDE <- ""
  }
  SUITESPARSE_INCLUDE

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
    SUITESPARSE_INCLUDE <- find_suitesparse()
    if(SUITESPARSE_LIB == "") {
      if(SUITESPARSE_INCLUDE != "") {
        message("SuiteSparse found! Setting up directories...")
        SUITESPARSE_LIB <- file.path(dirname(SUITESPARSE_INCLUDE), "lib")
      }
    }
  }
}

