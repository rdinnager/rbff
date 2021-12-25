VERSION <- commandArgs(TRUE)
if(!file.exists(sprintf("../windows/suitesparse-%s/include/cholmod.h", VERSION))){
  download.file(sprintf("https://github.com/rwinlib/suitesparse/archive/v%s.zip", VERSION), "lib.zip", quiet = TRUE)
  dir.create("../windows", showWarnings = FALSE)
  unzip("lib.zip", exdir = "../windows")
  unlink("lib.zip")
}
