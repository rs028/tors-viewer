###
### version 0.1, July 2025
### author: RS
### ---------------------------------------------------------------- ###

displayAVG <- function(df.in, min) {
  az <- length(df.in)
  aa <- az - min
  aa <- ifelse(aa > 0, aa, 1)
  df.avg <- mean(df.in[aa:az], na.rm=TRUE)
  format(df.avg, digits=4, scientific=TRUE)
}

readOUT <- function(read.in) {
  read.out <- ifelse(read.in < 0, 0, read.in)
  format(read.out, digits=3, scientific=FALSE)
}
