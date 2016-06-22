# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("Rtools", "car", "caret", "dichromat", "entropy", "foreach", "ggplot2", "gtable", "iterators", "itertools2", "labeling", "lme4", "MatrixModels", "minqa", "munsell", "nloptr", "pbkrtest", "proto", "quantreg", "RColorBrewer", "Rcpp", "RcppEigen", "reshape2", "scales", "stringi")
