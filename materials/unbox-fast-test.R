
library(tictoc)
library(Rcpp)
library(here)

sourceCpp(file = here("materials", "unbox-fast.cpp"))

tic() 
set.seed(999)
dat <- unboxer_rcpp(1000000, layers = 2)
toc() 
