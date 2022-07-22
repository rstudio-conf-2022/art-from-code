#include <Rcpp.h>
#include <iostream>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
NumericMatrix unboxer_rcpp(int iterations, int layers) {
  
  // variables
  NumericMatrix pts(iterations, 3); 
  NumericMatrix cff(9, layers);
  int r, f;
  double x, y, z, s;
  
  // coefficients
  for(int i = 0; i < 9; i++) {
    for(int j = 0; j < layers; j++) {
      cff(i,j) = R::runif(-1,1);
    }
  }
  
  // initial point
  pts(0, 0) = R::runif(-1, 1);
  pts(0, 1) = R::runif(-1, 1);
  pts(0, 2) = R::runif(-1, 1);
  
  // accumulate
  for(int t = 1; t < iterations; t++) {
    r = rand() % layers; // which transform to use?
      f = rand() % 3;      // which function to use?
        
        // apply transformation
      x = cff(0, r) * pts(t-1, 0) + cff(1, r) * pts(t-1, 1) + cff(2, r);
      y = cff(3, r) * pts(t-1, 0) + cff(4, r) * pts(t-1, 1) + cff(5, r);
      z = cff(6, r) * pts(t-1, 0) + cff(7, r) * pts(t-1, 1) + cff(8, r);
      
      // apply function
      if(f == 0) {
        s = pow(x*x + y*y + z*z, 1/3);
        x = x + s;
        y = y + s;
        z = z + s;
      } else if(f == 1) {
        x = sin(x);
        y = sin(y);
        z = sin(z);
      } else {
        x = 2 * sin(x);
        y = 2 * sin(y);
        z = 2 * sin(z);
      }
      
      // store new point
      pts(t, 0) = x;
      pts(t, 1) = y;
      pts(t, 2) = (z + pts(t-1, 2))/2;
  }
  return pts;
}
