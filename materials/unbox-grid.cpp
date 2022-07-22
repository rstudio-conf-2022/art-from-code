#include <Rcpp.h>
#include <iostream>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
NumericMatrix unboxer_grid(int iterations, 
                           int layers,
                           int pixels, 
                           double border) {
  
  // variables
  NumericMatrix image(pixels, pixels); 
  NumericMatrix cff(9, layers);
  int r, c, f, x_ind, y_ind;
  double x, y, z, s;
  
  // set image matrix to zeros
  for(int r = 0; r < pixels; r++) {
    for(int c = 0; c < pixels; c++) {
      image(c, r) = 0;
    }
  }
  
  // coefficients
  for(int i = 0; i < 9; i++) {
    for(int j = 0; j < layers; j++) {
      cff(i,j) = R::runif(-1,1);
    }
  }
  
  // values for initial state
  double x_old = R::runif(-1, 1);
  double y_old = R::runif(-1, 1);
  double z_old = R::runif(-1, 1);
  
  // accumulate
  for(int t = 1; t < iterations; t++) {
    r = rand() % layers; // which transform to use?
    f = rand() % 3;      // which function to use?
    
    // apply transformation
    x = cff(0, r) * x_old + cff(1, r) * y_old + cff(2, r);
    y = cff(3, r) * x_old + cff(4, r) * y_old + cff(5, r);
    z = cff(6, r) * x_old + cff(7, r) * y_old + cff(8, r);
    
    // apply function
    if(f == 0) {
      s = pow(x*x + y*y + z*z, 1/3);
      x = x + s;
      y = y + s;
      z = abs(z + s);
    } else if(f == 1) {
      x = sin(x);
      y = sin(y);
      z = sin(z) + 1;
    } else {
      x = 2 * sin(x);
      y = 2 * sin(y);
      z = 2 * (sin(z) + 1);
    }
    
    // compute indices to be updated
    x_ind = int (x * pixels / (2 * border)) + pixels / 2;
    y_ind = int (y * pixels / (2 * border)) + pixels / 2;
    
    // store results if they fall within the range
    if(x_ind >= 0 & x_ind < pixels) {
      if(y_ind >= 0 & y_ind < pixels) {
        image(x_ind, y_ind) = z;
      }
    }
    
    // move new to old
    x_old = x;
    y_old = y;
    z_old = (z + z_old) / 2; 
  }
  return image;
}
