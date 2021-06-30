#include <Rcpp.h>
#include <math.h>
#include <iostream>
#include <random>
using namespace Rcpp;;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
int cpp_geom(double p) {
  
  std::random_device rd;
  std::default_random_engine generator(rd());
  std::uniform_real_distribution<double> distribution(0.0,1.0);
  int t = 1;
  
  while(distribution(generator) > p) {
    t++;
  }
  return t;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
cpp_geom(0.1)
*/
