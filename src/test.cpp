#include <Rcpp.h>
using namespace Rcpp;

// // [[Rcpp::export]]
// void test(NumericMatrix x, NumericVector y){
//   CharacterVector xc = colnames(x);
//   CharacterVector yn = y.names();
//   IntegerVector m = match(xc,yn)-1;
//   Rcout << m << "\n";
//   // Rcout << x.row(0) << "\n";
//   NumericVector xr = x["a"];
//   Rcout << xr << "\n";
//   // Rcout << match(x.row(0),y);
// }

// // [[Rcpp::export]]
// void f(CharacterVector y, CharacterVector i){
//   bool a = is_true(any(y==i));
//   Rcout << a;
// }
// [[Rcpp::export]]
NumericVector f(NumericVector x){
  Function my_fun("my_fun");
  NumericVector out = my_fun(x);
  return out;
}


/*** R
x = 1:10
my_fun = function(x) sum(x^2)
f(x)
# f(LETTERS[2:10],"B")
# X = matrix(1:12,4)
# rownames(X) = LETTERS[1:4]
# colnames(X) = letters[1:3]
# y = c(a=3,b=4,d=5)
# test(X,y)
# X
# y
# library(microbenchmark)
# microbenchmark(
# )
*/
