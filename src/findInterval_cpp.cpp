#include <algorithm>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector findInterval_cpp(NumericVector x, NumericVector breaks) {
  IntegerVector out(x.size());

  NumericVector::iterator it, pos;
  IntegerVector::iterator out_it;

  // Compute beginning, and end of breaks
  NumericVector::iterator bbgn = breaks.begin();
  NumericVector::iterator bend = breaks.end();

  // Compute beginning, and end of x
  NumericVector::iterator xbgn = x.begin();
  NumericVector::iterator xend = x.end();

  // Compute beginning of out
  IntegerVector::iterator obgn = out.begin();

  for(it = xbgn, out_it = obgn; it != xend;
  ++it, ++out_it) {
    pos = std::upper_bound(bbgn, bend, *it);
    *out_it = std::distance(bbgn, pos);
  }

  return out;
}
