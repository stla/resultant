#include "resultant.h"

Poly1 makePoly1(
    Rcpp::IntegerVector Powers, Rcpp::IntegerVector Coeffs
) {

  PT1::Construct_polynomial construct_polynomial;

  int nterms = Coeffs.size();
  std::list<Monomial1> terms;

  for(int i = 0; i < nterms; i++) {
    terms.push_back(
      std::make_pair(
        CGAL::Exponent_vector(Powers(i)),
        Coeffs(i)
      )
    );
  }

  return construct_polynomial(terms.begin(), terms.end());
}

// [[Rcpp::export]]
void resultantCPP(
  Rcpp::IntegerVector PowersF, Rcpp::IntegerVector CoeffsF,
  Rcpp::IntegerVector PowersG, Rcpp::IntegerVector CoeffsG
) {
  Poly1 F = makePoly1(PowersF, CoeffsF);
  Poly1 G = makePoly1(PowersG, CoeffsG);
  // Resultant computation:
  PT1::Resultant resultant;
  std::cout << "The resultant of F and G is: " << resultant(F,G) << std::endl;
}
