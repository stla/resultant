#include "resultant.h"

std::string q2str(CGAL::Gmpq r) {
  CGAL::Gmpz numer = r.numerator();
  CGAL::Gmpz denom = r.denominator();
  size_t n = mpz_sizeinbase(numer.mpz(), 10) + 2;
  size_t d = mpz_sizeinbase(denom.mpz(), 10) + 2;
  char* cnumer = new char[n];
  char* cdenom = new char[d];
  cnumer = mpz_get_str(cnumer, 10, numer.mpz());
  cdenom = mpz_get_str(cdenom, 10, denom.mpz());
  std::string snumer = cnumer;
  std::string sdenom = cdenom;
  delete[] cnumer;
  delete[] cdenom;
  return snumer + "/" + sdenom;
}

Poly1 makePoly1(
    Rcpp::IntegerVector Powers, Rcpp::CharacterVector Coeffs
) {
  PT1::Construct_polynomial constructPolynomial;
  int nterms = Coeffs.size();
  std::list<Monomial1> terms;
  for(int i = 0; i < nterms; i++) {
    terms.push_back(
      std::make_pair(
        CGAL::Exponent_vector(Powers(i)),
        CGAL::Gmpq(Rcpp::as<std::string>(Coeffs(i)))
      )
    );
  }
  return constructPolynomial(terms.begin(), terms.end());
}

// [[Rcpp::export]]
Rcpp::CharacterVector resultantCPP1(
  Rcpp::IntegerVector PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerVector PowersG, Rcpp::CharacterVector CoeffsG
) {
  Poly1 F = makePoly1(PowersF, CoeffsF);
  Poly1 G = makePoly1(PowersG, CoeffsG);
  PT1::Resultant resultant;
  CGAL::Gmpq r = resultant(F, G);
  std::cout << "The resultant of F and G is: " << r << std::endl;
  Rcpp::CharacterVector out = Rcpp::CharacterVector::create(q2str(r));
  return out;
}

Poly2 makePoly2(
    Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs
) {
  PT2::Construct_polynomial constructPolynomial;
  int nterms = Coeffs.size();
  std::list<Monomial2> terms;
  for(int i = 0; i < nterms; i++) {
    Rcpp::IntegerVector powers = Powers(Rcpp::_, i);
    terms.push_back(
      std::make_pair(
        CGAL::Exponent_vector(powers.begin(), powers.end()),
        CGAL::Gmpq(Rcpp::as<std::string>(Coeffs(i)))
      )
    );
  }
  return constructPolynomial(terms.begin(), terms.end());
}

// [[Rcpp::export]]
Rcpp::CharacterVector resultantCPP2(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG
) {
  CGAL::IO::set_pretty_mode(std::cout);

  Poly2 F = makePoly2(PowersF, CoeffsF);
  Poly2 G = makePoly2(PowersG, CoeffsG);
  PT2::Resultant resultant;
  std::cout << "The resultant of F and G is: " << resultant(F, G) << std::endl;
  Poly1 R = resultant(F, G);
  PT1::Degree degree;
  int d = degree(R);
  PT1::Get_coefficient getCoefficient;
  Rcpp::CharacterVector Coeffs(d + 1);
  for(int i = 0; i <= d; i++) {
    Coeffs(i) = q2str(getCoefficient(R, i));
  }
  return Coeffs;
}
