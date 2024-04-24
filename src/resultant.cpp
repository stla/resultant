#include "resultant.h"

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
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

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
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

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
template <typename PolyX, typename PTX, typename MonomialX>
PolyX makePolyX(
  Rcpp::IntegerMatrix Powers, Rcpp::CharacterVector Coeffs
) {
  typename PTX::Construct_polynomial constructPolynomial;
  int nterms = Coeffs.size();
  std::list<MonomialX> terms;
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

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
template <typename PolyX, typename PTX, typename MonomialX>
Rcpp::List getPolynomial(
  PolyX P, int X
) {
  std::list<MonomialX> monomials;
  typename PTX::Monomial_representation mrepr;
  mrepr(P, std::back_inserter(monomials));
  int n = monomials.size();
  Rcpp::IntegerMatrix Powers(X, n);
  Rcpp::CharacterVector Coeffs(n);
  typename std::list<MonomialX>::iterator it_monoms;
  int i = 0;
  for(it_monoms = monomials.begin(); it_monoms != monomials.end(); it_monoms++) {
    CGAL::Exponent_vector exponents = (*it_monoms).first;
    Rcpp::IntegerVector powers(exponents.begin(), exponents.end());
    Powers(Rcpp::_, i) = powers;
    Coeffs(i) = q2str((*it_monoms).second);
    i++;
  }
  return Rcpp::List::create(Rcpp::Named("Powers") = Powers,
                            Rcpp::Named("Coeffs") = Coeffs);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::CharacterVector resultantCPP1(
  Rcpp::IntegerVector PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerVector PowersG, Rcpp::CharacterVector CoeffsG
) {
  Poly1 F = makePoly1(PowersF, CoeffsF);
  Poly1 G = makePoly1(PowersG, CoeffsG);
  PT1::Resultant resultant;
  CGAL::Gmpq r = resultant(F, G);
  Rcpp::CharacterVector out = Rcpp::CharacterVector::create(q2str(r));
  return out;
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::CharacterVector resultantCPP2(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    bool permute
) {
  Poly2 F = makePolyX<Poly2, PT2, Monomial2>(PowersF, CoeffsF);
  Poly2 G = makePolyX<Poly2, PT2, Monomial2>(PowersG, CoeffsG);
  if(permute) {
    PT2::Swap swap;
    F = swap(F, 0, 1);
    G = swap(G, 0, 1);
  }
  PT2::Resultant resultant;
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

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List resultantCPP3(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    Rcpp::IntegerVector permutation
) {
  Poly3 F = makePolyX<Poly3, PT3, Monomial3>(PowersF, CoeffsF);
  Poly3 G = makePolyX<Poly3, PT3, Monomial3>(PowersG, CoeffsG);
  PT3::Permute permute;
  F = permute(F, permutation.begin(), permutation.end());
  G = permute(G, permutation.begin(), permutation.end());
  PT3::Resultant resultant;
  Poly2 R = resultant(F, G);
  return getPolynomial<Poly2, PT2, Monomial2>(R, 2);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List resultantCPP4(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    Rcpp::IntegerVector permutation
) {
  Poly4 F = makePolyX<Poly4, PT4, Monomial4>(PowersF, CoeffsF);
  Poly4 G = makePolyX<Poly4, PT4, Monomial4>(PowersG, CoeffsG);
  PT4::Permute permute;
  F = permute(F, permutation.begin(), permutation.end());
  G = permute(G, permutation.begin(), permutation.end());
  PT4::Resultant resultant;
  Poly3 R = resultant(F, G);
  return getPolynomial<Poly3, PT3, Monomial3>(R, 3);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List resultantCPP5(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    Rcpp::IntegerVector permutation
) {
  Poly5 F = makePolyX<Poly5, PT5, Monomial5>(PowersF, CoeffsF);
  Poly5 G = makePolyX<Poly5, PT5, Monomial5>(PowersG, CoeffsG);
  PT5::Permute permute;
  F = permute(F, permutation.begin(), permutation.end());
  G = permute(G, permutation.begin(), permutation.end());
  PT5::Resultant resultant;
  Poly4 R = resultant(F, G);
  return getPolynomial<Poly4, PT4, Monomial4>(R, 4);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List resultantCPP6(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    Rcpp::IntegerVector permutation
) {
  Poly6 F = makePolyX<Poly6, PT6, Monomial6>(PowersF, CoeffsF);
  Poly6 G = makePolyX<Poly6, PT6, Monomial6>(PowersG, CoeffsG);
  PT6::Permute permute;
  F = permute(F, permutation.begin(), permutation.end());
  G = permute(G, permutation.begin(), permutation.end());
  PT6::Resultant resultant;
  Poly5 R = resultant(F, G);
  return getPolynomial<Poly5, PT5, Monomial5>(R, 5);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List resultantCPP7(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    Rcpp::IntegerVector permutation
) {
  Poly7 F = makePolyX<Poly7, PT7, Monomial7>(PowersF, CoeffsF);
  Poly7 G = makePolyX<Poly7, PT7, Monomial7>(PowersG, CoeffsG);
  PT7::Permute permute;
  F = permute(F, permutation.begin(), permutation.end());
  G = permute(G, permutation.begin(), permutation.end());
  PT7::Resultant resultant;
  Poly6 R = resultant(F, G);
  return getPolynomial<Poly6, PT6, Monomial6>(R, 6);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List resultantCPP8(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    Rcpp::IntegerVector permutation
) {
  Poly8 F = makePolyX<Poly8, PT8, Monomial8>(PowersF, CoeffsF);
  Poly8 G = makePolyX<Poly8, PT8, Monomial8>(PowersG, CoeffsG);
  PT8::Permute permute;
  F = permute(F, permutation.begin(), permutation.end());
  G = permute(G, permutation.begin(), permutation.end());
  PT8::Resultant resultant;
  Poly7 R = resultant(F, G);
  return getPolynomial<Poly7, PT7, Monomial7>(R, 7);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List resultantCPP9(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    Rcpp::IntegerVector permutation
) {
  Poly9 F = makePolyX<Poly9, PT9, Monomial9>(PowersF, CoeffsF);
  Poly9 G = makePolyX<Poly9, PT9, Monomial9>(PowersG, CoeffsG);
  PT9::Permute permute;
  F = permute(F, permutation.begin(), permutation.end());
  G = permute(G, permutation.begin(), permutation.end());
  PT9::Resultant resultant;
  Poly8 R = resultant(F, G);
  return getPolynomial<Poly8, PT8, Monomial8>(R, 8);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
template 
  <typename PolyX, typename PTX, typename MonomialX, 
   typename PolyW, typename PTW, typename MonomialW, int W>
Rcpp::List subresultantsCPPX(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  Rcpp::IntegerVector permutation
) {
  PolyX F = makePolyX<PolyX, PTX, MonomialX>(PowersF, CoeffsF);
  PolyX G = makePolyX<PolyX, PTX, MonomialX>(PowersG, CoeffsG);
  typename PTX::Permute permute;
  F = permute(F, permutation.begin(), permutation.end());
  G = permute(G, permutation.begin(), permutation.end());
  std::vector<PolyW> subresultants;
  CGAL::principal_subresultants(F, G, std::back_inserter(subresultants));
  int n = subresultants.size();
  Rcpp::List out(n);
  for(int i = 0; i < n ; i++) {
    out(i) = getPolynomial<PolyW, PTW, MonomialW>(subresultants[i], W);
  }
  return out;
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::CharacterVector subresultantsCPP1(
  Rcpp::IntegerVector PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerVector PowersG, Rcpp::CharacterVector CoeffsG
) {
  Poly1 F = makePoly1(PowersF, CoeffsF);
  Poly1 G = makePoly1(PowersG, CoeffsG);
  std::vector<CGAL::Gmpq> subresultants;
  CGAL::principal_subresultants(F, G, std::back_inserter(subresultants));
  int n = subresultants.size();
  Rcpp::CharacterVector out(n);
  for(int i = 0; i < n ; i++) {
    out(i) = q2str(subresultants[i]);
  }
  return out;
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List subresultantsCPP2(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  bool permute
) {
  Poly2 F = makePolyX<Poly2, PT2, Monomial2>(PowersF, CoeffsF);
  Poly2 G = makePolyX<Poly2, PT2, Monomial2>(PowersG, CoeffsG);
  if(permute) {
    PT2::Swap swap;
    F = swap(F, 0, 1);
    G = swap(G, 0, 1);
  }
  std::vector<Poly1> subresultants;
  CGAL::principal_subresultants(F, G, std::back_inserter(subresultants));
  int n = subresultants.size();
  Rcpp::List out(n);
  for(int i = 0; i < n ; i++) {
    out(i) = getPolynomial<Poly1, PT1, Monomial1>(subresultants[i], 1);
  }
  return out;
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List subresultantsCPP3(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  Rcpp::IntegerVector permutation
) {
  return subresultantsCPPX<Poly3, PT3, Monomial3, Poly2, PT2, Monomial2, 2>(
    PowersF, CoeffsF, PowersG, CoeffsG, permutation
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List subresultantsCPP4(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  Rcpp::IntegerVector permutation
) {
  return subresultantsCPPX<Poly4, PT4, Monomial4, Poly3, PT3, Monomial3, 3>(
    PowersF, CoeffsF, PowersG, CoeffsG, permutation
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List subresultantsCPP5(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  Rcpp::IntegerVector permutation
) {
  return subresultantsCPPX<Poly5, PT5, Monomial5, Poly4, PT4, Monomial4, 4>(
    PowersF, CoeffsF, PowersG, CoeffsG, permutation
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List subresultantsCPP6(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  Rcpp::IntegerVector permutation
) {
  return subresultantsCPPX<Poly6, PT6, Monomial6, Poly5, PT5, Monomial5, 5>(
    PowersF, CoeffsF, PowersG, CoeffsG, permutation
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List subresultantsCPP7(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  Rcpp::IntegerVector permutation
) {
  return subresultantsCPPX<Poly7, PT7, Monomial7, Poly6, PT6, Monomial6, 6>(
    PowersF, CoeffsF, PowersG, CoeffsG, permutation
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List subresultantsCPP8(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  Rcpp::IntegerVector permutation
) {
  return subresultantsCPPX<Poly8, PT8, Monomial8, Poly7, PT7, Monomial7, 7>(
    PowersF, CoeffsF, PowersG, CoeffsG, permutation
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List subresultantsCPP9(
  Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
  Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
  Rcpp::IntegerVector permutation
) {
  return subresultantsCPPX<Poly9, PT9, Monomial9, Poly8, PT8, Monomial8, 8>(
    PowersF, CoeffsF, PowersG, CoeffsG, permutation
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List gcdCPP1(
    Rcpp::IntegerVector PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerVector PowersG, Rcpp::CharacterVector CoeffsG,
    bool UTCF
) {
  Poly1 F = makePoly1(PowersF, CoeffsF);
  Poly1 G = makePoly1(PowersG, CoeffsG);
  Poly1 D;
  if(UTCF) {
    typename PT1::Gcd_up_to_constant_factor gcd_utcf;
    D = gcd_utcf(F, G);  
  } else {
    typename PT1::Gcd gcd;
    D = gcd(F, G);
  }
  return getPolynomial<Poly1, PT1, Monomial1>(D, 1);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
template <typename PolyX, typename PTX, typename MonomialX, int X>
Rcpp::List gcdCPPX(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    bool UTCF
) {
  PolyX F = makePolyX<PolyX, PTX, MonomialX>(PowersF, CoeffsF);
  PolyX G = makePolyX<PolyX, PTX, MonomialX>(PowersG, CoeffsG);
  PolyX D;
  if(UTCF) {
    typename PTX::Gcd_up_to_constant_factor gcd_utcf;
    D = gcd_utcf(F, G);  
  } else {
    typename PTX::Gcd gcd;
    D = gcd(F, G);
  }
  return getPolynomial<PolyX, PTX, MonomialX>(D, X);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List gcdCPP2(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    bool UTCF
) {
  return gcdCPPX<Poly2, PT2, Monomial2, 2>(
    PowersF, CoeffsF, PowersG, CoeffsG, UTCF
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List gcdCPP3(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    bool UTCF
) {
  return gcdCPPX<Poly3, PT3, Monomial3, 3>(
    PowersF, CoeffsF, PowersG, CoeffsG, UTCF
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List gcdCPP4(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    bool UTCF
) {
  return gcdCPPX<Poly4, PT4, Monomial4, 4>(
    PowersF, CoeffsF, PowersG, CoeffsG, UTCF
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List gcdCPP5(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    bool UTCF
) {
  return gcdCPPX<Poly5, PT5, Monomial5, 5>(
    PowersF, CoeffsF, PowersG, CoeffsG, UTCF
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List gcdCPP6(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    bool UTCF
) {
  return gcdCPPX<Poly6, PT6, Monomial6, 6>(
    PowersF, CoeffsF, PowersG, CoeffsG, UTCF
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List gcdCPP7(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    bool UTCF
) {
  return gcdCPPX<Poly7, PT7, Monomial7, 7>(
    PowersF, CoeffsF, PowersG, CoeffsG, UTCF
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List gcdCPP8(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    bool UTCF
) {
  return gcdCPPX<Poly8, PT8, Monomial8, 8>(
    PowersF, CoeffsF, PowersG, CoeffsG, UTCF
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List gcdCPP9(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG,
    bool UTCF
) {
  return gcdCPPX<Poly9, PT9, Monomial9, 9>(
    PowersF, CoeffsF, PowersG, CoeffsG, UTCF
  );
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
int numberOfRealRootsCPP(
  Rcpp::IntegerVector Powers, Rcpp::CharacterVector Coeffs
) {
  Poly1 P = makePoly1(Powers, Coeffs);
  return CGAL::number_of_real_roots(P);
}