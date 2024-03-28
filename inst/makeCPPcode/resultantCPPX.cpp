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

