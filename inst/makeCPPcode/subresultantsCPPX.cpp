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

