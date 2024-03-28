// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List gcdCPP4(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG
) {
  return gcdCPPX<Poly4, PT4, Monomial4, 4>(PowersF, CoeffsF, PowersG, CoeffsG);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List gcdCPP5(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG
) {
  return gcdCPPX<Poly5, PT5, Monomial5, 5>(PowersF, CoeffsF, PowersG, CoeffsG);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List gcdCPP6(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG
) {
  return gcdCPPX<Poly6, PT6, Monomial6, 6>(PowersF, CoeffsF, PowersG, CoeffsG);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List gcdCPP7(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG
) {
  return gcdCPPX<Poly7, PT7, Monomial7, 7>(PowersF, CoeffsF, PowersG, CoeffsG);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List gcdCPP8(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG
) {
  return gcdCPPX<Poly8, PT8, Monomial8, 8>(PowersF, CoeffsF, PowersG, CoeffsG);
}

// -------------------------------------------------------------------------- //
// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List gcdCPP9(
    Rcpp::IntegerMatrix PowersF, Rcpp::CharacterVector CoeffsF,
    Rcpp::IntegerMatrix PowersG, Rcpp::CharacterVector CoeffsG
) {
  return gcdCPPX<Poly9, PT9, Monomial9, 9>(PowersF, CoeffsF, PowersG, CoeffsG);
}

