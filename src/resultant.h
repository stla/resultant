#ifndef __HEADER__
#define __HEADER__

#include <Rcpp.h>

#define CGAL_EIGEN3_ENABLED 1

#include "gmp.h"
#include <CGAL/Gmpq.h>
#include <CGAL/Gmpz.h>
#include <CGAL/Polynomial.h>
#include <CGAL/Polynomial_traits_d.h>
#include <CGAL/Polynomial_type_generator.h>
#include <CGAL/polynomial_utils.h>
#include <CGAL/Polynomial/Monomial_representation.h>

typedef CGAL::Polynomial_type_generator<CGAL::Gmpq, 1>::Type Poly1;
typedef CGAL::Polynomial_traits_d<Poly1>              PT1;
typedef std::pair<CGAL::Exponent_vector, PT1::Innermost_coefficient_type> Monomial1;

#endif
