#include <R.h>
#include <Rinternals.h>

#include "macros.h"
#include "hv.h"

// Interface to hypervolume algorithm by Fonseca et al.
//
// @param r_points [matrix]
//   Matrix of points.
// @param ref.point [numeric}]
//   Reference point.
// @return [numeric(1)] Dominated hypervolume.
SEXP computeDominatedHypervolumeC(SEXP r_points, SEXP r_ref_point) {
  // unpack R data
  EXTRACT_NUMERIC_MATRIX(r_points, c_points, dim, n_points);
  EXTRACT_NUMERIC_VECTOR(r_ref_point, c_ref_point, len_ref);

  // allocate memory for Hypervolume value
  SEXP r_hv = ALLOC_REAL_VECTOR(1);
  double *c_hv = REAL(r_hv);

  // call Fonseca et al. algorithm
  c_hv[0] = fpli_hv(c_points, dim, n_points, c_ref_point);

  // free memory and return
  UNPROTECT(1);
  return r_hv;
}
