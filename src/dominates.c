#include <R.h>
#include <Rinternals.h>

#include <stdlib.h>
#include "macros.h"


/*
 * Check dominance relation for two columns of a matrix.
 *
 * @param *points [pointer]
 *   Pointer to the array/matrix.
 * @param col1 [integer(1)]
 *   Index of the first column.
 * @param col2 [integer(1)]
 *   Index of the second column.
 * @param dim [integer(1)]
 *   Dimension of the objective space. I.e., number of rows.
 * @return [integer(1)]
 *    1 if col1 dominates col2
 *    0 if col1 and col2 are incommensurable
 *    -1 if col2 dominates col1
 */
static int dominance(double *points, R_len_t col1, R_len_t col2, R_len_t dim) {
  int dom1 = 0, dom2 = 0;

  // get start of i-th and j-th column
  double *sol1 = points + col1 * dim;
  double *sol2 = points + col2 * dim;

  for (R_len_t i = 0; i < dim; ++i) {
    double val1 = sol1[i];
    double val2 = sol2[i];
    if (val1 < val2) {
      dom2 = 1;
    } else if (val1 > val2) {
      dom1 = 1;
    }
  }
  return(dom2 - dom1);
}

/*
 * Expect a numeric matrix and return a logical vector indicating which columns
 * are dominated and which are not.
 *
 * @param r_points [matrix]
 *   Numeric matrix.
 * @return [logical(n)]
 */
SEXP dominatedC(SEXP r_points) {
  // first unpack R structures
  EXTRACT_NUMERIC_MATRIX(r_points, c_points, dim, n_points);

  // allocate memory for result vector
  // I.e., logical vector: component i is TRUE, if i is dominated by at least
  // one j != i
  SEXP r_res = ALLOC_LOGICAL_VECTOR(n_points);
  int *dominated = LOGICAL(r_res);

  // do not forget to initialize
  for (int i = 0; i < n_points; ++i) {
    dominated[i] = FALSE;
  }

  // now actually check for dominance
  for (int i = 0; i < n_points; ++i) {
    if (dominated[i]) {
      continue;
    }
    for (int j = (i + 1); j < n_points; ++j) {
      if (dominated[j]) {
        continue;
      }
      // check if i dominates j or vice verca
      int isDominated = dominance(c_points, i, j, dim);
      if (isDominated > 0) {
        dominated[j] = TRUE;
      } else if (isDominated < 0) {
        dominated[i] = TRUE;
      }
    }
  }

  UNPROTECT(1);
  return(r_res);
}
