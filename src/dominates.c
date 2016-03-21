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

/*
 * Nondominated sorting algorithm as used in NSGA EMOA.
 *
 * @description
 *  Returns a list of two vectors:
 *  - front (integer): Ranks/front of the corresponding points.
 *  - domcount (integer): Number of points the corresponding points dominates.
 *
 * @param r_points [matrix]
 *   Numeric (n x m) matrix.
 * @return [list]
 */
SEXP doNondominatedSortingC(SEXP r_points) {
  // unwrap R structure into C objects
  EXTRACT_NUMERIC_MATRIX(r_points, c_points, dim, n_points);

  // rank, i.e., number of front
  SEXP r_rank = ALLOC_INTEGER_VECTOR(n_points);
  int* c_rank = INTEGER(r_rank);

  for (int i = 0; i < n_points; ++i) {
    c_rank[i] = 0;
  }

  // count how many points are already correctly sorted
  int n_sorted = 0;

  // count number of how any individuals i is dominated
  SEXP r_domcounter = ALLOC_INTEGER_VECTOR(n_points);
  int* c_domcounter = INTEGER(r_domcounter);

  for (int i = 0; i < n_points; ++i) {
    c_domcounter[i] = 0;
  }

  for (unsigned int i = 0; i < n_points; ++i) {
    for (unsigned int j = (i + 1); j < n_points; ++j) {
      int isDominated = dominance(c_points, i, j, dim);
      if (isDominated > 0) {
        //FIXME: save which point(s) are dominated by which
        ++c_domcounter[j];
      } else if (isDominated < 0) {
        ++c_domcounter[i];
      }
    }

    // assign rank 1 to all points that are nondominated
    if (c_domcounter[i] == 0) {
      c_rank[i] = 1;
      ++n_sorted;
    }
  }

  unsigned int cur_rank = 1;
  while (n_sorted < n_points) {
    //Rprintf("Sorted: %i of %i\n", n_sorted, n_points);
    // iterate over all points and search for the ones with the currently
    // "active" rank
    for (int i = 0; i < n_points; ++i) {
      if (c_rank[i] != cur_rank) {
        continue;
      }
      // otherwise check if current point dominates another one
      for (int j = 0; j < n_points; ++j) {
        //FIXME: replace with non-redundant dominance check
        if (dominance(c_points, i, j, dim) > 0) { // i dominates j
          --c_domcounter[j];
          if (c_domcounter[j] == 0) { /* now on first front */
            c_rank[j] = (cur_rank + 1);
            ++n_sorted;
          }
        }
      }
    }
    ++cur_rank;
  }

  //FIXME: make copy of c_domcounter
  UNPROTECT(1);
  //Free(n_sorted);
  //Free(c_domcounter);
  return(r_rank);
}
