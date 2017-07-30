/* =============================================================================
 * quickblock -- Quick Threshold Blocking
 * https://github.com/fsavje/quickblock
 *
 * Copyright (C) 2017  Fredrik Savje, Jasjeet Sekhon, Michael Higgins
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see http://www.gnu.org/licenses/
 * ========================================================================== */

#ifndef QBC_ESTIMATOR_HG
#define QBC_ESTIMATOR_HG

#include <R.h>
#include <Rinternals.h>

SEXP qbc_est_potential_outcomes(SEXP R_outcomes,
                                SEXP R_blocking,
                                SEXP R_treatments,
                                SEXP R_num_treatments);

#endif // ifndef QBC_ESTIMATOR_HG
