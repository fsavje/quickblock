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

#include "estimator.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <R.h>
#include <Rinternals.h>
#include "error.h"


// =============================================================================
// External function implementations
// =============================================================================

SEXP qbc_est_potential_outcomes(const SEXP R_outcomes,
                                const SEXP R_blocking,
                                const SEXP R_treatments,
                                const SEXP R_num_treatments)
{
	if (!isReal(R_outcomes)) {
		iqbc_error("`R_outcomes` must be numeric.");
	}
	if (!isInteger(R_blocking)) {
		iqbc_error("`R_blocking` must be integer.");
	}
	if (xlength(R_blocking) != xlength(R_outcomes)) {
		iqbc_error("`R_blocking` and `R_outcomes` must be same length.");
	}
	if (!isInteger(getAttrib(R_blocking, install("cluster_count")))) {
		iqbc_error("`R_blocking` is not valid `scclust` object.");
	}
	if (asInteger(getAttrib(R_blocking, install("cluster_count"))) <= 0) {
		iqbc_error("`R_blocking` is empty.");
	}
	if (!isInteger(R_treatments)) {
		iqbc_error("`R_treatments` must be integer.");
	}
	if (xlength(R_treatments) != xlength(R_outcomes)) {
		iqbc_error("`R_treatments` and `R_outcomes` must be same length.");
	}
	if (!isInteger(R_num_treatments)) {
		iqbc_error("`R_num_treatments` must be integer.");
	}
	if (asInteger(R_num_treatments) < 2) {
		iqbc_error("Must be at least two treatment conditions.");
	}


	// R objects to C
	const size_t num_observations = (size_t) xlength(R_outcomes);
	// unblocked units in group 0, the rest `block ID + 1`
	const uint32_t num_blocks = (uint32_t) asInteger(getAttrib(R_blocking, install("cluster_count"))) + 1;
	const double* const outcomes = REAL(R_outcomes);
	const int* const treatments = INTEGER(R_treatments);
	const uint32_t num_treatments = (uint32_t) asInteger(R_num_treatments);
	const int* const blocking = INTEGER(R_blocking);


	// Bounds and sanity checks
	{
		uint32_t treatments_bc = 0;
		uint32_t blocking_bc = 0;
		uint32_t blocked_assigned_bc = 0;
		for (size_t i = 0; i < num_observations; ++i) {
			treatments_bc += (treatments[i] != NA_INTEGER) * ((treatments[i] <= 0) + (treatments[i] > num_treatments));
			blocking_bc += (blocking[i] != NA_INTEGER) * ((blocking[i] < 0) + ((blocking[i] + 1) >= num_blocks));
			blocked_assigned_bc += (blocking[i] != NA_INTEGER) * (treatments[i] == NA_INTEGER);
		}
		if (treatments_bc != 0) {
			iqbc_error("Treatment out of bounds.");
		}
		if (blocking_bc != 0) {
			iqbc_error("Blocking out of bounds.");
		}
		if (blocked_assigned_bc != 0) {
			iqbc_error("All units assigned to blocks must be assigned treatments.");
		}
	}


	// Allocate output variables
	SEXP R_est_means = PROTECT(allocVector(REALSXP, (R_xlen_t) num_treatments));
	SEXP R_est_vars = PROTECT(allocVector(REALSXP, (R_xlen_t) num_treatments));

	const SEXP R_po_obj = PROTECT(allocVector(VECSXP, 2));
	SET_VECTOR_ELT(R_po_obj, 0, R_est_means);
	SET_VECTOR_ELT(R_po_obj, 1, R_est_vars);

	const SEXP R_po_obj_names = PROTECT(allocVector(STRSXP, 2));
	SET_STRING_ELT(R_po_obj_names, 0, mkChar("means"));
	SET_STRING_ELT(R_po_obj_names, 1, mkChar("vars"));
	setAttrib(R_po_obj, R_NamesSymbol, R_po_obj_names);

	double* const est_means = REAL(R_est_means);
	double* const est_vars = REAL(R_est_vars);
	for (size_t t = 0; t < num_treatments; ++t) {
		est_means[t] = 0;
		est_vars[t] = 0;
	}


	// Allocate working memory
	uint64_t num_blocked_units = 0;
	uint32_t* const block_size = calloc(num_blocks, sizeof(uint32_t));
	uint32_t* const treatment_count = calloc(num_blocks * num_treatments, sizeof(uint32_t));
	double* const outcome_sum = calloc(num_blocks * num_treatments, sizeof(double));
	double* const outcome_sq_sum = calloc(num_blocks * num_treatments, sizeof(double));

	if (block_size == NULL ||
			treatment_count == NULL ||
			outcome_sum == NULL ||
			outcome_sq_sum == NULL) {
		free(block_size);
		free(treatment_count);
		free(outcome_sum);
		free(outcome_sq_sum);
		iqbc_error("Out of memory.");
	}


	// Get block info
	for (size_t i = 0; i < num_observations; ++i) {
		const bool unit_blocked = (blocking[i] != NA_INTEGER);
		num_blocked_units += unit_blocked;
		const size_t block_index = unit_blocked * (blocking[i] + 1);
		++block_size[block_index];
		// This avoids the interger overflow from `(treatments[i] != NA_INTEGER) * (treatments[i] - 1)`
		const size_t treatment_index = (treatments[i] != NA_INTEGER) * treatments[i] - (treatments[i] != NA_INTEGER);
		const size_t array_index = block_index * num_treatments + treatment_index;
		++treatment_count[array_index];
		outcome_sum[array_index] += outcomes[i];
		outcome_sq_sum[array_index] += outcomes[i] * outcomes[i];
	}


	// Check so sufficient units in blocks
	bool estimate_var = true;
	{
		uint64_t block_treat_zero = 0;
		uint64_t block_treat_one = 0;
		for (size_t b = 1; b < num_blocks; ++b) {
			for (size_t t = 0; t < num_treatments; ++t) {
				const size_t array_index = b * num_treatments + t;
				block_treat_zero += (block_size[b] != 0) * (treatment_count[array_index] == 0);
				block_treat_one += (treatment_count[array_index] == 1);
			}
		}
		if (block_treat_zero != 0) {
			iqbc_error("All treatments must be respresented in each block.");
		}
		if (block_treat_one != 0) {
			estimate_var = false;
			warning("Variance estimation requires that all blocks contain at least two units assigned to each treatment condition.");
		}
	}


	// Calculate estimators
	uint32_t unbalanced_assignments = 0;
	const double n = (double) num_blocked_units;
	const double r = (double) num_treatments;
	for (size_t b = 1; b < num_blocks; ++b) {
		const uint32_t n_c_int = block_size[b];
		if (n_c_int > 0) {
			const uint32_t z_c_int = n_c_int % num_treatments;
			iqbc_assert(n_c_int - z_c_int > 0);
			const uint32_t min_treated = (n_c_int - z_c_int) / num_treatments; // floor(n_c / r)
			const double n_c = (double) n_c_int;
			const double z_c = (double) z_c_int;
			const double mean_weight = n_c / n;
			const double var_weight = mean_weight * (r + ((r * z_c * (r - z_c)) / ((n_c - z_c) * (n_c + r - z_c))));
			/*
				(n_c^2 / n^2) * (r / (n_c - 1) + (r * z_c * (r - z_c)) / ((n_c - 1) * (n_c - z_c) * (n_c + r - z_c))) * (((n_c - 1) / n_c) * (sigma_1 + sigma_2))
				(n_c / n^2) * (r + (r * z_c * (r - z_c)) / ((n_c - z_c) * (n_c + r - z_c))) * (sigma_1 + sigma_2)
				(1 / n) * mean_weight * (r + (r * z_c * (r - z_c)) / ((n_c - z_c) * (n_c + r - z_c))) * (sigma_1 + sigma_2)
				(1 / n) * var_weight * (sigma_c' + sigma_c'')
			*/

			for (size_t t = 0; t < num_treatments; ++t) {
				const size_t array_index = b * num_treatments + t;
				const uint32_t num_treated_c_int = treatment_count[array_index];
				iqbc_assert(num_treated_c_int > 0);
				unbalanced_assignments += (num_treated_c_int != min_treated) * (num_treated_c_int != (min_treated + 1));
				const double num_treated_c = (double) num_treated_c_int;
				const double mu_c = outcome_sum[array_index] / num_treated_c;
				// sigma in note = ((n_c - 1) / n_c) * sigma_c
				const double sigma_c = (outcome_sq_sum[array_index] - num_treated_c * mu_c * mu_c) / (num_treated_c - estimate_var);

				est_means[t] += mean_weight * mu_c;
				est_vars[t] += var_weight * sigma_c;
			}
		}
	}

	if (unbalanced_assignments != 0) {
		estimate_var = false;
		warning("Treatment assignment is unbalanced, variance cannot be estimated.");
	}

	if (estimate_var) {
		for (size_t t = 0; t < num_treatments; ++t) {
			est_vars[t] /= n;
		}
	} else {
		for (size_t t = 0; t < num_treatments; ++t) {
			est_vars[t] = NA_REAL;
		}
	}


	free(block_size);
	free(treatment_count);
	free(outcome_sum);
	free(outcome_sq_sum);

	UNPROTECT(4);
	return R_po_obj;
}
