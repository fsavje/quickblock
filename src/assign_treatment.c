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

#include "assign_treatment.h"
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <R.h>
#include <Rinternals.h>
#include "error.h"


// =============================================================================
// Internal function implementations
// =============================================================================

static inline void shuffle(int* array,
                           uint32_t len_array,
                           const uint32_t num_to_shuffle) {
	const int* const stop_array = array + num_to_shuffle;
	for (; array != stop_array; ++array, --len_array) {
		uint32_t pick = (uint32_t) (unif_rand() * ((double) len_array));
		int tmp = array[0];
		array[0] = array[pick];
		array[pick] = tmp;
	}
}


// =============================================================================
// External function implementations
// =============================================================================

SEXP qbc_assign_treatments(const SEXP R_blocking,
                           const SEXP R_num_treatments)
{
	if (!isInteger(R_blocking)) {
		iqbc_error("`R_blocking` must be integer.");
	}
	if (!isInteger(getAttrib(R_blocking, install("cluster_count")))) {
		iqbc_error("`R_blocking` is not valid `scclust` object.");
	}
	if (asInteger(getAttrib(R_blocking, install("cluster_count"))) <= 0) {
		iqbc_error("`R_blocking` is empty.");
	}
	if (!isInteger(R_num_treatments)) {
		iqbc_error("`R_num_treatments` must be integer.");
	}
	if (asInteger(R_num_treatments) < 2) {
		iqbc_error("Must be at least two treatment conditions.");
	}

	// R objects to C
	const size_t num_observations = (size_t) xlength(R_blocking);
	// unblocked units in group 0, the rest `block ID + 1`
	const uint32_t num_blocks = (uint32_t) asInteger(getAttrib(R_blocking, install("cluster_count"))) + 1;
	const int* const blocking = INTEGER(R_blocking);
	const uint32_t num_treatments = (uint32_t) asInteger(R_num_treatments);

	// Bounds and sanity checks
	{
		uint32_t blocking_bc = 0;
		for (size_t i = 0; i < num_observations; ++i) {
			blocking_bc += (blocking[i] != NA_INTEGER) * ((blocking[i] < 0) + ((blocking[i] + 1) >= num_blocks));
		}
		if (blocking_bc != 0) {
			iqbc_error("Blocking out of bounds.");
		}
	}

	SEXP R_treatment_assignment = PROTECT(allocVector(INTSXP, (R_xlen_t) num_observations));
	int* treatment_assignment = INTEGER(R_treatment_assignment);

	// Allocate working memory
	uint32_t* const block_size = calloc(num_blocks, sizeof(uint32_t));
	int* const extra_scratch = malloc(sizeof(int[num_treatments]));
	int* const treatment_scratch = malloc(sizeof(int[num_observations]));
	int** const block_treatments = malloc(sizeof(int*[num_blocks]));

	if (block_size == NULL ||
			extra_scratch == NULL ||
			treatment_scratch == NULL ||
			block_treatments == NULL) {
		free(block_size);
		free(extra_scratch);
		free(treatment_scratch);
		free(block_treatments);
		iqbc_error("Out of memory.");
	}

	for (size_t i = 0; i < num_observations; ++i) {
		++block_size[(blocking[i] != NA_INTEGER) * (blocking[i] + 1)];
	}

	GetRNGstate();

	int* treatment_pointer = treatment_scratch;
	uint32_t size_block_warning = 0;
	for (uint32_t b = 1; b < num_blocks; ++b) {
		const uint32_t b_size = block_size[b];
		if (b_size > 0) {
			size_block_warning += (b_size < num_treatments);
			block_treatments[b] = treatment_pointer;
			const uint32_t num_extra = b_size % num_treatments;
			const uint32_t closest_multiple = b_size - num_extra;
			iqbc_assert(closest_multiple + num_extra == b_size);
			iqbc_assert(num_treatments * (b_size / num_treatments) == closest_multiple);

			for (int i = 0; i < closest_multiple; ++i, ++treatment_pointer) {
				*treatment_pointer = (i % num_treatments) + 1;
			}

			if (num_extra > 0) {
				for (int i = 0; i < num_treatments; ++i) {
					extra_scratch[i] = i + 1;
				}
				shuffle(extra_scratch, num_treatments, num_extra);
				for (uint32_t i = 0; i < num_extra; ++i, ++treatment_pointer) {
					*treatment_pointer = extra_scratch[i];
				}
			}

			iqbc_assert(block_treatments[b] + b_size == treatment_pointer);

			shuffle(block_treatments[b], b_size, b_size - 1);
		}
	}

	PutRNGstate();

	if (size_block_warning > 0) {
		warning("Some blocks contain fewer units than the number of treatment conditions.");
	}

	for (size_t i = 0; i < num_observations; ++i) {
		if (blocking[i] == NA_INTEGER) {
			treatment_assignment[i] = NA_INTEGER;
		} else {
			treatment_assignment[i] = *block_treatments[blocking[i] + 1];
			++block_treatments[blocking[i] + 1];
		}
	}

	free(block_size);
	free(extra_scratch);
	free(treatment_scratch);
	free(block_treatments);

	UNPROTECT(1);
	return R_treatment_assignment;
}
