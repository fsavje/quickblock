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

#include <R_ext/Rdynload.h>
#include "assign_treatment.h"
#include "estimator.h"

static const R_CallMethodDef callMethods[] = {
	{"qbc_assign_treatments",       (DL_FUNC) &qbc_assign_treatments,       2},
	{"qbc_est_potential_outcomes",  (DL_FUNC) &qbc_est_potential_outcomes,  4},
	{NULL,                          NULL,                                   0}
};

void R_init_quickblock(DllInfo *info) {
	R_registerRoutines(info, NULL, callMethods, NULL, NULL);
	R_useDynamicSymbols(info, FALSE);
}
