# ==============================================================================
# quickblock -- Quick Threshold Blocking
# https://github.com/fsavje/quickblock
#
# Copyright (C) 2017  Fredrik Savje -- http://fredriksavje.com
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see http://www.gnu.org/licenses/
# ==============================================================================

library(quickblock)
context("Input checking in C code")

# ==============================================================================
# assign_treatment.c
# ==============================================================================

t_qbc_assign_treatments <- function(blocking = qb_blocking(c("A", "A", "B", "C", "B", "C", "C", "A", "B", "B", "C")),
                                    treatment_conditions = c(1L, 2L)) {
  .Call(qbc_assign_treatments,
        blocking,
        treatment_conditions)
}

test_that("`qbc_assign_treatments` checks input.", {
  expect_silent(t_qbc_assign_treatments())
  expect_error(t_qbc_assign_treatments(blocking = letters[1:11]),
               regexp = "`R_blocking` must be integer.")
  expect_error(t_qbc_assign_treatments(blocking = 1:11),
               regexp = "`R_blocking` is not valid `scclust` object.")
  expect_error(t_qbc_assign_treatments(blocking = structure(1:11, "cluster_count" = 0L, class = c("qb_blocking", "scclust"))),
               regexp = "`R_blocking` is empty.")
  expect_error(t_qbc_assign_treatments(treatment_conditions = letters[1:2]),
               regexp = "`R_treatment_conditions` must be integer.")
  expect_error(t_qbc_assign_treatments(treatment_conditions = 1L),
               regexp = "Must be at least two treatment conditions.")
  expect_error(t_qbc_assign_treatments(blocking = structure(c(0L, 1L, 1L, 0L, 2L), "cluster_count" = 2L, class = c("qb_blocking", "scclust"))),
               regexp = "Blocking out of bounds.")
  expect_error(t_qbc_assign_treatments(blocking = structure(c(0L, 1L, -1L, 0L, 1L), "cluster_count" = 2L, class = c("qb_blocking", "scclust"))),
               regexp = "Blocking out of bounds.")
  expect_warning(t_qbc_assign_treatments(treatment_conditions = c(1L, 2L, 3L, 4L)),
               regexp = "Some blocks contain less units than the number of treatment conditions.")
})
