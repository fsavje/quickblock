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
context("Input checking in R code")


# ==============================================================================
# new_error & new_warning
# ==============================================================================

t_new_error <- function(...) {
  temp_func <- function(...) {
    new_error(...)
  }
  temp_func(...)
}

t_new_warning <- function(...) {
  temp_func <- function(...) {
    new_warning(...)
  }
  temp_func(...)
}

test_that("`new_error` & `new_warning` make warnings and errors.", {
  expect_error(t_new_error("This is an error."),
               regexp = "This is an error.")
  expect_error(t_new_error("This is", " also ", "an error."),
               regexp = "This is also an error.")
  expect_warning(t_new_warning("This is a warning."),
                 regexp = "This is a warning.")
  expect_warning(t_new_warning("This is", " also ", "a warning."),
                 regexp = "This is also a warning.")
})


# ==============================================================================
# is.numeric_integer
# ==============================================================================

test_that("`is.numeric_integer` makes correct output.", {
  expect_true(is.numeric_integer(c(1, 2, 3, 4, 5)))
  expect_true(is.numeric_integer(1:5))
  expect_true(is.numeric_integer(c(1, 2, NA, 4, 5)))
  expect_false(is.numeric_integer(c(1, 2, NaN, 4, 5)))
  expect_false(is.numeric_integer(c(1, 2, 3, Inf, 5)))
  expect_false(is.numeric_integer(c(1, 2.5, 3, 4, 5)))
})


# ==============================================================================
# ensure_distances
# ==============================================================================

t_ensure_distances <- function(t_distances = distances::distances(matrix(1:10, nrow = 5))) {
  ensure_distances(t_distances)
}

test_that("`ensure_distances` checks input.", {
  expect_silent(t_ensure_distances())
  expect_error(t_ensure_distances(t_distances = "a"),
               regexp = "`t_distances` is not a `distances` object.")
})


# ==============================================================================
# ensure_blocking
# ==============================================================================

t_ensure_blocking <- function(t_blocking = qb_blocking(c("A", "A", "B", "C", "B", "C", "C", "A", "B", "B")),
                              t_req_length = 10L) {
  ensure_blocking(t_blocking, t_req_length)
}

test_that("`ensure_blocking` checks input.", {
  expect_silent(t_ensure_blocking())
  expect_silent(t_ensure_blocking(t_req_length = NULL))
  expect_silent(t_ensure_blocking(scclust::scclust(c("A", "A", "B", "C", "B", "C", "C", "A", "B", "B"))))
  expect_error(t_ensure_blocking(t_blocking = "a"),
               regexp = "`t_blocking` is not a valid blocking object.")
  expect_error(t_ensure_blocking(t_req_length = 5L),
               regexp = "`t_blocking` does not contain `t_req_length` units.")
})


# ==============================================================================
# ensure_caliper
# ==============================================================================

t_ensure_caliper <- function(t_caliper = 0.5) {
  ensure_caliper(t_caliper)
}

test_that("`ensure_caliper` checks input.", {
  expect_silent(t_ensure_caliper())
  expect_silent(t_ensure_caliper(t_caliper = 1L))
  expect_silent(t_ensure_caliper(t_caliper = NULL))
  expect_error(t_ensure_caliper(t_caliper = "a"),
               regexp = "`t_caliper` must be numeric or `NULL`.")
  expect_error(t_ensure_caliper(t_caliper = c(0.5, 0.3)),
               regexp = "`t_caliper` must be scalar.")
  expect_error(t_ensure_caliper(t_caliper = NA),
               regexp = "`t_caliper` may not be NA.")
  expect_error(t_ensure_caliper(t_caliper = -1.0),
               regexp = "`t_caliper` must be positive or `NULL`.")
})


# ==============================================================================
# coerce_size_constraint
# ==============================================================================

t_coerce_size_constraint <- function(t_size_constraint = 4L,
                                     t_num_data_points = 100L) {
  coerce_size_constraint(t_size_constraint,
                         t_num_data_points)
}

test_that("`coerce_size_constraint` checks input.", {
  expect_silent(t_coerce_size_constraint())
  expect_silent(t_coerce_size_constraint(t_size_constraint = 4.0))

  expect_error(t_coerce_size_constraint(t_size_constraint = NULL),
               regexp = "`t_size_constraint` cannot be NULL.")
  expect_error(t_coerce_size_constraint(t_size_constraint = c(10L, 3L)),
               regexp = "`t_size_constraint` must be scalar.")
  expect_error(t_coerce_size_constraint(t_size_constraint = "a"),
               regexp = "`t_size_constraint` must be integer.")
  expect_error(t_coerce_size_constraint(t_size_constraint = as.integer(NA)),
               regexp = "`t_size_constraint` may not be NA.")
  expect_error(t_coerce_size_constraint(t_size_constraint = 1L),
               regexp = "`t_size_constraint` must be greater or equal to two.")
  expect_error(t_coerce_size_constraint(t_size_constraint = 200L),
               regexp = "`t_size_constraint` may not be great than the number of units.")
})

test_that("`coerce_size_constraint` coerces correctly.", {
  expect_identical(t_coerce_size_constraint(), 4L)
  expect_identical(t_coerce_size_constraint(t_size_constraint = 4.0), 4L)
})


# ==============================================================================
# coerce_treatment_desc
# ==============================================================================

t_coerce_treatment_desc <- function(t_treatments) {
  coerce_treatment_desc(t_treatments)
}

test_that("`coerce_treatment_desc` checks input.", {
  expect_silent(t_coerce_treatment_desc(t_treatments = c("A" = 1L, "B" = 2L)))
  expect_silent(t_coerce_treatment_desc(t_treatments = c("A" = 1.0, "B" = 2.0)))
  expect_silent(t_coerce_treatment_desc(t_treatments = c("A" = 1L, "B" = 2L, "C" = 4L)))
  expect_silent(t_coerce_treatment_desc(t_treatments = c("A", "B")))
  expect_silent(t_coerce_treatment_desc(t_treatments = c("A", "B", "C")))
  expect_silent(t_coerce_treatment_desc(t_treatments = c(1, 2)))
  expect_silent(t_coerce_treatment_desc(t_treatments = c(TRUE, FALSE)))
  expect_silent(t_coerce_treatment_desc(t_treatments = c(1L, 5L)))

  expect_error(t_coerce_treatment_desc(t_treatments = dist(1:10)),
               regexp = "`t_treatments` must be vector.")
  expect_error(t_coerce_treatment_desc(t_treatments = c("A", "B", "A")),
               regexp = "`t_treatments` may not contain duplicate names.")
  expect_error(t_coerce_treatment_desc(t_treatments = c("A" = "1", "B" = "2")),
               regexp = "`t_treatments` must be integer.")
  expect_error(t_coerce_treatment_desc(t_treatments = c("A" = NA, "B" = 2L)),
               regexp = "`t_treatments` may not contain NAs.")
  expect_error(t_coerce_treatment_desc(t_treatments = c("B" = 2L)),
               regexp = "`t_treatments` must contain at least two treartment conditions.")
  expect_error(t_coerce_treatment_desc(t_treatments = c("A" = 1L, "B" = -2L)),
               regexp = "Elements in `t_treatments` must be positive.")
  expect_error(t_coerce_treatment_desc(t_treatments = c("A" = 0L, "B" = 2L)),
               regexp = "Elements in `t_treatments` must be positive.")
})

test_that("`coerce_treatment_desc` coerces correctly.", {
  expect_identical(t_coerce_treatment_desc(t_treatments = c("A" = 1L, "B" = 2L)),
                   list(conditions = c(1L, 2L, 2L),
                        names = c("A", "B")))
  expect_identical(t_coerce_treatment_desc(t_treatments = c("A" = 1.0, "B" = 2.0)),
                   list(conditions = c(1L, 2L, 2L),
                        names = c("A", "B")))
  expect_identical(t_coerce_treatment_desc(t_treatments = c("A" = 1L, "B" = 2L, "C" = 4L)),
                   list(conditions = c(1L, 2L, 2L, 3L, 3L, 3L, 3L),
                        names = c("A", "B", "C")))
  expect_identical(t_coerce_treatment_desc(t_treatments = c("A", "B")),
                   list(conditions = c(1L, 2L),
                        names = c("A", "B")))
  expect_identical(t_coerce_treatment_desc(t_treatments = c("A", "B", "C")),
                   list(conditions = c(1L, 2L, 3L),
                        names = c("A", "B", "C")))
  expect_identical(t_coerce_treatment_desc(t_treatments = c(1, 2)),
                   list(conditions = c(1L, 2L),
                        names = c("1", "2")))
  expect_identical(t_coerce_treatment_desc(t_treatments = c(TRUE, FALSE)),
                   list(conditions = c(1L, 2L),
                        names = c("TRUE", "FALSE")))
  expect_identical(t_coerce_treatment_desc(t_treatments = c(1L, 5L)),
                   list(conditions = c(1L, 2L),
                        names = c("1", "5")))
})
