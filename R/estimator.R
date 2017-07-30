# ==============================================================================
# quickblock -- Quick Threshold Blocking
# https://github.com/fsavje/quickblock
#
# Copyright (C) 2017  Fredrik Savje, Jasjeet Sekhon, Michael Higgins
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

#' Estimator for treatment effects in blocked experiments
#'
#' \code{blocking_estimator} estimates treatment effects in blocked experiments.
#' The function expects the user to provide the outcomes, a blocking object
#' and treatment assignments. It returns point estimates of sample average
#' treatment effects and variance estimates.
#'
#' To produce point estimates, \code{blocking_estimator} requires that each block
#' contains at least one unit assigned to each treatment condition. For variance
#' estimation, it requires that each block contains at least two units assigned to
#' each condition. When treatments have been assigned with the
#' \code{\link{assign_treatment}} function (or an equivalent procedure), the
#' variance estimators are conservative in expectation (see the referenced
#' note below for details). If treatment is assigned with another method, the
#' estimator might not be valid.
#'
#' The function estimates treatment effects by aggregating block-level
#' effect estimates. It estimates effects within each block by taking the difference
#' in mean outcomes in the block. The sample-level estimate is then derived as the
#' weighted average of the block-level effects using the size of the blocks as weights.
#' In detail, let \eqn{n_b} be the number of units assigned to block \eqn{b}, and
#' \eqn{n} be the total number of units in the sample. Let \eqn{Y(t, b)} be the
#' average outcome for units assigned to treatment \eqn{t} in block \eqn{b}. The
#' effect of treatment \eqn{t} versus treatment \eqn{s} is then estimated as:
#'
#' \deqn{\sum\frac{n_b}{n}[Y(t, b) - Y(s, b)],}
#'
#' where the sum is taken over the blocks in the experiment. See the referenced note
#' for more details.
#'
#' @param outcomes
#'    numeric vector with observed outcomes.
#' @param blocking
#'    \code{\link{qb_blocking}} or \code{\link[scclust]{scclust}} object with
#'    the block assignments.
#' @param treatments
#'    factor specifying the units' treatment assignments.
#'
#' @return
#'    A list with two numeric matrices with estimated treatment effects and
#'    their estimated variances is returned. The first matrix (\code{effects})
#'    contains estimated treatment effects. Rows in this matrix indicate minuends
#'    in the treatment effect contrast and columns indicate subtrahends. For
#'    example, in the matrix:
#'    \tabular{rrrr}{
#'      \tab a \tab b \tab c\cr
#'      a \tab 0.0 \tab 4.5 \tab 5.5\cr
#'      b \tab -4.5 \tab 0.0 \tab 1.0\cr
#'      c \tab -5.5 \tab -1.0 \tab 0.0\cr
#'    }
#'    the estimated treatment effect between conditions \eqn{a} and \eqn{b} is
#'    \eqn{4.5}, and the estimated treatment effect between conditions \eqn{c}
#'    and \eqn{b} is \eqn{-1.0}.
#'
#'    The second matrix (\code{effect_variances}) contains estimates of
#'    variances of the corresponding effect estimators.
#'
#' @references
#'    Higgins, Michael J., Fredrik Sävje and Jasjeet S. Sekhon (2015),
#'    \sQuote{Blocking estimators and inference under the Neyman-Rubin model},
#'    arXiv 1510.01103. \url{https://arxiv.org/abs/1510.01103}
#'
#' @examples
#' # Example blocking
#' my_blocking <- qb_blocking(c("A", "A", "B", "C", "B",
#'                              "C", "B", "C", "B", "A",
#'                              "C", "C", "A", "B", "B",
#'                              "B", "B", "A", "A", "C"))
#'
#' # Two treatment conditions
#' my_treatments <- assign_treatment(my_blocking)
#' my_outcomes <- rnorm(20)
#' blocking_estimator(my_outcomes, my_blocking, my_treatments)
#'
#' # Three treatment conditions
#' my_treatments <- assign_treatment(my_blocking, c("T1", "T2", "C"))
#' my_outcomes <- rnorm(20)
#' blocking_estimator(my_outcomes, my_blocking, my_treatments)
#'
#' # Four treatment conditions
#' # (This will throw an error because variances cannot be estimated)
#' my_treatments <- assign_treatment(my_blocking, c("T1", "T2", "T3", "C"))
#' my_outcomes <- rnorm(20)
#' \dontrun{blocking_estimator(my_outcomes, my_blocking, my_treatments)}
#'
#' @export
blocking_estimator <- function(outcomes,
                               blocking,
                               treatments) {
  outcomes <- coerce_double(outcomes)
  num_observations <- length(outcomes)
  ensure_blocking(blocking, num_observations)
  treatments <- coerce_treatments(treatments, num_observations)

  pos <- .Call(qbc_est_potential_outcomes,
               outcomes,
               blocking,
               unclass(treatments),
               nlevels(treatments))

  con_mat <- matrix(1, nrow = nlevels(treatments), ncol = 1L)
  out_te <- pos$means %*% t(con_mat) - con_mat %*% pos$means
  out_te_vars <- pos$vars %*% t(con_mat) + con_mat %*% pos$vars
  diag(out_te_vars) <- 0
  dimnames(out_te) <- dimnames(out_te_vars) <- list(levels(treatments), levels(treatments))
  list(effects = out_te, effect_variances = out_te_vars)
}
