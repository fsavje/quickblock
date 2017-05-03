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

#' Random treatment assignment with blocks
#'
#' Provided a blocking and treatment conditions, \code{assign_treatment} randomly
#' assigns treatments to the units in the sample so to best maintain the desired
#' proportion of treatments within each block.
#'
#'
#'
#' Units not assigned to blocks will not be assigned treatments.
#'
#' @param blocking
#'    \code{\link{qb_blocking}} or \code{\link[scclust]{scclust}} object with
#'    the blocked units.
#' @param treatments
#'    vector with treatment conditions. If unnamed vector, elements in the
#'    vector indicates treatment conditions that will be assigned in equal
#'    proportions within each block. If named integer vector, the element
#'    names indicate treatment conditions which will be assigned in the
#'    proportions indicated by the values. See below for details.
#'
#' @return
#'    Returns a factor with the assigned treatments.
#'
#' @examples
#' # Example blocking
#' my_blocking <- qb_blocking(c("A", "A", "B", "C", "B",
#'                              "C", "C", "A", "B", "B"))
#'
#' # Two treatment conditions in equal propositions
#' assign_treatment(my_blocking)
#'
#' # Two treatment conditions in with twice as many controls as treated
#' assign_treatment(my_blocking, c("Treated" = 1, "Control" = 2))
#'
#' # Three treatment conditions in equal propositions
#' assign_treatment(my_blocking, c("T1", "T2", "C"))
#'
#' # Four treatment conditions in equal propositions
#' # This gives a warning because some blocks have less than four units
#' \dontrun{assign_treatment(my_blocking, c("T1", "T2", "T3", "C"))}
#'
#' @export
assign_treatment <- function(blocking,
                             treatments = c("Treated", "Control")) {
  ensure_blocking(blocking)
  treatments <- coerce_treatment_desc(treatments)

  assignments <- .Call(qbc_assign_treatments,
                       blocking,
                       treatments$conditions)
  attributes(assignments) <- list("class" = "factor",
                                  "levels" = treatments$names)
  assignments
}
