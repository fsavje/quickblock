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


#' quickblock: Quick Threshold Blocking
#'
#' Provides functions for constructing near-optimal threshold blockings. The
#' package is made with large data sets in mind and derives blockings more than
#' an order of magnitude quicker than other methods.
#'
#' See the package's website for more information:
#' \url{https://github.com/fsavje/quickblock}.
#'
#' Bug reports and suggestions are greatly appreciated. They are best reported
#' here: \url{https://github.com/fsavje/quickblock/issues}.
#'
#' @references
#' Higgins, Michael J., Fredrik Sävje and Jasjeet S. Sekhon (2016),
#' \sQuote{Improving massive experiments with threshold blocking},
#' \emph{Proceedings of the National Academy of Sciences}, \bold{113:27}, 7369--7376.
#' \url{http://www.pnas.org/lookup/doi/10.1073/pnas.1510504113}
#'
#' @docType package
#' @name quickblock-package
#'
#' @import distances
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("The `quickblock` package is under development. Please use it with caution.")
}

## @useDynLib quickblock, .registration = TRUE
#.onUnload <- function (libpath) {
#  library.dynam.unload("quickblock", libpath)
#}
