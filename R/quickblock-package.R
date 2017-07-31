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

#' quickblock: Quick Threshold Blocking
#'
#' Provides functions for assigning treatments in randomized experiments using
#' near-optimal threshold blocking. The package is made with large data sets in
#' mind and derives blocks more than an order of magnitude quicker than other
#' methods.
#'
#' See \code{\link{quickblock}} for the main blocking function.
#'
#' See the package's website for more information:
#' \url{https://github.com/fsavje/quickblock}.
#'
#' Bug reports and suggestions are greatly appreciated. They are best reported
#' here: \url{https://github.com/fsavje/quickblock/issues}.
#'
#' @references
#'    Higgins, Michael J., Fredrik Sävje and Jasjeet S. Sekhon (2016),
#'    \sQuote{Improving massive experiments with threshold blocking},
#'    \emph{Proceedings of the National Academy of Sciences}, \bold{113:27}, 7369--7376.
#'    \url{http://www.pnas.org/lookup/doi/10.1073/pnas.1510504113}
#'
#' @docType package
#' @name quickblock-package
#'
#' @import distances
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Please cite the `quickblock` package as:")
  packageStartupMessage("   Higgins, Michael J., Fredrik Savje and Jasjeet S. Sekhon (2016),")
  packageStartupMessage("   \"Improving massive experiments with threshold blocking\",")
  packageStartupMessage("   Proceedings of the National Academy of Sciences 113:27, 7369--7376.")
}

#' @useDynLib quickblock, .registration = TRUE
.onUnload <- function (libpath) {
  library.dynam.unload("quickblock", libpath)
}
