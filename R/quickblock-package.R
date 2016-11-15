# ==============================================================================
# quickblock -- Blocking in Experiments with Large Samples
# https://github.com/fsavje/quickblock
#
# Copyright (C) 2016  Fredrik Savje -- http://fredriksavje.com
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


#' quickblock: Blocking in Experiments with Large Samples
#'
#' The package is still under development. Please use it with caution.
#'
#' More information and the latest version is found here:
#' \url{https://github.com/fsavje/quickblock}.
#'
#' Bug reports and suggestions are greatly appreciated. They
#' are best reported here:
#' \url{https://github.com/fsavje/quickblock/issues}.
#'
#' @docType package
#' @name quickblock-package
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("The `quickblock` package is under development. Please use it with caution.")
  packageStartupMessage("Bug reports and suggestions are greatly appreciated: https://github.com/fsavje/quickblock/issues")
}
