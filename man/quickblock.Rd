% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quickblock.R
\name{quickblock}
\alias{quickblock}
\title{Construct threshold blockings}
\usage{
quickblock(
  distances,
  size_constraint = 2L,
  caliper = NULL,
  break_large_blocks = FALSE,
  ...
)
}
\arguments{
\item{distances}{\code{\link[distances]{distances}} object or a numeric vector, matrix
or data frame. The parameter describes the similarity of the units to be
blocked. It can either be preprocessed distance information using a
\code{\link[distances]{distances}} object, or raw covariate data. When
called with covariate data, Euclidean distances are calculated unless
otherwise specified.}

\item{size_constraint}{integer with the required minimum number of units in each block.}

\item{caliper}{restrict the maximum within-block distance.}

\item{break_large_blocks}{logical indicating whether large blocks should be broken up into smaller blocks.}

\item{...}{additional parameters to be sent either to the \code{\link[distances]{distances}}
function when the \code{distances} parameter contains covariate data, or
to the underlying \code{\link[scclust]{sc_clustering}} function.}
}
\value{
Returns a \code{\link{qb_blocking}} object with the constructed blocks.
}
\description{
\code{quickblock} constructs near-optimal threshold blockings. The function
expects the user to provide distances measuring the similarity of
units and a required minimum block size. It then constructs a blocking
so that units assigned to the same block are as similar as possible while
satisfying the minimum block size.
}
\details{
The \code{caliper} parameter constrains the maximum distance between units
assigned to the same block. This is implemented by restricting the
edge weight in the graph used to construct the blocks (see
\code{\link[scclust]{sc_clustering}} for details). As a result, the caliper
will affect all blocks and, in general, make it harder for
the function to find good matches even for blocks where the caliper is not
binding. In particular, a too tight \code{caliper} can lead to discarded
units that otherwise would be assigned to a block satisfying both the
matching constraints and the caliper. For this reason, it is recommended
to set the \code{caliper} value quite high and only use it to avoid particularly
poor blocks. It strongly recommended to use the \code{caliper} parameter only
when \code{primary_unassigned_method = "closest_seed"} in the underlying
\code{\link[scclust]{sc_clustering}} function (which is the default
behavior).

The main algorithm used to construct the blocking may produce
some blocks that are much larger than the minimum size constraint. If
\code{break_large_blocks} is \code{TRUE}, all blocks twice as large as
\code{size_constraint} will be broken into two or more smaller blocks. Block
are broken so to ensure that the new blocks satisfy the size constraint.
In general, large blocks are produced when units are highly clustered,
so breaking up large blocks will often only lead to small improvements. The
blocks are broken using the \code{\link[scclust]{hierarchical_clustering}}
function.

\code{quickblock} calls \code{\link[scclust]{sc_clustering}} with
\code{seed_method = "inwards_updating"}. The \code{seed_method} parameter
governs how the seeds are selected in the nearest neighborhood graph that
is used to construct the blocks (see \code{\link[scclust]{sc_clustering}}
for details). The \code{"inwards_updating"} option generally works well
and is safe with most datasets. Using \code{seed_method = "exclusion_updating"}
often leads to better performance (in the sense of blocks with more
similar units), but it may increase run time. Discrete data (or more generally
when units tend to be at equal distance to many other units) will lead to
particularly poor run time with this option. If the dataset has at least one
continuous covariate, \code{"exclusion_updating"} is typically quick. A third
option is \code{seed_method = "lexical"}, which decreases the run time relative
to \code{"inwards_updating"} (sometimes considerably) at the cost of performance.
\code{quickblock} passes parameters on to \code{\link[scclust]{sc_clustering}},
so to change \code{seed_method}, call \code{quickblock} with the parameter
specified as usual: \code{quickblock(..., seed_method = "exclusion_updating")}.
}
\examples{
# Construct example data
my_data <- data.frame(x1 = runif(100),
                      x2 = runif(100))

# Make distances
my_distances <- distances(my_data, dist_variables = c("x1", "x2"))

# Make blocking with at least two units in each block
quickblock(my_distances)

# Require at least three units in each block
quickblock(my_distances, size_constraint = 3)

# Impose caliper
quickblock(my_distances, caliper = 0.2)

# Break large block
quickblock(my_distances, break_large_blocks = TRUE)

# Call `quickblock` directly with covariate data (ie., not pre-calculating distances)
quickblock(my_data[c("x1", "x2")])

# Call `quickblock` directly with covariate data using Mahalanobis distances
quickblock(my_data[c("x1", "x2")], normalize = "mahalanobize")

}
\references{
Higgins, Michael J., Fredrik Sävje and Jasjeet S. Sekhon (2016),
   \sQuote{Improving massive experiments with threshold blocking},
   \emph{Proceedings of the National Academy of Sciences}, \bold{113:27}, 7369--7376.
}
\seealso{
See \code{\link[scclust]{sc_clustering}} for the underlying function used
  to construct the blocks.
}
