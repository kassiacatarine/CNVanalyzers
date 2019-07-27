#' Create `Copy Number Array' data object
#'
#' Creates a `copy number array' data object used for DNA copy number
#' analyses.
#'
#' @param genomdat a vector or matrix of data from array-CGH, ROMA, or
#' other copy number experiments. If it is a matrix the rows correspond
#' to the markers and the columns to the samples.
#'
#' @param chrom the chromosomes (or other group identifier) from which
#' the markers came.  Vector of length same as the number of rows of
#' genomdat.  If one wants the chromosomes to be ordered in the
#' natural order, this variable should be numeric or ordered category.
#'
#' @param maploc the locations of marker on the genome.  Vector of length
#' same as the number of rows of genomdat. This has to be numeric.
#'
#' @param data.type logratio (aCGH, ROMA, etc.) or binary (LOH).
#'
#' @param sampleid sample identifier.  If missing the samples are named
#' by prefixing "Sample" to consecutive integers.
#'
#' @param presorted logical indicator telling if the data have already
#' been sorted by chrom and maploc.  Default is FALSE.
#'
#' @return An object of class \code{CNA}

CNA <- function(genomdat, chrom, maploc, data.type=c("logratio","binary"),
                sampleid=NULL, presorted=FALSE) {
  res <- DNAcopy::CNA(genomdat, chrom, maploc, data.type, sampleid, presorted)
  class(res) <- 'CNA'
  res
}
