#' Class CNSample
#'
#' @export CNSample

setClass("CNSample", representation(
  name="character",
  log2Ratio="numeric"
))

validateCNSampleClass <- function(object) {
  if (!nchar(object@name)) {
    return("'name' is empty")
  } else {
    return(TRUE)
  }
}

setValidity("CNSample", validateCNSampleClass)

#' Class CNRanges
#'
#' @export CNRanges

setClass("CNRanges",
         representation(
           chromosome="integer",
           start="integer",
           end="integer",
           samples="CNSample"
         )
)

validateCNRangesClass <- function(object) {
  if (!all(!is.na(object@chromosome))) {
    return("missing chromosome markers")
  } else if (!all(is.finite(object@start)) || !all(is.finite(object@end))) {
    return("start and/or end markers removed")
  } else {
    return(TRUE)
  }
}

setValidity("CNRanges", validateCNRangesClass)




#' Create `Copy Number Ranges' data object
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
#' @return An object of class \code{CNRanges}

CNRanges <- function(samples, chromosome, start, end, sampleNames = NULL) {
  if (is.data.frame(samples))
    genomdat <- as.matrix(samples)
  if (!missing(sampleNames)) {
    if (length(sampleNames) != ncol(samples)) {
      warning("length(sampleNames) and ncol(samples) differ, names ignored\n")
      sampleNames <- paste("Sample", 1:ncol(samples))
    }
  } else {
    sampleNames <- paste("Sample", 1:ncol(samples))
  }
  colnames(samples) <- sampleNames

  listSamples <- createCNSamples(samples, sampleNames)
  if (length(ii <- which(diff(start) == 0)) > 0) {
    if (any(chromosome[ii] == chromosome[ii + 1]))
      warning("array has repeated start positions\n")
  }
  # cnSamples <- new("CNSample", name=colnames(samples), log2Ratio=samples)
  cnRanges <- new("CNRanges", chromosome, start, end, samples=listSamples)
  return(cnRanges)
}

createCNSamples <- function(samples, names) {
  listSamples <- apply(samples, 2, CNSample, samples)
}

CNSample <- function(name, data) {
  cnSamples <- new("CNSample", name=name, log2Ratio=data)
  return(cnSamples)
}
