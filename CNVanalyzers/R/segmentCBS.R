#' Segment copy number array with CBS
#'
#'

segmentCBS <- function(x) {
  # Validar os dados
  cnas <- createCNA(x)
  cnas
}

createCNA <- function(genomdat, chrom, maploc, data.type=c("logratio","binary"),
                      sampleid=NULL, presorted=FALSE) {
  # Create list of copy number array for analysis
    DNAcopy::CNA(
      genomdat=genomdat,
      chrom=chrom,
      maploc=maploc,
      data.type=data.type,
      sampleid=x)
}

# .getCBSSegments <- function(x, alpha=1e-10, undo.splits="sdundo", undo.SD=1.0,
#                             segmentStatistic="seg.mean", normalizeSegmentedBins=FALSE,
#                             inter=c(-0.1, 0.1)){
#
#   condition <- x$usebin
#
#   sampleNames <- names(x)[!(names(x) %in%
#                               c("chromosome", "start", "end", "usebin"))]
#
#   copyNumber <- x[sampleNames]
#   copyNumber[!condition, ] <- NA_real_
#
#   # To Stabilize variance for CBS
#   copyNumber <- .toLog2(copyNumber, inv=TRUE)
#   copyNumber <- .stabilisedTrans(copyNumber, inv=FALSE)
#   copyNumber <- as.matrix(copyNumber)
#
#   chromosome <- x$chromosome
#   start <- x$start
#
#   # Create a list of CNA objects that can be analyzed with *lapply()
# cna <- lapply(sampleNames, function(x)
#   DNAcopy::CNA(
#     genomdat=copyNumber[condition, x, drop=FALSE],
#     chrom=factor(chromosome[condition], levels=unique(chromosome),
#                  ordered=TRUE),
#     maploc=start[condition],
#     data.type="logratio",
#     sampleid=x,
#     presorted=TRUE))
#
#   rm(list=c("chromosome", "start")); gc(FALSE)
#
#   # Create a vector of messages to be printed
#   msgs <- paste0(sampleNames)
#
#   # Use sample names for indexing, they are available in the CNA objects
#   # as the name of the third column
#   names(msgs) <- sampleNames
#   segments <- .flapply(cna, FUN=function(x, ...) {
#     message(msgs[colnames(x)[3]])
#     DNAcopy::segment(x, alpha=alpha, undo.splits=undo.splits,
#                      undo.SD=undo.SD, verbose=0, ...)
#   })
#
#   segmentStatisticCol <- grep(segmentStatistic,
#                               colnames(DNAcopy::segments.summary(segments[[1]])))
#
#   segDF <- matrix(NA_real_, nrow=nrow(copyNumber), ncol=ncol(copyNumber))
#
#   segDF[condition, ] <- do.call(cbind, lapply(segments, function(x)
#     rep(DNAcopy::segments.summary(x)[,segmentStatisticCol],
#         x$output$num.mark)))
#
#   colnames(segDF) <- sampleNames
#   row.names(segDF) <- NULL
#   segDF[is.na(copyNumber)] <- NA_real_
#   segDF <- .toLog2(.stabilisedTrans(segDF, inv=TRUE), inv=FALSE)
#
#   rm(list=c("cna")); gc(FALSE)
#
#   if(normalizeSegmentedBins){
#
#     x <- data.frame(
#       chromosome=x$chromosome,
#       start=x$start,
#       end=x$end,
#       usebin=x$usebin,
#       segDF,
#       stringsAsFactors=FALSE)
#     names(x) <- c("chromosome", "start", "end", "usebin", sampleNames)
#
#
#     seg <- .cghSegConversion(x, copyNumber)
#     rm(list=c("copyNumber")); gc(FALSE)
#
#     # Normalize
#     postseg <- CGHcall::postsegnormalize(seg, inter=inter)
#
#     # Return the normalized segments and copynumber values in a list
#     segDF <- matrix(NA_real_, nrow=nrow(segDF), ncol=ncol(segDF))
#     segDF[condition, ] <- CGHbase::segmented(postseg)
#
#     #copyNumber[condition, ] <- CGHbase::copynumber(postseg)
#
#     row.names(segDF) <- NULL
#     #row.names(copyNumber) <- NULL
#
#     colnames(segDF) <- sampleNames
#     #colnames(copyNumber) <- sampleNames
#
#   }
#
#   return(segDF)
#   #return(list(segDF=segDF, copyNumber=copyNumber))
#
# }
