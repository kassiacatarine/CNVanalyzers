#' Segmentation of copy number array
#'
#' @param x
#'
#' @param segmentType
#'
#' @export segmentation
#'
#' @return An object of class \code{CNV}
#'
#' @author Kássia Catarine

segmentation <- function(x, segmentType=c("CBS", "fastseg"), summaryMethod="mean", verbose=1) {

  segmentType <- match.arg(segmentType,
                           choices=c("CBS", "fastseg"),
                           several.ok=TRUE)

  summaryMethod <- match.arg(summaryMethod,
                             choices=c("mean", "median", "min", "max", "Q1", "Q3"),
                             several.ok=FALSE)

  sampleNames <- names(x)[!(names(x) %in%
                              c("chromosome", "start", "end", "usebin"))]

  segmentationExecution(x, segmentType, verbose);

}

segmentationExecution <- function(x, segmentType, verbose=1) {
  # segCBS <- data.frame();
  if (verbose>=1) cat(paste("|--- Performing Segmentations","---|\n"));
  segCBS <- segmentations <- data.frame();
  if(identical("CBS", segmentType)) {
    # if (verbose>=2) cat(paste("|---* Performing CBS Segmentation","*---|\n"));

    segmentCBS(x)
  }
  if(identical("fastseg", segmentType)) {
    if (verbose>=2) cat(paste("|---* Performing fastseg Segmentation","*---|\n"));
  }
}
