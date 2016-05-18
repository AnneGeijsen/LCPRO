#' Check the TIC of all samples
#'
#' Check the sample total ion count (TIC) for all samples in an experiment/batch.
#'
#' @param filepath a filepath of \code{.mzML} or \code{.mzXML} files
#' @param injord a numeric vector of the injection order for all raw files. This vector must be
#' in file order, not numeric order
#' @param class a character vector of class information for all the raw files. This vector must be in file order
#' @return a \code{ticCheck} class
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

ticCheck <- function(filepath, injord, cls, saveplot = TRUE)
  {
  rawFiles <- list.files(filepath, full = TRUE, pattern = ".mzXML|.mzML")
  if(length(rawFiles) == 0){
    stop(paste("...no .mzXML or .mzML files in",filepath, sep = " "), call. = FALSE)
  }

  check_counts <- function(x)
    {
    file_x <- openMSfile(x)
    head_x <- header(file_x)

    ms1_x <- head_x[which(head_x[,"msLevel"] == 1),]
    tic_x <- sum(ms1_x[,"totIonCurrent"])

    return(as.numeric(tic_x))
    }

  ticAll <- sapply(rawFiles, check_counts)

  ticRes <- data.frame(ticAll, injord = injord, class = as.character(cls), stringsAsFactors = FALSE)

  class(ticRes) <- "ticCheck"

  return(ticRes)
  }


