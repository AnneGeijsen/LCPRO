#' Peak Detection
#'
#' This is a wrapper function for LC-HRMS peak detection using xcms::findPeaks.centwave. Values for parameters
#' such as \code{snthresh}, \code{ppm}, \code{peakwidth}, etc.. have been hardcoded to reflect typical and optimised
#' values for non-targeted profiling of human urine using Reverse Phase (RP) C18 chromatography and Orbitrap mass detection.
#'
#' The wrapper performs the nesscary checks of meta data prior to peak detection and helps to set up a multi-threaded (parallel)
#' environment
#'
#' @param filepath the filepath containing raw (\code{.mzML, .mzXML}) files
#' @param info a runinfo  \code{data.frame}
#' @param rtcor logical; if \code{TRUE} then OBIWARP retention time correction is applied
#' @return a \code{xcmsSet}
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

peakDetection <- function(filepath, info, rtcor = TRUE)
  {

  rawFiles <- list.files(filepath, full = TRUE, pattern = ".mzML|.mzXML")

  avalCores <- parallel::detectCores()

  if(avalCores > length(rawFiles)){
    nCores <- length(rawFiles)
  }
  if(avalCores < length(rawFiles)){
    nCores <- avalCores - 1
  }

  info <- infoCheck(filepath, info)

#'@export
  peakDet <- function(x,y,nc){
  peaks_set <- xcmsSet(x, method = "centWave", ppm = 1.5, snthresh = 10, peakwidth = c(10,50),
                   integrate = 2, mzdiff = 0.001, snames = y$name, sclass = y$class, noise = 1000,
                   prefilter = c(3,1000), nSlaves = nc)
  return(peaks_set)
  }

  peaks <- .peakDet(rawFiles, info,nCores)

  if(isTRUE(rtcor)){
    peaks <- retcor(peaks, method = "obiwarp", profStep = 0.1)
  }

  return(peaks)
  }

