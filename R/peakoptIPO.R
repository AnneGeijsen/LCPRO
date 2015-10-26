#' Optimise Peak Detection using IPO
#' @description
#' @param filepath
#' @param parallel
#' @param QC
#' @return
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export
#' @references Libiseller, Gunnar, et al. \emph{IPO: a tool for automated optimization of XCMS parameters.}
#' BMC bioinformatics 16.1 (2015): 118.
#' @references \url{http://www.biomedcentral.com/1471-2105/16/118/}
#'
#'
peakoptIPO <- function(filepath, parallel = TRUE, QC = TRUE)
  {

  myfiles <- list.files(filepath, pattern = "*.mzXML", full = TRUE)

  idx <- grepl("QC|qc", files)
  idx <- which(idx == TRUE)

  if((QC == TRUE) & length(idx) == 0){
      stop(".....no QC samples found ", call. = FALSE)
  }

  myfiles <- myfiles[idx]

  optParams <- getDefaultXcmsSetStartingParams()
  optParams$min_peakwidth <- c(2)
  optParams$max_peakwidth <- c(20)
  optParams$ppm <- c(10,12)
  optParams$snthresh <- 3
  optParams$mzdiff <- 0.001
  optParams$nSlaves <- 1
  optParams$fitgauss <- FALSE
  optParams$nSlaves <- 1
  optParams$integrate <- 2
  optParams$mzCenterFun <- "apex"

  slavesavail <- parallel::detectCores()

  if(length(myfiles) < slavesavail){
      myslaves <- length(myfiles)
  }else{
      myslaves <- slavesavail  }


  optResults <- optimizeXcmsSet(myfiles, params = optParams, nSlaves = 3, subdir = NULL)


  optSettings <- optResults$best_settings$parameters

  cat("Optimised settings are.....", "\n")
  cat("Minimum peak width of", paste(optSettings$min_peakwidth),"seconds")
  cat("Maximum peak width of", paste(optSettings$max_peakwidth),"seconds")
  cat("Tolerated threshold of", paste(optSettings$ppm),"ppm")

  params <- unlist(optSettings)
  params <- data.frame(params)
  names(params)[1] <- "OptValue"

  return(params)

  }
