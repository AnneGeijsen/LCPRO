#' Parallel xcmsSet (matchedFilter) without Rmpi
#'
#' This function is wrapper for using xcmsRaw across a FORK cluster to construct
#' an xcmsSet. By using a FORK cluster, which is easily intitialed using the \code{parallel}
#  package, \code{Rmpi} and the associated MPI dependancies don't need to be available. This is
#' especially useful for when running xcmsSet on large UNIX cluster's, where \code{Rmpi} continues
#' to be problematic.
#'
#' @param filepath the filepath containing raw (\code{.mzML, .mzXML}) files
#' @param info a runinfo  \code{data.frame}
#' @param fwhm a numeric value for the full width at half maximum
#' @param snthresh a numeric value for the signal to noise threshold
#' @param peakwidth numeric range (min,max) for chromatographic peak width in seconds
#' @return a \code{xcmsSet}
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export
xcmsRawSetMF <- function(filepath, info, fwhm, snthresh)
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

  cluster <- makeForkCluster(nCores)

  raw_imports <- parLapply(cluster, rawFiles, xcmsRaw, profstep = 1.0)

  peaks <- parLapply(cluster, raw_imports, findPeaks.matchedFilter,
                     fwhm = fwhm, snthresh = snthresh)


  stopCluster(cluster)

  xset <- new("xcmsSet")
  peaks_tmp <- plyr::ldply(peaks)
  xset@filepaths <- rawFiles

  samplen <- NULL
  for(i in 1:length(peaks)){
    samplen[[i]] <- rep(i, nrow(peaks[[i]]))
  }
  peaks_tmp <- data.frame(peaks_tmp,sample = unlist(samplen))

  rt <- lapply(raw_imports, function(x)(x@scantime))

  xset@rt <- list(raw = rt, corrected = rt)

  xset@peaks <- as.matrix(peaks_tmp)
  phenoData <- data.frame(class = info$class)
  rownames(phenoData) <- info$name
  xset@phenoData <- phenoData
  xset@profinfo <- list(method = "bin", step = 1.0)

  return(xset)
}
