#' Build dd FT-MS2 List
#'
#' Compile a list object of data dependant (dd) MS2 spectra acquired using FT-MS, which can be used further downstream
#' for matching to MS1 feature groups and peak annotation
#'
#' @param filename a raw \code{.mzML} or \code{.mzXML} file containing FT-MS1 and dd-FT-MS2 spectra only.
#' @return returns an object of \code{class} \code{ftms2}
#'
#' The \code{ftms2} class is a list containing the following elements
#'  \itemize{
#'    \item{pMz} the precursor m/z value
#'    \item{pRt} the precursor retention time in minutes
#'    \item{pInt} the precursor intensity
#'    \item{spec} a matrix of MS2 spectra.
#'  }
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

compileMS2 <- function(fileName, save = FALSE)
  {

  rawFile <- openMSfile(filename)

  header_raw <- header(rawFile)
  peaks_raw <- peaks(rawFile)

  if(length(unique(header_raw[,"msLevel"])) > 2){
    stop("...more than 2 MS levels present", call. = FALSE)
  }

  ms1_head <- header_raw[which(header_raw$msLevel == 1),]
  ms1_peaks <- peaks_raw[which(header_raw$msLevel == 1)]
  ms2_head <- header_raw[which(header_raw$msLevel == 2),]
  ms2_peaks <- peaks_raw[which(header_raw$msLevel == 2)]


  ms2List <- NULL
  for(i in 1:nrow(ms2_head)){
    pSc <- ms2_head[i,"precursorScanNum"]
    ms2List[[i]] <- list(pMz = ms2_head[i,"precursorMZ"],pRt = round((ms1_head[which(ms1_head$acquisitionNum == pSc),"retentionTime"] / 60), digits = 3),
                         pInt = ms2_head[i,"precursorIntensity"],spec = ms2_peaks[[i]])

    ms2List[[i]]$spec <- cbind(ms2List[[i]]$spec,ms2List[[i]]$spec[,2] / (max(ms2List[[i]]$spec[,2])) * 100)
    colnames(ms2List[[i]]$spec) <- c("mz", "int","rel")
    class(ms2List[[i]]) <- "ftms2"
  }

  return(ms2List)
  }

