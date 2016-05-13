#' Evaluate a QC dilution series
#'
#'
#'
#' @param filepath the filepath containing raw (\code{.mzML, .mzXML}) files
#' @param info a runinfo  \code{data.frame}. The \code{class} column should denote the dilution factor
#' @param nCores a numeric value for the number of cores to utilise
#'
#'
#' @author Tom Wilson \email{tpw2@aber.ac.uk}
#' @export

dilutionCheck <- function(filepath, runinfo)
  {
  rawFiles <- list.files(filepath, full = TRUE, pattern = ".mzML|.mzXML")

  info <- infoCheck(filepath, info)

  split_files <- split(rawFiles, info$class)
  split_info <- split(info, info$class)

  for(i in 1:length(split_info)){
    split_info[[i]]$class <- as.factor(split_info[[i]]$class)
  }

    split_peaks <- list()
  for(i in 1:length(split_files)){
        split_peaks[[i]] <- peakDet(split_files[[i]], split_info[[i]], nCores)
  }

  split_group <- lapply(split_peaks, function(x)(group(x, method = "density", mzwid = 0.015,
                                                        bw = 10, minfrac = 0.4)))
  split_fill <- lapply(split_group, fillPeaks)

  qc_rep <- split_varQC <- lapply(split_fill, variableQC)

  return(qc_rep)

  }
