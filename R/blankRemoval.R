#' Remove backgroud signals
#'
#' Use a peak table of CTRL samples to filter out background signals which have formed valid feature groups during processing.
#' In the first instance, extact matches are sought between the CTRL peak table and QC peak table using feature ID's. Exact
#' matches are removed from the QC peak table if the fold change in raw intensity is less than 2.0 between CTRL and QC. In the second round
#' CTRL signals are located within the Rt and mz ranges of the QC peak table. Again, any matches with less than 2.0 fold change between CTRL and QC
#' are removed.
#'
#' @param peaks_all a formated feature table of all biological samples
#' @param peaks_ctrl a formated feature table of control sample only
#' @return a feature table
#'
#' @seealso featureTable
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export
#'


peaks_all <- featureTable(negFill)
peaks_ctrl <- featureTable(negCFill)

a <- blankRemoval(peaks_all, peaks_ctrl)


blankRemoval <- function(peaks_all, peaks_ctrl){

  ## look for exact matches first

  matched <- NULL
  for(i in 1:nrow(peaks_ctrl)){
    matched[[i]] <- match(peaks_ctrl[i,"ID"], peaks_all[,"ID"])
  }

  matched_index <- which(matched != "NA")
  matched_peaks <- matched[!is.na(matched)]

  # eval exact matches first
  exact_fc <-  peaks_all[matched_peaks,"Int_mean"] / peaks_ctrl[matched_index,"Int_mean"]
  exact_remove <- which(exact_fc <= 2.0)
  exact_corr <- cor(peaks_ctrl[matched_index,"Int_mean"][exact_remove], peaks_all[matched_peaks,"Int_mean"][exact_remove])

  message("...",paste(length(exact_fc)), " exact matches found")
  message("...",paste(length(exact_remove)), " matches have FC < 2.0 between CTRL and QC
  Correlation (r) of ", paste(round(exact_corr, digits = 2)), " between intensities")
  message("...removing signals")
    # remove exact match signals

  peaks_all <- peaks_all[-matched_peaks[exact_remove],]
  peaks_ctrl <- peaks_ctrl[-matched_index[exact_remove],]

  ## look for close matches

  matched_close <- NULL
  for(i in 1:nrow(peaks_ctrl)){
    match_idx <- which(peaks_all[,"rtmin"] < peaks_ctrl[i,"rt"] & peaks_all[,"rtmax"] > peaks_ctrl[i,"rt"] &
                                  peaks_all[,"mzmin"] < peaks_ctrl[i,"mz"] & peaks_all[,"mzmax"] > peaks_ctrl[i,"mz"])

    if(length(match_idx) == 0){match_idx = "NA"}

    matched_close[[i]] <- match_idx
    }


  matched_index2 <- which(matched_close != "NA")
  matched_peaks2 <- as.numeric(matched_close[matched_index2])


  cor(peaks_ctrl[matched_index,"Int_mean"],peaks_all[matched_peaks,"Int_mean"])

  close_fc <- peaks_all[matched_peaks2,"Int_mean"] / peaks_ctrl[matched_index2,"Int_mean"]
  close_remove <- which(close_fc <= 2.0)
  close_corr <- cor(peaks_ctrl[matched_index2,"Int_mean"][close_remove], peaks_all[matched_peaks2,"Int_mean"][close_remove])

  message("...",paste(length(close_fc)), " close range matches found")
  message("...",paste(length(close_remove)), " matches have FC < 2.0 between CTRL and QC
  Correlation (r) of ", paste(round(close_corr, digits = 2)), " between intensities")
  message("...removing signals")


  peaks_all <- peaks_all[-matched_peaks2[close_remove],]
  peaks_ctrl <- peaks_ctrl[-matched_index2[close_remove],]

  return(peaks_all)
  }
