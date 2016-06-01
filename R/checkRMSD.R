#' Find outlier runs
#' 
#' Use the root mean square deviation (RMSD) between raw and corrected retenion times to identify potential outlier samples
#'
#' @param xcms_set an \code{xcmsSet} which has had retention time correction applied
#' @param info a runinfo \code{data.frame}
#' @return a \code{RMSD data.frame}
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

checkRSMD <- function(xcms_set, info)
  {
  # check that retcor has been done
  ChRt <- NULL
  for(i in 1:length(xcms_set@rt$raw)){
    ChRt[[i]] <- xcms_set@rt$raw[[i]] == xcms_set@rt$corrected[[i]]
  }
  
  ChRt <- unlist(ChRt)
  if(all(ChRt) == TRUE){
    stop("...it appears retention time correction (xcms::retcor) hasn't been performed yet", call. = FALSE)
  }
  RMSD <- sapply(1:length(xcms_set@rt$corrected),function(x) {sqrt(sum((xcms_set@rt$raw[[x]]-xcms_set@rt$corrected[[x]]) ** 2))})
  
  RtRes <- data.frame(RMSD, injord = info$injord, class = info$class)
  
  class(RtRes) <- "RMSD"
  
  return(RtRes)
  }
  