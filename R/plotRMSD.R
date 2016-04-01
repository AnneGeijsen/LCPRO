# 'Plot Retention Time Deviation'
#'
#' Plot the root mean square deviation (RMSD) in order to visualise the extent of applied retention time
#' correction in each sample.
#'
#' @param xcmsSet an \code{xcmsSet} which has had retention time's corrected
#' @param injord a numeric vector of the injection order for all raw files. This vector must be
#' in file order, not numeric order
#' @param class a character vecotr of class information for all the raw files. This vecotr must be in file order
#' @param saveplot a logical statement. If \code{TRUE} the plot is saved as \code{.svg} to the current working directory
#' @return NULL
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

plotRMSD <- function(xcmsSet, injord, cls, saveplot = TRUE)
  {
# check that retcor has been done

  ChRt <- NULL
  for(i in 1:length(xcmsSet@rt$raw)){
    ChRt[[i]] <- xcmsSet@rt$raw[[i]] == xcmsSet@rt$corrected[[i]]
  }

  ChRt <- unlist(ChRt)
  if(all(ChRt) == TRUE){
    stop("...it appears retention time correction (xcms::retcor) hasn't been performed yet", call. = FALSE)
  }

  RMSD <- sapply(1:length(xcmsSet@rt$corrected),function(x) {sqrt(sum((xcmsSet@rt$raw[[x]]-xcmsSet@rt$corrected[[x]]) ** 2))})

  RtRes <- data.frame(RMSD, injord = injord, class = cls)

  p1 <- ggplot(RtRes, aes(x = injord, y = RMSD, colour = class)) + geom_point(size = 3) +
    geom_line(data = RtRes[which(RtRes$class == "QC"),], aes(x = injord, y = RMSD), size = 1) +
    theme_bw() + labs(x = "Injection Order", y = "Rt RMSD") + theme(axis.title = element_text(size = 14),
    axis.text=element_text(size=12,face="bold")) + theme(legend.title=element_blank())

  p2 <- p1 + ggtitle("Retention Time Correction RMSD")

  if(isTRUE(saveplot)){
    svg("rtRMSD.svg", width = 10, height = 5)
    print(p2)
    dev.off()
  }
  if(!isTRUE(saveplot)){
    print(p2)
  }

  return(invisible(NULL))
  }
