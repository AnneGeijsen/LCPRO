#' Plot Retention Time Deviation 
#'
#' Plot a scatter plot of root mean standard deviation between raw and corrected retention times for each sample.
#' @param x a \code{RMSD data.frame}
#' @return a scatter plot
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

plot.RMSD <- function(x)
  {

  class(x) <- "data.frame"
  
  p1 <- ggplot(x, aes(x = injord, y = RMSD, colour = class)) + geom_point(size = 3) +
    geom_line(data = x[which(x$class == "QC"),], aes(x = injord, y = RMSD), size = 1) +
    theme_bw() + labs(x = "Injection Order", y = "Rt RMSD") + theme(axis.title = element_text(size = 14),
    axis.text=element_text(size=12,face="bold")) + theme(legend.title=element_blank())

  p2 <- p1 + ggtitle("Retention Time Correction RMSD")
  print(p2)
  return(invisible(NULL))
  }
