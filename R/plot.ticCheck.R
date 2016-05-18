#' Plot ticCheck
#'
#' Plot the TIC profile for a batch of samples
#'
#' @param x a \code{ticCheck} class
#' @return NULL
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

plot.ticCheck <- function(x,...)
  {

  ticDat <- data.frame(injord = x$injord, class = x$class, ticAll = x$ticAll)

  p1 <- ggplot(data = ticDat, aes(x = injord, y = ticAll, colour = class)) + geom_point(size = 2) +
    geom_line(data = ticDat[which(ticDat$class == "QC"),], aes(x = injord, y = ticAll), size = 1) +
    theme_bw() + labs(x = "Injection Order", y = "Total Ion Count") + theme(axis.title = element_text(size = 16),
    axis.text=element_text(size=12,face="bold")) + theme(legend.title=element_blank())

  print(p1)
  return(invisible(NULL))

    }
