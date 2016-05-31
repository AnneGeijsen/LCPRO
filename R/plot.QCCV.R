#' Plot QC-RSD
#'
#' Plot a histogram of QC-RSD values
#'
#' @param x a \code{QCCV data.frame}
#' @return a histogram
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

plot.QCCV <- function(x)
  {
  class(x) <- "data.frame"

  p1 <- ggplot(data = x, aes(RSD, fill = "red")) + geom_histogram(binwidth = 5, alpha = .6) + theme_bw() +
    labs(x = "Coefficent of Variation (%)", y = "Count")

  p2 <- p1 + theme(axis.text = element_text(face = "bold")) +
    theme(axis.title = element_text(face = "bold")) +
    theme(legend.title = element_text(face = "bold")) +
    theme(legend.position="none")

  title_a <- paste("Median CV (%): ", round(median(x$RSD, na.rm = TRUE), digits = 2))

  p3 <- p2 + ggtitle(title_a) + theme(plot.title = element_text(lineheight=.8, face="bold"))

  print(p3)

  }
