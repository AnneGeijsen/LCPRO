#' Plot dd-FT-MS2 Spectra
#'
#' Plot the m/z and relative intensity of individual MS2 spectra
#'
#' @param x an object of \code{ftms2} class
#' @param saveplot logical; if \code{TRUE} the plot is saved to the current working directory.
#' @return NULL
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

plot.ftms2 <- function(x, saveplot = FALSE)
  {
  p1 <- ggplot(data = data.frame(x$spec), aes(x = mz, y = rel / 100)) +
          geom_bar(aes(width = 0.10),stat = "identity", position = "dodge", colour = "black") + theme_bw() +
            labs(x = "m/z", y = "Relative Intensity") + theme(axis.title = element_text(size = 14, face = "bold"),
              axis.text=element_text(size=12,face="bold")) + theme(legend.title=element_blank())

  ptitle <- paste("pMz", round(x$pMz,digits = 4), ":", "pRt (min)", x$pRt, sep = " ")
  p2 <- p1 + ggtitle(ptitle) + theme(plot.title = element_text(size=14,face="bold"))

  if(isTRUE(saveplot)){
    filename <- paste("pMz",round(x$pMz,digits = 2),"pRt",round(x$pRt, digits = 2), sep = "")
    svg(paste(filename, "svg", sep = "."), width = 10, height = 5)
    print(p2)
    dev.off()
  }
  print(p2)

  return(invisible(NULL))
  }
