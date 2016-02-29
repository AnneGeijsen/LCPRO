#' Plot CV
#'
#' Plot the Coefficent of variation (%) for all available Quality Control (QC) samples
#'
#' @param peak_table A formatted peak table. 
#' @param info A runinfo \code{data.frame}. Quality control samples shoule be denoted by \code{QC} in the
#'  \code{class} column.
#'
#' @return A histogram is saved to the current working directory
#'
#' @author Tom Wilsoin \email{tpw2@@aber.ac.uk}
#' @export

CVplot <- function(peak_table, info) 
  {
  if(nrow(info) > nrow(peak_table)){
    stop("...more rows in runinif than in peak table", call. = FALSE)
  }

  if(nrow(peak_table) > nrow(info)){
    stop("...more rows in peak table than in info", call. = FALSE)
  } 

  if(!"class" %in% names(info)){
    stop("...info does not contain a class column", call. = FALSE)
  }
    
  QC_idx <- which(info$class == "QC")

  if(length(QC_idx) == 0){
    stop("..no QC class found, check info file", call. = FALSE)
  }

  QC_table <- peak_table[QC_idx,]
  
  RSD <- function(x)
  {
  sd.x <- apply(x, 2, sd)
  me.x <- apply(x, 2, mean)
  cv.x <- (sd.x/me.x) * 100
  return(cv.x)
  }

  QC_RSD <- RSD(QC_table)

  QC_RSD <- data.frame(QC_RSD)
  names(QC_RSD) <- "RSD"

  p1 <- ggplot(data = QC_RSD, aes(RSD, fill = "red")) + geom_histogram(binwidth = 5, alpha = .6) + theme_bw() +
    labs(x = "Coefficent of Variation (%)", y = "Count")  

  p2 <- p1 + theme(axis.text = element_text(face = "bold")) +
      theme(axis.title = element_text(face = "bold")) +
      theme(legend.title = element_text(face = "bold")) +
      theme(legend.position="none")

  title_a <- paste("Median CV (%): ", round(median(QC_RSD$RSD, na.rm = TRUE), digits = 2))
  
  p3 <- p2 + ggtitle(title_a) + theme(plot.title = element_text(lineheight=.8, face="bold"))
  
  svg("QC-RSD.svg", width = 10, height = 5)
  print(p3)
  dev.off()
  
  return(invisible(NULL))
  }