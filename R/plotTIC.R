#' Check the TIC of all samples
#' 
#' Plot the sample total ion count (TIC) for all samples in an experiment/batch. #'
#'
#' @param filepath a filepath of \code{.mzML} or \code{.mzXML} files
#' @param injord a numeric vector of the injection order for all raw files. This vector must be
#' in file order, not numeric order
#' @param class a character vecotr of class information for all the raw files. This vecotr must be in file order
#' @param saveplot a logical statement. If \code{TRUE} the plot is saved as \code{.svg} to the current working directory
#' @return NULL
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

plotTIC <- function(filepath, injord, cls, saveplot = TRUE)
  {
  rawFiles <- list.files(filepath, full = TRUE, pattern = ".mzXML|.mzML")
  if(length(rawFiles) == 0){
    stop(paste("...no .mzXML or .mzML files in",filepath, sep = " "), call. = FALSE)
  }
  
  check_counts <- function(x)
    {
    file_x <- openMSfile(x)
    head_x <- header(file_x)
    
    ms1_x <- head_x[which(head_x[,"msLevel"] == 1),]
    tic_x <- sum(ms1_x[,"totIonCurrent"])
    
    return(as.numeric(tic_x))
    }
  
  
  ticAll <- sapply(rawFiles, check_counts)
  
  ticRes <- data.frame(ticAll, injord = injord, class = as.character(cls), stringsAsFactors = FALSE)
  
  p1 <- ggplot(ticRes, aes(x = injord, y = ticAll, colour = class)) + geom_point(size = 3) + 
    geom_line(data = ticRes[which(ticRes$class == "QC"),], aes(x = injord, y = ticAll), size = 1) + 
    theme_bw() + labs(x = "Injection Order", y = "Total Ion Count") + theme(axis.title = element_text(size = 16),
    axis.text=element_text(size=12,face="bold")) + theme(legend.title=element_blank())
  
  
  if(isTRUE(saveplot)){
    svg("checkTIC.svg", width = 10, height = 5)
    print(p1)
    dev.off()
  }
  if(!isTRUE(saveplot)){
    print(p1)
  }
  
  return(invisible(NULL))
  }
