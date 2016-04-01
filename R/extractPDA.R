#' Extract PDA Data
#'
#' Extract the UV Diodearray data from a \code{.mzML} file and save into a much small \code{.RData} file
#'
#' @param filepath a valid filepath containing \code{.mzML} files
#' @param savedir location to save extracted PDA data to
#' @return NULL
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

extractPDA <- function(filepath, savedir)
  {
  rawFiles <- list.files(filepath, full = TRUE, pattern = ".mzML")
  if(length(rawFiles) == 0){
    stop("...no .mzML files found in ")
  }
  
  if(!file.exists(savedir)){
    dir.create(savedir)
  }
  
  pdaRemove <- function(x)
    {
    
    file_x <- openMSfile(x)
    head_x <- header(file_x)
    
    UVidx <- which(head_x$msLevel == 0)
    if(length(UVidx) == 0){
      stop("...no UV data found in file", call. = FALSE)
    }
  
    peaks_x <- peaks(file_x)
    
    UVData <- peaks_x[UVidx]
    
    filename_a <- strsplit(x, "\\/")
    filename_b <- filename_a[[1]][length(filename_a[[1]])]
    filename <- gsub(".mzML", ".RData", filename_b)
    
    savedir <- "~/Desktop/UV"
    save(UVData, file = paste(savedir, filename, sep = "/"))
    
    return(invisible(NULL))
    }
  
    sapply(rawFiles, pdaRemove)
  
    return(invisible(NULL))
    }