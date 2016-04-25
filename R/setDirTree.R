#' Create Project Directory Tree
#'
#' Create a structure of directories which can be used alongside a the package workflow
#'
#' @param filepath the top level directory for the LC-MS project
#' @return NULL
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export


setDirTree <- function(filepath)
  {

  DIR1 <- paste(filepath, "core", sep = "/")
  DIR2 <- paste(filepath, "QC", sep = "/")
  DIR3 <- paste(filepath, "peakTables", sep = "/")
  DIR4 <- paste(filepath, "ddMS2", sep = "/")


  return(invisible(NULL))
  }
