#' Sort runinfo file by file order
#' @description
#' @param
#' @param
#' @return
#' @author Tom Wilson
#' @export
#' @examples \dontrun{
#'  head(runinfo)
#'    Injection      Sample  Class
#'         1      ctrl01   CTRL
#'         2      ctrl02   CTRL
#'         3      ctrl03   CTRL
#'         4     dirty01     QC
#'         5 diet1_TP7-8 DIET-1
#'         6 diet2_Tp7-8 DIET-2
#'
#'  fileord <- sortFileOrder <- function(filepath, runinfo)
#'
#'  head(fileord)
#'      Order Injection                 Sample  Class
#'        1         1                   ctrl01   CTRL
#'        2         2                   ctrl02   CTRL
#'        3         3                   ctrl03   CTRL
#'        4        33                   ctrl04   CTRL
#'        5        34                   ctrl05   CTRL
#'        6        17 diet1_Tp7-8_151015204433 DIET-1
#' }
#'
sortFileOrder <- function(filepath, runinfo)
  {

  rawfiles <- list.files(filepath, pattern = ".mzXML", full = TRUE)

  # clean up filename

  fileinfo <- gsub(filepath, "", rawfiles)
  fileinfo <- gsub("/", "", fileinfo)
  fileinfo <- gsub(".mzXML", "", fileinfo)

  # match filenames with runinfo

  file.idx <- match(fileinfo, runinfo$Sample)


  file.ord <- data.frame(cbind(file.idx, runinfo[file.idx,]))
  file.ord[,1] <- seq(from = 1, to = nrow(file.ord), by = 1)

  names(file.ord)[1] <- "Order"


  if(any(file.ord$Sample == fileinfo) == FALSE){
      stop("....erorr during filename sorting", call. = FALSE)
  }
  return(file.ord)
  }



