#' Check QC-RSD
#'
#' Determine the Coefficent of variation (CV) / Relative Standard Deviation (RSD) for all available Quality Control (QC) samples
#'
#' @param peak_table a formatted peak table.
#' @param info a runinfo \code{data.frame}. Quality control samples shoule be denoted by \code{QC} in the
#'  \code{class} column.
#' @return a \code{QC} \code{data.frame}
#'
#' @seealso formatTable
#' @seealso plot.CV
#' @author Tom Wilsoin \email{tpw2@@aber.ac.uk}
#' @export

checkQCRSD <- function(peak_table, info)
  {
  if(nrow(info) > nrow(peak_table)){
    stop("...more rows in runinifo than in peak table", call. = FALSE)
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

  QC_RSD <- data.frame("RSD" = QC_RSD)

  class(QC_RSD) <- "QCCV"

  return(QC_RSD)
  }
