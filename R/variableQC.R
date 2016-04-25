#' Summary QC report for variables
#'
#' Create a small report table, which can be used an a heuristic evaluation of a processed feature matrix
#'
#' @param xcms_set a xcms set with peak group information
#' @return a \code{data.frame} giving a summary of variable "quality"
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export
#'
variableQC <- function(xcms_set)
  {

  peak_table <- formatTable(xcms_set)
  feat_table <- featureTable(xcms_set)

  MV <- apply(peak_table,2,function(x)(length(which(x == 0))/length(x)) * 100)
  INT <- apply(peak_table,2,mean)

  mvCrt <- cor(MV, feat_table$rt)
  mvCmz <- cor(MV, feat_table$mz)
  mvCint <- cor(MV, INT)


  QCres <- data.frame()
  QCres["Number of Features",1] <- ncol(peak_table)
  QCres["% of Missing Values (MV)",1] <- (length(which(peak_table == 0)) / (ncol(peak_table) * nrow(peak_table))) * 100
  QCres["% of Features with > 40 % MV",1] <- length(which(MV > 40)) / length(MV) * 100
  QCres["Cor % MV ~ m/z",1] <- mvCmz
  QCres["Cor % MV ~ Rt",1] <- mvCrt
  QCres["Cor % MV ~ Mean Abundance",1] <- mvCint
  QCres["Median CV (%)",1] <- metProc::CV(tab[which(xcms_set@phenoData$class == "QC"),])$CVmd
  names(QCres)[1] <- "value"

  QCres[,1] <- round(QCres[,1], digits = 2)

  }
