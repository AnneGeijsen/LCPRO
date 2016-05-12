#' Summary QC report for variables
#'
#' Create a small report table, which can be used an a heuristic evaluation of a processed feature matrix
#'
#' @param xcms_set a xcms set with peak group information
#' @return a \code{data.frame} giving a summary of variable "quality"
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

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
  QCres["% of Missing Values (MV)",1] <- round((length(which(peak_table == 0)) / (ncol(peak_table) * nrow(peak_table))) * 100, digits = 2)
  QCres["% of Features with > 40 % MV",1] <- round(length(which(MV > 40)) / length(MV) * 100, digits =2)
  QCres["Cor % MV ~ m/z",1] <- round(mvCmz, digits = 2)
  QCres["Cor % MV ~ Rt",1] <- round(mvCrt, digits = 2)
  QCres["Cor % MV ~ Mean Abundance",1] <- round(mvCint, digits = 2)
  if(any(xcms_set@phenoData$class == "QC")){
    QCres["Median CV (%)",1] <- round(metProc::CV(peak_table[which(xcms_set@phenoData$class == "QC"),])$CVmd, digits = 2)
  }
  if(((any(xcms_set@phenoData$class == "QC")) == FALSE) & length(unique(xcms_set@phenoData$class) == 1)){
    QCres["Median CV (%)",1] <- round(metProc::CV(peak_table[which(xcms_set@phenoData$class == unique(xcms_set@phenoData$class)),])$CVmd, digits = 2)
  }
  QCres["% of Features with RSD < 20%",1] <- round((length(which(metProc::CV(peak_table[which(xcms_set@phenoData$class == unique(xcms_set@phenoData$class)),])$CV <= 20)) / ncol(peak_table)) * 100, digits = 2)


  QCres["Mean peak width (mins)",1] <- paste(round(mean(feat_table$rtmax - feat_table$rtmin),digits = 2),round(sd(feat_table$rtmax - feat_table$rtmin),digits = 2), sep = " +/- ")
    QCres["Mean feature group error (ppm)",1] <- paste(round(mean((feat_table$mzmax - feat_table$mzmin) / feat_table$mz) * 1E06, digits = 2),
            round(sd((feat_table$mzmax - feat_table$mzmin) / feat_table$mz) * 1E06, digits = 2), sep = " +/- ")


  occ <- function(x)
  {
    ox <- length(which(x !=0)) / length(x)
    return(ox)
  }

  if(((any(xcms_set@phenoData$class == "QC")) == FALSE) & length(unique(xcms_set@phenoData$class) == 1)){
  occu <- sapply(data.frame(peak_table), function(x)(tapply(x, factor(xcms_set@phenoData$class), occ))) * 100
  # % of total features which occupancy > 80% in QC class
  occu80 <- (length(which(occu >= 80)) / length(occu)) * 100
  }
  if(any(xcms_set@phenoData$class == "QC")){
  occu <- sapply(data.frame(peak_table), function(x)(tapply(x, factor(xcms_set@phenoData$class), occ))) * 100
  occu <- data.frame(occu[which(rownames(occu) == "QC"),])
  occu80 <- (length(which(occu[,1] >= 80)) / length(occu[,1])) * 100
  }

  QCres["% Features > 80 % Occupancy in QC",1] <- round(occu80, digits = 2)

  names(QCres)[1] <- "value"

  return(QCres)
  }
