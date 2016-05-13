#' Format xcmsSet into Feature table
#'
#' Format xcmsSet into a feature summary table
#'
#' @param xcms_set a \code{xcmsSet} containing peak group information

#' @return A \code{data.frame} containing feature group details
#'
#' @author Tom Wilson \email{tpw2@@bert.ibers.aber.uk}
#' @export

featureTable <- function(xcms_set)
{

  if(attr(xcms_set, "class")[1] != "xcmsSet"){
    stop("input must be an xcmsSet object")
  }
  if(length(xcms_set@groupidx) == 0){
    stop("...xcms set must contain peak group information")
  }

  tab.tmp <- peakTable(xcms_set)
  peak_table <- formatTable(xcms_set)

  tab.tmp$npeaks <- NULL

  feat_info <- data.frame(tab.tmp$mz, tab.tmp$mzmin, tab.tmp$mzmax, tab.tmp$rt, tab.tmp$rtmin, tab.tmp$rtmax)
  names(feat_info) <- c("mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax")

  feat_info$rt <- round(feat_info$rt/60, digits = 2)
  feat_info$rtmin <- round(feat_info$rtmin/60, digits = 2)
  feat_info$rtmax <- round(feat_info$rtmax/60, digits = 2)

  feat_rt_ran <- round(feat_info$rtmax - feat_info$rtmin, digits = 2)
  feat_mz_dppm <- round(((feat_info$mzmax - feat_info$mzmin) / feat_info$mz) * 1E06, digits = 2)
  feat_mz_damu <- round((feat_info$mzmax - feat_info$mzmin) / feat_info$mz, digits = 5)

  feat_id <- paste("M", round(feat_info$mz, digits = 2) , "T", feat_info$rt, sep = "")

  feat_tab <- data.frame(feat_id,feat_info, feat_rt_ran, feat_mz_damu, feat_mz_dppm)
  names(feat_tab)[1] <- "ID"
  names(feat_tab)[8:10] <- c("rt_range", "amu_dev", "ppm_dev")

  if(any(xcms_set@phenoData$class == "QC")){
    QCval <- metProc::CV(peak_table[which(xcms_set@phenoData$class == "QC"),])$CV
  }
  if(((any(xcms_set@phenoData$class == "QC")) == FALSE) & length(unique(xcms_set@phenoData$class) == 1)){
    QCval <- metProc::CV(peak_table[which(xcms_set@phenoData$class == unique(xcms_set@phenoData$class)),])$CV
  }

  if(any(xcms_set@phenoData$class == "QC")){
    table_tmp <- peak_table[which(xcms_set@phenoData$class == "QC"),]
    QCme <- apply(table_tmp,2,mean)
  }
  if(((any(xcms_set@phenoData$class == "QC")) == FALSE) & length(unique(xcms_set@phenoData$class) == 1)){
    QCme <- apply(peak_table,2,mean)
  }

  sn <- NULL
  for(i in 1:length(neg_fill@groupidx)){
    sn[[i]] <- neg_fill@peaks[neg_fill@groupidx[[i]], "sn"]
  }

  perc_filled <- lapply(sn, function(x)((length(which(is.na(x)))) / length(x)) * 100)


  feat_tab <- data.frame(feat_tab, RSD = QCval, Int_mean = QCme, perc_infilled = round(unlist(perc_filled), digits = 2))

  return(feat_tab)
  }
