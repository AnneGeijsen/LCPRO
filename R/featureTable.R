#' Format xcmsSet into Feature table
#'
#' Format xcmsSet into a feature summary table
#'
#' @param xcms_set a \code{xcmsSet} containing peak group information
#' @param cls an optional parameter. If a vecotr of \code{class} is given, then this will be used instead of the class information
#' in \code{xcms_set@phenoData$class}
#' @return A \code{data.frame} containing feature group details
#'
#' @author Tom Wilson \email{tpw2@@bert.ibers.aber.uk}
#' @export

featureTable <- function(xcms_set, cls = NULL)
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


  if(!is.null(cls)){
    if(length(cls) != nrow(peak_table)){
      stop("...dimension of class information and peak_table do not match", call. = FALSE)
    }
    if(length(grep("QC", cls)) == 0){
      stop("...no QC samples found in the class information provided", call. = FALSE)
    }
  }

  if(is.null(cls)){
      if(any(xcms_set@phenoData$class == "QC")){
      QCval <- metProc::CV(peak_table[which(xcms_set@phenoData$class == "QC"),])$CV
    }
    if(((any(xcms_set@phenoData$class == "QC")) == FALSE) & length(unique(xcms_set@phenoData$class) == 1)){
      QCval <- metProc::CV(peak_table[which(xcms_set@phenoData$class == unique(xcms_set@phenoData$class)),])$CV
    }
  }

  if(!is.null(cls)){
    QCval <- metProc::CV(peak_table[which(cls == "QC"),])$CV
  }


  if(is.null(cls)){
    if(any(xcms_set@phenoData$class == "QC")){
      table_tmp <- peak_table[which(xcms_set@phenoData$class == "QC"),]
      QCme <- apply(table_tmp,2,mean)
    }
    if(((any(xcms_set@phenoData$class == "QC")) == FALSE) & length(unique(xcms_set@phenoData$class) == 1)){
      QCme <- apply(peak_table,2,mean)
    }
  }

  if(!is.null(cls)){
    table_tmp <- peak_table[which(cls == "QC"),]
    QCme <- apply(table_tmp,2,mean)
  }



  sn <- samp <-  NULL
  for(i in 1:length(xcms_set@groupidx)){
    sn[[i]] <- xcms_set@peaks[xcms_set@groupidx[[i]], "sn"]
    samp[[i]] <- xcms_set@peaks[xcms_set@groupidx[[i]], "sample"]
    }

  qc_pheno_idx <- which(xcms_set@phenoData$class == "QC")

  qc_sn <- NULL
  for(i in 1:length(sn)){
    idx <- match(qc_pheno_idx, samp[[i]])
    qc_sn[[i]] <- sn[[i]][idx]
  }

  perc_filled <- lapply(qc_sn, function(x)((length(which(is.na(x)))) / length(x)) * 100)

  feat_tab <- data.frame(feat_tab, RSD = QCval, Int_mean = QCme, perc_infilled = round(unlist(perc_filled), digits = 2))

  return(feat_tab)
  }
