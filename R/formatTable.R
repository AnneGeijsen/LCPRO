#' Format xcmsSet
#'
#' Format xcmsSet into peak table ready for statistical analysis
#'
#' @param xcms_set A \code{xcmsSet} containing peak group information
#' @param feat_ids A logical statement. If \code{TRUE} a \code{.csv} file is written to the working
#' directory containing a numeric identification number for each feature.
#'
#' @return A formatted peak matrix
#'
#' @author Tom Wilson \email{tpw2@@bert.ibers.aber.uk}
#' @export

formatTable <- function(xcms_set, feat_ids = FALSE)
  {
  
  if(attr(xcms_set, "class")[1] != "xcmsSet"){
    stop("input must be an xcmsSet object")
  }
  if(length(xcms_set@groupidx) == 0){
    stop("...xcms set must contain peak group information")
  }

  tab.tmp <- peakTable(xcms_set)

  tab.tmp$npeaks <- NULL

  feat_info <- data.frame(cbind(tab.tmp$mz, tab.tmp$mzmin, tab.tmp$mzmax, tab.tmp$rt, tab.tmp$rtmin, tab.tmp$rtmax))
  names(feat_info) <- c("mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax")


  feat_info$rt <- round(feat_info$rt/60, digits = 2)
  feat_info$rtmin <- round(feat_info$rtmin/60, digits = 2)
  feat_info$rtmax <- round(feat_info$rtmax/60, digits = 2)

  feat_id <- paste("M", round(feat_info$mz, digits = 2) , "T", feat_info$rt, sep = "")

  nclass <- length(unique(xcms_set@phenoData[,"class"]))

  tab.tmp <- tab.tmp[,(7 + nclass):ncol(tab.tmp)]

  tab.tmp <- t(tab.tmp)
  colnames(tab.tmp) <- feat_id
  if(isTRUE(feat_ids)){
    feat_idx <- seq(from = 1, to = length(feat_id), by = 1)
    max_digits <- length(strsplit(as.character(feat_idx[length(feat_idx)]), split = "")[[1]])
    arg <- paste("%","0",max_digits,"d", sep = "")
    feat_idx <- sprintf(arg, feat_idx)
    feature_id <- data.frame(cbind(feat_id, feat_idx))
    names(feature_id) <- c("Feature", "FeatureIndex")
    write.csv(feature_id, "feature_index.csv", row.names = FALSE)
  }

  return(tab.tmp)
  }
