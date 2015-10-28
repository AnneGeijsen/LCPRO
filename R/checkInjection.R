#' Check injection of all samples
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' @examples \dontrun{
#'    filepath <- "~/rawfiles"
#'    runinfo <- read.csv("~/runinfo.csv",head = TRUE, stringsAsFactors = FALSE)
#'    names(runinfo) <- c("Injection", "Sample","Class")
#'    checkInjection(filepath, runinfo, plot = TRUE, parallel = FALSE)
#' }
#'

checkInjection <- function(filepath,runinfo,plot = TRUE, parallel = FALSE)
  {

  rawfiles <- list.files(filepath, pattern = ".mzXML", full = TRUE)
  if(length(rawfiles) == 0){
    stop("...no rawfiles found in ",filepath, sep = " ", call. = FALSE)
  }


  if(ncol(runinfo) < 3){
    stop("...runinfo file is missing essential fields")
  }
  if(!"Injection" %in% names(runinfo)){
      stop("Injection information not found.....check column headers")
  }
  if(!"Sample" %in% names(runinfo)){
    stop("Sample information not found.....check column headers")
  }
  if(!"Class" %in% names(runinfo)){
    stop("Class information not found.....check column headers")
  }

  rawfiles <- sapply(rawfiles, list)

  if(parallel == FALSE){
    TIC <- sapply(rawfiles, function(x)(sum(xcmsRaw(x)@tic)))
  }

  if(parallel == TRUE){
    cluster = parallel::makePSOCKcluster(parallel::detectCores())
    parallel::clusterExport(cluster,varlist = c("rawfiles", "xcmsRaw"), envir = environment())
    TIC <- parallel::parSapply(cluster, rawfiles, function(x)(sum(xcmsRaw(x)@tic)))
    stopCluster(cluster);gc()
  }

  fileinfo <- gsub(filepath, "", rawfiles)
  fileinfo <- gsub("/", "", fileinfo)
  fileinfo <- gsub(".mzXML", "", fileinfo)

  file.idx <- match(fileinfo, runinfo$Sample)

  res <- data.frame(cbind(TIC, runinfo[file.idx,]))
  rownames(res) <- NULL

  res <- res[order(res$Injection),]


  if(plot == TRUE){
  plot.1 <-  ggplot(res, aes(x = Injection, y = log(TIC))) + geom_line(linetype = "dashed") +
                  geom_point(size = 4.0,aes(colour = Class)) +
                    xlab("Injection Order") + ylab("log (Total Ion Count)") +
                      theme_bw()

  pdf(width = 13,height = 9,"tic~injection.pdf")
  print(plot.1)
  dev.off()
  }

  MdTIC = median(log(res$TIC))
  SdTIC = sd(log(res$TIC))

  m= median(log(res$TIC))
  s = sd(log(res$TIC))
  out_idx <- which(log(res$TIC) < (MdTIC - SdTIC))

  if(any(res[out_idx,"Class"] != "CTRL")){
    out_id <- which(res[out_idx,"Class"] != "CTRL")
    outliers <- res[out_idx,][out_id, "Sample"]

    cat("...technical outliers found:", outliers, sep = "\n")
    write.table(outliers, file = "tech_outliers.txt", row.names = FALSE, col.names = FALSE)
  }else{
    message("....no injection outliers found")
  }

  }



