#' Check info file
#'
#' Check that the supplied \code{info} file contains all the nesscary information in the correct format
#'
#' @param filepath the filepath containing raw (\code{.mzML, .mzXML}) files
#' @param info a runinfo \code{data.frame}. See \url{https://github.com/wilsontom/LCPRO/wiki} for more details in runinfo files
#' @return a runinfo \code{data.frame}
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export


infoCheck <- function(filepath, info)
  {
  files <- list.files(filepath, full = FALSE, pattern = c(".mzML | .mzXML"))

	if(length(files) != nrow(info)){
		stop("Length of files and runinfo do not match")
	}

  ## Checks that the run info file is formatted with the correct column headers,

  # Some columns are mandatory !
  if (!("injord" %in% names(info))) stop(paste("injorder column not found!"))
  if (!("file" %in% names(info))) stop(paste("file column not found!"))
  if (!("name" %in% names(info))) stop(paste("name column not found!"))
  if (!("class" %in% names(info))) stop(paste("class column not found!"))
  if (!("batch" %in% names(info))) stop(paste("batch column not found!"))

  if (!is.numeric(info$injord)) stop(paste("injorder should be numeric"))
  if (!is.numeric(info$batch)) stop(paste("batch should be numeric"))


  ## If the columns "filepath", "path" , "singba" are not found, a warning message will message
	# these columns are not essential, so the function will continue

  if (!("filepath" %in% names(info))) message(paste("Column filepath not found!"))
  if (!("path" %in% names(info))) message(paste("Column path not found!"))
  if (!("singba" %in% names(info))) message(paste("Column singba not found!"))
  if (!("classn" %in% names(info))) message(paste("Column classn not found!"))

  if ("classn" %in% names(info)){
    if (!is.numeric(info$classn)) stop(paste("classn should be numeric"))
  }

  ## Check that the is a set of QC samples.

  if(!("QC" %in% info$class)) message(paste("WARNING: No QC samples present!"))


  ## Checks to insure that the file names used in info$name are identical (in name & order)
    # to the raw  files which are read in using list.files

  if(all(info$file == files) == FALSE){
    message("...the runinfo file is not in the correct order", "\n")
    message("...re-ordering info file so that it matches the file order", "\n")

    if("fileord" %in% names(info)){
       message(paste("file order information found in infofile,using for re-sorting", "\n"))
      info <- info[order(info$fileord),]
    }

    if(!("fileord" %in% names(info))){
       idx_ord <- match(info$file, files)
       info <- data.frame(fileord = idx_ord,info)
       info <- info[order(info$fileord),]
    }
  }

  ch <- all(info$file == files)
  if(isTRUE(ch)){
    message("...check complete... all good!")
    return(info)
  }
  if(ch == FALSE){
    message("...check fail... this is most likely due to a naming conflict between info$file and files. Please Check Manuall!")
  }
