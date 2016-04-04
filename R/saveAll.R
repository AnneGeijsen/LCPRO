#' Save all xcmsSet's
#'
#' Save all of the xcmsSet's currently in the global environment (\code{.GlobalEnv}) into the specified directory.
#'
#' @param path directory where xcmsSet's will be saved to
#' @return NULL
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export
#' @seealso \link{loadAll}

saveAll <- function(path)
{
  xsets <- Filter( function(x) 'xcmsSet' %in% class( get(x) ), ls() )
  for(i in 1:length(xsets)){
    file <- paste(path,paste(xsets[i], ".RData", sep = ""), sep = "/")
    xset <- eval(parse(text = xsets[i]))
    eval(parse(text=paste(save(xset, file = file))))
  }
  return(invisible(NULL))
}
