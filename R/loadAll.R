#' Load all xcmsSet's
#'
#' Load all of the xcmsSet's in the specified directory.
#'
#' @param path directory containing xcmsSet's which were saved using \code{saveAll}
#' @return NULL
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export
#' @seealso \link{saveAll}

loadAll <- function(path)
{
  all_files <- list.files(path, pattern = ".RData")
  for(i in 1:length(all_files)){
    load(all_files[i])
    varName <- gsub(".RData", "", all_files[i])
    assign(eval(parse(text = "varName")), xset, envir = .GlobalEnv)
  }
  return(invisible(NULL))
}
