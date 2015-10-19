#' 
#' @description
#' @param
#' @return
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export
#' @example
#' \dontrun{
#' 	getStandardInfo("ferulate")
#'	Synonym                    InChIKey   ExactMass       MF
#'	Ferulate KSEBMYQBYZTDHS-HWKANZROSA-N 194.0579088 C10H10O4
#'	}
#'
#'
getStandardInfo <- function(std_name)
	{
	cts_a <- cts_convert(std_name, from = "Chemical Name", to = "InChIKey", first = TRUE, verbos = FALSE)
	
	if(is.na(cts_a)){
		stop(".....standard not found", call. = FALSE)
	}
	
	cts_info <- cts_compinfo(cts_a, verbose = FALSE)
	
	cts_df <- data.frame(matrix(nrow = 1, ncol = 4))
	cts_df[,1:4] <- c(std_name, cts_info$inchikey, cts_info$exactmass,cts_info$formula)
	names(cts_df) <- c("Synonym", "InChIKey", "ExactMass", "MF")
	
	return(cts_df)
	}