#USEAGE: convert(c("9321","9322","9330"), type = 'EGP')
#ISCO: int or char -> int
#fromFormate: ISCO88. Can be other formats but will return first match rather than best match
#to Format: can be EGP, SIOPS, ISEI. Will add ABS criteria soon
#translator is a data frame consiting of conversion table
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' convert2
#'
#' Converts ISCO-88 to Common SES Codes
#' @param x Vector of four digit ISCO-88 codes as int or cha
#' @param type The requested conversion type (see details)
#' @param selfEmployed The code that is used for self-employed individuals (see details)
#' @param unemployed The code that is used for unemployed individuals (see details)
#' @description Converts ISCO 88 to common ses or social class codes.
#' @details Types include EGP, ISEI, and SIOPS. We strictly use Appendix A from Ganzeboom & Treiman (1996) for conversions. This means no distinction is made between Routine non-manual employees higher grade or lower grade. Both are coded as 3. ISCO-88 does not contain codes for self-employed or unemployed individuals. Thus user input is required to specify the codes used for each (if any). Self-employed and unemployed codes will be translated to EGP codes of 6 and 12 respectively. They will be coded as NA for ISEI or SIOPS.
#' @examples convert(c(1000,9000), type = "EGP")
#' @import data.table
#' @export

convert2 <- function(x, type = "EGP", selfEmployed = NULL, unemployed = NULL){

	valid_keys = c(selfEmployed, unemployed, hash::keys(ISCOhash))

	if(any(!x %in% valid_keys)){
		warning("One or more inputs is not a valid ISCO-88 code")
	}

	vars = c("ISCO", type)

	result <- data.table(ISCOhash_dt)[ISCO %in% x, c(vars), with = FALSE]
	original = data.table(ISCO = as.character(x))
	result <- result[original, on = "ISCO", allow.cartesian = TRUE]

	if(!is.null(selfEmployed)){
	result[ISCO %in% selfEmployed,2] <- 6
	}

	if(!is.null(unemployed)){
		result[ISCO %in% unemployed, 2] <- 12
	}

	as.numeric(result[,2][[1]])
}

