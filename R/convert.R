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

#' convert
#'
#' Converts ISCO-88 to Common SES Codes
#' @param ISCO Vector of four digit ISCO-88 codes as int or cha
#' @param type The requested conversion type (see details)
#' @param selfEmployed The code that is used for self-employed individuals (see details)
#' @param unemployed The code that is used for unemployed individuals (see details)
#' @description Converts ISCO 88 to common ses or social class codes. See \href{http://www.harryganzeboom.nl/isco88/index.htm}{Ganzeboom's website for official correspondence tables.
#' @details Types include EGP, ISEI, and SIOPS. We strictly use Appendix A from Ganzeboom & Treiman (1996) for conversions. This means no distinction is made between Routine non-manual employees higher grade or lower grade. Both are coded as 3. ISCO-88 does not contain codes for self-employed or unemployed individuals. Thus user input is required to specify the codes used for each (if any). Self-employed and unemployed codes will be translated to EGP codes of 6 and 12 respectively. They will be coded as NA for ISEI or SIOPS.
#' @examples convert(c(1000,9000), type = "EGP")
#' @export

convert <- function(ISCO, type = "EGP", selfEmployed = NULL, unemployed = NULL){

	if (type == "EGP" & length(selfEmployed) !=0 & ISCO %in% selfEmployed){
		return(6)
	}
	else if (type == "EGP" & length(unemployed) !=0 & ISCO %in% unemployed){
		return(12)
	}

	else if(ISCO %in% hash::keys(ISCOhash) == FALSE){
		warning("One or more inputs is not a valid ISCO-88 code")
		return(NA)
	}

	else{f1 <- function(type){
		switch(type,
			   EGP = ISCOhash[[as.character(ISCO)]]$EGP,
			   ISEI = ISCOhash[[as.character(ISCO)]]$ISEI,
			   SIOPS = ISCOhash[[as.character(ISCO)]]$SIOPS,
			   JOB = ISCOhash[[as.character(ISCO)]]$JobDescription
		)
	}
	tmp <- unlist(f1(type))
	if(length(tmp)==0) {return(NA)
		}else if(type != 'JOB'){return(as.numeric(tmp))
		}else {return(tmp)
		}
	}
}
#Vectorize the function to m ake it faster
convert <- Vectorize(convert)
