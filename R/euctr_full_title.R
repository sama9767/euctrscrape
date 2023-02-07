#' @title euctr_full_title
#'
#' @description Retrieves full title for a EUCTR entry from the protocol
#'
#' @param trn A character vector containing a EUCTR trial registry
#'     number
#'
#' @return full title of the trial
#'

library(stringr)

euctr_full_title <- function (trn) {
  
  ## Check for well-formed input
  assertthat::assert_that(
    is.character(trn),
    grepl(
      "^[0-9]{4}-[0-9]{6}-[0-9]{2}$",
      trn
    )
  )
  
  url <- paste0(
   "https://www.clinicaltrialsregister.eu/",
    "ctr-search/rest/download/full?query=",
    trn,
    "&mode=current_page"
  )
  

  dlfile <- tempfile()
  
  ## I'd much prefer to use `polite`, but there's a TLS problem, so
  ## this is basically the only solution that works
  utils::download.file(url, dlfile, method="curl", extra="-k -s")
  
  lines <- readLines(dlfile)
  
  full_title <- NA

 
 # Search for a "full title of the trial"
 for (line in lines) {
  if(str_detect(line,pattern = fixed("A.3 Full title of the trial: "))){
    full_title <- str_extract(line, pattern = "*:.")
    }
        
      
}
print(full_title)
 }


