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
    m <- str_match(line,"A\\.3 Full title of the trial:(.+)")
    
    if(!is.na(m[2])){
      if(!is.na(full_title)){
        print("More than one full title found")
        print(trn)
        break
        
      }
      
      full_title <- m[2]
      
      
    }
    
    
  }
  return(full_title)
}


    

