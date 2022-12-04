#' @title euctr_reg_identifiers
#'
#' @description Retrieves other identifiers for a EUCTR entry
#'
#' @param trn A character vector containing a EUCTR trial registry
#'     number
#'
#' @return A vector of additional identifiers
#'
#' @export
#'
#' @importFrom magrittr "%>%"


euctr_reg_identifiers <- function (trn) {

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
    
    ctg_identifier <- NA
    other_identifier <- NA
    
    # Search for a CT.gov identifier
    for (line in lines) {
        if (grepl("A\\.5\\.2 US NCT \\(ClinicalTrials\\.gov registry\\) number: (?i)NCT\\W*0\\d{7}", line)) {
            
            if (!is.na(ctg_identifier)) {
                print("More than one NCT identifier found!")
                print(trn)
                break
            }
            
            ctg_identifier <- stringr::str_extract(line, "(?i)NCT\\W*0\\d{7}")
        }
    }

    # Search further identifiers
    if (length(lines) > 0 & length(grep("^A.5.4 Other Identifiers:", lines))) {
    
        index <- which(lines == "A.5.4 Other Identifiers:")
        if (length(index) > 1) {
            print("More than one other identifier found!")
            print(trn)
            index <- index[1]
        }
        other_identifier <- lines[index + 1]
    }
    
    
    return(c(ctg_identifier, other_identifier))
}
