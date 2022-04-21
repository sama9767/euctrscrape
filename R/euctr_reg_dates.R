#' @title euctr_reg_dates
#'
#' @description Retrieves the registration dates for a EUCTR entry
#'
#' @param trn A character vector containing a EUCTR trial registry
#'     number
#'
#' @return A vector of dates
#'
#' @export
#'
#' @importFrom magrittr "%>%"
euctr_reg_dates <- function (trn) {

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

    download.file(url, dlfile, method="wget", extra="--no-check-certificate --quiet")

    lines <- readLines(dlfile)

    dates <- c()

    for (line in lines) {
        if (grepl("^Date on which this record was first entered in the EudraCT database: [0-9]{4}-[0-9]{2}-[0-9]{2}$", line)) {

            dates <- c(
                dates,
                stringr::str_extract(line, "([0-9]{4}-[0-9]{2}-[0-9]{2})$")
            )
            
        }
    }

    dates <- sort(dates)
    
    return(dates)
    
}
