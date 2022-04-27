#' @title euctr_start_date
#'
#' @description Retrieves the start date for a EUCTR entry
#'
#' @param trn A character vector containing a EUCTR trial registry
#'     number
#'
#' @param n_retry A maximum number of times that the function should
#'     try to download the page
#'
#' @return A character vector containing an ISO-8601 formatted date
#'
#' @export
#'
#' @importFrom magrittr %>%
euctr_start_date <- function (trn, n_retry = 10) {

    ## Check for well-formed input
    assertthat::assert_that(
                    is.character(trn),
                    grepl(
                        "^[0-9]{4}-[0-9]{6}-[0-9]{2}$",
                        trn
                    )
                )

    ## Check that the number of times to retry is numeric and an
    ## integer
    assertthat::assert_that(
                    is.numeric(n_retry),
                    n_retry %% 1 == 0
                )

    url <- paste0(
        "https://www.clinicaltrialsregister.eu/ctr-search/",
        "search?query=",
        trn
    )

    resulttablecells <- c()

    while(length(resulttablecells) == 0) {

        dlfile <- tempfile()
        
        ## I'd much prefer to use `polite`, but there's a TLS problem,
        ## so this is basically the only solution that works
        utils::download.file(
                   url, dlfile, method="curl", extra="-k -s"
               )

        html <- rvest::read_html(dlfile)

        resulttablecells <- html %>%
            rvest::html_nodes("table.result td") %>%
            rvest::html_text2()
        
    }

    for (rcell in resulttablecells) {

        rcell <- rcell %>%
            stringr::str_extract(
                         "^Start Date.: [0-9]{4}-[0-9]{2}-[0-9]{2}$"
                     )

        if (! is.na(rcell)) {
            
            startdate <- sub(
                "^Start Date.: ([0-9]{4}-[0-9]{2}-[0-9]{2})$",
                "\\1",
                rcell
            )
            
        }

    }

    return(startdate)
    
}
