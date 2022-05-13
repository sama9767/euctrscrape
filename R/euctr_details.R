#' @title euctr_details
#'
#' @description Retrieves the start date, whether results were posted,
#'     the publication date and the completion date for a EUCTR entry
#'
#' @param trn A character vector containing a EUCTR trial registry
#'     number
#'
#' @param n_retry Integer number of times to try downloading again
#'     before failing, default 10
#'
#' @return A list containing: 1. a logical TRUE/FALSE indicating
#'     whether results are posted, 2. the trial start date, 3. a date
#'     indicating the first version publication date and 4. the
#'     "Global completion date" or the "Global end of trial date" if
#'     the "Global completion date" is not reported, and NA if neither
#'     is reported
#'
#' @examples
#' euctr_details("2012-001661-32") ## results
#' euctr_details("2020-005087-66") ## no results
#'
#' @export
#'
#' @importFrom magrittr "%>%"
euctr_details <- function (trn, n_retry = 10) {

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

    ## See if there are results

    url <- paste0(
        "https://www.clinicaltrialsregister.eu/ctr-search/",
        "search?query=",
        trn
    )

    resulttablecells <- c()

    retries <- 0
    
    while(length(resulttablecells) == 0 & retries < n_retry) {
        
        retries <- retries + 1
        
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
    
    resulttablelinks <- html %>%
        rvest::html_nodes("table.result td a") %>%
        rvest::html_text2()

    trial_results <- FALSE
    
    for (rtlink in resulttablelinks) {
        if (rtlink == "View results") {
            trial_results <- TRUE
        }
    }

    ## Get the start date

    startdate <- NA

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
    
    ## Now get the date of first results publication

    if (trial_results) {

        url <- paste0(
            "https://www.clinicaltrialsregister.eu/ctr-search/",
            "trial/",
            trn,
            "/results"
        )

        resulttablerows <- c()

        inner_retries <- 0
        
        while(length(resulttablerows) == 0 & inner_retries < n_retry) {

            inner_retries <- inner_retries + 1

            dlfile <- tempfile()
            
            ## I'd much prefer to use `polite`, but there's a TLS problem,
            ## so this is basically the only solution that works
            utils::download.file(
                       url, dlfile, method="curl", extra="-k -s"
                   )

            html <- rvest::read_html(dlfile)

            resulttablerows <- html %>%
                rvest::html_nodes("#resultContent table tr") %>%
                rvest::html_text2()

        }

        for (resrow in resulttablerows) {

            resrow <- gsub("[\n\r]", "", resrow)

            resrow <- resrow %>%
                stringr::str_extract(
                             "^First version publication date(.*)+"
                         )

            if (! is.na(resrow)) {
                pubdate <- sub(
                    "^First version publication date(.*)",
                    "\\1",
                    resrow
                ) %>%
                    trimws() %>%
                    as.Date(format="%d %b %Y") %>%
                    as.character()
                
            }
            
        }

        gcdate <- NA

        for (resrow in resulttablerows) {

            resrow <- gsub("[\n\r]", "", resrow)

            resrow <- resrow %>%
                stringr::str_extract(
                             "^Global completion date(.*)+"
                         )

            if (! is.na(resrow)) {
                gcdate <- sub(
                    "^Global completion date(.*)",
                    "\\1",
                    resrow
                ) %>%
                    trimws() %>%
                    as.Date(format="%d %b %Y") %>%
                    as.character()
                
            }
            
        }

        if (is.na(gcdate)) {

            for (resrow in resulttablerows) {

                resrow <- gsub("[\n\r]", "", resrow)

                resrow <- resrow %>%
                    stringr::str_extract(
                                 "^Global end of trial date(.*)+"
                             )

                if (! is.na(resrow)) {
                    gcdate <- sub(
                        "^Global end of trial date(.*)",
                        "\\1",
                        resrow
                    ) %>%
                        trimws() %>%
                        as.Date(format="%d %b %Y") %>%
                        as.character()
                    
                }
                
            }
            
        }

    } else {
        pubdate <- NA
        gcdate <- NA
    }

    to_return <- list(
        trn = trn,
        start_date = startdate,
        trial_results = trial_results,
        pub_date = pubdate,
        global_completion_date = gcdate
    )

    return(to_return)
    
}
