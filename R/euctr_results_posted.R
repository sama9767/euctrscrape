#' @title euctr_results_posted
#'
#' @description Retrieves the date that results were first posted if
#'     there is a link to trial results for a EUCTR entry
#'
#' @param trn A character vector containing a EUCTR trial registry
#'     number
#'
#' @return A list containing a logical TRUE/FALSE indicating whether
#'     results are posted and a date indicating the first version
#'     publication date
#'
#' @export
#'
#' @importFrom magrittr "%>%"
euctr_results_posted <- function (trn, n_retry = 10) {

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
    
    resulttablelinks <- html %>%
        rvest::html_nodes("table.result td a") %>%
        rvest::html_text2()

    trial_results <- FALSE
    
    for (rtlink in resulttablelinks) {
        if (rtlink == "View results") {
            trial_results <- TRUE
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

        while(length(resulttablerows) == 0) {

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

    } else {
        pubdate <- NA
    }

    

    to_return <- list(
        trial_results = trial_results,
        pub_date = pubdate
    )

    return(to_return)
    
}

euctr_results_posted("2012-001661-32") ## results

euctr_results_posted("2020-005087-66") ## no results
