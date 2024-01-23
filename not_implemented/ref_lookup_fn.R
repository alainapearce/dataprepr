#' ref_lookup_fn: Reference documentation for lookup functions called in other scripts
#'
#' This function includes sub-functions that perform table lookup tasks. The included functions include: ref_brief2_lookup
#'
#'
#' @param ref_table string matching the prefix of the desired reference table (e.g., 'breif2' will find the brief2_scoretables.rda reference data)
#' @param value raw value/score to look up
#' @param age numeric value for age. Required for the following ref_tables: 'brief2'
#' @param sex numeric value for sex: 1 for male and 0 for female. Required for the following ref_tables: 'brief2'
#' @param item string matching the item/subscale prefix in lookup tables (e.g., to look up t-score for the BRIEF-2 inhibition subscale, enter 'inhibit')
#'
#' @return A list with values from the lookup table
#'
#' @examples
#'
#' # look up t-score and percentile for the inhibit subscale of BRIEF-2 for a 7-year-old male with a raw score of 14
#' brief_score_data <- ref_lookup_fn(brief2, value = 14, age = 7, sex = 0, item = 'inhibit')
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{qualtrics_parent_v4dat}}
#'
#'
#' @export

ref_lookup_fn <- function(ref_table, value, age, sex, item) {

    #### 1. Set up/initial checks #####

    # check that brief_data exist and is a data.frame
    ref_arg <- methods::hasArg(ref_table)

    if (isTRUE(ref_arg) & !is.character(ref_arg)) {
        stop("ref_table must be entered as a character")
    } else if (isFALSE(data_arg)) {
        stop("ref_table must be a string matching the prefix of the desired reference table")
    }

    # check reference table requirements
    if (ref_table == 'brief2'){

        ## age
        age_arg <- methods::hasArg(age)

        if (isTRUE(age_arg)) {
            if (is.character(age)) {
                age <- as.numeric(age)
            }
        } else {
            stop('a numeric value for age is required to use the brief2 lookup table')
        }

        ## sex
        sex_arg <- methods::hasArg(sex)

        if (isTRUE(sex_arg)) {
            if (is.character(sex)) {
                if (sex == '0' | sex == '1') {
                    sex <- as.numeric(sex)
                } else {
                    stop('sex must be entered as a numeric value with male = 0 and female = 1')
                }
            } else {
                if (sex != 0 & sex != 1) {
                    stop('sex must be entered as a numeric value with male = 0 and female = 1')
                }
            }
        } else {
            stop('a numeric value for sex (male = 0 and female = 1) is required to use the brief2 lookup table')
        }
    }

    #### BRIEF-2 ####
    if (ref_table == 'brief2') {
        scores <- ref_brief2_lookup(sex, age, value, item)
        return(scores)
    }
}

