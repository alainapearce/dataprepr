#' ref_brief2_lookup: Lookup function the Behavioral Rating Inventory of Executive Function-2
#'
#' This function pulls t-scores and percentiles based on age and sex for the the Behavioral Rating Inventory of Executive Function-2 using brief2_scoretables data available in dataprepr
#'
#' @param value raw value/score to look up
#' @param age numeric value for age. Required for the following for subscale, index, and GEC values. Not needed for quality control scales (i.e., negativity, inconsistency, and frequency)
#' @param sex numeric value for sex: 1 for male and 0 for female. equired for the following for subscale, index, and GEC values. Not needed for quality control scales (i.e., negativity, inconsistency, and frequency)
#' @param item string matching the item/subscale prefix in lookup tables (e.g., to look up t-score for the BRIEF-2 inhibition subscale, enter 'inhibit')
#'
#' @return A list with values from the lookup table
#'
#' @examples
#'
#' # look up t-score and percentile for the inhibit subscale of BRIEF-2 for a 7-year-old male with a raw score of 14
#' brief_score_data <- ref_brief2_lookup(value = 14, age = 7, sex = 0, item = 'inhibit')
#'
#' \dontrun{
#' }
#'
#' @seealso This can be called from general script: \code{\link{ref_lookup_fn}}
#'
#'
#' @export

#### BRIEF-2 ####
ref_brief2_lookup <- function(sex, age, value, item) {

    #### 1. Set up/initial checks #####

    # check reference table requirements

    ## item
    item_arg <- methods::hasArg(item)

    if (isTRUE(item_arg)) {
        if (!is.character(item)){
            stop('item must be a string matching the item/subscale prefix in lookup tables')
        }
    } else {
        stop('must enter the item to reference in the table')
    }

    #get raw value column
    subscale_vars <- c('inhibit', 'selfmon', 'shift', 'emcont', 'initiate', 'wm', 'planorg', 'taskmon', 'orgmat')
    index_vars <- c('bri', 'eri', 'cri')
    qc_vars <- c('negativity', 'inconsistency', 'infrequency')

    ## only need age and sex if looking up subscale or composite scores
    if (item %in% subscale_vars | item %in% index_vars | item == 'gec'){
        ## age
        age_arg <- methods::hasArg(age)

        if (isTRUE(age_arg)) {
            if (is.na(age)){
                scores <- list(item = item, tscore = NA, percentile = NA)
                return(scores)
            } else if (is.character(age)) {
                age <- as.numeric(age)
            }
        } else {
            stop('a numeric value for age is required to use the brief2 lookup table')
        }

        ## sex
        sex_arg <- methods::hasArg(sex)
        if (isTRUE(sex_arg)) {
            if (is.na(sex)){
                scores <- list(item = item, tscore = NA, percentile = NA)
                return(scores)
            } else if (is.character(sex)) {
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

        #identify data.frame for age and sex
        if (sex == 0){
            if (age < 5){
                stop('minimum age for the BRIEF-2 is 5 years old')
            } else if (age < 8){
                ref_dat <- as.data.frame(dataprepr::brief2_scoretables$boys5_7)
            } else if (age < 11){
                ref_dat <- as.data.frame(dataprepr::brief2_scoretables$boys8_10)
            } else if (age < 14){
                ref_dat <- as.data.frame(dataprepr::brief2_scoretables$boys11_13)
            } else if (age <= 18){
                ref_dat <- as.data.frame(dataprepr::brief2_scoretables$boys14_18)
            } else {
                stop('maximum age for the BRIEF-2 is 18 years old')
            }
        } else {
            if (age < 5){
                stop('minimum age for the BRIEF-2 is 5 years old')
            } else if (age < 8){
                ref_dat <- as.data.frame(dataprepr::brief2_scoretables$girls5_7)
            } else if (age < 11){
                ref_dat <- as.data.frame(dataprepr::brief2_scoretables$girls8_10)
            } else if (age < 14){
                ref_dat <- as.data.frame(dataprepr::brief2_scoretables$girls11_13)
            } else if (age <= 18){
                ref_dat <- as.data.frame(dataprepr::brief2_scoretables$girls14_18)
            } else {
                stop('maximum age for the BRIEF-2 is 18 years old')
            }
        }
    }

    ## value
    value_arg <- methods::hasArg(value)

    if (isTRUE(value_arg)) {
        if (is.na(value)){
            scores <- list(item = item, tscore = NA, percentile = NA)
            return(scores)
        } else if (is.character(value)) {
            value <- as.numeric(value)
        }
    } else {
        stop('a numeric value for is required for the value argument to use the brief2 lookup table')
    }

    if (item %in% subscale_vars){
        #scale variable names
        t_var <- paste0(item, '_t')
        p_var <- paste0(item, '_p')

        #score lookup
        tscore <- ref_dat[!is.na(ref_dat[['subscale_raw']]) & ref_dat[['subscale_raw']] == value, t_var]
        percentile <- ref_dat[!is.na(ref_dat[['subscale_raw']]) & ref_dat[['subscale_raw']] == value, p_var]

        #save and return
        scores <- list(item = item, tscore = tscore, percentile = percentile)
        return(scores)

    } else if (item %in% index_vars){
        #scale variable names
        t_var <- paste0(item, '_t')
        p_var <- paste0(item, '_p')

        #score lookup
        tscore <- ref_dat[!is.na(ref_dat[['index_raw']]) & ref_dat[['index_raw']] == value, t_var]
        percentile <- ref_dat[!is.na(ref_dat[['index_raw']]) & ref_dat[['index_raw']] == value, p_var]

        #save and return
        scores <- list(item = item, tscore = tscore, percentile = percentile)
        return(scores)

    } else if (item %in% qc_vars) {
        #score lookup
        if (item == 'inconsistency'){
            percentile <- ifelse(value <= 6, '<=98', ifelse(value < 11, '99', '>99'))
            category <- ifelse(value <= 6, 'Acceptable', ifelse(value < 11, 'Questionable', 'Inconsistant'))
        } else if (item == 'negativity'){
            percentile <- ifelse(value <= 6, '<=98', ifelse(value == 7, '99', '>99'))
            category <- ifelse(value <= 6, 'Acceptable', ifelse(value == 7, 'Elevated', 'Highly Elevated'))
        } else if (item == 'infrequency'){
            percentile <- ifelse(value == 0, '99', '>99')
            category <- ifelse(value == 0, 'Acceptable', 'Questionnable')
        }

        #save and return
        scores <- list(item = item, percentile = percentile, category = category)
        return(scores)

    } else {
        #scale variable names
        t_var <- paste0(item, '_t')
        p_var <- paste0(item, '_p')

        tscore <- ref_dat[!is.na(ref_dat[['gec_raw']]) & ref_dat[['gec_raw']] == value, t_var]
        percentile <- ref_dat[!is.na(ref_dat[['gec_raw']]) & ref_dat[['gec_raw']] == value, p_var]

        scores <- list(item = item, tscore = tscore, percentile = percentile)
        return(scores)
    }
}

