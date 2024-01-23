#' score_paq: Score the Physical Activity Questionnaire
#'
#' This function scores the parent-reported Physical Activity Questionnaire from visit 1 in order to calculate total
#' amount of sedentary to low physical activity (SLPA) and moderate to vigorous physical activity (MVPA) per day and week.
#' Optionally, this will also provide sleep duration per day.
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all items related to sleep and activity for all fives days requested (i.e., Monday - Friday)
#' 2) The  columns/variables must match the following naming conventions for each day item -
#' 'paq_d_item' where 'd' is the day (i.e., 'm', 't', 'w', 'th', 'f') and item is the scale item.
#' 3) The following scale items must be for each day: 'wakeup', 'indoor_lowintensity',
#' 'indoor_highintensity', 'outdoor_lowintensity', 'outdoor_highintensity', 'bedtime'
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Harro, M. (1997). Validation of a Questionnaire to Assess Physical Activity of Children Ages 4–8 Years. Research Quarterly for Exercise and Sport, 68(4), 259–268. https://doi.org/10.1080/02701367.1997.10608007 (\href{https://pubmed.ncbi.nlm.nih.gov/9421838/}{PubMed})
#'
#' @param paq_data a data.frame all items for the Physical Activity Questionnaire following the
#' naming conventions described above
#' @param study a string indicating which study collected the data. Currently, only option and default is 'fbs'. This
#' parameter is included so this script can be adapted for future studies that collect proper continuous data.
#' @param sleep (optional) A boolean indicator for whether parent-reported sleep should be estimated
#' for each day and week. Default = FALSE.
#' @inheritParams fbs_intake
#'
#' @return A dataset with amount of MVPA calculated for each day and week. If sleep = TRUE,
#' parent-reported sleep will also be included
#'
#' @examples
#'
#' # physical activity only
#' mvpa_score <- score_paq(paq_data, parID = 'ID')
#'
#' # physical activity AND sleep
#' mvpa_sleep_score <- score_paq(paq_data, sleep = TRUE, parID = 'ID')
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{util_fbs_parent_v1dat}}
#'
#'
#' @export

score_paq <- function(paq_data, study = "fbs", sleep = FALSE, parID) {

    #### 1. Set up/initial checks #####

    # check that paq_data exist and is a data.frame
    data_arg <- methods::hasArg(paq_data)

    if (isTRUE(data_arg) & !is.data.frame(paq_data)) {
        stop("paq_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("paq_data must set to the data.frame with amount consumed for each food item")
    }

    # check that study exists and is a string
    study_arg <- methods::hasArg(study)

    if (isTRUE(study_arg)) {
        if (study != "fbs" & study != "FBS") {
            stop("only option currently available for study is 'fbs'. If have an alternative
                 study you wish to use this script for please contact package maintainers to
                 get your study added")
        }
    }

    # check that sleep exist and is a string
    sleep_arg <- methods::hasArg(sleep)

    if (isTRUE(sleep_arg)) {
        if (sleep == "true" | sleep == "True" | sleep == "TRUE") {
            # convert to boolean
            sleep = TRUE
        } else if (sleep == "false" | sleep == "False" | sleep == "FALSE") {
            # convert to boolean
            sleep = FALSE
        }
    }

    if (!isTRUE(sleep) & !isFALSE(sleep)) {
        stop("sleep must be entered as a boolean and can either be: TRUE or FALSE")
    }

    # check if parID exists
    ID_arg <- methods::hasArg(parID)

    if (isTRUE(ID_arg)){
        if (!(parID %in% names(paq_data))) {
            stop("variable name entered as parID is not in paq_data")
        }
    }

    #### 2. Set Up Data #####

    # set up labels for pds_score_dat
    paq_dat_labels <- lapply(paq_data, function(x) attributes(x)$label)

    # set up database for results create empty matrix
    if (isTRUE(sleep)) {
        base_names <- c("wakeup", "indoor_lowintensity", "indoor_highintensity",
                        "outdoor_lowintensity", "outdoor_highintensity", "bedtime")

        # with sleep, have 6 variables per day per participant (row)
        paq_calc_dat <- data.frame(matrix(rep(NA, 6 * 5 * nrow(paq_data)), nrow = nrow(paq_data)))
    } else {
        base_names <- c("indoor_lowintensity", "indoor_highintensity", "outdoor_lowintensity",
                        "outdoor_highintensity")

        # without sleep, have 4 variables per day per participant (row)
        paq_calc_dat <- data.frame(matrix(rep(NA, 4 * 5 * nrow(paq_data)), nrow = nrow(paq_data)))
    }

    day_abreviations <- c("m", "t", "w", "th", "f")

    day_names <- NA

    for (d in 1:5) {
        day <- day_abreviations[d]

        for (n in 1:length(base_names)) {
            var <- base_names[n]

            day_names[n] <- paste0("paq_", day, "_", var, "_est")
        }

        if (d == 1) {
            paq_calc_names <- day_names
        } else {
            paq_calc_names <- c(paq_calc_names, day_names)
        }
    }

    # assign variable names
    names(paq_calc_dat) <- paq_calc_names

    if (isTRUE(ID_arg)) {
        paq_calc_dat <- data.frame(paq_data[[parID]], paq_calc_dat)
        names(paq_calc_dat)[1] <- parID
    }

    # set up labels for paq_calc_dat
    paq_calc_dat_labels <- lapply(paq_calc_dat, function(x) attributes(x)$label)

    # loop through variables in paq_data and convert to usable forms

    ## The FBS study collected data in a categorical manner so need to convert what
    ## we can to continuous values by assigning the middle of the provided ranges.
    ## values that can not be converted (e.g., >60 min) will be set to NA and can
    ## be analyzed separately in a categorical manner if desired
    if (study == "fbs" | study == "FBS") {
        for (var in 1:length(names(paq_data))) {
            var_name <- as.character(names(paq_data)[var])

            # name in new 'calc' dataset
            calc_name <- paste0(var_name, "_est")

            if (grepl("intensity", var_name, fixed = TRUE)) {
                # get center of intervals provided
                paq_calc_dat[[calc_name]] <- ifelse(paq_data[[var_name]] == 0, 0, ifelse(paq_data[[var_name]] == 1, 2.5, ifelse(paq_data[[var_name]] ==  2, 8, ifelse(paq_data[[var_name]] == 3, 13, ifelse(paq_data[[var_name]] == 4, 18, ifelse(paq_data[[var_name]] == 5, 23, ifelse(paq_data[[var_name]] == 6, 28, ifelse(paq_data[[var_name]] == 7, 33, ifelse(paq_data[[var_name]] == 8, 38, ifelse(paq_data[[var_name]] == 9, 43, ifelse(paq_data[[var_name]] == 10, 48, ifelse(paq_data[[var_name]] == 11, 53, ifelse(paq_data[[var_name]] == 12, 58, NA)))))))))))))
                # add labels to data
                paq_calc_dat_labels[[calc_name]] <- paste0(paq_dat_labels[[var_name]], ": middle of range chosen to get continuous data when scored")

            }

            # add nonsense dates to time so can calculate the difference
            if (grepl("wakeup", var_name, fixed = TRUE) & !grepl("_cat", var_name, fixed = TRUE)) {

                paq_calc_dat[[calc_name]] <- paq_data[[var_name]]

                # add labels to data
                paq_calc_dat_labels[[calc_name]] <- paq_dat_labels[[var_name]]

            } else if (grepl("bedtime", var_name, fixed = TRUE) & !grepl("_cat", var_name, fixed = TRUE)) {

                paq_calc_dat[[calc_name]] <- paq_data[[var_name]]

                # add labels to data
                paq_calc_dat_labels[[calc_name]] <- paq_dat_labels[[var_name]]
            }
        }
    }

    # calculate PAQ scores - loop through days
    for (d in 1:5) {
        day_str <- paste0("_", day_abreviations[d], "_")
        day_name <- ifelse(day_abreviations[d] == "m", "Monday", ifelse(day_abreviations[d] == "t", "Tuesday", ifelse(day_abreviations[d] == "w", "Wednesday", ifelse(day_abreviations[d] == "th", "Thursday", "Friday"))))

        # get varnames with matching day_str
        var_day_names <- names(paq_calc_dat)[grepl(day_str, names(paq_calc_dat),  fixed = TRUE)]

        # sedentary to low physical activity
        low_day_name <- paste0("paq", day_str, "slpa")

        low_vars <- grepl("lowintensity", var_day_names, fixed = TRUE)
        paq_calc_dat[[low_day_name]] <- rowSums(paq_calc_dat[var_day_names[low_vars]])

        # add labels to data
        paq_calc_dat_labels[[low_day_name]] <- paste0("Parent-Reported sedentary to low physical activity on ", day_name)

        # moderate to vigorous physical activity
        high_day_name <- paste0("paq", day_str, "mvpa")

        high_vars <- grepl("highintensity", var_day_names, fixed = TRUE)
        paq_calc_dat[[high_day_name]] <- rowSums(paq_calc_dat[var_day_names[high_vars]])

        # add labels to data
        paq_calc_dat_labels[[high_day_name]] <- paste0("Parent-Reported moderate to vigorous physical activity on ", day_name)


        # if sleep
        if (isTRUE(sleep)) {
            sleep_day_name <- paste0("paq", day_str, "sleep")
            bed_var <- paste0("paq", day_str, "bedtime_est")
            wake_var <- paste0("paq", day_str, "wakeup_est")

            # get time difference, arbitrarily assigning bedtime to 2021-1-1 and waketime
            # to 2021-1-2 could use Sys.Date(), but may be wrong if happens to run on a
            # daylight savings period
            sleep_dur_h <- as.POSIXct(paste("2021-01-02", paq_calc_dat[[wake_var]]), format = "%Y-%m-%d %H:%M:%S") - as.POSIXct(paste("2021-01-01", paq_calc_dat[[bed_var]]), format = "%Y-%m-%d %H:%M:%S")

            paq_calc_dat[[sleep_day_name]] <- as.numeric(sleep_dur_h)

            # add labels to data
            paq_calc_dat_labels[[sleep_day_name]] <- paste0("Parent-Reported sleep duration in hours on ", day_name)
        }
    }

    # total and average physical activity

    ## low intensity
    paq_calc_dat[["paq_slpa_total"]] <- rowSums(paq_calc_dat[c("paq_m_slpa", "paq_t_slpa",  "paq_w_slpa", "paq_th_slpa", "paq_f_slpa")])
    paq_calc_dat_labels[["paq_slpa_total"]] <- "Total sedentary to low physical activity for the week in minutes"

    paq_calc_dat[["paq_slpa_ndays_data"]] <- rowSums(!is.na(paq_calc_dat[c("paq_m_slpa", "paq_t_slpa", "paq_w_slpa", "paq_th_slpa", "paq_f_slpa")]))
    paq_calc_dat_labels[["paq_slpa_ndays_data"]] <- "Number of days with usable continuous data for sedentary to low physical activity - if \">60 min\" chosen, cannot use as continuous so set
    to NA. Will have to pursue categorical analyses for those data"

    paq_calc_dat[["paq_slpa_avg"]] <- rowMeans(paq_calc_dat[c("paq_m_slpa", "paq_t_slpa", "paq_w_slpa", "paq_th_slpa", "paq_f_slpa")], na.rm = TRUE)
    paq_calc_dat_labels[["paq_slpa_avg"]] <- "Average sedentary to low physical activity in minutes per day, ignoring days with missing data (see slpa_ndays_data)"

    ## high intensity
    paq_calc_dat[["paq_mvpa_total"]] <- rowSums(paq_calc_dat[c("paq_m_mvpa", "paq_t_mvpa", "paq_w_mvpa", "paq_th_mvpa", "paq_f_mvpa")])
    paq_calc_dat_labels[["paq_mvpa_total"]] <- "Total moderate to vigorous physical activity for the week in minutes"

    paq_calc_dat[["paq_mvpa_ndays_data"]] <- rowSums(!is.na(paq_calc_dat[c("paq_m_mvpa", "paq_t_mvpa", "paq_w_mvpa", "paq_th_mvpa", "paq_f_mvpa")]))
    paq_calc_dat_labels[["paq_mvpa_ndays_data"]] <- "Number of days with usable continuous data for moderate to vigorous physical activity - if \">60 min\" chosen, cannot use as continuous so set to NA. Will have to pursue categorical analyses for those data"

    paq_calc_dat[["paq_mvpa_avg"]] <- rowMeans(paq_calc_dat[c("paq_m_mvpa", "paq_t_mvpa", "paq_w_mvpa", "paq_th_mvpa", "paq_f_mvpa")], na.rm = TRUE)
    paq_calc_dat_labels[["paq_mvpa_avg"]] <- "Average moderate to vigorous physical activity in minutes per day, ignoring days with missing data (see mvpa_ndays_data)"

    ## sleep
    if (isTRUE(sleep)) {
        paq_calc_dat[["paq_sleep_ndays_data"]] <- rowSums(!is.na(paq_calc_dat[c("paq_m_sleep", "paq_t_sleep", "paq_w_sleep", "paq_th_sleep", "paq_f_sleep")]))
        paq_calc_dat_labels[["paq_sleep_ndays_data"]] <- "Number of days with usable continuous data for wakeup and bed times - if \"Before 5 am\", \"After 8 am\", or \"After 10 pm\" were chosen, cannot use as continuous so set to NA. Will have to pursue categorical analyses for those data"

        paq_calc_dat[["paq_sleep_avg"]] <- rowMeans(paq_calc_dat[c("paq_m_sleep", "paq_t_sleep", "paq_w_sleep", "paq_th_sleep", "paq_f_sleep")], na.rm = TRUE)
        paq_calc_dat_labels[["paq_sleep_avg"]] <- "Average moderate to vigorous physical activity in minutes per day, ignoring days with missing data (see sleep_ndays_data)"
    }

    #### 3. Clean Export/Scored Data ####

    ## make sure the variable labels match in the dataset
    paq_calc_dat = sjlabelled::set_label(paq_calc_dat, label = matrix(unlist(paq_calc_dat_labels,  use.names = FALSE)))

    return(paq_calc_dat)
}
