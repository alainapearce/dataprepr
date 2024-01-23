#' score_cwc: Score data from the Child Weight Concerns
#'
#' This function scores the Child Weight Concerns
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'cwc#' where # is the question number (1-5)
#' 3) All questions must have the numeric value for the choice starting with the value 1. Each item is scored differently so responses are scaled to be 0-100 for each item.
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Killen JD, Taylor CB, Hayward C, et al. Pursuit of thinness and onset of eating disorder symptoms in a community sample of adolescent girls: A three-year prospective analysis. Int J Eat Disord. 1994;16(3):227-238. doi:10.1002/1098-108X(199411)16:3<227::AID-EAT2260160303>3.0.CO;2-L (\href{https://pubmed.ncbi.nlm.nih.gov/7833956/}{PubMed})
#'
#' Taylor CB, Sharpe T, Shisslak C, et al. Factors associated with weight concerns in adolescent girls. Int J Eat Disord. 1998;24(1):31-42. doi:10.1002/(SICI)1098-108X(199807)24:1<31::AID-EAT3>3.0.CO;2-1 (\href{https://pubmed.ncbi.nlm.nih.gov/9589309/}{PubMed})
#'
#' @param cwc_data a data.frame all items for the Child Weight Concerns following the naming conventions described above
#' @inheritParams fbs_intake
#'
#' @return A dataset with a score for the Child Weight Concerns
#' @examples
#'
#' # scoring for the cwc with IDs
#' cwc_score_data <- score_cwc(cwc_data, parID = 'ID')
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{util_fbs_child_v4dat}} and \code{\link{util_fbs_child_v4dat_home}}
#'
#'
#' @export

score_cwc <- function(cwc_data, parID) {

    #### 1. Set up/initial checks #####

    # check that cwc_data exist and is a data.frame
    data_arg <- methods::hasArg(cwc_data)

    if (isTRUE(data_arg) & !is.data.frame(cwc_data)) {
        stop("cwc_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("cwc_data must set to the data.frame with amount consumed for each food item")
    }

    # check if parID exists
    ID_arg <- methods::hasArg(parID)

    if (isTRUE(ID_arg)){
        if (!(parID %in% names(cwc_data))) {
            stop("variable name entered as parID is not in cwc_data")
        }
    }

    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    cwc_score_dat <- data.frame(cwc_total = rep(NA, nrow(cwc_data)))

    if (isTRUE(ID_arg)) {
        cwc_score_dat <- data.frame(cwc_data[[parID]], cwc_score_dat)
        names(cwc_score_dat)[1] <- parID
    }

    # set up labels for cwc_score_dat
    cwc_score_dat_labels <- lapply(cwc_score_dat, function(x) attributes(x)$label)

    ## deal with -99's/Don't  Know
    cwc_vars <- c("cwc1", "cwc2", "cwc3", "cwc4", "cwc5")

    for (v in 1:length(cwc_vars)){
        var_name <- cwc_vars[v]

        if(class(cwc_data[var_name])[1] == 'haven_labelled'){
            haven::labelled(cwc_data[[var_name]], labels = NULL)
        }

        cwc_data[[var_name]] <- ifelse(is.na(cwc_data[[var_name]]), NA, ifelse(cwc_data[[var_name]] == -99, NA, as.numeric(cwc_data[[var_name]])))
    }

    ## scale values to be 0-100
    qs_5scale <- c("cwc1", "cwc2", "cwc5")

    for (v in 1:length(qs_5scale)){
        var_name <- qs_5scale[v]

        cwc_data[[var_name]] <- ifelse(is.na(cwc_data[[var_name]]), NA, ifelse(cwc_data[[var_name]] == 1, 0, ifelse(cwc_data[[var_name]] == 2, 16.67, ifelse(cwc_data[[var_name]] == 3, 50, ifelse(cwc_data[[var_name]] == 4, 75, ifelse(cwc_data[[var_name]] == 5, 100, NA))))))
    }

    # 7-chioce question
    cwc_data[["cwc3"]] <- ifelse(is.na(cwc_data[["cwc3"]]), NA, ifelse(cwc_data[["cwc3"]] == 1, 0, ifelse(cwc_data[["cwc3"]] == 2, 25, ifelse(cwc_data[["cwc3"]] == 3, 33.34, ifelse(cwc_data[["cwc3"]] == 4, 50, ifelse(cwc_data[["cwc3"]] == 5, 66.68, ifelse(cwc_data[["cwc3"]] == 6, 83.35, ifelse(cwc_data[["cwc3"]] == 7, 100, NA))))))))

    # 4-chioce question
    cwc_data[["cwc4"]] <- ifelse(is.na(cwc_data[["cwc4"]]), NA, ifelse(cwc_data[["cwc4"]] == 1, 0, ifelse(cwc_data[["cwc4"]] == 2, 33.33, ifelse(cwc_data[["cwc4"]] == 3, 66.66, ifelse(cwc_data[["cwc4"]] == 4, 100, NA)))))

    #score if have at least 2 scores
    cwc_vars <- c("cwc1", "cwc2", "cwc3", "cwc4", "cwc5")
    cwc_score_dat[["cwc_total"]] <- rowMeans(cwc_data[cwc_vars])

    ## add labels to data
    cwc_score_dat_labels[["cwc_total"]] <- "CWC Total Score"

    #### 3. Clean Export/Scored Data #####

    ## make sure the variable labels match in the dataset
    cwc_score_dat = sjlabelled::set_label(cwc_score_dat, label = matrix(unlist(cwc_score_dat_labels,
        use.names = FALSE)))

    return(cwc_score_dat)
}

