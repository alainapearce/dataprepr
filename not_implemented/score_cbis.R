#' score_cbis: Score data from the Children's Body Image Scale
#'
#' This function scores the Children's Body Image Scale
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'cbis_perc_x' and 'cbis_ideal_x' where x is male or female
#' 3) All questions must have the numeric value for the choice: 1- Picture A, 2 - Picture B, 3 - Picture C, 4 - Picture D, 5 - Picture E, 6 - Picture F, 7- Picture G
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Truby H, Paxton SJ. Development of the Children’s Body Image Scale. British Journal of Clinical Psychology. 2002;41(2):185-203. doi:10.1348/014466502163967 (\href{https://pubmed.ncbi.nlm.nih.gov/12034005/}{PubMed})
#'
#' @param cbis_data a data.frame all items for the Children's Body Image Scale following the naming conventions described above
#' @inheritParams fbs_intake
#'
#' @return A dataset with a score for the Children's Body Image Scale
#' @examples
#'
#' # scoring for the cbis with IDs
#' cbis_score_data <- score_cbis(cbis_data, parID = 'ID')
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{util_fbs_child_v4dat}} and \code{\link{util_fbs_child_v4dat_home}}
#'
#'
#' @export

score_cbis <- function(cbis_data, parID) {

    #### 1. Set up/initial checks #####

    # check that cbis_data exist and is a data.frame
    data_arg <- methods::hasArg(cbis_data)

    if (isTRUE(data_arg) & !is.data.frame(cbis_data)) {
        stop("cbis_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("cbis_data must set to the data.frame with amount consumed for each food item")
    }

    # check if parID exists
    ID_arg <- methods::hasArg(parID)

    if (isTRUE(ID_arg)){
        if (!(parID %in% names(cbis_data))) {
            stop("variable name entered as parID is not in cbis_data")
        }
    }

    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    cbis_score_dat <- data.frame(cbis_score = rep(NA, nrow(cbis_data)), cbis_score_abs = rep(NA, nrow(cbis_data)))

    if (isTRUE(ID_arg)) {
        cbis_score_dat <- data.frame(cbis_data[[parID]], cbis_score_dat)
        names(cbis_score_dat)[1] <- parID
    }

    # set up labels for cbis_score_dat
    cbis_score_dat_labels <- lapply(cbis_score_dat, function(x) attributes(x)$label)

    # difference score
    cbis_score_dat[["cbis_score"]] <- ifelse(!is.na(cbis_data[["cbis_perc_male"]]) & !is.na(cbis_data[["cbis_ideal_male"]]), cbis_data[["cbis_perc_male"]] - cbis_data[["cbis_ideal_male"]], ifelse(!is.na(cbis_data[["cbis_perc_female"]]) & !is.na(cbis_data[["cbis_ideal_female"]]), cbis_data[["cbis_perc_female"]] - cbis_data[["cbis_ideal_female"]], NA))

    ## add labels to data
    cbis_score_dat_labels[["cbis_score"]] <- "CBIS Difference Score (Percieved - Ideal)"

    # absolute difference score
    cbis_score_dat[["cbis_score_abs"]] <- ifelse(is.na(cbis_score_dat[["cbis_score"]]), NA, ifelse(cbis_score_dat[["cbis_score"]] < 0, abs(cbis_score_dat[["cbis_score"]]), cbis_score_dat[["cbis_score"]]))

    ## add labels to data
    cbis_score_dat_labels[["cbis_score_abs"]] <- "CBIS Absolute Value of Difference Score"

    #### 3. Clean Export/Scored Data #####

    ## make sure the variable labels match in the dataset
    cbis_score_dat = sjlabelled::set_label(cbis_score_dat, label = matrix(unlist(cbis_score_dat_labels,
        use.names = FALSE)))

    return(cbis_score_dat)
}

