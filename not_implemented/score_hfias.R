#' score_hfias: Scored data from the Household Food Insecurity Access Scale
#'
#' This function scores the Household Food Insecurity Access Scale and provides an overall HFIAS Score and Food Insecurity Status
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'hfias#' where # is question number (1-9 and 1a-9a).
#' 3) Questions must have the numeric values for the choices. The options include:
#' 3a) Yes - 1, No - 0, I don't know or Don't want to answer - 99
#' 3b) Rarely - 1, Sometimes - 2, Often - 3, I don't know or Don't want to answer - 99
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Coates J, Swindale A, Bilinsky P. Household Food Insecurity Access Scale (HFIAS) for measurement of food access: indicator guide: version 3. Published online 2007.
#'
#' @param hfias_data a data.frame all items for the Household Food Insecurity Access Scale following the naming conventions described above
#' @inheritParams fbs_intake
#'
#' @return A dataset with scores for the Household Food Insecurity Access Scale
#' @examples
#'
#' # scoring for the hfias with IDs
#' hfias_score_data <- score_hfias(hfias_data, parID = 'ID')
#'
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{util_fbs_parent_v4dat}}
#'
#'
#' @export

score_hfias <- function(hfias_data, parID) {

    #### 1. Set up/initial checks #####

    # check that hfias_data exist and is a data.frame
    data_arg <- methods::hasArg(hfias_data)

    if (isTRUE(data_arg) & !is.data.frame(hfias_data)) {
        stop("hfias_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("hfias_data must set to the data.frame with amount consumed for each food item")
    }

    # check if parID exists
    ID_arg <- methods::hasArg(parID)

    if (isTRUE(ID_arg)){
        if (!(parID %in% names(hfias_data))) {
            stop("variable name entered as parID is not in hfias_data")
        }
    }

    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    hfias_score_dat <- data.frame(hfias_total = rep(NA, nrow(hfias_data)), hfias_category = rep(NA, nrow(hfias_data)))

    if (isTRUE(ID_arg)) {
        hfias_score_dat <- data.frame(hfias_data[[parID]], hfias_score_dat)
        names(hfias_score_dat)[1] <- parID
    }

    # set up labels for hfssm_score_dat
    hfias_score_dat_labels <- lapply(hfias_score_dat, function(x) attributes(x)$label)

    ## Score Subscales
    # HFIAS score
    hfias_vars <- c("hfias1a", "hfias2a", "hfias3a", "hfias4a", "hfias5a", "hfias5a", "hfias6a", "hfias7a", "hfias8a", "hfias9a")
    hfias_score_dat[["hfias_total"]] <- rowSums(hfias_data[hfias_vars], na.rm = TRUE)

    ## add labels to data
    hfias_score_dat_labels[["hfias_total"]] <- "HFIAS Total Score"

    # Food Insecurity Category
    hfias_score_dat[["hfias_category"]] <-
        ifelse((hfias_data[["hfias1"]] == 0 | hfias_data[["hfias1a"]] == 1) & ( hfias_data[["hfias2"]] == 0 & hfias_data[["hfias3"]] == 0 & hfias_data[["hfias4"]] == 0 & hfias_data[["hfias5"]] == 0 & hfias_data[["hfias6"]] == 0 & hfias_data[["hfias7"]] == 0 & hfias_data[["hfias8"]] == 0 & hfias_data[["hfias9"]] == 0), 0,
               ifelse((hfias_data[["hfias1a"]] > 1 | hfias_data[["hfias2a"]] > 0 | hfias_data[["hfias3a"]] == 1 | hfias_data[["hfias4a"]] == 1) & (hfias_data[["hfias5"]] == 0 & hfias_data[["hfias6"]] == 0 & hfias_data[["hfias7"]] == 0 & hfias_data[["hfias8"]] == 0 & hfias_data[["hfias9"]] == 0), 1,
                      ifelse((hfias_data[["hfias3a"]] > 1 | hfias_data[["hfias4a"]] > 1 | hfias_data[["hfias5a"]] == 1 | hfias_data[["hfias5a"]] == 2 | hfias_data[["hfias6a"]] == 1 | hfias_data[["hfias6a"]] == 2) & (hfias_data[["hfias7"]] == 0 & hfias_data[["hfias8"]] == 0 & hfias_data[["hfias9"]] == 0), 2,
                             ifelse(hfias_data[["hfias5a"]] == 3 | hfias_data[["hfias6a"]] == 3 | hfias_data[["hfias7a"]] > 0 | hfias_data[["hfias8a"]] > 0 | hfias_data[["hfias9a"]] > 0, 3, -99))))

    hfias_score_dat[["hfias_category"]] <- sjlabelled::add_labels(hfias_score_dat[["hfias_category"]], labels = c(`No Category Fit` = -99, `Severely Food Insecure` = 3, `Moderately Food Insecure` = 2, `Midly Food Insecure` = 1, `Food Secure` = 0))
    class(hfias_score_dat[["hfias_category"]]) <- c("haven_labelled", "vctrs_vctr", "double")


    hfias_score_dat_labels[["hfias_category"]] <- "HFIAS Food Insecurity Access Category"

    #### 3. Clean Export/Scored Data #####

    ## make sure the variable labels match in the dataset
    hfias_score_dat = sjlabelled::set_label(hfias_score_dat, label = matrix(unlist(hfias_score_dat_labels,
        use.names = FALSE)))

    return(hfias_score_dat)
}
