#' score_cchip: Scored data from the Community Childhood Hunger Identification Project
#'
#' This function scores the Community Childhood Hunger Identification Project and provides an overall CCHIP Score and Food Insecurity Status
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'cchip#' where # is question number. For the Food and Brain Study, the 8 primary questions needed are: cchip1, cchip5, cchip9, cchip13, cchip17, cchip21, cchip25, and cchip29. Can be adapted to processes other data/studies in the future.
#' 3) The primary indicatory questions must have the following numeric values: Yes - 1, No - 0
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Wehler CA, Scott RI, Anderson JJ. The community childhood hunger identification project: A model of domestic hunger—Demonstration project in Seattle, Washington. Journal of Nutrition Education. 1992;24(1):29S-35S. doi:10.1016/S0022-3182(12)80135-X
#'
#' @param cchip_data a data.frame all items for the Community Childhood Hunger Identification Project following the naming conventions described above
#' @inheritParams fbs_intake
#'
#' @return A dataset with scores for the Community Childhood Hunger Identification Project
#' @examples
#'
#' # scoring for the cchip with IDs
#' cchip_score_data <- score_cchip(cchip_data, parID = 'ID')
#'
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{util_fbs_parent_v4dat}}
#'
#'
#' @export

score_cchip <- function(cchip_data, parID) {

    #### 1. Set up/initial checks #####

    # check that cchip_data exist and is a data.frame
    data_arg <- methods::hasArg(cchip_data)

    if (isTRUE(data_arg) & !is.data.frame(cchip_data)) {
        stop("cchip_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("cchip_data must set to the data.frame with amount consumed for each food item")
    }

    # check if parID exists
    ID_arg <- methods::hasArg(parID)

    if (isTRUE(ID_arg)){
        if (!(parID %in% names(cchip_data))) {
            stop("variable name entered as parID is not in cchip_data")
        }
    }

    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    cchip_score_dat <- data.frame(cchip_total = rep(NA, nrow(cchip_data)), cchip_category = rep(NA, nrow(cchip_data)))

    if (isTRUE(ID_arg)) {
        cchip_score_dat <- data.frame(cchip_data[[parID]], cchip_score_dat)
        names(cchip_score_dat)[1] <- parID
    }

    # set up labels for hfssm_score_dat
    cchip_score_dat_labels <- lapply(cchip_score_dat, function(x) attributes(x)$label)

    ## Score Subscales
    # CCHIP score
    cchip_vars <- c('cchip1', 'cchip5', 'cchip9', 'cchip13', 'cchip17', 'cchip21', 'cchip25', 'cchip29')
    cchip_score_dat[["cchip_total"]] <- rowSums(cchip_data[cchip_vars], na.rm = TRUE)

    ## add labels to data
    cchip_score_dat_labels[["cchip_total"]] <- "CCHIP Total Score"

    # Food Insecurity Category
    cchip_score_dat[["cchip_category"]] <- ifelse(cchip_score_dat[["cchip_total"]] >= 5, 2, ifelse(cchip_score_dat[["cchip_total"]] >= 1, 1, 0))

    cchip_score_dat[["cchip_category"]] <- sjlabelled::add_labels(cchip_score_dat[["cchip_category"]], labels = c(`Hungry` = 2, `At Risk for Hunger` = 1, `Not Hungry` = 0))
    class(cchip_score_dat[["cchip_category"]]) <- c("haven_labelled", "vctrs_vctr", "double")

    cchip_score_dat_labels[["cchip_category"]] <- "CCHIP Hunger Category"

    #### 3. Clean Export/Scored Data #####

    ## make sure the variable labels match in the dataset
    cchip_score_dat = sjlabelled::set_label(cchip_score_dat, label = matrix(unlist(cchip_score_dat_labels,
        use.names = FALSE)))

    return(cchip_score_dat)
}
