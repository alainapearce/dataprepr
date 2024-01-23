#' score_tesqe: Score data from the Tempest Self-Regulation Questionnaire for Eating
#'
#' This function scores the Tempest Self-Regulation Questionnaire for Eating for the following subscales: Avoidance of Temptations, Controlling Temptations, Distraction, Suppression, Setting Goals and Rules, and Goal Deliberation
#'
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'tesqe#' where # is the question number (1-24)
#' 3) All questions must have the numeric value for the choice:
#' 1 - Never, 2 - Rarely, 3 - Sometimes, 4 - Regularly, 5 - Always
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' De Vet E, De Ridder D, Stok M, Brunso K, Baban A, Gaspar T. Assessing self-regulation strategies: development and validation of the tempest self-regulation questionnaire for eating (TESQ-E) in adolescents. Int J Behav Nutr Phys Act. 2014;11(1):106. doi:10.1186/s12966-014-0106-z
#'
#' @param tesqe_data a data.frame all items for the Tempest Self-Regulation Questionnaire for Eating following the naming conventions described above
#' @inheritParams fbs_intake
#'
#' @return A dataset with a score for the Tempest Self-Regulation Questionnaire for Eating
#' @examples
#'
#' # scoring for the tesqe with IDs
#' tesqe_score_data <- score_tesqe(tesqe_data, parID = 'ID')
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{util_fbs_child_v2dat}} and \code{\link{util_fbs_child_v2dat_home}}
#'
#'
#' @export

score_tesqe <- function(tesqe_data, parID) {

    #### 1. Set up/initial checks #####

    # check that tesqe_data exist and is a data.frame
    data_arg <- methods::hasArg(tesqe_data)

    if (isTRUE(data_arg) & !is.data.frame(tesqe_data)) {
        stop("tesqe_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("tesqe_data must set to the data.frame with amount consumed for each food item")
    }

    # check if parID exists
    ID_arg <- methods::hasArg(parID)

    if (isTRUE(ID_arg)){
        if (!(parID %in% names(tesqe_data))) {
            stop("variable name entered as parID is not in tesqe_data")
        }
    }

    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    tesqe_score_dat <- data.frame(tesqe_avoid = rep(NA, nrow(tesqe_data)), tesqe_avoid_nNA = rep(NA, nrow(tesqe_data)), tesqe_control = rep(NA, nrow(tesqe_data)), tesqe_control_nNA = rep(NA, nrow(tesqe_data)), tesqe_distract = rep(NA, nrow(tesqe_data)), tesqe_distract_nNA = rep(NA, nrow(tesqe_data)), tesqe_supress = rep(NA, nrow(tesqe_data)), tesqe_supress_nNA = rep(NA, nrow(tesqe_data)), tesqe_goal_rules = rep(NA, nrow(tesqe_data)), tesqe_goal_rules_nNA = rep(NA, nrow(tesqe_data)), tesqe_goal_delib = rep(NA, nrow(tesqe_data)), tesqe_goal_delib_nNA = rep(NA, nrow(tesqe_data)))

    if (isTRUE(ID_arg)) {
        tesqe_score_dat <- data.frame(tesqe_data[[parID]], tesqe_score_dat)
        names(tesqe_score_dat)[1] <- parID
    }

    # set up labels for tesqe_score_dat
    tesqe_score_dat_labels <- lapply(tesqe_score_dat, function(x) attributes(x)$label)

    ## deal with -99's/Don't  Know
    tesqe_vars <- c("tesqe1", "tesqe2", "tesqe3", "tesqe4", "tesqe5", "tesqe6", "tesqe7", "tesqe8", "tesqe9", "tesqe10", "tesqe11", "tesqe12", "tesqe13", "tesqe14", "tesqe15", "tesqe16", "tesqe17", "tesqe18", "tesqe19", "tesqe20", "tesqe21", "tesqe22", "tesqe23", "tesqe24")

    for (v in 1:length(tesqe_vars)){
        var_name <- tesqe_vars[v]

        if(class(tesqe_data[var_name])[1] == 'haven_labelled'){
            haven::labelled(tesqe_data[[var_name]], labels = NULL)
        }

        tesqe_data[[var_name]] <- ifelse(is.na(tesqe_data[[var_name]]), NA, ifelse(tesqe_data[[var_name]] == -99, NA, as.numeric(tesqe_data[[var_name]])))
    }

    ## Avoidance of Temptations
    tesqe_avoid_vars <- c("tesqe1", "tesqe2", "tesqe3", "tesqe4")

    ## check number of NAs
    tesqe_score_dat[["tesqe_avoid_nNA"]] <- rowSums(is.na(tesqe_data[tesqe_avoid_vars]))

    #score if have at least 2 scores
    tesqe_score_dat[["tesqe_avoid"]] <- ifelse(tesqe_score_dat[["tesqe_avoid_nNA"]] > 2, NA, rowMeans(tesqe_data[tesqe_avoid_vars], na.rm = TRUE))

    ## add labels to data
    tesqe_score_dat_labels[["tesqe_avoid"]] <- "TESQE Avoidance of Temptations Total Score"
    tesqe_score_dat_labels[["tesqe_avoid_nNA"]] <- "TESQE Avoidance of Temptations Number of NA's/Don't Know"

    ## Controlling Temptations
    tesqe_control_vars <- c("tesqe5", "tesqe6", "tesqe7", "tesqe8")

    ## check number of NAs
    tesqe_score_dat[["tesqe_control_nNA"]] <- rowSums(is.na(tesqe_data[tesqe_control_vars]))

    #score if have at least 2 scores
    tesqe_score_dat[["tesqe_control"]] <- ifelse(tesqe_score_dat[["tesqe_control_nNA"]] > 2, NA, rowMeans(tesqe_data[tesqe_control_vars], na.rm = TRUE))

    ## add labels to data
    tesqe_score_dat_labels[["tesqe_control"]] <- "TESQE Controlling Temptations Total Score"
    tesqe_score_dat_labels[["tesqe_control_nNA"]] <- "TESQE Controlling Temptations Number of NA's/Don't Know"

    ## Distraction
    tesqe_distract_vars <- c("tesqe9", "tesqe10", "tesqe11", "tesqe12")

    ## check number of NAs
    tesqe_score_dat[["tesqe_distract_nNA"]] <- rowSums(is.na(tesqe_data[tesqe_distract_vars]))

    #score if have at least 2 scores
    tesqe_score_dat[["tesqe_distract"]] <- ifelse(tesqe_score_dat[["tesqe_distract_nNA"]] > 2, NA, rowMeans(tesqe_data[tesqe_distract_vars], na.rm = TRUE))

    ## add labels to data
    tesqe_score_dat_labels[["tesqe_distract"]] <- "TESQE Distraction Total Score"
    tesqe_score_dat_labels[["tesqe_distract_nNA"]] <- "TESQE Distraction Number of NA's/Don't Know"

    ##Suppression
    tesqe_supress_vars <- c("tesqe13", "tesqe14", "tesqe15", "tesqe16")

    ## check number of NAs
    tesqe_score_dat[["tesqe_supress_nNA"]] <- rowSums(is.na(tesqe_data[tesqe_supress_vars]))

    #score if have at least 2 scores
    tesqe_score_dat[["tesqe_supress"]] <- ifelse(tesqe_score_dat[["tesqe_supress_nNA"]] > 2, NA, rowMeans(tesqe_data[tesqe_supress_vars], na.rm = TRUE))

    ## add labels to data
    tesqe_score_dat_labels[["tesqe_supress"]] <- "TESQE Suppression Total Score"
    tesqe_score_dat_labels[["tesqe_supress_nNA"]] <- "TESQE Suppression Number of NA's/Don't Know"

    ## Setting Goals and Rules
    tesqe_goal_rules_vars <- c("tesqe17", "tesqe18", "tesqe19", "tesqe20")

    ## check number of NAs
    tesqe_score_dat[["tesqe_goal_rules_nNA"]] <- rowSums(is.na(tesqe_data[tesqe_goal_rules_vars]))

    #score if have at least 2 scores
    tesqe_score_dat[["tesqe_goal_rules"]] <- ifelse(tesqe_score_dat[["tesqe_goal_rules_nNA"]] > 2, NA, rowMeans(tesqe_data[tesqe_goal_rules_vars], na.rm = TRUE))

    ## add labels to data
    tesqe_score_dat_labels[["tesqe_goal_rules"]] <- "TESQE Setting Goals and Rules Total Score"
    tesqe_score_dat_labels[["tesqe_goal_rules_nNA"]] <- "TESQE Setting Goals and Rules Number of NA's/Don't Know"

    ## Goal Deliberation
    tesqe_goal_delib_vars <- c("tesqe21", "tesqe22", "tesqe23", "tesqe24")

    ## check number of NAs
    tesqe_score_dat[["tesqe_goal_delib_nNA"]] <- rowSums(is.na(tesqe_data[tesqe_goal_delib_vars]))

    #score if have at least 2 scores
    tesqe_score_dat[["tesqe_goal_delib"]] <- ifelse(tesqe_score_dat[["tesqe_goal_delib_nNA"]] > 2, NA, rowMeans(tesqe_data[tesqe_goal_delib_vars], na.rm = TRUE))

    ## add labels to data
    tesqe_score_dat_labels[["tesqe_goal_delib"]] <- "TESQE Goal Deliberation Total Score"
    tesqe_score_dat_labels[["tesqe_goal_delib_nNA"]] <- "TESQE Goal Deliberation Number of NA's/Don't Know"

    #### 3. Clean Export/Scored Data #####

    ## make sure the variable labels match in the dataset
    tesqe_score_dat = sjlabelled::set_label(tesqe_score_dat, label = matrix(unlist(tesqe_score_dat_labels,
        use.names = FALSE)))

    return(tesqe_score_dat)
}

