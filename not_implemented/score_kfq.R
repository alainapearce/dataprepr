#' score_kfq: Score data from the Kid's Food Questionnaire
#'
#' This function scores the Kid's Food Questionnaire, which is a lab generated measure of child reported food frequency. The modal frequency and score is calculated for the following subscales: Breads and Grains, Vegetables, Fruits, Dairy, Protein, Beverages, Snacks, Deserts, Condiments.
#'
#' Note: number of items in each category differs so the total score (i.e., sum) cannot be compared across categories.
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items
#' 2) The  columns/variables must match the following naming convention: 'kfq#' where # is the question number (1-46)
#' 3) All questions must have the numeric value for the choice:
#' 0 - Never Eat This, 1 - Less than once in 7 days, 2 - 1-2 times in 7 days, 3 - 3-5 times in 7 days, 4 - 6-7 times in 7 days, 5 - More than 7 times in 7 days
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @param kfq_data a data.frame all items for the Kid's Food Questionnaire following the naming conventions described above
#' @inheritParams fbs_intake
#'
#' @return A dataset with a score for the Kid's Food Questionnaire
#' @examples
#'
#' # scoring for the kfq with IDs
#' kfq_score_data <- score_kfq(kfq_data, parID = 'ID')
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{util_fbs_child_v2dat}} and \code{\link{util_fbs_child_v2dat_home}}
#'
#'
#' @export

score_kfq <- function(kfq_data, parID) {

    #### 1. Set up/initial checks #####

    # check that kfq_data exist and is a data.frame
    data_arg <- methods::hasArg(kfq_data)

    if (isTRUE(data_arg) & !is.data.frame(kfq_data)) {
        stop("kfq_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("kfq_data must set to the data.frame with amount consumed for each food item")
    }

    # check if parID exists
    ID_arg <- methods::hasArg(parID)

    if (isTRUE(ID_arg)){
        if (!(parID %in% names(kfq_data))) {
            stop("variable name entered as parID is not in kfq_data")
        }
    }

    ## function for mode
    mode <- function(x) {
        uniqv <- unique(as.numeric(x))
        uniqv[which.max(tabulate(match(x, uniqv)))]
    }

    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    kfq_score_dat <- data.frame(kfq_grains = rep(NA, nrow(kfq_data)), kfq_grains_modefreq = rep(NA, nrow(kfq_data)), kfq_veg = rep(NA, nrow(kfq_data)), kfq_veg_modefreq = rep(NA, nrow(kfq_data)), kfq_fruit = rep(NA, nrow(kfq_data)), kfq_fruit_modefreq = rep(NA, nrow(kfq_data)), kfq_dairy = rep(NA, nrow(kfq_data)), kfq_dairy_modefreq = rep(NA, nrow(kfq_data)), kfq_protein = rep(NA, nrow(kfq_data)), kfq_protein_modefreq = rep(NA, nrow(kfq_data)), kfq_bev = rep(NA, nrow(kfq_data)), kfq_bev_modefreq = rep(NA, nrow(kfq_data)), kfq_snack = rep(NA, nrow(kfq_data)), kfq_snack_modefreq = rep(NA, nrow(kfq_data)), kfq_desert = rep(NA, nrow(kfq_data)), kfq_desert_modefreq = rep(NA, nrow(kfq_data)), kfq_cond = rep(NA, nrow(kfq_data)), kfq_cond_modefreq = rep(NA, nrow(kfq_data)))

    if (isTRUE(ID_arg)) {
        kfq_score_dat <- data.frame(kfq_data[[parID]], kfq_score_dat)
        names(kfq_score_dat)[1] <- parID
    }

    # set up labels for kfq_score_dat
    kfq_score_dat_labels <- lapply(kfq_score_dat, function(x) attributes(x)$label)


    ## Breads and Grains
    kfq_grain_vars <- c("kfq1", "kfq2", "kfq3", "kfq4", "kfq5", "kfq6", "kfq7")
    kfq_score_dat[["kfq_grains"]] <- rowSums(kfq_data[kfq_grain_vars])

    #get modal frequency
    ## get each row as vector in list
    kfq_grain_list <- as.list(data.frame(t(kfq_data[kfq_grain_vars])))

    ## apply mode to each list element (row)
    kfq_score_dat[["kfq_grains_modefreq"]] <- sapply(kfq_grain_list, mode)

    kfq_score_dat[["kfq_grains_modefreq"]] <- sjlabelled::add_labels(kfq_score_dat[["kfq_grains_modefreq"]], labels = c(`Never` = 0, `<1 time wk` = 1, `1-2 times wk` = 2, `3-5 times wk` = 3, `6-7 times wk` = 4, `>7 times wk` = 5))
    class(kfq_score_dat[["kfq_grains_modefreq"]]) <- c("haven_labelled", "vctrs_vctr", "double")

    ## add labels to data
    kfq_score_dat_labels[["kfq_grains"]] <- "KFQ Breads and Grains Total Score"
    kfq_score_dat_labels[["kfq_grains_modefreq"]] <- "KFQ Breads and Grains Modal Freqency"

    ##Vegetables
    kfq_veg_vars <- c("kfq8", "kfq9", "kfq10", "kfq11", "kfq12", "kfq13", "kfq14")
    kfq_score_dat[["kfq_veg"]] <- rowSums(kfq_data[kfq_veg_vars])

    #get modal frequency
    ## get each row as vector in list
    kfq_veg_list <- as.list(data.frame(t(kfq_data[kfq_veg_vars])))

    ## apply mode to each list element (row)
    kfq_score_dat[["kfq_veg_modefreq"]] <- sapply(kfq_veg_list, mode)

    kfq_score_dat[["kfq_veg_modefreq"]] <- sjlabelled::add_labels(kfq_score_dat[["kfq_veg_modefreq"]], labels = c(`Never` = 0, `<1 time wk` = 1, `1-2 times wk` = 2, `3-5 times wk` = 3, `6-7 times wk` = 4, `>7 times wk` = 5))
    class(kfq_score_dat[["kfq_veg_modefreq"]]) <- c("haven_labelled", "vctrs_vctr", "double")

    ## add labels to data
    kfq_score_dat_labels[["kfq_veg"]] <- "KFQ Vegetables Total Score"
    kfq_score_dat_labels[["kfq_veg_modefreq"]] <- "KFQ Vegetables Modal Freqency"

    ##Fruits
    kfq_fruit_vars <- c("kfq15", "kfq16", "kfq17")
    kfq_score_dat[["kfq_fruit"]] <- rowSums(kfq_data[kfq_fruit_vars])

    #get modal frequency
    ## get each row as vector in list
    kfq_fruit_list <- as.list(data.frame(t(kfq_data[kfq_fruit_vars])))

    ## apply mode to each list element (row)
    kfq_score_dat[["kfq_fruit_modefreq"]] <- sapply(kfq_fruit_list, mode)

    kfq_score_dat[["kfq_fruit_modefreq"]] <- sjlabelled::add_labels(kfq_score_dat[["kfq_fruit_modefreq"]], labels = c(`Never` = 0, `<1 time wk` = 1, `1-2 times wk` = 2, `3-5 times wk` = 3, `6-7 times wk` = 4, `>7 times wk` = 5))
    class(kfq_score_dat[["kfq_fruit_modefreq"]]) <- c("haven_labelled", "vctrs_vctr", "double")

    ## add labels to data
    kfq_score_dat_labels[["kfq_fruit"]] <- "KFQ Fruits Total Score"
    kfq_score_dat_labels[["kfq_fruit_modefreq"]] <- "KFQ Fruits Modal Freqency"

    ##Dairy
    kfq_dairy_vars <- c("kfq18", "kfq19", "kfq20")
    kfq_score_dat[["kfq_dairy"]] <- rowSums(kfq_data[kfq_dairy_vars])

    #get modal frequency
    ## get each row as vector in list
    kfq_dairy_list <- as.list(data.frame(t(kfq_data[kfq_dairy_vars])))

    ## apply mode to each list element (row)
    kfq_score_dat[["kfq_dairy_modefreq"]] <- sapply(kfq_dairy_list, mode)

    kfq_score_dat[["kfq_dairy_modefreq"]] <- sjlabelled::add_labels(kfq_score_dat[["kfq_dairy_modefreq"]], labels = c(`Never` = 0, `<1 time wk` = 1, `1-2 times wk` = 2, `3-5 times wk` = 3, `6-7 times wk` = 4, `>7 times wk` = 5))
    class(kfq_score_dat[["kfq_dairy_modefreq"]]) <- c("haven_labelled", "vctrs_vctr", "double")

    ## add labels to data
    kfq_score_dat_labels[["kfq_dairy"]] <- "KFQ Dairy Total Score"
    kfq_score_dat_labels[["kfq_dairy_modefreq"]] <- "KFQ Dairy Modal Freqency"

    ##Protein
    kfq_protein_vars <- c("kfq21", "kfq22", "kfq23", "kfq24", "kfq25", "kfq26", "kfq27", "kfq28")
    kfq_score_dat[["kfq_protein"]] <- rowSums(kfq_data[kfq_protein_vars])

    #get modal frequency
    ## get each row as vector in list
    kfq_protein_list <- as.list(data.frame(t(kfq_data[kfq_protein_vars])))

    ## apply mode to each list element (row)
    kfq_score_dat[["kfq_protein_modefreq"]] <- sapply(kfq_protein_list, mode)

    kfq_score_dat[["kfq_protein_modefreq"]] <- sjlabelled::add_labels(kfq_score_dat[["kfq_protein_modefreq"]], labels = c(`Never` = 0, `<1 time wk` = 1, `1-2 times wk` = 2, `3-5 times wk` = 3, `6-7 times wk` = 4, `>7 times wk` = 5))
    class(kfq_score_dat[["kfq_protein_modefreq"]]) <- c("haven_labelled", "vctrs_vctr", "double")

    ## add labels to data
    kfq_score_dat_labels[["kfq_protein"]] <- "KFQ Protein Total Score"
    kfq_score_dat_labels[["kfq_protein_modefreq"]] <- "KFQ Protein Modal Freqency"

    ##Beverages
    kfq_bev_vars <- c("kfq29", "kfq30", "kfq31")
    kfq_score_dat[["kfq_bev"]] <- rowSums(kfq_data[kfq_bev_vars])

    #get modal frequency
    ## get each row as vector in list
    kfq_bev_list <- as.list(data.frame(t(kfq_data[kfq_bev_vars])))

    ## apply mode to each list element (row)
    kfq_score_dat[["kfq_bev_modefreq"]] <- sapply(kfq_bev_list, mode)

    kfq_score_dat[["kfq_bev_modefreq"]] <- sjlabelled::add_labels(kfq_score_dat[["kfq_bev_modefreq"]], labels = c(`Never` = 0, `<1 time wk` = 1, `1-2 times wk` = 2, `3-5 times wk` = 3, `6-7 times wk` = 4, `>7 times wk` = 5))
    class(kfq_score_dat[["kfq_bev_modefreq"]]) <- c("haven_labelled", "vctrs_vctr", "double")

    ## add labels to data
    kfq_score_dat_labels[["kfq_bev"]] <- "KFQ Beverages Total Score"
    kfq_score_dat_labels[["kfq_bev_modefreq"]] <- "KFQ Beverages Modal Freqency"

    #Snacks
    kfq_snacks_vars <- c("kfq32", "kfq33", "kfq34")
    kfq_score_dat[["kfq_snack"]] <- rowSums(kfq_data[kfq_snacks_vars])

    #get modal frequency
    ## get each row as vector in list
    kfq_snacks_list <- as.list(data.frame(t(kfq_data[kfq_snacks_vars])))

    ## apply mode to each list element (row)
    kfq_score_dat[["kfq_snack_modefreq"]] <- sapply(kfq_snacks_list, mode)

    kfq_score_dat[["kfq_snack_modefreq"]] <- sjlabelled::add_labels(kfq_score_dat[["kfq_snack_modefreq"]], labels = c(`Never` = 0, `<1 time wk` = 1, `1-2 times wk` = 2, `3-5 times wk` = 3, `6-7 times wk` = 4, `>7 times wk` = 5))
    class(kfq_score_dat[["kfq_snack_modefreq"]]) <- c("haven_labelled", "vctrs_vctr", "double")

    ## add labels to data
    kfq_score_dat_labels[["kfq_snack"]] <- "KFQ Snacks Total Score"
    kfq_score_dat_labels[["kfq_snack_modefreq"]] <- "KFQ Snacks Modal Freqency"

    #Deserts
    kfq_desert_vars <- c("kfq35", "kfq36", "kfq37", "kfq38", "kfq39", "kfq40")
    kfq_score_dat[["kfq_desert"]] <- rowSums(kfq_data[kfq_desert_vars])

    #get modal frequency
    ## get each row as vector in list
    kfq_desert_list <- as.list(data.frame(t(kfq_data[kfq_desert_vars])))

    ## apply mode to each list element (row)
    kfq_score_dat[["kfq_desert_modefreq"]] <- sapply(kfq_desert_list, mode)

    kfq_score_dat[["kfq_desert_modefreq"]] <- sjlabelled::add_labels(kfq_score_dat[["kfq_desert_modefreq"]], labels = c(`Never` = 0, `<1 time wk` = 1, `1-2 times wk` = 2, `3-5 times wk` = 3, `6-7 times wk` = 4, `>7 times wk` = 5))
    class(kfq_score_dat[["kfq_desert_modefreq"]]) <- c("haven_labelled", "vctrs_vctr", "double")

    ## add labels to data
    kfq_score_dat_labels[["kfq_desert"]] <- "KFQ Deserts Total Score"
    kfq_score_dat_labels[["kfq_desert_modefreq"]] <- "KFQ Deserts Modal Freqency"

    #Condiments
    kfq_cond_vars <- c("kfq41", "kfq42", "kfq43", "kfq44", "kfq45", "kfq46")
    kfq_score_dat[["kfq_cond"]] <- rowSums(kfq_data[kfq_cond_vars])

    #get modal frequency
    ## get each row as vector in list
    kfq_cond_list <- as.list(data.frame(t(kfq_data[kfq_cond_vars])))

    ## apply mode to each list element (row)
    kfq_score_dat[["kfq_cond_modefreq"]] <- sapply(kfq_cond_list, mode)

    kfq_score_dat[["kfq_cond_modefreq"]] <- sjlabelled::add_labels(kfq_score_dat[["kfq_cond_modefreq"]], labels = c(`Never` = 0, `<1 time wk` = 1, `1-2 times wk` = 2, `3-5 times wk` = 3, `6-7 times wk` = 4, `>7 times wk` = 5))
    class(kfq_score_dat[["kfq_cond_modefreq"]]) <- c("haven_labelled", "vctrs_vctr", "double")

    ## add labels to data
    kfq_score_dat_labels[["kfq_cond"]] <- "KFQ Condiments Total Score"
    kfq_score_dat_labels[["kfq_cond_modefreq"]] <- "KFQ Condiments Modal Freqency"

    #### 3. Clean Export/Scored Data #####

    ## make sure the variable labels match in the dataset
    kfq_score_dat = sjlabelled::set_label(kfq_score_dat, label = matrix(unlist(kfq_score_dat_labels, use.names = FALSE)))

    return(kfq_score_dat)
}

