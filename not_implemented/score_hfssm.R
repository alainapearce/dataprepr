#' score_hfssm: Scored data from the US Household Food Security Survey Module: Three Stage
#'
#' This function scores the US Household Food Security Survey Module: Three Stage and provides subscale scores for the following: Household Food Security Scale, U.S. Adult Food Security Scale, Six-Item Food Security Scale, Children’s Food Security Scale.
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items for each module. Only complete modules will be completed.
#' 2) The  columns/variables must match the following naming convention: 'hfssm_mod#' where mod is the module ('hh' - household, 'ad' - adult, 'ch' - child).
#' 3) To compute modules, the following questions are required:
#' 3a) Household Module - requires items hfssm_hh2 through hfssm_ch7
#' 3b) Adult Module - requires items hfssm_hh2 through hfssm_ch7
#' 3c) Six-Item Module - requires items hfssm_hh2 through hfssm_ad5a
#' 3d) Child Module - requires items hfssm_ch2 through hfssm_ch7
#' 4) Questions must have the numeric values for the choices. The options include:
#' 4a) Yes - 1, No - 0, I don't know or Don't want to answer - 99
#' 4b) Often True - 1, Sometimes True - 1, Never True - 0, I don't know or Don't want to answer - 99
#' 4c) Almost Every Month - 1, Some Months but not Every Month - 1, Only 1 or 2 months - 0, I don't know or Don't want to answer - 99
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Bickel G, Nord M, Price C, Hamilton W, Cook J, others. Guide to measuring household food security, revised 2000. US Department of Agriculture, Food and Nutrition Service. Published online 2000:52.
#'
#' Nord M. Measuring Children’s Food Security in US Households, 1995-99. US Department of Agriculture, Economic Research Service; 2002.
#'
#' @param hfssm_data a data.frame all items for the US Household Food Security Survey Module: Three Stage following the naming conventions described above
#' @inheritParams fbs_intake
#'
#' @return A dataset with subscale scores for the US Household Food Security Survey Module: Three Stage
#' @examples
#'
#' # scoring for the hfssm with IDs
#' hfssm_score_data <- score_hfssm(hfssm_data, parID = 'ID')
#'
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{util_fbs_parent_v4dat}}
#'
#'
#' @export

score_hfssm <- function(hfssm_data, parID) {

    #### 1. Set up/initial checks #####

    # check that hfssm_data exist and is a data.frame
    data_arg <- methods::hasArg(hfssm_data)

    if (isTRUE(data_arg) & !is.data.frame(hfssm_data)) {
        stop("hfssm_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("hfssm_data must set to the data.frame with amount consumed for each food item")
    }

    # check if parID exists
    ID_arg <- methods::hasArg(parID)

    if (isTRUE(ID_arg)){
        if (!(parID %in% names(hfssm_data))) {
            stop("variable name entered as parID is not in hfssm_data")
        }
    }

    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    hfssm_score_dat <- data.frame(hfssm_household = rep(NA, nrow(hfssm_data)), hfssm_adult = rep(NA, nrow(hfssm_data)), hfssm_6item = rep(NA, nrow(hfssm_data)), hfssm_child = rep(NA, nrow(hfssm_data)), hfssm_household_cat = rep(NA, nrow(hfssm_data)), hfssm_adult_cat = rep(NA, nrow(hfssm_data)), hfssm_6item_cat = rep(NA, nrow(hfssm_data)), hfssm_child_cat = rep(NA, nrow(hfssm_data)))

    if (isTRUE(ID_arg)) {
        hfssm_score_dat <- data.frame(hfssm_data[[parID]], hfssm_score_dat)
        names(hfssm_score_dat)[1] <- parID
    }

    # set up labels for hfssm_score_dat
    hfssm_score_dat_labels <- lapply(hfssm_score_dat, function(x) attributes(x)$label)

    # change -99 to NA
    hhfssm_vars <- c("hfssm_hh1", "hfssm_hh2", "hfssm_hh3", "hfssm_hh4", "hfssm_ad1", "hfssm_ad1a", "hfssm_ad2", "hfssm_ad3", "hfssm_ad4", "hfssm_ad5", "hfssm_ad5a", "hfssm_ch1", "hfssm_ch2", "hfssm_ch3", "hfssm_ch4", "hfssm_ch5", "hfssm_ch6", "hfssm_ch7")

    for (var in 1:length(hhfssm_vars)){
        var_name <- hhfssm_vars[var]
        hfssm_data[[var_name]] <- as.numeric(hfssm_data[[var_name]])
        hfssm_data[var_name] <- ifelse(is.na(hfssm_data[[var_name]]), NA, ifelse(hfssm_data[[var_name]] < 0, NA, hfssm_data[[var_name]]))
    }

    ## Score Subscales

    # Adult Food Security Scale
    adult_vars <- c("hfssm_hh2", "hfssm_hh3", "hfssm_hh4", "hfssm_ad1", "hfssm_ad1a", "hfssm_ad2", "hfssm_ad3", "hfssm_ad4", "hfssm_ad5", "hfssm_ad5a")
    hfssm_score_dat[["hfssm_adult"]] <- rowSums(hfssm_data[adult_vars], na.rm = TRUE)

    hfssm_score_dat[["hfssm_adult_cat"]] <- ifelse(hfssm_score_dat[["hfssm_adult"]] >= 6, 3, ifelse(hfssm_score_dat[["hfssm_adult"]] >=3, 2, ifelse(hfssm_score_dat[["hfssm_adult"]] > 0, 1, 0)))

    hfssm_score_dat[["hfssm_adult_cat"]] <- sjlabelled::add_labels(hfssm_score_dat[["hfssm_adult_cat"]], labels = c(`Very Low Food Security` = 3, `Low Food Security` = 2, `Marginal Food Security` = 1, `High Food Security` = 0))
    class(hfssm_score_dat[["hfssm_adult_cat"]]) <- c("haven_labelled", "vctrs_vctr", "double")


    ## add labels to data
    hfssm_score_dat_labels[["hfssm_adult"]] <- "HFSSM Adult Food Security Scale Score"
    hfssm_score_dat_labels[["hfssm_adult_cat"]] <- "HFSSM Adult Food Security Category"

    # Six-Item Food Security Scale
    fs6item_vars <- c("hfssm_hh3", "hfssm_hh4", "hfssm_ad1", "hfssm_ad1a", "hfssm_ad2", "hfssm_ad3")
    hfssm_score_dat[["hfssm_6item"]] <- rowSums(hfssm_data[fs6item_vars], na.rm = TRUE)

    hfssm_score_dat[["hfssm_6item_cat"]] <- ifelse(hfssm_score_dat[["hfssm_6item"]] >= 5, 2, ifelse(hfssm_score_dat[["hfssm_6item"]] >=2, 1, 0))

    hfssm_score_dat[["hfssm_6item_cat"]] <- sjlabelled::add_labels(hfssm_score_dat[["hfssm_6item_cat"]], labels = c(`Very Low Food Security` = 2, `Low Food Security` = 1, `High or Marginal Food Security` = 0))
    class(hfssm_score_dat[["hfssm_6item_cat"]]) <- c("haven_labelled", "vctrs_vctr", "double")

    ## add labels to data
    hfssm_score_dat_labels[["hfssm_6item"]] <- "HFSSM Six-Item Food Security Scale Score"
    hfssm_score_dat_labels[["hfssm_6item_cat"]] <- "HFSSM Six-Item Food Security Category"

    # Children’s Food Security Scale
    child_vars <- c("hfssm_ch1", "hfssm_ch2", "hfssm_ch3", "hfssm_ch4", "hfssm_ch5", "hfssm_ch6", "hfssm_ch7")
    hfssm_score_dat[["hfssm_child"]] <- rowSums(hfssm_data[child_vars], na.rm = TRUE)

    hfssm_score_dat[["hfssm_child_cat"]] <- ifelse(hfssm_score_dat[["hfssm_child"]] >= 5, 2, ifelse(hfssm_score_dat[["hfssm_child"]] >=2, 1, 0))
    class(hfssm_score_dat[["hfssm_child_cat"]]) <- c("haven_labelled", "vctrs_vctr", "double")

    hfssm_score_dat[["hfssm_child_cat"]] <- sjlabelled::add_labels(hfssm_score_dat[["hfssm_child_cat"]], labels = c(`Very Low Food Security` = 2, `Low Food Security` = 1, `High or Marginal Food Security` = 0))

    ## add labels to data
    hfssm_score_dat_labels[["hfssm_child"]] <- "HFSSM Children’s Food Security Scale Score"
    hfssm_score_dat_labels[["hfssm_child_cat"]] <- "HFSSM Children’s Food Security Category"

    # Household Food Security Scale
    hfssm_score_dat[["hfssm_household"]] <- rowSums(hfssm_data[c(adult_vars, child_vars)], na.rm = TRUE)

    hfssm_score_dat[["hfssm_household_cat"]] <- ifelse(hfssm_score_dat[["hfssm_household"]] >= 8, 3, ifelse(hfssm_score_dat[["hfssm_household"]] >=3, 2, ifelse(hfssm_score_dat[["hfssm_household"]] > 0, 1, 0)))

    hfssm_score_dat[["hfssm_household_cat"]] <- sjlabelled::add_labels(hfssm_score_dat[["hfssm_household_cat"]], labels = c(`Very Low Food Security` = 3, `Low Food Security` = 2, `Marginal Food Security` = 1, `High Food Security` = 0))
    class(hfssm_score_dat[["hfssm_household_cat"]]) <- c("haven_labelled", "vctrs_vctr", "double")

    ## add labels to data
    hfssm_score_dat_labels[["hfssm_household"]] <- "HFSSM Household Food Security Scale Score"
    hfssm_score_dat_labels[["hfssm_household_cat"]] <- "HFSSM Household Food Security Category"

    #### 3. Clean Export/Scored Data #####

    ## make sure the variable labels match in the dataset
    hfssm_score_dat = sjlabelled::set_label(hfssm_score_dat, label = matrix(unlist(hfssm_score_dat_labels,
        use.names = FALSE)))

    return(hfssm_score_dat)
}

