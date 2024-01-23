#' model_dd: Model data from Delay Discounting
#'
#' This function models data from Delay Discounting using the miyamot0/discountingtools github package. This script will use approximate Bayesian model selection to identify the best best-performing model at the individual level. The model candidates include: the Exponential (Samuelson, 1937), the Hyperbolic (Mazur, 1987), the Generalized Hyperboloid (Rodriguez & Logue, 1997), the Quasi-Hyperbolic (Laibson, 1997), the Green & Myerson (Green & Myerson, 2004), the Ebert & Prelec Constant Sensitivity Model (Ebert & Prelec, 2007), the Bleichrodt et al. Constant Relative Decreasing Impatience Model (Bleichrodt et al., 2009), and a Noise model. More info can be found in fitting packages miyamot0/discountingtools: Delay Discounting Tools (https://github.com/miyamot0/discountingtools). Since individuals may differ in their 'true' model, only generalized indices are computed: Effective Delay 50 (ED50; Yoon & Higgins, 2008) and numerical integration performed upon the "true" model in normal and log10 scaling. Effective Delay 50 is the delay time needed for subjective value to decrease to 50 percent. The numerican integration approaches allow for calculation of model-based area under the curve with the log10 scaling computing AUC after scaling the delays by log10.
#'
#' Note: this approach does take a few minutes to compute
#'
#' To use this function, the data must be prepared according to the following criteria:
#' 1) The data must include all individual questionnaire items and child grade
#' 2) The columns/variables must match the following naming convention: 'dd#' where # is the question number (1-69)
#' 3) All questions must have the numeric value for the choice:
#' 3a) Questions 1-66: 0 - delay chosen, 1 - now chosen
#' 3b) Questions 67-69: 0 - $10 in 0 days, 1 - $X now
#'
#' Note, as long as variable names match those listed, the dataset can include other variables
#'
#' @references
#' Wilson VB, Mitchell SH, Musser ED, Schmitt CF, Nigg JT. Delay discounting of reward in ADHD: application in young children: Delay discounting and ADHD. Journal of Child Psychology and Psychiatry. 2011;52(3):256-264. doi:10.1111/j.1469-7610.2010.02347.x (\href{https://pubmed.ncbi.nlm.nih.gov/21083561/}{PubMed})
#'
#' @param dd_data a data.frame all items for the Delay Discounting following the naming conventions described above
#' @inheritParams score_bes
#'
#'
#' @return A dataset with a score for the Delay Discounting
#'
#' @examples
#'
#' # scoring for the dd with IDs
#' dd_score_data <- model_dd(dd_data, id = 'ID')
#'
#' \dontrun{
#' }
#'
#' @seealso Raw data from Qualtrics was processed using the following script: \code{\link{qualtrics_child_v3dat}} and \code{\link{qualtrics_child_v3dat_home}}
#'
#'
#' @export

model_dd <- function(dd_data, id) {

    #### 1. Set up/initial checks #####

    # check that dd_data exist and is a data.frame
    data_arg <- methods::hasArg(dd_data)

    if (isTRUE(data_arg) & !is.data.frame(dd_data)) {
        stop("dd_data must be entered as a data.frame")
    } else if (isFALSE(data_arg)) {
        stop("dd_data must set to the data.frame with amount consumed for each food item")
    }

    # check if id exists
    ID_arg <- methods::hasArg(id)

    if (isTRUE(ID_arg)){
        if (!(id %in% names(dd_data))) {
            stop("variable name entered as id is not in dd_data")
        }
    }

    #### 2. Set Up Data #####

    # set up database for results create empty matrix
    dd_score_dat <- data.frame(dd_checks_score = rep(NA, nrow(dd_data)), dd_checks_exclude = rep(NA, nrow(dd_data)))

    #add id so can merge later
    if (isTRUE(ID_arg)) {
        dd_score_dat <- data.frame(dd_data[[id]], dd_score_dat)
        names(dd_score_dat)[1] <- id
    } else {
        dd_score_dat <- data.frame(seq(1,nrow(dd_data),1), dd_score_dat)
        names(dd_score_dat)[1] <- 'id'
    }

    # set up labels for dd_score_dat
    dd_score_dat_labels <- lapply(dd_score_dat, function(x) attributes(x)$label)

    ## Check Exclusion Questions
    dd_score_dat[["dd_checks_score"]] <- rowSums(dd_data[["dd67"]] == 0 & dd_data[c("dd68", "dd69")] == 1)
    dd_score_dat[["dd_checks_exclude"]] <- ifelse(dd_score_dat[["dd_checks_score"]] > 0, 1, 0)

    dd_score_dat[["dd_checks_exclude"]] <- sjlabelled::add_labels(dd_score_dat[["dd_checks_exclude"]], labels = c(Include = 0, Exclude = 1))
    class(dd_score_dat[["dd_checks_exclude"]]) <- c("haven_labelled", "vctrs_vctr", "double")


    # add to labels
    dd_score_dat_labels[["dd_checks_score"]] <- "DD Exclusion Checks Score (out of 3; higher is worse)"
    dd_score_dat_labels[["dd_checks_exclude"]] <- "DD Exclusion Status - failure of any check questions (score > 0)"

    ## Set up delay discounting functions
    delay_list <- c(7, 30, 90)

    dd_vars <- c("dd1", "dd2", "dd3", "dd4", "dd5", "dd6", "dd7", "dd8", "dd9", "dd10", "dd11", "dd12", "dd13", "dd14", "dd15", "dd16", "dd17", "dd18", "dd19", "dd20", "dd21", "dd22", "dd23", "dd24", "dd25", "dd26", "dd27", "dd28", "dd29", "dd30", "dd31", "dd32", "dd33", "dd34", "dd35", "dd36", "dd37", "dd38", "dd39", "dd40", "dd41", "dd42", "dd43", "dd44", "dd45", "dd46", "dd47", "dd48", "dd49", "dd50", "dd51", "dd52", "dd53", "dd54", "dd55", "dd56", "dd57", "dd58", "dd59", "dd60", "dd61", "dd62", "dd63", "dd64", "dd65", "dd66")

    ## make long data
    # remove haven labels
    if (isTRUE(ID_arg)){
        if(class(dd_data[id]) == 'haven_labelled'){
            haven::labelled(dd_data[[id]], labels = NULL)
            haven::labelled(dd_data[[id]], label = NULL)
        }
    } else {
        if(class(dd_data['id']) == 'haven_labelled'){
            haven::labelled(dd_data[['id']], labels = NULL)
            haven::labelled(dd_data[['id']], label = NULL)
        }
    }

    for (v in 1:length(dd_vars)){
        var_name <- dd_vars[v]
        if(class(dd_data[var_name]) == 'haven_labelled'){
            haven::labelled(dd_data[[var_name]], labels = NULL)
            haven::labelled(dd_data[[var_name]], label = NULL)
        }
    }

    ## make long
    if (isTRUE(ID_arg)){
        dd_data_long <- reshape2::melt(dd_data[c(id, dd_vars)], id.vars = id)
    } else {
        dd_data[["id"]] <- seq(1,nrow(dd_data),1)
        dd_data_long <- reshape2::melt(dd_data[c('id', dd_vars)], id.vars = 'id')
    }

    names(dd_data_long)[2:3] <- c('question', 'choice')

    ## add delays and choice values
    #get number of IDs
    npar <- nrow(dd_data)
    dd_data_long[["delay"]] <- c(rep(7, npar*22), rep(30, npar*22), rep(90, npar*22))

    #value choices equal to 1
    dd_data_long[["delay_val"]] <- c(rep(0, npar), rep(0.5, npar), rep(1, npar), rep(1.5, npar), rep(2, npar), rep(2.5, npar), rep(3, npar), rep(3.5, npar), rep(4, npar), rep(4.5, npar), rep(5, npar), rep(5.5, npar), rep(6, npar), rep(6.5, npar), rep(7, npar), rep(7.5, npar), rep(8, npar), rep(8.5, npar), rep(9, npar), rep(9.5, npar) ,rep(10, npar), rep(10.5, npar))

    dd_data_long[["choice_val"]] <- ifelse(dd_data_long[["choice"]] == 1, dd_data_long[["delay_val"]], 10)

    # calculate values for model
    dd_data_long[["values"]] <- dd_data_long[["choice_val"]]/10

    #set values > 1 to 1 - do we need?? how to handle the 10.5 now, 10 later???
    dd_data_long[["values"]] <- ifelse(dd_data_long[["values"]] > 1, 1, dd_data_long[["values"]])

    # set those that need to be excluded to NA
    if(isTRUE(ID_arg)){
        #identify ids to be excluded
        ex_ids <- dd_score_dat[is.na(dd_score_dat[["dd_checks_exclude"]]) | dd_score_dat[["dd_checks_exclude"]] == 1, id]

        #set values to NA if excluded
        dd_data_long[['values']] <- ifelse(dd_data_long[[id]] %in% ex_ids, NA, dd_data_long[['values']])
    } else {
        #identify ids to be excluded
        ex_ids <- dd_score_dat[is.na(dd_score_dat[["dd_checks_exclude"]]) | dd_score_dat[["dd_checks_exclude"]] == 1, 'id']

        #set values to NA if excluded
        dd_data_long[['values']] <- ifelse(dd_data_long[['id']] %in% ex_ids, NA, dd_data_long[['values']])
    }

    # remove NAs
    dd_data_long_use <- dd_data_long[!is.na(dd_data_long[['values']]), ]

    ## set up models
    fitobj <- discountingtools::fitDDCurves(data = dd_data_long_use, settings = list(Delays = delay, Values = values, Individual = id),  maxValue = 1, verbose  = TRUE)

    fitobj <- discountingtools::dd_modelOptions(fitobj, plan = c("mazur", "bleichrodt", "ebertprelec", "exponential", "greenmyerson", "laibson", "noise", "rachlin", "rodriguezlogue"))

    fitobj <- discountingtools::dd_metricOptions(fitobj, metrics = c("lned50", "mbauc", "logmbauc"))

    fitobj <- discountingtools::dd_screenOption(fitobj, screen = FALSE)

    results <- discountingtools::dd_analyze(fitobj)

    results_sum <- discountingtools::summary.discountingtools(results)

    ## add data to data.frame
    #rename summary ID and get desired summary results
    if (isTRUE(ID_arg)){
        names(results_sum)[1] <- id
        summary_vars <- c(id, 'ProbableModel', 'ProbableModel.BF', 'ProbableModel.Prob', 'LnED50', 'MBAUC', 'Log10MBAUC')
    } else {
        names(results_sum)[1] <- 'id'
        summary_vars <- c('id', 'ProbableModel', 'ProbableModel.BF', 'ProbableModel.Prob', 'LnED50', 'MBAUC', 'Log10MBAUC')
    }

    # merge
    dd_score_dat <- merge(dd_score_dat, results_sum[summary_vars], id_var = 'id', all.x = TRUE)

    # rename dd vars
    names(dd_score_dat)[4:9] <- c('dd_probmod', 'dd_probmod_bf', 'dd_probmod_prob', 'dd_ed50', 'dd_mb_auc', 'dd_mb_auc_log10')

    #model names to labels
    dd_score_dat[["dd_probmod"]] <- ifelse(dd_score_dat[["dd_probmod"]] == "mazur", 0, ifelse(dd_score_dat[["dd_probmod"]] == "bleichrodt", 1, ifelse(dd_score_dat[["dd_probmod"]] == "ebertprelec", 2, ifelse(dd_score_dat[["dd_probmod"]] == "exponential", 3, ifelse(dd_score_dat[["dd_probmod"]] == "greenmyerson", 4, ifelse(dd_score_dat[["dd_probmod"]] == "laibson", 5, ifelse(dd_score_dat[["dd_probmod"]] == "rachlin", 6, ifelse(dd_score_dat[["dd_probmod"]] == "rodriguezlogue", 7, 8))))))))

    dd_score_dat[["dd_probmod"]] <- sjlabelled::add_labels(dd_score_dat[["dd_probmod"]], labels = c(mazur = 0, bleichrodt = 1, ebertprelec = 2, exponential = 3, greenmyerson = 4, laibson = 5, rachlin = 6, rodriguezlogue = 7, noise = 8))
    class(dd_score_dat[["dd_probmod"]]) <- c("haven_labelled", "vctrs_vctr", "double")

    # add labels
    dd_score_dat_labels[["dd_probmod"]] <- "DD best-fitting model from approximate Bayesian Model Selection"
    dd_score_dat_labels[["dd_probmod_bf"]] <- "DD Bayes Factor for best-fitting model from approximate Bayesian Model Selection"
    dd_score_dat_labels[["dd_probmod_prob"]] <- "DD Probability for best-fitting model from approximate Bayesian Model Selection"
    dd_score_dat_labels[["dd_ed50"]] <- "DD Effective Dealy 50 for best-fitting model"
    dd_score_dat_labels[["dd_mb_auc"]] <- "DD model-based area under the curve for best-fitting model"
    dd_score_dat_labels[["dd_mb_auc_log10"]] <- "DD model-based area under the curve with log-10 delay scaling for best-fitting model"

    #### 3. Clean Export/Scored Data #####

    ## make sure the variable labels match in the dataset
    dd_score_dat = sjlabelled::set_label(dd_score_dat, label = matrix(unlist(dd_score_dat_labels,
        use.names = FALSE)))

    return(dd_score_dat)
}

