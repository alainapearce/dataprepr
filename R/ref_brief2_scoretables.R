#' brief2_scoretables: Reference tables for scoring the Behavioral Rating Inventory of Executive Function-2
#'
#' Data for each sex and age category have been copied into the list of data.frames
#'
#' @references
#' Gioia GA, Isquith PK, Guy SC, Kenworthy L. BRIEF-2: Behavior Rating Inventory of Executive Function: Professional Manual. Psychological Assessment Resources; 2015.
#'
#' @docType data
#'
#' @usage data(brief2_scoretables)
#'
#' @format A list of data.frames for boys and girls with age categories 5-7 years, 8-10 years, 11-13 years, and 14-18 years. All data.frames have the following columns:
#' \describe{
#'     \item{subscale_raw}{sum of all items in each subscale}
#'     \item{inhibit_t}{age- and sex-normed t-score for inhibit}
#'     \item{inhibit_p}{age- and sex-normed percentile for inhibit}
#'     \item{selfmon_t}{age- and sex-normed t-score for self-monitoring}
#'     \item{selfmon_p}{age- and sex-normed percentile for self-monitoring}
#'     \item{shift_t}{age- and sex-normed t-score for shift}
#'     \item{shift_p}{age- and sex-normed percentile for shif}t
#'     \item{emcont_t}{age- and sex-normed t-score for emotional control}
#'     \item{emcont_p}{age- and sex-normed percentile for emotional control}
#'     \item{initiate_t}{age- and sex-normed t-score for initiate}
#'     \item{initiate_p}{age- and sex-normed percentile for initiate}
#'     \item{wm_t}{age- and sex-normed t-score for working memory}
#'     \item{wm_p}{age- and sex-normed percentile for working memory}
#'     \item{planorg_t}{age- and sex-normed t-score for plan/organization}
#'     \item{planorg_p}{age- and sex-normed percentile for plan/organization}
#'     \item{taskmon_t}{age- and sex-normed t-score for task monitoring}
#'     \item{taskmon_p}{age- and sex-normed percentile for task monitoring}
#'     \item{orgmat_t}{age- and sex-normed t-score for organization of material}
#'     \item{orgmat_p}{age- and sex-normed percentile for organization of material}
#'     \item{index_raw}{sum of all items in the composite indices}
#'     \item{bri_t}{age- and sex-normed t-score for the behavioral regulation index}
#'     \item{bri_p}{age- and sex-normed percentile for the behavioral regulation index}
#'     \item{eri_t}{age- and sex-normed t-score for the emotional regulation index}
#'     \item{eri_p}{age- and sex-normed percentile for the emotional regulation index}
#'     \item{cri_t}{age- and sex-normed t-score for the cognitive regulation index}
#'     \item{cri_p}{age- and sex-normed percentile for the cognitive regulation index}
#'     \item{gec_raw}{sum of all items in questionnaire}
#'     \item{gec_t}{age- and sex-normed t-score for the general executive composite index}
#'     \item{gec_p}{age- and sex-normed percentile for the general executive composite index}
#' }
#'
#' @keywords datasets
#'
#' @examples
#' data(brief2_scoretables)
#' boys5_7_reftable <- brief2_scoretables$boys_5_7
#'
"brief2_scoretables"
