% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score_pmum.R
\name{score_pmum}
\alias{score_pmum}
\title{score_pmum: Scored data from the Problematic Media Use Measure}
\usage{
score_pmum(
  pmum_data,
  pna_value,
  base_zero = TRUE,
  id,
  session_id,
  extra_scale_cols = c()
)
}
\arguments{
\item{pmum_data}{a data.frame all items for the Problematic Media Use Measure following the naming conventions described above}

\item{pna_value}{value used when participant prefers not to answer/elects to skip}

\item{base_zero}{(logical) TRUE indicates the smallest value assigned to a choice is 0. FALSE indicates indicates the smallest value assigned to a choice is 1. Default = TRUE.}

\item{id}{(optional) name of participant ID column in input data. If included, the output dataset will be matched by id, if not included the output dataset will be in the order of the input data but will have no participant identifier. Required to get the phenotype dataset (raw data merged with scores.)}

\item{session_id}{(optional) name of session ID column in input data if there are multiple observations per participant. If included, the output dataset will be matched by id and session id, if not included the output dataset will be in the order of the input data. Required to get the phenotype dataset if have multiple sessions (raw data merged with scores.)}

\item{extra_scale_cols}{a vector of character strings that begin with 'pmum' but are not scale items. Any columns in pmum_data that begin with 'pmum' but are not scale items must be included here. Default is empty vector.}
}
\value{
A dataset with total score for the Problematic Media Use Measure
}
\description{
This function scores the Problematic Media Use Measure
}
\details{
For data to be scored correctly, the data must be prepared according to the following criteria: \cr
\itemize{
 \item{The data must include all individual questionnaire items}
 \item{The columns/variables must match the following naming convention: 'pmum#' or 'pmum_#' where # is the question number (1-27)}
 \item{All questionnaire responses must be a numeric value ranging from 0-4 (base_zero = TRUE) or 1-5 (base_zero = FALSE) where: }
 \itemize{
    \item{For base_zero = TRUE: 0 = Never; 1 = Rarely; 2 = Sometimes; 3 = Often; 4 = Always}
    \item{For base_zero = FALSE: 1 = Never; 2 = Rarely; 3 = Sometimes; 4 = Often; 5 = Always}
  }
 \item{Missing values must be coded as NA}
}
\cr
Note, as long as variable names match those listed, the dataset can include other variables
}
\examples{
# scoring for the PMUM with IDs, when values range from 0-4
pmum_score_data <- score_pmum(pmum_data, base_zero = TRUE, id = 'ID')

# scoring for the PMUM with IDs, when values range from 1-5
pmum_score_data <- score_pmum(pmum_data, base_zero = FALSE, id = 'ID')

\dontrun{
}

}
\references{
Domoff SE, Harrison K, Gearhardt AN, Gentile DA, Lumeng JC, Miller AL. Development and Validation of the Problematic Media Use Measure: A Parent Report Measure of Screen Media "Addiction" in Children. Psychol Pop Media Cult. 2019 Jan;8(1):2-11. doi: 10.1037/ppm0000163. Epub 2017 Nov 16. PMID: 30873299; PMCID: PMC6411079.
}
