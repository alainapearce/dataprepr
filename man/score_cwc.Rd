% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score_cwc.R
\name{score_cwc}
\alias{score_cwc}
\title{score_cwc: Score data from the Child Weight Concerns}
\usage{
score_cwc(
  cwc_data,
  pna_value,
  id,
  session_id,
  base_zero = TRUE,
  extra_scale_cols = c()
)
}
\arguments{
\item{cwc_data}{a data.frame all items for the Child Weight Concerns following the naming conventions described above}

\item{pna_value}{(integer) integer used for items where child 'prefers not to answer'}

\item{id}{(optional) name of participant ID column in input data. If included, the output dataset will be matched by id, if not included the output dataset will be in the order of the input data but will have no participant identifier. Required to get the phenotype dataset (raw data merged with scores.)}

\item{session_id}{(optional) name of session ID column in input data if there are multiple observations per participant. If included, the output dataset will be matched by id and session id, if not included the output dataset will be in the order of the input data. Required to get the phenotype dataset if have multiple sessions (raw data merged with scores.)}

\item{base_zero}{(logical) TRUE indicates the smallest value assigned to a choice is 0. FALSE indicates indicates the smallest value assigned to a choice is 1. Default = TRUE.}

\item{extra_scale_cols}{vector of character strings that begin with 'cwc' but are not scale items. Any columns in cwc_data that begin with 'cwc' but are not scale items must be included here. Default is empty vector.}
}
\value{
A dataset with a score for the Child Weight Concerns
}
\description{
This function scores the Child Weight Concerns
}
\details{
To use this function, the data must be prepared according to the following criteria: \cr
\itemize{
 \item{The data must include all individual questionnaire items}
 \item{The  columns/variables must match the following naming convention: 'cwc#' or 'cwc_# where # is the question number (1-5)}
 \item{All questions must have the numeric value for the choice starting with the value 0 (base_zero = TRUE) or 1 (base_zero = FALSE). Each item is scored differently so responses are scaled to be 0-100 for each item.}
}
Note, as long as variable names match those listed, the dataset can include other variables
}
\examples{

# scoring for the cwc with IDs
cwc_score_data <- score_cwc(cwc_data, parID = 'ID')

\dontrun{
}


}
\references{
Killen JD, Taylor CB, Hayward C, et al. Pursuit of thinness and onset of eating disorder symptoms in a community sample of adolescent girls: A three-year prospective analysis. Int J Eat Disord. 1994;16(3):227-238. doi:10.1002/1098-108X(199411)16:3<227::AID-EAT2260160303>3.0.CO;2-L (\href{https://pubmed.ncbi.nlm.nih.gov/7833956/}{PubMed})

Taylor CB, Sharpe T, Shisslak C, et al. Factors associated with weight concerns in adolescent girls. Int J Eat Disord. 1998;24(1):31-42. doi:10.1002/(SICI)1098-108X(199807)24:1<31::AID-EAT3>3.0.CO;2-1 (\href{https://pubmed.ncbi.nlm.nih.gov/9589309/}{PubMed})
}
