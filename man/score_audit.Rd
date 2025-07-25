% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score_audit.R
\name{score_audit}
\alias{score_audit}
\title{score_audit: Score data from the Alcohol Use Disorders Identification Test}
\usage{
score_audit(
  audit_data,
  pna_value,
  id,
  session_id,
  base_zero = TRUE,
  extra_scale_cols = c()
)
}
\arguments{
\item{audit_data}{a data.frame all items for the Alcohol Use Disorders Identification Test following the naming conventions described above}

\item{pna_value}{value used when participant prefers not to answer/elects to skip}

\item{id}{(optional) name of participant ID column in input data. If included, the output dataset will be matched by id, if not included the output dataset will be in the order of the input data but will have no participant identifier. Required to get the phenotype dataset (raw data merged with scores.)}

\item{session_id}{(optional) name of session ID column in input data if there are multiple observations per participant. If included, the output dataset will be matched by id and session id, if not included the output dataset will be in the order of the input data. Required to get the phenotype dataset if have multiple sessions (raw data merged with scores.)}

\item{base_zero}{(logical) TRUE indicates the smallest value assigned to a choice is 0. FALSE indicates indicates the smallest value assigned to a choice is 1. Default = TRUE.}

\item{extra_scale_cols}{vector of character strings that begin with 'audit' but are not scale items. Any columns in audit_data that begin with 'audit' but are not scale items must be included here. Default is empty vector.}
}
\value{
A dataset with a score for the Alcohol Use Disorders Identification Test
}
\description{
This function scores the Alcohol Use Disorders Identification Test
}
\details{
#' For data to be scored correctly, the data must be prepared according to the following criteria: \cr
\itemize{
 \item{The data must include all individual questionnaire items}
 \item{The columns/variables must match the following naming convention: 'audit#' or 'audit_#' where # is the question number (1-10)}
 \item{Questionnaire responses for items 1-8 must be a numeric value ranging from 0-4 (base_zero = TRUE) or 1-5 (base_zero = FALSE) where: }
 \itemize{
    \item{question 1: }
    \itemize{
       \item{For base_zero = TRUE: 0 = Never, 1 = Monthly or Less, 2 = 2-4 times a month, 3 = 2-3 times a week, 4 = 4 or more times a week}
       \item{For base_zero = FALSE: 1 = Never, 2 = Monthly or Less, 3 = 2-4 times a month, 4 = 2-3 times a week, 5 = 4 or more times a week}
   }
   \item{question 2: }
    \itemize{
       \item{For base_zero = TRUE: 0 = 1 or 2, 1 = 3 or 4, 2 = 5 or 6, 3 = 7 to 9, 4 = 10}
       \item{For base_zero = FALSE: 1 = 1 or 2, 2 = 3 or 4, 3 = 5 or 6, 4 = 7 to 9, 5 = 10}
   }
   \item{question 3-8: }
    \itemize{
       \item{For base_zero = TRUE: 0 = Never, 1 = Less than Monthly, 2 = Monthly, 3 = Weekly, 4 = Daily or Almost Daily}
       \item{For base_zero = FALSE: 1 = Never, 2 = Less than Monthly, 3 = Monthly, 4 = Weekly, 5 = Daily or Almost Daily}
   }
  }
 \item{Questionnaire responses for items 9-10 must be a numeric value of 0,2,4 (base_zero = TRUE) or 1,3,5 (base_zero = FALSE) where: }
 \itemize{
    \item{For base_zero = TRUE: 0 = No; 2 = Yes, but not in the last year; 4 = Yes, during the last year}
    \item{For base_zero = FALSE: 1 = No; 3 = Yes, but not in the last year; 5 = Yes, during the last year}
  }
 \item{Missing values must be coded as NA}
}
\cr

Note, as long as variable names match those listed, the dataset can include other variables
}
\examples{

# scoring for the audit with IDs
audit_score_data <- score_audit(audit_data, id = 'ID')

\dontrun{
}

}
\references{
Saunders JB, Aasland OG, Babor TF, De La Fuente JR, Grant M. Development of the Alcohol Use Disorders Identification Test (AUDIT): WHO Collaborative Project on Early Detection of Persons with Harmful Alcohol Consumption-II. Addiction. 1993;88(6):791-804. doi:10.1111/j.1360-0443.1993.tb02093.x (\href{https://pubmed.ncbi.nlm.nih.gov/8329970/}{PubMed})
}
