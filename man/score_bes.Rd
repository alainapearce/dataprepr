% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score_bes.R
\name{score_bes}
\alias{score_bes}
\title{score_bes: Scored data from the Binge Eating Scale}
\usage{
score_bes(bes_data, score_base = TRUE, pna = NA, id)
}
\arguments{
\item{bes_data}{a data.frame all items for the Binge Eating Scale following the naming conventions described above}

\item{score_base}{the smallest value assigned to a choice is 0 (i.e., range 0-3). Default = TRUE.}

\item{pna}{value used when participant prefers not to answer/elects to skip}

\item{id}{(optional) name of participant ID column in bes_data. If included the output dataset will be matched by id, if not included the output dataset will be in the order of bes_data but will have no participant identifier. Required to get the phenotype dataset (raw data merged with scores.)}
}
\value{
A dataset with total score for the Binge Eating Scale
}
\description{
This function scores the Binge Eating Scale
}
\details{
To use this function, the data must be prepared according to the following criteria:
1) The data must include all individual questionnaire items
2) The  columns/variables must match the following naming convention: 'bes#' or 'bes_#' where # is the question number (1-16)
3) This script will apply specific scoring transformations that are specific to each question. For example, the first 2 statements are reset to 0 for question 1 but not for question 2.

Note, as long as variable names match those listed, the dataset can include other variables
}
\examples{

# scoring for the bes with IDs
bes_score_data <- score_bes(bes_data, id = 'ID')

\dontrun{
}


}
\references{
Gormally, J., Black, S., Daston, S., & Rardin, D. (1982). The assessment of binge eating severity among obese persons. Addictive Behaviors, 7(1), 47–55. https://doi.org/10.1016/0306-4603(82)90024-7  (\href{https://pubmed.ncbi.nlm.nih.gov/7080884/}{PubMed})

Timmerman, G. M. (1999). Binge Eating Scale: Further Assessment of Validity and Reliability. Journal of Applied Biobehavioral Research, 4(1), 1–12. https://doi.org/10.1111/j.1751-9861.1999.tb00051.x
}
