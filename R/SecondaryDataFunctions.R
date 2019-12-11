#' This function extracts the secondary data
#' It produces an output that contains the handedness score, the maths test score,
#' the language response code (1,2,3), the age entered, and the aAMAS
#' (math anxiety score)
#' @export
ExtractSecondaryData <- function(this.SubjectData) {
  ExtractHandedness <- function(data) {
    Left = data$handed[, 1]
    Neither = data$handed[, 2]
    Right = data$handed[, 3]

    sum(Left, Neither, Right)

  }

  ExtractMathTest <- function(data) {
    return(data$mathTestScore)
  }

  ExtractLanguage <- function(data) {
    return(data$lang)
  }

  ExtractAMAS <- function(data) {
    return(sum(data$aMAS))
  }

  ExtractAge <- function(data) {
    returnvalue = data$age
    if (purrr::is_empty(returnvalue) == TRUE) {
      return(NaN)
    } else{
      return(data$age)
    }
  }

  secondaryDataStruct = data.frame(
    handedness = ExtractHandedness(this.SubjectData),
    mathTest = ExtractMathTest(this.SubjectData),
    language = ExtractLanguage(this.SubjectData),
    age = ExtractAge(this.SubjectData),
    amas = ExtractAMAS(this.SubjectData)
  )

  secondaryDataStruct %<>% as.tibble()

  return(secondaryDataStruct)
}

#' This function extracts the answer to the post experiment questinnnaire.
#' That is, wether the participant correct guessed the purpose of the experiment.
#' The input data is the finger counting data structure.
#' @export
ExtractExperimentGuess <- function(data) {
  guessPurpose = toupper(substr(as.character(data$fingercountingData[, , ]$reject), 1, 1))

  # check if it's blank!

  guessPurpose = ifelse(test = length(guessPurpose) == 0, yes = "Err",no = guessPurpose)

  return(guessPurpose)
}
