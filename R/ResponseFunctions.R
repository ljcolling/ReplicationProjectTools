#' This function extracts the reaction time data.
#' It produces an output for each trial, including the digit shown the target
#' location, the delay duration, and whether a response was made
#' @export
ExtractResponse <- function(this.SubjectData) {
  UnlistTibble <- function(data) {
    colToUnlist = names(data)[(unlist(lapply(1:dim(data)[2], function(X)
      tibble::type_sum(data[[X]]))) == "list")]

    for (col in colToUnlist) {
      data[[col]] = unlist(data[[col]])
    }
    return(data)
  }



  responseData =  tibble::as.tibble(as.data.frame(t(this.SubjectData$responseStruct[, 1, 1:dim(this.SubjectData$responseStruct)[3]])))
  responseData$thekey[unlist(lapply(responseData$thekey, function(X)
    length(X)) == 0)] = "nr"
  responseData = UnlistTibble(responseData)

  responseData %>% mutate(thekey = case_when(thekey != "nr" ~ "space", TRUE ~ "nr")) -> responseData
  responseData %>% mutate(RT = ifelse(RT < 0, yes = 0, RT)) -> responseData

  responseData  %>% mutate(correct = case_when(targetLocation == "c" & thekey == "nr" ~ 1,
                                               targetLocation != "c" & thekey == "nr" ~ 0,
                                               targetLocation != "c" & thekey == "space" ~ 1,
                                               targetLocation == "c" & thekey == "space" ~ 0)) -> responseData


  ((responseData$thekey %>% unique() %>% length()) == 2) +
    ((responseData$correct %>% unique() %>% length()) == 2) +
    ((which(responseData$RT < 0)  %>% length()) == 0) -> chk
  if(chk != 3){
    cat(paste0("error on ",this.SubjectData[["params"]][,,1]$subCode[1],"\n\n"))
  }

  cat(".\n\n")
  responseData = tibble::add_column(responseData, trial = seq(1:800), .before = 1)
}
