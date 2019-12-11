#' This function is applies a fix to the Exclude Data column in a lab's
#' FileList file. This is needed because different labs took different
#' approaches to filling in this column. Some examples, include:
#' Using True/False, using YES/NO, using 1/0, only making FALSE etc
#' This rewrites the column to a start of TRUE/FALSE
#'
#' @export
FixExcludeData <- function(this.FileList) {
  if ((sum(sort(unique(
    this.FileList$`Exclude Data`
  )) %in% c("FALSE", "TRUE")) == 2) == TRUE) {
    cat("**TYPE 1: Can use as is...**\n")
    # NO RECODING NECESSARY
  } else if ((sum(sort(unique(
    this.FileList$`Exclude Data`
  )) %in% c("false", "true")) == 2) == TRUE) {
    cat("**TYPE 2: Recoding strings to boolen...**\n")
    # DO RECODING HERE
    this.FileList$`Exclude Data`[this.FileList$`Exclude Data` %in% "false"] <-
      "FALSE"
    this.FileList$`Exclude Data`[this.FileList$`Exclude Data` %in% "true"] <-
      "TRUE"


    #
  } else if ((sum(sort(unique(
    this.FileList$`Exclude Data`
  )) %in% c("NO", "YES")) == 2) == TRUE) {
    cat("**TYPE 3: Recoding YES/NO to TRUE/FALSE...**\n")
    # DO RECODING HERE
    this.FileList$`Exclude Data`[this.FileList$`Exclude Data` %in% "NO"] <-
      "FALSE"
    this.FileList$`Exclude Data`[this.FileList$`Exclude Data` %in% "YES"] <-
      "TRUE"
    #
  } else if ((sum(sort(unique(
    this.FileList$`Exclude Data`
  )) %in% c(0, 1)) == 2) == TRUE) {
    cat("**TYPE 4: Recoding 1/0 to TRUE/FALSE...**\n")
    # DO RECODING HERE
    this.FileList$`Exclude Data`[this.FileList$`Exclude Data` == 0] <-
      "FALSE"
    this.FileList$`Exclude Data`[this.FileList$`Exclude Data` == 1] <-
      "TRUE"
    #
  } else if ((sum(sort(unique(
    this.FileList$`Exclude Data`
  )) %in% c("FALSE")) == 1) == TRUE) {
    cat("**TYPE 1: Can use as is...**\n")
    # DO RECODING HERE
  } else if (sum(sort(is.na(unique(
    this.FileList$`Exclude Data`
  )) == c(FALSE, TRUE))) == 2) {
    this.FileList$`Exclude Data`[is.na(this.FileList$`Exclude Data`)] <-
      "FALSE"
    cat("**TYPE 6: Recoding missing values to FALSE...**\n")
  }  else if (is.na(unique(this.FileList$`Exclude Data`))  == TRUE) {
    cat("**TYPE 5: Recoding missing values to FALSE...**\n")
    # DO RECODING HERE
    this.FileList$`Exclude Data`[is.na(this.FileList$`Exclude Data`)] <-
      "FALSE"
    #
  } else if (sum((unique(this.FileList$`Exclude Data`) %in% c("TRUE", NA))) == 2) {
    cat("**TYPE 5: Recoding missing values to FALSE...**\n")
    this.FileList$`Exclude Data`[is.na(this.FileList$`Exclude Data`)] <-
      "FALSE"
  } else {
    cat("**TYPE NOT MARKED**\n\n")
  }
  if ((sum(sort(unique(
    this.FileList$`Exclude Data`
  )) %in% c("FALSE", "TRUE")) == 2) == TRUE) {
    cat("DONE!\n")
  } else if ((sum(sort(unique(
    this.FileList$`Exclude Data`
  )) %in% c("FALSE")) == 1) == TRUE) {
    cat("DONE!\n")
  }

  this.FileList %>% select(
    c(
      "Subject Code",
      "Main Data",
      "Secondary Data",
      "Finger Counting Data",
      "Exclude Data",
      "Reason",
      "Other comments"
    )
  ) %>% mutate(`Exclude Data` = as.logical(`Exclude Data`)) -> this.FileList

  return(this.FileList)
}
