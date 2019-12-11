#' This function extracts the results of the finger counting task.
#' It produces an output that include the first finger used in for each
#' of the sentences. It also marks the version. This corresponds to whether
#' the experimenter recorded all the fingers used (old) or whether they simply
#' recorded the first finger used on each hand
#' @export
ExtractFingerData <- function(fingerData) {
  ExtractFingercounting <- function(data) {
    version = ifelse(length(data$fingercountingData[, , ]$fingers) == 40,
                     "old",
                     "new")

    if (version == "new") {
      df = t(as.data.frame(data$fingercountingData[, , ]$fingers))
      df = as.data.frame(df)
      rownames(df) <- NULL

      #df = df[as.numeric(data$fingercountingData[,,]$order),] # reorder
      df$h1f1 = as.character(unlist(df$h1f1))
      df$h2f1 = as.character(unlist(df$h2f1))
      df$order = as.numeric(data$fingercountingData[, , ]$order) # store order
      df$version = version # store the version type
      return(tibble::as_tibble(df))
    } else if (version == "old") {
      df.pre = data$fingercountingData[, , ]$fingers[, , ]
      h1 = t(df.pre) == "1"
      h2 = t(df.pre) == "6"
      h1f1 = character(4)
      h2f1 = character(4)
      for (i in 1:4) {
        h1f1[i] = ifelse(sum(h1[i, ] == TRUE) > 0, colnames(h1)[h1[i, ] == TRUE], "NA")
        h2f1[i] = ifelse(sum(h2[i, ] == TRUE) > 0, colnames(h2)[h2[i, ] == TRUE], "NA")
      }
      df = tibble(
        h1f1 = h1f1,
        h2f1 = h2f1,
        order = as.numeric(data$fingercountingData[, , ]$order),
        version = version
      )
      return(df)
    }
  }

  fingerDataStructAll = ExtractFingercounting(fingerData)
  fingerDataStruct = as_tibble(t(fingerDataStructAll$h1f1)) # take the data from hand 1 only
  names(fingerDataStruct) <-
    paste0("fc", fingerDataStructAll$order) # rename
  fingerDataStruct <-
    fingerDataStruct[paste0("fc", seq(1, 4, 1))] # reorder
  fingerDataStruct$version = fingerDataStructAll$version[1] # save recording method version

  return(fingerDataStruct)

}
