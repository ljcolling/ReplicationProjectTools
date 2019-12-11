#' This function is used for extracting the eye tracker data that is written
#' into the matlab struct from the main PTB script.
#' It produces an output that includes whether to accept or reject the trail
#' and a number corresponding the max horizontal deviation in degrees
#' trials are marked as rejected in the max horizontal deviation exceeds
#' 2 degrees. Deviations before the digit appeared are not marked.
#' These parameters can be over-written by editing the function directly
#' as they have been hard-coded for simiplicity
#'
#' If no eye tracking data exists, then the output shows accept for all trials
#' and lists the max horizontal deviation as zero
#' @export
ExtractEyetracker <- function(this.SubjectData) {
  timePoints = c("initDisplay", "digitDisplay", "variableDelay")
  timePointReject = timePoints[2:3]
  maxXDeviation = 2

  has.EyeTracking <- function(this.subject.pri.data)
  {
    return("eyeTrackingStruct" %in% ls(this.subject.pri.data))
  }


  ExtractEyetrackerMain <- function(this.SubjectData) {
    eyeTrackingStruct = this.SubjectData$eyeTrackingStruct

    fixationBreaks = which(as.numeric(lapply(X = 1:dim(eyeTrackingStruct)[3], function(X)
      length(as.data.frame(eyeTrackingStruct[, , X])) > 0)) == 1)

    screenParams = ExtractScreenParams(this.SubjectData)
    eyeTrackingStruct.breaksonly = lapply(X = 1:length(fixationBreaks), function(X)
      MakeEyeTrackerTrial(
        this.EyetrackerTrial = eyeTrackingStruct[, , fixationBreaks[X]]$fixationBreaks,
        screenParams = screenParams
      ))

    A <- rep(FALSE, 1, dim(eyeTrackingStruct)[3])
    B <- rep(FALSE, 1, dim(eyeTrackingStruct)[3])

    eyeTrackingStruct.breaksonly = as.data.frame.fromlist(eyeTrackingStruct.breaksonly)

    A[fixationBreaks]  = eyeTrackingStruct.breaksonly[, 1]
    B[fixationBreaks]  = eyeTrackingStruct.breaksonly[, 2]

    df = data.frame(A = A, B = B)
    names(df) <- names(eyeTrackingStruct.breaksonly)
    return(tibble::as.tibble(df))

  }


  ExtractScreenParams <- function(this.subject.pri.data) {
    paramsToExtract = c("xCenter", "yCenter", "pixPerDegWidth", "pixPerDegHeight")


    paramValues = c(
      this.subject.pri.data$params[, , 1][paramsToExtract[1]][[1]],
      this.subject.pri.data$params[, , 1][paramsToExtract[2]][[1]],
      this.subject.pri.data$params[, , 1][paramsToExtract[3]][[1]],
      this.subject.pri.data$params[, , 1][paramsToExtract[4]][[1]]
    )


    return(as.list(setNames(paramValues, paramsToExtract)))

  }


  MakeEyeTrackerTrial <- function(this.EyetrackerTrial, screenParams) {
    #this.EyetrackerTrial = eyetrackerStruct[,,fixationBreaks[t]]$fixationBreaks
    if (length(this.EyetrackerTrial) > 4) {
      this.EyetrackerTrial.df = as.data.frame(`row.names<-`(Reduce(
        function(x, y)
          rbind(x, y),
        lapply(
          X = 1:(length(this.EyetrackerTrial) / 4),
          FUN = function(X)
            unlist(this.EyetrackerTrial, use.names = F)[seq(X,
                                                            length(this.EyetrackerTrial),
                                                            length(this.EyetrackerTrial) / 4)]
        )
      ), NULL))
    }  else {
      this.EyetrackerTrial.df <-
        as.data.frame(t(unlist(this.EyetrackerTrial)))
    }


    names(this.EyetrackerTrial.df) <- c("Part", "Frame", "Xpx", "Ypx")
    this.EyetrackerTrial.df$Frame = as.numeric(as.character(this.EyetrackerTrial.df$Frame))
    this.EyetrackerTrial.df$Xpx = as.numeric(as.character(this.EyetrackerTrial.df$Xpx))
    this.EyetrackerTrial.df$Ypx = as.numeric(as.character(this.EyetrackerTrial.df$Ypx))

    this.EyetrackerTrial.df$Xoffset = abs(this.EyetrackerTrial.df$Xpx - screenParams$xCenter)
    this.EyetrackerTrial.df$Yoffset = abs(this.EyetrackerTrial.df$Ypx - screenParams$yCenter)

    this.EyetrackerTrial.df$Xdeviation = this.EyetrackerTrial.df$Xoffset / screenParams$pixPerDegWidth
    this.EyetrackerTrial.df$Ydeviation = this.EyetrackerTrial.df$Yoffset / screenParams$pixPerDegHeight

    # Find the blinks
    # And zero them out
    this.EyetrackerTrial.df[this.EyetrackerTrial.df$Xpx < 0, c(3:8)] = 0


    # Now calculate the max deviation
    # The max deviation is
    if (sum(this.EyetrackerTrial.df$Part %in% timePointReject) > 0) {
      deviations = this.EyetrackerTrial.df[this.EyetrackerTrial.df$Part %in% timePointReject, ]$Xdeviation
      max.deviations = max(deviations)
      mark = max.deviations > maxXDeviation


      return(list(Reject = mark, maxHorzDeviation = max.deviations))
    } else {
      return(list(Reject = FALSE, maxHorzDeviation = 0))
    }
  }

  as.data.frame.fromlist <- function(list) {
    this.list = unlist(list, recursive = F, use.names = T)
    this.names = unique(names(this.list))
    for (name in this.names) {
      eval(parse(
        text = paste0(
          name,
          ' = unlist(this.list[names(this.list) %in% "',
          name ,
          '"], use.names = FALSE)'
        )
      ))
    }

    df = data.frame()

    for (name in this.names) {
      df = rbind(df, t(eval(parse(text = name))))
    }

    df = t(df)
    df = as.data.frame(df, row.names = NULL)
    names(df) <- this.names
    row.names(df) <- NULL
    return(df)
  }

  if (has.EyeTracking(this.SubjectData) == TRUE) {
    eyetrackerStruct = ExtractEyetrackerMain(this.SubjectData)
    eyetrackerStruct = tibble::add_column(eyetrackerStruct, trial = seq(1:800),.before = 1)
    return(eyetrackerStruct)
  } else if (has.EyeTracking(this.SubjectData) == FALSE) {
    eyetrackerStruct = tibble::tibble(Reject = rep(0, dim(this.SubjectData$responseStruct)[3]),
                                      maxHorzDeviation = rep(0, dim(this.SubjectData$responseStruct)[3]))
    eyetrackerStruct = tibble::add_column(eyetrackerStruct, trial = seq(1:800),.before = 1)
    return(eyetrackerStruct)
  }



}
