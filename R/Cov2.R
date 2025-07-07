setClass(
   Class = "Cov2",
   contains = "Cov",
   slots = c(CovCount = "numeric")
)

setValidity(
   Class = "Cov2",
   method = function(object) {
      err <- New.SysMessage()
      # Validate @CovCount
      isValid <- Validate(Validator.Range(minValue = 0), object@CovCount)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot @CovCount must contain a non-negative value."
      }
      if(NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

Cov2 <- function(
      issDate = as.Date("1900-01-01"),
      issAge = integer(0L),
      riskClass = character(0L),
      faceAmt = numeric(0L),
      premMode = integer(0L),
      modPrem = NA_real_,
      reinProp = NA_real_,
      puaAmt = 0,
      accBal = NA_real_,
      issAge2 = NA_integer_,
      riskClass2 = NA_character_,
      faceAmt2 = NA_real_,
      lifeStatus = NA_integer_,
      lifeStatus2 = NA_integer_,
      expnsWeight = NA_real_,
      reportClass1 = character(0L),
      reportClass2 = character(0L),
      reportClass3 = character(0L),
      reportClass4 = character(0L),
      reportClass5 = character(0L),
      planId = character(0L),
      id = character(0L),
      descrip = character(0L),
      covCount = 1
) {
   object <- new(
      Class = "Cov2",
      PlanId = as.character(planId),
      IssDate = as.Date(issDate),
      IssAge = as.integer(issAge),
      RiskClass = riskClass,
      FaceAmt = faceAmt,
      PremMode = as.integer(premMode),
      ModPrem = modPrem,
      ReinProp = reinProp,
      PUAAmt = puaAmt,
      AccBal = accBal,
      IssAge2 = issAge2,
      RiskClass2 = riskClass2,
      FaceAmt2 = faceAmt2,
      LifeStatus = lifeStatus,
      LifeStatus2 = lifeStatus2,
      ReportClass1 = reportClass1,
      ReportClass2 = reportClass2,
      ReportClass3 = reportClass3,
      ReportClass4 = reportClass4,
      ReportClass5 = reportClass5,
      Id = as.character(id),
      Descrip = as.character(descrip),
      CovCount = covCount
   )
   return(object)
}

setMethod(
   f = "GetCovCount",
   signature = "Cov2",
   definition = function(object) {
      return(object@CovCount)
   }
)

setMethod(
   f = "SetCovCount<-",
   signature = "Cov2",
   definition = function(object, value) {
      object@CovCount <- value
      validObject(object)
      return(object)
   }
)

# Helper function to convert data.frame to CovData
as.Cov2List <- function(dfCov) {
   stopifnot(is.data.frame(dfCov))
   if (nrow(dfCov) == 0) {return(list())}
   fieldList <- colnames(dfCov)
   slotList <- slotNames(new("Cov2"))
   slotsWithData <- slotList[which(slotList %in% fieldList)]
   covList <- lapply(
      X = 1:nrow(dfCov),
      FUN = function(r) {
         cov <- new(Class = "Cov2")
         for (s in slotsWithData) {
            slot(cov, s) <- switch (class(slot(cov, s)),
                                    "integer" = as.integer(dfCov[[r, s]]),
                                    dfCov[[r, s]]
            )
         }
         if (length(GetCovCount(cov)) == 0) {
            SetCovCount(cov) <- 1   # Set default value to 1
         }
         return(cov)
      }
   )
   return(covList)
}


