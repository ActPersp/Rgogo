setClass(
   Class = "Cov",
   slots = c(Id = "character",
             PlanId = "character",
             IssDate = "Date",
             IssAge = "integer",
             RiskClass = "character",
             FaceAmt = "numeric",
             ModPrem = "numeric",
             PremMode = "integer",
             ReinId = "character",
             ReinProp = "numeric",
             PUAAmt = "numeric",
             AccBal = "numeric",
             IssAge2 = "integer",
             RiskClass2 = "character",
             FaceAmt2 = "numeric",
             LifeStatus = "integer",
             LifeStatus2 = "integer",
             ExpnsWeight = "numeric",
             ReportClass1 = "character",
             ReportClass2 = "character",
             ReportClass3 = "character",
             ReportClass4 = "character",
             ReportClass5 = "character",
             CommSchd = "character",
             OvrdOnPremSchd = "character",
             OvrdOnCommSchd = "character",
             CommSchd2 = "character",
             OvrdOnPremSchd2 = "character",
             OvrdOnCommSchd2 = "character",
             MortMultExtra = "numeric",
             MortMultExtra2 = "numeric",
             Descrip = "character"
   )
)

setValidity(
   Class = "Cov",
   method = function(object) {
      err <- New.SysMessage()
      # Validate @Id
      isValid <- Validate(
         Validator.Length(minLen = 0, maxLen = 1),
         object@Id
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "The length of slot value @Id cannot be greater than 1."
      }
      # Validate @PlanId
      isValid <- Validate(
         Validator.Length(minLen = 0, maxLen = 1),
         object@PlanId
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "The length of slot value @PlanId cannot be greater than 1."
      }
      # Validate @IssDate
      isValid <- Validate(
         Validator.Length(minLen = 1, maxLen = 1),
         object@IssDate
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "The length of slot value @IssDate must be 1."
      }
      # Validate @IssAge
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 0, maxLen = 1),
            Validator.Range(minValue = 0, maxValue = 120)
         ),
         object@IssAge
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot @IssAge must contain an integer scalar between 0 and 120."
      }
      # Validate @RiskClass
      isValid <- Validate(
         Validator.Length(minLen = 0, maxLen = 1),
         object@RiskClass
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot @RiskClass must contain a string scalar."
      }
      # Validate @FaceAmt
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 0, maxLen = 1),
            Validator.Range(minValue = 0)
         ),
         object@FaceAmt
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot @FaceAmt must contain a non-negative numeric scalar."
      }
      # Validate @ModPrem
      isValid <- Validate(
         Validator.Length(minLen = 0, maxLen = 1),
         object@ModPrem
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "The length of slot value @ModPrem cannot be greater than 1."
      }
      # Valid @PremMode
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 0, maxLen = 1),
            Validator.InList(valuesAllowed = c(1L, 2L, 4L, 12L))
         ),
         object@PremMode
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "The length of slot value @PremMode cannot be greater than 1.  The value must be 1L, 2L, 4L or 12L."
      }
      # Validate @ReinId
      isValid <- Validate(
         Validator.Length(minLen = 0, maxLen = 1),
         object@ReinId
      )
      # Validate @ReinProp
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 0, maxLen = 1),
            Validator.Range(minValue = 0, allowNA = TRUE)
         ),
         object@ReinProp
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot @ReinProp must contain a non-negative numeric scalar."
      }
      # Validate @PUAAmt
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 0, maxLen = 1),
            Validator.Range(minValue = 0, allowNA = TRUE)
         ),
         object@PUAAmt
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot @PUAAmt must contain a non-negative numeric scalar."
      }
      # Validate @AccBal
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 0, maxLen = 1),
            Validator.Range(minValue = 0, allowNA = TRUE)
         ),
         object@AccBal
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot @AccBal must contain a non-negative numeric scalar."
      }
      # Validate @IssAge2
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 0, maxLen = 1),
            Validator.Range(minValue = 0, maxValue = 120, allowNA = TRUE)
         ),
         object@IssAge2
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot @IssAge2 must contain an integer scalar between 0 and 120."
      }
      # Validate @RiskClass2
      isValid <- Validate(
         Validator.Length(minLen = 0, maxLen = 1),
         object@RiskClass2
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot @RiskClass2 must contain a string scalar."
      }
      # Validate @FaceAmt2
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 0, maxLen = 1),
            Validator.Range(minValue = 0, allowNA = TRUE)
         ),
         object@FaceAmt2
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot @FaceAmt2 must contain a non-negative numeric scalar."
      }
      # Valid @LifeStatus
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 0, maxLen = 1),
            Validator.InList(valuesAllowed = c(0L, 1L), allowNA = TRUE)
         ),
         object@LifeStatus
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "The length of slot value @LifeStatus cannot be greater than 1.  The value must be 0L or 1L"
      }
      # Valid @LifeStatus2
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 0, maxLen = 1),
            Validator.InList(valuesAllowed = c(0L, 1L), allowNA = TRUE)
         ),
         object@LifeStatus2
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "The length of slot value @LifeStatus2 cannot be greater than 1.  The value must be 0L or 1L"
      }
      # Validate @ExpnsWeight
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 0, maxLen = 1),
            Validator.Range(minValue = 0, allowNA = TRUE)
         ),
         object@ExpnsWeight
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot @ExpnsWeight must contain a non-negative numeric scalar."
      }
      # Validate @ReportClass1
      isValid <- Validate(
         Validator.Length(minLen = 0, maxLen = 1),
         object@ReportClass1
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot @ReportClass1 must contain a string scalar."
      }
      # Validate @ReportClass2
      isValid <- Validate(
         Validator.Length(minLen = 0, maxLen = 1),
         object@ReportClass2
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot @ReportClass2 must contain a string scalar."
      }
      # Validate @ReportClass3
      isValid <- Validate(
         Validator.Length(minLen = 0, maxLen = 1),
         object@ReportClass3
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot @ReportClass3 must contain a string scalar."
      }
      # Validate @ReportClass4
      isValid <- Validate(
         Validator.Length(minLen = 0, maxLen = 1),
         object@ReportClass4
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot @ReportClass4 must contain a string scalar."
      }
      # Validate @ReportClass5
      isValid <- Validate(
         Validator.Length(minLen = 0, maxLen = 1),
         object@ReportClass5
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot @ReportClass5 must contain a string scalar."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
      # Validate @MortMultExtra
      isValid <- Validate(Validator.Length(minLen = 0, maxLen = 1), object@MortMultExtra)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot @MortMultExtra must be a numeric scalar."
      }
      # Validate @MortMultExtra2
      isValid <- Validate(Validator.Length(minLen = 0, maxLen = 1), object@MortMultExtra2)
      if (isValid != TRUE) {
         AddMessage(err) <- "Slot @MortMultExtra2 must be a numeric scalar."
      }
   }
)

Cov <- function(issDate = as.Date("1900-01-01"),
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
                descrip = character(0L)
                ) {
   object <- new(
      Class = "Cov",
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
      Descrip = as.character(descrip)
   )
   return(object)
}

setMethod(
   f = "GetId",
   signature = "Cov",
   definition = function(object) {
      return(object@Id)
   }
)

setMethod(
   f = "SetId<-",
   signature = c("Cov", "character"),
   definition = function(object, value) {
      object@Id <- as.character(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetPlan",
   signature = "Cov",
   definition = function(object) {
      planId <- ifelse(startsWith(object@PlanId, "Plan."), object@PlanId, paste0("Plan.", object@PlanId))
      return(eval(expr = parse(text = planId)))
   }
)

setMethod(
   f = "SetPlan<-",
   signature = c("Cov", "IPlan"),
   definition = function(object, value) {
      object@PlanId <- GetId(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "SetPlan<-",
   signature = c("Cov", "character"),
   definition = function(object, value) {
      if (startsWith(value, "Plan.")) {
         value <- substr(value, 6, nchar(value))
      }
      object@PlanId <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetPlanId",
   signature = "Cov",
   definition = function(object) {
      return(object@PlanId)
   }
)

setMethod(
   f = "GetIssDate",
   signature = "Cov",
   definition = function(object) {
      return(object@IssDate)
   }
)

setMethod(
   f = "SetIssDate<-",
   signature = "Cov",
   definition = function(object, value) {
      object@IssDate <- as.Date(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetIssAge",
   signature = "Cov",
   definition = function(object) {
      return(object@IssAge)
   }
)

setMethod(
   f = "SetIssAge<-",
   signature = "Cov",
   definition = function(object, value) {
      object@IssAge <- as.integer(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetRiskClass",
   signature = "Cov",
   definition = function(object) {
      return(object@RiskClass)
   }
)

setMethod(
   f = "SetRiskClass<-",
   signature = "Cov",
   definition = function(object, value) {
      object@RiskClass <- as.character(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetFaceAmt",
   signature = "Cov",
   definition = function(object) {
      return(object@FaceAmt)
   }
)

setMethod(
   f = "SetFaceAmt<-",
   signature = "Cov",
   definition = function(object, value) {
      object@FaceAmt <- as.numeric(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetPremMode",
   signature = "Cov",
   definition = function(object) {
      return(object@PremMode)
   }
)

setMethod(
   f = "SetPremMode<-",
   signature = "Cov",
   definition = function(object, value) {
      object@PremMode <- as.integer(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetModPrem",
   signature = "Cov",
   definition = function(object) {
      return(object@ModPrem)
   }
)

setMethod(
   f = "SetModPrem<-",
   signature = "Cov",
   definition = function(object, value) {
      object@ModPrem <- as.numeric(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetReinId",
   signature = "Cov",
   definition = function(object) {
      return(object@ReinId)
   }
)

setMethod(
   f = "SetReinId<-",
   signature = "Cov",
   definition = function(object, value) {
      object@ReinId <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetRein",
   signature = "Cov",
   definition = function(object) {
      reinId <- GetReinId(object)
      if (length(reinId) == 0) {
         return(NULL)
      }
      if (!startsWith(reinId, "Rein.")) {
         reinId <- paste0("Rein.", reinId)
      }
      return(eval(expr = parse(text = reinId)))
   }
)

setMethod(
   f = "GetReinProp",
   signature = "Cov",
   definition = function(object) {
      return(object@ReinProp)
   }
)

setMethod(
   f = "SetReinProp<-",
   signature = "Cov",
   definition = function(object, value) {
      object@ReinProp <- as.numeric(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetPUAAmt",
   signature = "Cov",
   definition = function(object) {
      return(ifelse(HasValue(object@PUAAmt), object@PUAAmt, 0))
   }
)

setMethod(
   f = "SetPUAAmt<-",
   signature = "Cov",
   definition = function(object, value) {
      object@PUAAmt <- as.numeric(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetAccBal",
   signature = "Cov",
   definition = function(object) {
      return(object@AccBal)
   }
)
setMethod(
   f = "SetAccBal<-",
   signature = "Cov",
   definition = function(object, value) {
      object@AccBal <- as.numeric(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetExpnsWeight",
   signature = "Cov",
   definition = function(object) {
      if (HasValue(object@ExpnsWeight)) {
         return(object@ExpnsWeight)
      } else {
         return(1)
      }
   }
)

setMethod(
   f = "SetExpnsWeight<-",
   signature = "Cov",
   definition = function(object, value) {
      object@ExpnsWeight <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetReportClass1",
   signature = "Cov",
   definition = function(object) {
      return(object@ReportClass1)
   }
)

setMethod(
   f = "SetReportClass1<-",
   signature = "Cov",
   definition = function(object, value) {
      object@ReportClass1 <- as.character(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetReportClass2",
   signature = "Cov",
   definition = function(object) {
      return(object@ReportClass2)
   }
)

setMethod(
   f = "SetReportClass2<-",
   signature = "Cov",
   definition = function(object, value) {
      object@ReportClass2 <- as.character(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetReportClass3",
   signature = "Cov",
   definition = function(object) {
      return(object@ReportClass3)
   }
)

setMethod(
   f = "SetReportClass3<-",
   signature = "Cov",
   definition = function(object, value) {
      object@ReportClass3 <- as.character(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetReportClass4",
   signature = "Cov",
   definition = function(object) {
      return(object@ReportClass4)
   }
)

setMethod(
   f = "SetReportClass4<-",
   signature = "Cov",
   definition = function(object, value) {
      object@ReportClass4 <- as.character(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetReportClass5",
   signature = "Cov",
   definition = function(object) {
      return(object@ReportClass5)
   }
)

setMethod(
   f = "SetReportClass5<-",
   signature = "Cov",
   definition = function(object, value) {
      object@ReportClass5 <- as.character(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetIssAge2",
   signature = "Cov",
   definition = function(object) {
      return(object@IssAge2)
   }
)

setMethod(
   f = "SetIssAge2<-",
   signature = "Cov",
   definition = function(object, value) {
      object@IssAge2 <- as.integer(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetRiskClass2",
   signature = "Cov",
   definition = function(object) {
      return(object@RiskClass2)
   }
)

setMethod(
   f = "SetRiskClass2<-",
   signature = "Cov",
   definition = function(object, value) {
      object@RiskClass2 <- as.character(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetFaceAmt2",
   signature = "Cov",
   definition = function(object) {
      return(object@FaceAmt2)
   }
)

setMethod(
   f = "SetFaceAmt2<-",
   signature = "Cov",
   definition = function(object, value) {
      object@FaceAmt2 <- as.numeric(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetLifeStatus",
   signature = "Cov",
   definition = function(object) {
      if (HasValue(object@LifeStatus)) {
         return(object@LifeStatus)
      } else {
         return(1L)    # Both lives are alive.
      }
   }
)

setMethod(
   f = "SetLifeStatus<-",
   signature = "Cov",
   definition = function(object, value) {
      object@LifeStatus <-value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetLifeStatus2",
   signature = "Cov",
   definition = function(object) {
      if (HasValue(object@LifeStatus2)) {
         return(object@LifeStatus2)
      } else {
         return(1L)    # Both lives are alive.
      }
   }
)

setMethod(
   f = "SetLifeStatus2<-",
   signature = "Cov",
   definition = function(object, value) {
      object@LifeStatus2 <-value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetCovMonths",
   signature = "Cov",
   definition = function(object) {
      return(GetCovMonths(GetPlan(object), object))
   }
)

setMethod(
   f = "GetPremMonths",
   signature = "Cov",
   definition = function(object) {
      return(GetPremMonths(GetPlan(object), object))
   }
)

setMethod(
   f = "GetCovYears",
   signature = "Cov",
   definition = function(object) {
      return(GetCovYears(GetPlan(object), object))
   }
)

setMethod(
   f = "GetPremYears",
   signature = "Cov",
   definition = function(object) {
      return(GetPremYears(GetPlan(object), object))
   }
)

setMethod(
   f = "Project",
   signature = "Cov",
   definition = function(object, resultContainer = list()) {
      return(Project(GetPlan(object), object, resultContainer))
   }
)

setMethod(
   f = "GetExpiryDate",
   signature = "Cov",
   definition = function(object) {
      plan <- GetPlan(object)
      return(GetExpiryDate(plan, object))
   }
)

# Helper function to convert data.frame to CovData
as.CovList <- function(dfCov) {
   stopifnot(is.data.frame(dfCov))
   if (nrow(dfCov) == 0) {return(list())}
   fieldList <- colnames(dfCov)
   slotList <- slotNames(new("Cov"))
   slotsWithData <- slotList[which(slotList %in% fieldList)]
   covList <- lapply(
      X = 1:nrow(dfCov),
      FUN = function(r) {
         cov <- new(Class = "Cov")
         for (s in slotsWithData) {
            slot(cov, s) <- switch (class(slot(cov, s)),
                                    "integer" = as.integer(dfCov[[r, s]]),
                                    "S4" = as.Date(dfCov[[r, s]]),
                                    dfCov[[r, s]]
            )
         }
         return(cov)
      }
   )
   return(covList)
}

IsSingleLife <- function(cov) {
   return(!HasValue(GetIssAge2(cov)) | !HasValue(GetRiskClass2(cov)))
}

setMethod(
   f = "GetCommSchd",
   signature = "Cov",
   definition = function(object) {
      # This method returns an instance of Const.
      if (HasValue(object@CommSchd)) {
         if (startsWith(object@CommSchd, "Const.")) {
            return(eval(expr = parse(text = object@CommSchd)))
         } else {
            return(eval(expr = parse(text = paste0("Const.", object@CommSchd))))
         }
      } else {
         return(NULL)
      }
   }
)

setMethod(
   f = "SetCommSchd<-",
   signature = c("Cov", "Const"),
   definition = function(object, value) {
      object@CommSchd <- GetId(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetOvrdOnPremSchd",
   signature = "Cov",
   definition = function(object) {
      # This method returns an instance of Const.
      if (HasValue(object@OvrdOnPremSchd)) {
         if (startsWith(object@OvrdOnPremSchd, "Const.")) {
            return(eval(expr = parse(text = object@OvrdOnPremSchd)))
         } else {
            return(eval(expr = parse(text = paste0("Const.", object@OvrdOnPremSchd))))
         }
      } else {
         return(NULL)
      }
   }
)

setMethod(
   f = "SetOvrdOnPremSchd<-",
   signature = c("Cov", "Const"),
   definition = function(object, value) {
      object@OvrdOnPremSchd <- GetId(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetOvrdOnCommSchd",
   signature = "Cov",
   definition = function(object) {
      # This method returns an instance of Const.
      if (HasValue(object@OvrdOnCommSchd)) {
         if (startsWith(object@OvrdOnCommSchd, "Const.")) {
            return(eval(expr = parse(text = object@OvrdOnCommSchd)))
         } else {
            return(eval(expr = parse(text = paste0("Const.", object@OvrdOnCommSchd))))
         }
      } else {
         return(NULL)
      }
   }
)

setMethod(
   f = "SetOvrdOnCommSchd<-",
   signature = c("Cov", "Const"),
   definition = function(object, value) {
      object@OvrdOnCommSchd <- GetId(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetCommSchd2",
   signature = "Cov",
   definition = function(object) {
      # This method returns an instance of Const.
      if (HasValue(object@CommSchd2)) {
         if (startsWith(object@CommSchd2, "Const.")) {
            return(eval(expr = parse(text = object@CommSchd2)))
         } else {
            return(eval(expr = parse(text = paste0("Const.", object@CommSchd2))))
         }
      } else {
         return(NULL)
      }
   }
)

setMethod(
   f = "SetCommSchd2<-",
   signature = c("Cov", "Const"),
   definition = function(object, value) {
      object@CommSchd2 <- GetId(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetOvrdOnPremSchd2",
   signature = "Cov",
   definition = function(object) {
      # This method returns an instance of Const.
      if (HasValue(object@OvrdOnPremSchd2)) {
         if (startsWith(object@OvrdOnPremSchd2, "Const.")) {
            return(eval(expr = parse(text = object@OvrdOnPremSchd2)))
         } else {
            return(eval(expr = parse(text = paste0("Const.", object@OvrdOnPremSchd2))))
         }
      } else {
         return(NULL)
      }
   }
)

setMethod(
   f = "SetOvrdOnPremSchd2<-",
   signature = c("Cov", "Const"),
   definition = function(object, value) {
      object@OvrdOnPremSchd2 <- GetId(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetOvrdOnCommSchd2",
   signature = "Cov",
   definition = function(object) {
      # This method returns an instance of Const.
      if (HasValue(object@OvrdOnCommSchd2)) {
         if (startsWith(object@OvrdOnCommSchd2, "Const.")) {
            return(eval(expr = parse(text = object@OvrdOnCommSchd2)))
         } else {
            return(eval(expr = parse(text = paste0("Const.", object@OvrdOnCommSchd2))))
         }
      } else {
         return(NULL)
      }
   }
)

setMethod(
   f = "SetOvrdOnCommSchd2<-",
   signature = c("Cov", "Const"),
   definition = function(object, value) {
      object@OvrdOnCommSchd2 <- GetId(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetMortMultExtra",
   signature = "Cov",
   definition = function(object) {
      if (HasValue(object@MortMultExtra)) {
         return(object@MortMultExtra)
      } else {
         return(0)
      }
   }
)

setMethod(
   f = "SetMortMultExtra<-",
   signature = "Cov",
   definition = function(object, value) {
      object@MortMultExtra <- as.numeric(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetMortMultExtra2",
   signature = "Cov",
   definition = function(object) {
      if (HasValue(object@MortMultExtra2)) {
         return(object@MortMultExtra2)
      } else {
         return(0)
      }
   }
)

setMethod(
   f = "SetMortMultExtra2<-",
   signature = "Cov",
   definition = function(object, value) {
      object@MortMultExtra2 <- as.numeric(value)
      validObject(object)
      return(object)
   }
)
