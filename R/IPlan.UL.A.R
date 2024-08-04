setClass(
   Class = "IPlan.UL.A",          # This class implements universal life type A
   contains = c("IPlan.UL"),
   slots = c(
      Corridor ="numeric"         # Corridor: the minimum amount of difference between death benefit and cash value
   )
)

setValidity(
   Class = "IPlan.UL.A",
   method = function(object) {
      err <- New.SysMessage()
      # Validate @Corridoor
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1, maxLen = 1),
            Validator.Range(minValue = 0, maxValue = Inf, allowNA = FALSE)
         ),
         object@Corridor
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "Invaid corridor amount.  The value must be a non-negative numeric scalar."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

IPlan.UL.A <- function(covYears = NA, covToAge = NA,
                       corridor = 0,
                       premTable = character(0L), modFactor = c("1" = 1, "2" = 0.5, "4" = 0.25, "12" = 1/12),
                       polFee = numeric(0), premTaxRate = numeric(0L), coiTable = character(0L),
                       premLoadSchd = numeric(0L), premLoadSchd2 = numeric(0L),
                       expnsChrgSchd = numeric(0L), expnsChrgMode = 1L, expnsChrgTiming = 0L, expnsChrgType = 0L,
                       surChrgSchd = numeric(0L), minIntrCredRate = numeric(0L),
                       commSchd = numeric(0L), commSchd2 = numeric(0L),
                       ovrdOnPremSchd = numeric(0L), ovrdOnPremSchd2 = numeric(0L),
                       ovrdOnCommSchd = numeric(0L), ovrdOnCommSchd2 = numeric(0L),
                       rein = character(0L), id = character(0L), descrip = character(0L)) {
   stopifnot(any(!is.na(c(covYears, covToAge))))
   covPeriod <- c(CovYears = covYears, CovToAge = as.integer(covToAge))
   covPeriod <- covPeriod[!is.na(covPeriod)]
   premPeriod <- c(PremYears = covYears, PremToAge = as.integer(covToAge))
   premPeriod <- premPeriod[!is.na(premPeriod)]
   plan <- new(Class = "IPlan.UL.A",
               CovPeriod = covPeriod,
               PremPeriod = premPeriod,
               Corridor = corridor,
               PremTable = premTable,
               ModFactor = modFactor,
               PolFee = polFee,
               PremTaxRate = premTaxRate,
               COITable = coiTable,
               PremLoadSchd = premLoadSchd,
               PremLoadSchd2 = premLoadSchd2,
               ExpnsChrgSchd = expnsChrgSchd,
               ExpnsChrgMode = as.integer(expnsChrgMode),
               ExpnsChrgTiming = as.integer(expnsChrgTiming),
               ExpnsChrgType = as.integer(expnsChrgType),
               SurChrgSchd = surChrgSchd,
               MinIntrCredRate = minIntrCredRate,
               CommSchd = commSchd,
               CommSchd2 = commSchd2,
               OvrdOnPremSchd = ovrdOnPremSchd,
               OvrdOnPremSchd2 = ovrdOnPremSchd2,
               OvrdOnCommSchd = ovrdOnCommSchd,
               OvrdOnCommSchd2 = ovrdOnCommSchd2,
               Rein = rein,
               Descrip = as.character(descrip)
   )
   SetPlanId(plan) <- as.character(id)
   return(plan)
}

setMethod(
   f = "GetCorridor",
   signature = "IPlan.UL.A",
   definition = function(object) {
      return(object@Corridor)
   }
)

setMethod(
   f = "SetCorridor<-",
   signature = "IPlan.UL.A",
   definition = function(object, value) {
      object@Corridor <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "ProjFund",
   signature = "IPlan.UL.A",
   definition = function(object, cov, resultContainer) {
      covMonths <- GetCovMonths(object, cov)
      # Project premium load
      minPremLoad <- -resultContainer$Proj$Prem.Min * GetPremLoadSchd(object, cov)
      exsPremLoad <- -resultContainer$Proj$Prem.Exs * GetPremLoadSchd2(object, cov)
      premLoad <- minPremLoad + exsPremLoad
      prem <- resultContainer$Proj$Prem
      # Project expense charge
      expnsChrg <- -GetExpnsChrgSchd(object, cov)
      # Get cost of insurance charge rate
      coiTable <- GetCOITable(object, cov)
      if (!is.null(coiTable)) {
         coiRate <- rep(LookUp(coiTable, cov, len = ceiling(covMonths / 12)), each = 12, length.out = covMonths) / 12     # Monthly cost of insurance charge rate
      } else {
         coiRate <- rep(0, length.out = covMonths)
      }
      # Get interest credit rate
      iMin <- GetMinIntrCredRate(object, cov)
      intrCredAssump <- GetIntrCredAssump(resultContainer)
      if (is.null(intrCredAssump)) {
         i <- iMin
      } else {
         i <- GetAssump(intrCredAssump, cov, object)
         i <- ifelse(i < iMin, iMin, i)
      }
      j <- (1 + i) ^ (1 / 12) - 1
      if (is.null(resultContainer$Timeline)) {
         projStartPolMonth <- 1
      } else {
         projStartPolMonth <- GetProjPolMonths(resultContainer$Timeline)[1]
      }
      # Project fund
      covAccBal <- GetAccBal(cov)
      fundAdj <- iCred <- fundBeg <- fundEnd <- coi <- naar <- dthBen <- rep(0, covMonths)
      expnsChrgTiming <- GetExpnsChrgTiming(object)
      expnsChrgType <- GetExpnsChrgType(object)
      resultContainer$.ProjEndPolMonth <- covMonths
      corridor <- GetCorridor(object)
      faceAmt <- GetFaceAmt(cov)
      for (t in 1:covMonths) {
         fundBeg[t] <- ifelse(t == 1, 0, fundEnd[t - 1])
         naar[t] <- max(faceAmt - fundBeg[t], corridor)
         dthBen[t] <- fundBeg[t] + naar[t]
         coi[t] <- -naar[t] * coiRate[t]
         openBal <- fundBeg[t] + prem[t] + premLoad[t] + expnsChrg[t] * (expnsChrgTiming == 0) + coi[t]
         iCred[t] <- openBal * j[t]
         fundEnd[t] <- openBal + iCred[t]
         fundEnd[t] <- fundEnd[t] + expnsChrg[t] * (expnsChrgTiming == 1) * ifelse(expnsChrgType == 0L, 1, fundEnd[t])
         fundAdj[t] <- ifelse(HasValue(covAccBal), (covAccBal - fundEnd[t]) * (t == projStartPolMonth), 0)
         fundEnd[t] <- fundEnd[t] + fundAdj[t]
         if (t >=  projStartPolMonth & fundEnd[t] < 0) {
            resultContainer$.ProjEndPolMonth <- t
            break
         }
      }
      # Save fund projection results
      resultContainer$Proj$Fund.PremLoad.MinPrem <- minPremLoad
      resultContainer$Proj$Fund.PremLoad.ExsPrem <- exsPremLoad
      resultContainer$Proj$Fund.PremLoad <- premLoad
      resultContainer$Proj$Fund.ExpnsChrg <- expnsChrg
      resultContainer$Proj$Fund.COI <- coi
      resultContainer$Proj$Fund.IntrCred <- iCred
      resultContainer$Proj$Fund.Adj <- fundAdj
      resultContainer$Proj$Fund <- fundEnd
      resultContainer$Proj$Ben.Dth <- dthBen
      return(resultContainer)
   }
)

setMethod(
   f = "ProjDthBen",
   signature = "IPlan.UL.A",
   definition = function(object, cov, resultContainer){
      # Death benefit has been projected in ProjFund
      return(resultContainer)
   }
)



