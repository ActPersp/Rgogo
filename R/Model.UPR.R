# Unearned Premium Reserve Model
setClass(Class = "Model.UPR", contains = "IModel")

Model.UPR <- function(args = ArgSet.UPR(), id = character(0L), descrip = as.character(0L)) {
   model <- new(
      Class = "Model.UPR",
      Args = args,
      Descrip = as.character(descrip)
   )
   SetModelId(model) <- as.character(id)
   return(model)
}

setMethod(
   f = "Run",
   signature = c("Model.UPR", "Cov"),
   definition = function(object, var, result = list()) {
      projStartDate <- GetArgValue(object, "ValuDate") + 1
      result <- Project(GetPlan(var), var, result)
      result$Timeline <- GetProjTimelineInfo(projStartDate, var, GetPlan(var))
      # Calculate unearned portion
      tInfo <- result$Timeline
      premDueDates <- GetCovTimeline(tInfo)[((1:length(GetCovProjTimeIndex(tInfo))) - 1) %% (12 / GetPremMode(var)) == 0]
      x <- sum(premDueDates < projStartDate)
      if (x > 0 & x < length(premDueDates)) {
         prevPremDate <- premDueDates[x]
         nextPremDate <- premDueDates[x + 1]
         urndProp <- (lubridate::interval(projStartDate, nextPremDate) / lubridate::days(1)) / (lubridate::interval(prevPremDate, nextPremDate) / lubridate::days(1))
      } else {
         urndProp <- 0
      }
      uprGross <- GetModPrem(var) * urndProp
      #uprCeded <- uprGross * ifelse(HasValue(GetReinProp(var)), GetReinProp(var), 0)
      uprRein <- 0
      uprNet <- uprGross - uprRein
      result$Res <- data.frame(
         Res.Gross = uprGross,
         Res.Rein = uprRein,
         Res.Net = uprNet,
         stringsAsFactors = FALSE
      )
      result$UrndProp <- urndProp
      result$ValuSumm <- .SumrzResult.Model.UPR(object, var, result)
      result$CovData <- var
      result$ArgSet <- GetArgs(object)
      return(result)
   }
)

.SumrzResult.Model.UPR <- function(model, cov, result) {
   m <- GetPolMonth(GetIssDate(cov), GetArgValue(model, "ValuDate"))
   proj <- result$Proj
   res <- result$Res
   anlzPrem <- GetModPrem(cov) * GetPremMode(cov)
   curCV <- ifelse(is.null(proj$CV), 0, proj$CV[m])
   grossSumInsd <- ifelse(is.null(proj$Ben.Dth), 0, proj$Ben.Dth[m]) + ifelse(is.null(proj$Ben.Dth.PUA), 0, proj$Ben.Dth.PUA[m])
   reinSumInsd <- ifelse(is.null(proj$Rein.Ben), 0, proj$Rein.Ben[m])
   df <- data.frame(
      CovId = ifelse(length(GetId(cov)) > 0, GetId(cov), NA),
      PlanId = GetId(GetPlan(cov)),
      AnlzPrem = anlzPrem,
      CV = curCV,
      GrossSumInsd = grossSumInsd,
      ReinSumInsd = reinSumInsd,
      NetSumInsd = grossSumInsd - reinSumInsd,
      GrossRes = res$Res.Gross,
      ReinRes = res$Res.Rein,
      NetRes = res$Res.Net,
      LiabDur = GetProjLen(result$Timeline),
      PV.Prem = NA,
      PV.Prem.Tax = NA,
      PV.Comm = NA,
      PV.Comm.Ovrd = NA,
      PV.Ben.Dth = NA,
      PV.Ben.Mat = NA,
      PV.Ben.Sur = NA,
      PV.Ben.Anu = NA,
      PV.Ben.Dth.PUA = NA,
      PV.Ben.Mat.PUA = NA,
      PV.Ben.Sur.PUA = NA,
      PV.Expns.Acq = NA,
      PV.Expns.Mnt = NA,
      PV.Rein.Prem = NA,
      PV.Rein.Comm = NA,
      PV.Rein.Ben = NA,
      PV.Rein.Prem.Rfnd = NA,
      PV.Rein.Comm.Rfnd = NA,
      stringsAsFactors = FALSE
   )
   return(df)
}


setMethod(
   f = "Run",
   signature = c("Model.UPR", "Cov2"),
   definition = function(object, var, result = list()) {
      result <- callNextMethod()
      lives <- GetCovCount(var)
      result$Res$Res.Gross <- result$Res$Res.Gross * lives
      result$Res$Res.Rein <- result$Res$Res.Rein * lives
      result$Res$Res.Net <- result$Res$Res.Net * lives
      result$ValuSumm$AnlzPrem <- result$ValuSumm$AnlzPrem * lives
      result$ValuSumm$CV <- result$ValuSumm$CV * lives
      result$ValuSumm$GrossSumInsd <- result$ValuSumm$GrossSumInsd * lives
      result$ValuSumm$ReinSumInsd <- result$ValuSumm$ReinSumInsd * lives
      result$ValuSumm$NetSumInsd <- result$ValuSumm$NetSumInsd * lives
      result$ValuSumm$GrossRes <- result$ValuSumm$GrossRes * lives
      result$ValuSumm$ReinRes <- result$ValuSumm$ReinRes * lives
      result$ValuSumm$NetRes <- result$ValuSumm$NetRes * lives
      return(result)
   }
)

