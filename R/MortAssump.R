setClass(
   Class = "MortAssump",
   contains = "IMortAssump",
   slots = c(
      MortTable = "character",
      MortTableMult = "numeric",
      ExtraMortTable = "character",
      ExtraMortTableMult = "numeric",
      MortImprovRate = "numeric",
      Margin = "numeric"
   )
)

setValidity(
   Class = "MortAssump",
   method = function(object) {
      err <- New.SysMessage()
      # Validate @MortTable
      v <- Validator.Names(hasNames = (length(object@MortTable) > 1))
      if (Validate(v, object@MortTable) != TRUE) {
         AddMessage(err) <- "Value of slot @MortTable in class MortAssump is invalid."
      }
      # Validate @MortTableMult
      v <- ValidatorGroup(
         Validator.Names(hasNames = (length(object@MortTableMult) > 1)),
         Validator.Range(minValue = 0)
      )
      if (Validate(v, object@MortTableMult) != TRUE) {
         AddMessage(err) <- "Value of slot @MortTableMult in class MortAssump is invalid."
      }
      # Validate @ExtraMortTable
      v <- Validator.Names(hasNames = (length(object@ExtraMortTable) > 1))
      if (Validate(v, object@ExtraMortTable) != TRUE) {
         AddMessage(err) <- "Value of slot @ExtraMortTable in class MortAssump is invalid."
      }
      # Validate @ExtraMortTableMult
      v <- ValidatorGroup(
         Validator.Names(hasNames = (length(object@ExtraMortTableMult) > 1)),
         Validator.Range(minValue = 0)
      )
      if (Validate(v, object@ExtraMortTableMult) != TRUE) {
         AddMessage(err) <- "Value of slot @ExtraMortTableMult in class MortAssump is invalid."
      }
      # Validate @MortImprovRate
      v <- Validator.Names(hasNames = (length(object@MortImprovRate) > 1))
      if (Validate(v, object@MortImprovRate) != TRUE) {
         AddMessage(err) <- "Value of slot @MortImprovRate in class MortAssump is invalid."
      }
      # Validate @Margin
      v <- Validator.Names(hasNames = (length(object@Margin) > 1))
      if (Validate(v, object@Margin) != TRUE) {
         AddMessage(err) <- "Value of slot @Margin in class MortAssump is invalid."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

# Consturctor for Mortality Assumption
MortAssump <- function(mortTable = character(0L),
                       mortTableMult = 1,
                       extraMortTable = character(0L),
                       extraMortTableMult = 1,
                       mortImprovRate = 0,
                       margin = 0,
                       id = character(0L),
                       descrip = character(0L)) {
   assump <- new(
      Class = "MortAssump",
      MortTable = mortTable,
      MortTableMult = mortTableMult,
      ExtraMortTable = extraMortTable,
      ExtraMortTableMult = extraMortTableMult,
      MortImprovRate = mortImprovRate,
      Margin = margin,
      Descrip = as.character(descrip)
   )
   SetAssumpId(assump) <- as.character(id)
   return(assump)
}

setMethod(
   f = "GetMortTable",
   signature = "MortAssump",
   definition = function(object, cov = NULL, plan = NULL) {
      if(is.null(cov) & is.null(plan)) {
         return(object@MortTable)
      }
      if (length(object@MortTable) > 0) {
         if (length(object@MortTable) == 1) {
            tblId <- object@MortTable
         } else {
            riskClass <- GetRiskClass(object, cov, plan)
            tblId <- object@MortTable[riskClass]
         }
         tblId <- ifelse(startsWith(tblId, "Mort."), tblId, paste0("Mort.", tblId))
         return(eval(expr = parse(text = tblId)))
      } else {
         return(NULL)
      }
   }
)

setMethod(
   f = "SetMortTable<-",
   signature = "MortAssump",
   definition = function(object, value) {
      object@MortTable <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetMortTableMult",
   signature = "MortAssump",
   definition = function(object, cov = NULL, plan = NULL) {
      if(is.null(cov) & is.null(plan)) {
         return(object@MortTableMult)
      }
      if (length(object@MortTableMult) == 0) {
         return(1)
      } else if (length(object@MortTableMult) == 1) {
         return(object@MortTableMult)
      } else {
         return(object@MortTableMult[GetRiskClass(object, cov, plan)])
      }
   }
)

setMethod(
   f = "SetMortTableMult<-",
   signature = "MortAssump",
   definition = function(object, value) {
      object@MortTableMult <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetExtraMortTable",
   signature = "MortAssump",
   definition = function(object, cov = NULL, plan = NULL) {
      if(is.null(cov) & is.null(plan)) {
         return(object@ExtraMortTable)
      }
      if (length(object@ExtraMortTable) > 0) {
         if (length(object@ExtraMortTable) == 1) {
            tblId <- object@ExtraMortTable
         } else {
            riskClass <- GetRiskClass(object, cov, plan)
            tblId <- object@ExtraMortTable[riskClass]
         }
         return(eval(expr = parse(text = tblId)))
      } else {
         return(NULL)
      }
   }
)

setMethod(
   f = "SetExtraMortTable<-",
   signature = "MortAssump",
   definition = function(object, value) {
      object@ExtraMortTable <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetExtraMortTableMult",
   signature = "MortAssump",
   definition = function(object, cov = NULL, plan = NULL) {
      if(is.null(cov) & is.null(plan)) {
         return(object@ExtraMortTableMult)
      }
      if (length(object@ExtraMortTableMult) == 0) {
         return(1)
      } else if (length(object@ExtraMortTableMult) == 1) {
         return(object@ExtraMortTableMult)
      } else {
         return(object@ExtraMortTableMult[GetRiskClass(object, cov, plan)])
      }
   }
)

setMethod(
   f = "SetExtraMortTableMult<-",
   signature = "MortAssump",
   definition = function(object, value) {
      object@ExtraMortTableMult <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetMortImprovRate",
   signature = "MortAssump",
   definition = function(object, cov = NULL, plan = NULL) {
      if(is.null(cov) & is.null(plan)) {
         return(object@MortImprovRate)
      }
      if (length(object@MortImprovRate) == 0) {
         rate <- 0
      } else if (length(object@MortImprovRate) > 1) {
         rate <- object@MortImprovRate[GetRiskClass(object, cov, plan)]
      } else {
         rate <- object@MortImprovRate
      }
      return(rate)
   }
)

setMethod(
   f = "SetMortImprovRate<-",
   signature = "MortAssump",
   definition = function(object, value) {
      object@MortImprovRate <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetMargin",
   signature = "MortAssump",
   definition = function(object, cov = NULL, plan = NULL) {
      if(is.null(cov) & is.null(plan)) {
         return(object@Margin)
      }
      if (length(object@Margin) == 0) {
         return(0)
      } else if (length(object@Margin) == 1) {
         return(object@Margin)
      } else {
         return(object@Margin[GetRiskClass(object, cov, plan)])
      }
   }
)

setMethod(
   f = "SetMargin<-",
   signature = "MortAssump",
   definition = function(object, value) {
      object@Margin <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetRiskClass",
   signature = "MortAssump",
   definition = function(object, cov, plan) {
      return(GetRiskClass(plan, cov))
   }
)

setMethod(
   f = "GetBaseMortRateVector",
   signature = "MortAssump",
   definition = function(object, cov, plan, ignoreCovPeriod = FALSE) {
      qTable <- GetMortTable(object, cov, plan)
      covYears <- ceiling(GetCovYears(plan, cov))
      if (!is.null(qTable)) {
         qTableRate <- LookUp(qTable, cov, ifelse(ignoreCovPeriod, NA_integer_, covYears))
      } else {
         # qTableRate <- rep(0, length.out = ifelse(ignoreCovPeriod, Global.MaxProjYears(), covYears))
         qTableRate <- rep(0, length.out = ifelse(ignoreCovPeriod, GetValue(Rgogo::Const.MaxProjYears), covYears))
      }
      qMult <- GetMortTableMult(object, cov, plan)
      return(qTableRate * qMult * (1 + GetMortMultExtra(cov)))
   }
)

setMethod(
   f = "GetExtraMortRateVector",
   signature = "MortAssump",
   definition = function(object, cov, plan, ignoreCovPeriod = FALSE) {
      # Extra mortality to be added to base mortality rates
      xqTable <- GetExtraMortTable(object, cov, plan)
      if (!is.null(xqTable)) {
         len <- ifelse(ignoreCovPeriod, NA_integer_, ceiling(GetCovYears(plan, cov)))
         xqTableRate <- LookUp(xqTable, cov, len)
      } else {
         xqTableRate <- 0
      }
      xqMult <- GetExtraMortTableMult(object, cov, plan)
      return(xqTableRate * xqMult)
   }
)

setMethod(
   f = "GetMortImprovAdjVector",
   signature = "MortAssump",
   definition = function(object, cov, plan, ignoreCovPeriod = FALSE) {
      # len <- ifelse(ignoreCovPeriod, Global.MaxProjYears(), ceiling(GetCovYears(plan, cov)))
      len <- ifelse(ignoreCovPeriod, GetValue(Rgogo::Const.MaxProjYears), ceiling(GetCovYears(plan, cov)))
      rate <- GetMortImprovRate(object, cov, plan)
      v <- (1 - rate) ^ (1:len)
      return(v)
   }
)

setMethod(
   f = "GetExpdAssump",
   signature = "MortAssump",
   definition = function(object, cov, plan, assumpInfo) {
      covYears <-ceiling(GetCovYears(plan, cov))
      assumpInfo$.q <- q <- GetBaseMortRateVector(object, cov, plan, ignoreCovPeriod = TRUE)
      assumpInfo$.xq <- xq <- GetExtraMortRateVector(object, cov, plan, ignoreCovPeriod = TRUE)
      assumpInfo$.qImprovAdj <- qImprovAdj <- GetMortImprovAdjVector(object, cov, plan, ignoreCovPeriod = TRUE)
      len <- length(q)
      xq <- FillTail(xq, filler = 0, len)
      qImprovAdj <- FillTail(qImprovAdj, filler = 1, len)
      q.Expd <- (q + xq) * qImprovAdj
      assumpInfo$q.Expd <- ifelse(q.Expd <= 1, q.Expd, 1)[1:covYears]
      return(assumpInfo)
   }
)

.Get_q_Margin <- function(object, cov, plan, assumpInfo, projStartDate) {
   # Warning: in order to run this method, GetExpdAssum method must be run first and the results are stored in assumpInfo.
   # Determine margin for adverse deviation
   margin <- GetMargin(object, cov, plan)
   if (margin != 0) {
      # Check if the base mortality table is a select and ultimate table.  If yes, get the ultimate mortality rate to calculate PfAD
      qTable <- GetMortTable(object, cov, plan)
      if (class(qTable) == "Table.SU") {
         qUlt <- .GetUltMortRateVector(qTable, cov, GetPolYear(GetIssDate(cov), projStartDate)) * GetMortTableMult(object, cov, plan) * (1 + GetMortMultExtra(cov))
      } else {
         qUlt <- assumpInfo$.q
      }
      len <- length(qUlt)
      xq <- FillTail(assumpInfo$.xq, 0, len)
      qImprovAdj <- FillTail(assumpInfo$.qImprovAdj, 1, len)
      q <- (qUlt + xq) * qImprovAdj
      q <- ifelse(q <= 1, q, 1)
      ex <- sum(cumprod(1-q))
      assumpInfo$.qUlt <- qUlt
      assumpInfo$.qUlt_plus_xq_with_qImprovAdj <- q
      assumpInfo$.ex <- ex
      assumpInfo$q.Margin <- margin / 1000 / ex
   } else {
      assumpInfo$q.Margin <- 0
   }
   return(assumpInfo)
}

setMethod(
   f = "GetPaddAssump",
   signature = "MortAssump",
   definition = function(object, cov, plan, assumpInfo, projStartDate) {
      # In order to run this methode, GetExpdAssum method must be run first and the results are stored in assumpInfo.
      assumpInfo <- .Get_q_Margin(object, cov, plan, assumpInfo, projStartDate)
      q.Padd <- assumpInfo$q.Expd + assumpInfo$q.Margin
      assumpInfo$q.Padd <- q.Padd <- ifelse(q.Padd <= 1, q.Padd, 1)
      return(assumpInfo)
   }
)

setMethod(
   f = "GetAssump",
   signature = "MortAssump",
   definition = function(object, cov, plan, assumpInfo = list(), projStartDate = NULL) {
      if (is.null(projStartDate)) {
         projStartDate <- GetIssDate(cov)
      }
      assumpInfo <- GetExpdAssump(object, cov, plan, assumpInfo)
      # assumpInfo <- GetMargin(object, cov, plan, assumpInfo, projStartDate)
      assumpInfo <- GetPaddAssump(object, cov, plan, assumpInfo, projStartDate)
      return(assumpInfo)
   }
)


.GetUltMortRateVector <- function(tbl, cov, fromPolYear) {
   attAge <- GetIssAge(cov) + fromPolYear - 1
   if (class(tbl) == "Table.AA") {
      qUlt <- LookUp(tbl, list(AttAge = (attAge:tbl@MaxAge)))
   } else if (class(tbl) == "Table.SU") {
      qUlt <- LookUp(tbl, list(IssAge = tbl@MinSelAge))
      qUlt <- qUlt[(attAge - tbl@MinSelAge + 1):length(qUlt)]
   } else if (class(tbl) == "IAPY") {
      qUlt <- LookUp(tbl, list(IssAge = GetIssAge(cov), AttAge = (attAge:tbl@MaxAge)))
   } else if (class(tbl) == "AABY") {
      birthYear <- as.character(as.numeric(format(GetIssDate(cov), "%Y")) - GetIssAge(cov))
      qUlt <- LookUp(tbl, list(BirthYear = birthYear, AttAge = (attAge:tbl@MaxAge)))
   }
   return(qUlt)
}

GetLifeProb <- function(q_x) {
   p_x <- 1 - q_x
   t_p_x <- cumprod(p_x)
   df <- data.frame(
      q_x = q_x,
      p_x = p_x,
      t_p_x = t_p_x,
      stringsAsFactors = FALSE
   )
   return(df)
}


