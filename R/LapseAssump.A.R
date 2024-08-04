setClass(
   Class = "LapseAssump.A",
   contains = "ILapseAssump",
   slots = c(
      LapseTable = "character",
      LapseTableMult = "numeric",
      ExtraLapseTable = "character",
      ExtraLapseTableMult = "numeric",
      Margin = "numeric",
      NoLapseAfterPaidUp = "logical"
   )
)

setValidity(
   Class = "LapseAssump.A",
   method = function(object) {
      err <- New.SysMessage()
      # Validate @LapseTable
      v <- Validator.Names(hasNames = (length(object@LapseTable) > 1))
      if (Validate(v, object@LapseTable) != TRUE) {
         AddMessage(err) <- "Value of slot @LapseTable in class LapseAssump is invalid."
      }
      # Validate @LapseTableMult
      v <- ValidatorGroup(
         Validator.Names(hasNames = (length(object@LapseTableMult) > 1)),
         Validator.Range(minValue = 0)
      )
      if (Validate(v, object@LapseTableMult) != TRUE) {
         AddMessage(err) <- "Value of slot @LapseTableMult in class LapseAssump is invalid."
      }
      # Validate @ExtraLapseTable
      v <- Validator.Names(hasNames = (length(object@ExtraLapseTable) > 1))
      if (Validate(v, object@ExtraLapseTable) != TRUE) {
         AddMessage(err) <- "Value of slot @ExtraLapseTable in class LapseAssump is invalid."
      }
      # Validate @ExtraLapseTableMult
      v <- ValidatorGroup(
         Validator.Names(hasNames = (length(object@ExtraLapseTableMult) > 1)),
         Validator.Range(minValue = 0)
      )
      if (Validate(v, object@ExtraLapseTableMult) != TRUE) {
         AddMessage(err) <- "Value of slot @ExtraLapseTableMult in class LapseAssump is invalid."
      }
      # Validate @Margin
      v <- Validator.Names(hasNames = (length(object@Margin) > 1))
      if (Validate(v, object@Margin) != TRUE) {
         AddMessage(err) <- "Value of slot @Margin in class LapseAssump is invalid."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)


# Consturctor
LapseAssump.A <- function(
      lapseTable = character(0L),
      lapseTableMult = 1,
      extraLapseTable = character(0L),
      extraLapseTableMult = 1,
      margin = 0,
      noLapseAfterPaidUp = TRUE,
      id = character(0L),
      descrip = character(0L)
   ) {
   assump <- new(
      Class = "LapseAssump.A",
      LapseTable = lapseTable,
      LapseTableMult = lapseTableMult,
      ExtraLapseTable = extraLapseTable,
      ExtraLapseTableMult = extraLapseTableMult,
      NoLapseAfterPaidUp = noLapseAfterPaidUp,
      Margin = margin,
      Descrip = as.character(descrip)
   )
   SetAssumpId(assump) <- as.character(id)
   return(assump)
}



setMethod(
   f = "GetLapseTable",
   signature = "LapseAssump.A",
   definition = function(object, cov = NULL, plan = NULL) {
      if(is.null(cov) & is.null(plan)) {
         return(object@LapseTable)
      }
      if (length(object@LapseTable) > 0) {
         if (length(object@LapseTable) == 1) {
            tblId <- object@LapseTable
         } else {
            riskClass <- GetRiskClass(object, cov, plan)
            tblId <- object@LapseTable[riskClass]
         }
         #tblId <- ifelse(startsWith(tblId, "Lapse."), tblId, paste0("Lapse.", tblId))
         return(eval(expr = parse(text = tblId)))
      } else {
         return(NULL)
      }
   }
)

setMethod(
   f = "SetLapseTable<-",
   signature = "LapseAssump.A",
   definition = function(object, value) {
      object@LapseTable <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetLapseTableMult",
   signature = "LapseAssump.A",
   definition = function(object, cov = NULL, plan = NULL) {
      if(is.null(cov) & is.null(plan)) {
         return(object@LapseTableMult)
      }
      if (length(object@LapseTableMult) == 0) {
         return(1)
      } else if (length(object@LapseTableMult) == 1) {
         return(object@LapseTableMult)
      } else {
         return(object@LapseTableMult[GetRiskClass(object, cov, plan)])
      }
   }
)

setMethod(
   f = "SetLapseTableMult<-",
   signature = "LapseAssump.A",
   definition = function(object, value) {
      object@LapseTableMult <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetExtraLapseTable",
   signature = "LapseAssump.A",
   definition = function(object, cov = NULL, plan = NULL) {
      if(is.null(cov) & is.null(plan)) {
         return(object@ExtraLapseTable)
      }
      if (length(object@ExtraLapseTable) > 0) {
         if (length(object@ExtraLapseTable) == 1) {
            tblId <- object@ExtraLapseTable
         } else {
            riskClass <- GetRiskClass(object, cov, plan)
            tblId <- object@ExtraLapseTable[riskClass]
         }
         return(eval(expr = parse(text = tblId)))
      } else {
         return(NULL)
      }
   }
)

setMethod(
   f = "SetExtraLapseTable<-",
   signature = "LapseAssump.A",
   definition = function(object, value) {
      object@ExtraLapseTable <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetExtraLapseTableMult",
   signature = "LapseAssump.A",
   definition = function(object, cov = NULL, plan = NULL) {
      if(is.null(cov) & is.null(plan)) {
         return(object@ExtraLapseTableMult)
      }
      if (length(object@ExtraLapseTableMult) == 0) {
         return(1)
      } else if (length(object@ExtraLapseTableMult) == 1) {
         return(object@ExtraLapseTableMult)
      } else {
         return(object@ExtraLapseTableMult[GetRiskClass(object, cov, plan)])
      }
   }
)

setMethod(
   f = "SetExtraLapseTableMult<-",
   signature = "LapseAssump.A",
   definition = function(object, value) {
      object@ExtraLapseTableMult <- value
      validObject(object)
      return(object)
   }
)


setMethod(
   f = "GetMargin",
   signature = "LapseAssump.A",
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
   signature = "LapseAssump.A",
   definition = function(object, value) {
      object@Margin <- value
      validObject(object)
      return(object)
   }
)


setMethod(
   f = "NoLapseAfterPaidUp",
   signature = "LapseAssump.A",
   definition = function(object) {
      return(object@NoLapseAfterPaidUp)
   }
)

setMethod(
   f = "NoLapseAfterPaidUp<-",
   signature = "LapseAssump.A",
   definition = function(object, value) {
      object@NoLapseAfterPaidUp <- value
      return(object)
   }
)

setMethod(
   f = "GetRiskClass",
   signature = "LapseAssump.A",
   definition = function(object, cov, plan) {
      return(GetRiskClass(plan, cov))
   }
)

setMethod(
   f = "GetBaseLapseRateVector",
   signature = "LapseAssump.A",
   definition = function(object, cov, plan, ignoreCovPeriod = FALSE) {
      qTable <- GetLapseTable(object, cov, plan)
      covYears <- ceiling(GetCovYears(plan, cov))
      if (!is.null(qTable)) {
         qTableRate <- LookUp(qTable, cov, ifelse(ignoreCovPeriod, NA_integer_, covYears))
      } else {
         # qTableRate <- rep(0, length.out = ifelse(ignoreCovPeriod, Global.MaxProjYears(), covYears))
         qTableRate <- rep(0, length.out = ifelse(ignoreCovPeriod, GetValue(Rgogo::Const.MaxProjYears), covYears))
      }
      qMult <- GetLapseTableMult(object, cov, plan)
      return(qTableRate * qMult)
   }
)

setMethod(
   f = "GetExtraLapseRateVector",
   signature = "LapseAssump.A",
   definition = function(object, cov, plan, ignoreCovPeriod = FALSE) {
      # Extra Lapseality to be added to base Lapseality rates
      xqTable <- GetExtraLapseTable(object, cov, plan)
      if (!is.null(xqTable)) {
         len <- ifelse(ignoreCovPeriod, NA_integer_, ceiling(GetCovYears(plan, cov)))
         xqTableRate <- LookUp(xqTable, cov, len)
      } else {
         xqTableRate <- 0
      }
      xqMult <- GetExtraLapseTableMult(object, cov, plan)
      return(xqTableRate * xqMult)
   }
)

setMethod(
   f = "GetExpdAssump",
   signature = "LapseAssump.A",
   definition = function(object, cov, plan, assumpInfo) {
      covYears <-ceiling(GetCovYears(plan, cov))
      assumpInfo$.w <- q <- GetBaseLapseRateVector(object, cov, plan, ignoreCovPeriod = TRUE)
      assumpInfo$.xw <- xq <- GetExtraLapseRateVector(object, cov, plan, ignoreCovPeriod = TRUE)
      len <- length(q)
      xq <- FillTail(xq, filler = 0, len)
      q.Expd <- q + xq
      assumpInfo$w.Expd <- ifelse(q.Expd <= 1, q.Expd, 1)[1:covYears]
      return(assumpInfo)
   }
)

setMethod(
   f = "GetPaddAssump",
   signature = "LapseAssump.A",
   definition = function(object, cov, plan, assumpInfo, projStartDate) {
      assumpInfo$w.Margin <- assumpInfo$w.Expd * GetMargin(object, cov, plan)
      q.Padd <- assumpInfo$w.Expd + assumpInfo$w.Margin
      assumpInfo$w.Padd <- q.Padd <- ifelse(q.Padd <= 1, q.Padd, 1)
      return(assumpInfo)
   }
)

setMethod(
   f = "GetAssump",
   signature = "LapseAssump.A",
   definition = function(object, cov, plan, assumpInfo = list(), projStartDate = NULL) {
      if (is.null(projStartDate)) {
         projStartDate <- GetIssDate(cov)
      }
      assumpInfo <- GetExpdAssump(object, cov, plan, assumpInfo)
      assumpInfo <- GetPaddAssump(object, cov, plan, assumpInfo, projStartDate)
      return(assumpInfo)
   }
)



