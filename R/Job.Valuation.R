setClass(
   Class = "Job.Valuation",
   contains = "IJob",
   slots = c(
      DbAppend = "logical",
      CfExportYears = "numeric"
   )
)

Job.Valuation <- function(inpVars, dispatcher, dbDrvr, dbConnArgs, cfExportYears = NA_integer_, dbAppend = FALSE, id, descrip = character(0L), ...) {
   job <- new(
      Class = "Job.Valuation",
      InpVars = inpVars,
      Dispatcher = dispatcher,
      DbDriver = dbDrvr,
      DbConnArgs = dbConnArgs,
      DbAppend = dbAppend,
      CfExportYears = cfExportYears,
      Descrip = as.character(descrip)
   )
   SetJobId(job) <- as.character(id)
   return(job)
}

setValidity(
   Class = "Job.Valuation",
   method = function(object) {
      err <- New.SysMessage()
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1, maxLen = 1),
            Validator.Range(minValue = 0, maxValue = 100, allowNA = TRUE)
         ),
         object@CfExportYears
      )
      if(isValid != TRUE) {
         AddMessage(err) <- "Invalid cash flow export years.  It must be an integer scalar between 0 and 100."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

setMethod(
   f = "GetCfExportYears",
   signature = "Job.Valuation",
   definition = function(object) {
      return(object@CfExportYears)
   }
)

setMethod(
   f = "SetCfExportYears<-",
   signature = "Job.Valuation",
   definition = function(object, value) {
      object@CfExportYears <- as.integer(value)
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "Initialize",
   signature = "Job.Valuation",
   definition = function(object) {
      conn <- ConnectDb(object)
      if (!is.null(conn)) {
         if (object@DbAppend == FALSE) {
            ClearJobOutput(GetId(object), conn, c("ValuSumm", "Cf"))
         }
         DisconnectDb(conn)
      }
      return(object)
   }
)

setMethod(
   f = "Finalize",
   signature = "Job.Valuation",
   definition = function(object, result) {
      jobId <- GetJobId(object)
      valuSumm <- cbind(JobId = jobId, To.data.frame(result, "ValuSumm"))
      cfLen <- round(GetCfExportYears(object) * 12, digits = 0)
      if (!is.na(cfLen)) {
         result <- lapply(
            result,
            function(rslt) {
               if (!is.null(rslt$Cf)) {
                  rslt$Cf <- lapply(
                     rslt$Cf,
                     function(cf) {
                        length(cf) <- min(cfLen, length(cf))
                        return(cf)
                     }
                  )
               }
               return(rslt)
            }
         )
      }
      # Output job results
      conn <- ConnectDb(object)
      if (!is.null(conn)) {
         WriteTable.ValuSumm(conn, valuSumm)
         cf <- To.data.frame(result, "Cf")
         if (dim(cf)[1] > 0) {
            cf <- cbind(JobId = jobId, cf)
            WriteTable.Cf(conn, cf)
         }
         CompactDb(conn)
         DisconnectDb(conn)
      }
      return(list(ValuSumm = valuSumm, Cf = cf))
   }
)

ExportToExcel.Job.Valuation <- function(result, dir, annualized = TRUE, digits = 0, overwrite = FALSE) {
   lst <- lapply(result,
                 function(rslt, d, anlz, dgt, ow) {ExportToExcel.Model.PPM(rslt, d, anlz, dgt, ow)},
                 dir, annualized, digits, overwrite)
   n <- sum(unlist(lst))
   return(cat("Number of Excel files created:", n))
}




