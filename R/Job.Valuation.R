setClass(
   Class = "Job.Valuation",
   contains = "IJob",
   slots = c(
      DbAppend = "logical",
      DbSaveCf = "logical"
   )
)

Job.Valuation <- function(inpVars, dispatcher, dbDrvr, dbConnArgs, dbSaveCf = TRUE, dbAppend = FALSE, id, descrip = character(0L), ...) {
   job <- new(
      Class = "Job.Valuation",
      InpVars = inpVars,
      Dispatcher = dispatcher,
      DbDriver = dbDrvr,
      DbConnArgs = dbConnArgs,
      DbAppend = dbAppend,
      DbSaveCf = dbSaveCf,
      Descrip = as.character(descrip)
   )
   SetJobId(job) <- as.character(id)
   return(job)
}

setValidity(
   Class = "Job.Valuation",
   method = function(object) {
      err <- New.SysMessage()
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
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
      cf <- To.data.frame(result, "Cf")
      if (dim(cf)[1] > 0) {
         cf <- cbind(JobId = jobId, cf)
      } else {
         cf <- NULL
      }
      # Output job results
      conn <- ConnectDb(object)
      if (!is.null(conn)) {
         WriteTable.ValuSumm(conn, valuSumm)
         if (!is.null(cf) & object@DbSaveCf) {
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




