setClass(
   Class = "Job.Valuation",
   contains = "IJob",
   slots = c(
      DbAppend = "logical",
      CfExportYears = "numeric",
      ProjExportYears = "numeric"
   )
)

Job.Valuation <- function(inpVars, dispatcher, dbDrvr = NULL, dbConnArgs = character(0L), cfExportYears = NA_integer_, projExportYears = NA_integer_, dbAppend = FALSE, id, descrip = character(0L), ...) {
   job <- new(
      Class = "Job.Valuation",
      InpVars = inpVars,
      Dispatcher = dispatcher,
      DbDriver = dbDrvr,
      DbConnArgs = dbConnArgs,
      DbAppend = dbAppend,
      CfExportYears = cfExportYears,
      ProjExportYears = projExportYears,
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
      cf <- To.data.frame(result, "Cf")


      # Output coverage illustration.  This section needs to be reviewed
      projLen <- round(object@ProjExportYears * 12, digits = 0)
      if (!is.na(projLen)) {
         result <- lapply(
            result,
            function(rslt) {
               if (!is.null(rslt$Proj)) {
                  projStartDate <- GetArgValue(rslt$ArgSet, "ValuDate") + 1
                  projDates <- projStartDate %m+% months(0:(projLen - 1))
                  projTimeline <- paste0(lubridate::year(projDates), "-", sprintf(fmt = "%02d", lubridate::month(projDates)))
                  inTimeline <- rslt$Proj$Timeline %in% projTimeline
                  rslt$Proj <- lapply(
                     rslt$Proj,
                     function(proj) {
                        proj <- proj[inTimeline]
                        return(proj)
                     }
                  )
                  rslt$Proj <- dplyr::bind_cols(
                     CovId = GetId(rslt$CovData),
                     data.frame(rslt$Proj)
                  )
               }
               return(rslt)
            }
         )
         s <- paste(paste0("result[[", 1:length(result), "]]$Proj"),  collapse = ",")
         eval(expr = parse(text = paste0("proj <- dplyr::bind_rows(", s, ")")))
      }

      # Output job results
      conn <- ConnectDb(object)
      if (!is.null(conn)) {
         WriteTable.ValuSumm(conn, valuSumm)
         if (dim(cf)[1] > 0) {
            cf <- cbind(JobId = jobId, cf)
            WriteTable.Cf(conn, cf)
         }
         CompactDb(conn)
         DisconnectDb(conn)
      }
      # return(list(ValuSumm = valuSumm, Cf = cf))
      return(list(ValuSumm = valuSumm, Cf = cf, Proj = proj))
   }
)

ExportToExcel.Job.Valuation <- function(result, dir, annualized = TRUE, digits = 0, overwrite = FALSE) {
   lst <- lapply(result,
                 function(rslt, d, anlz, dgt, ow) {ExportToExcel.Model.PPM(rslt, d, anlz, dgt, ow)},
                 dir, annualized, digits, overwrite)
   n <- sum(unlist(lst))
   return(cat("Number of Excel files created:", n))
}




