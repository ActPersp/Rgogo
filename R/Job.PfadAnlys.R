setClass(
   Class = "Job.PfadAnlys",
   contains = "IJob"
)

Job.PfadAnlys <- function(inpVars, dispatcher, dbDrvr = NULL, dbConnArgs = character(0L), id, descrip = character(0L)) {
   job <- new(
      Class = "Job.PfadAnlys",
      InpVars = inpVars,
      Dispatcher = dispatcher,
      DbDriver = dbDrvr,
      DbConnArgs = dbConnArgs,
      Descrip = as.character(descrip)
   )
   SetJobId(job) <- as.character(id)
   return(job)
}

setMethod(
   f = "Initialize",
   signature = "Job.PfadAnlys",
   definition = function(object) {
      conn <- ConnectDb(object)
      if (!is.null(conn)) {
         # DeleteRows(conn, "Pfad", paste0("JobId = '", GetId(object), "'"))
         ClearJobOutput(GetId(object), conn, c("Pfad"))
         DisconnectDb(conn)
      }
      return(object)
   }
)

setMethod(
   f = "Finalize",
   signature = "Job.PfadAnlys",
   definition = function(object, result) {
      pfad <- To.data.frame(result[sapply(result, function(x){!is.null(x)})], "Pfad")
      if (dim(pfad)[1] > 0) {
         pfad <- cbind(JobId = GetId(object), pfad)
      } else {
         pfad <- NULL
      }
      conn <- ConnectDb(object)
      if (!is.null(conn) & !is.null(pfad)) {
         WriteTable.Pfad(conn, pfad)
         CompactDb(conn)
         DisconnectDb(conn)
      }
      return(list(Pfad = pfad))
   }
)


