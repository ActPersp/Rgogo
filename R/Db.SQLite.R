setMethod(
   f = "ReadTable.Cov",
   signature = "SQLiteConnection",
   definition = function(conn, ...) {
      df <- ReadTable(conn, "Cov", ...)
      for (colName in colnames(df)) {
         if(endsWith(colName, "Date")) {
            col <- df[, colName]
            eval(expr = parse(text = paste0("df$", colName, " <- as.Date('1970-01-01') + lubridate::dseconds(df$", colName, ")")))
         }
      }
      return(df)
   }
)

setMethod(
   f = "CompactDb",
   signature = "SQLiteConnection",
   definition = function(conn) {
      DBI::dbExecute(conn, "VACUUM")
   }
)

