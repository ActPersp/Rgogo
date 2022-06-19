setMethod(
   f = "CompactDb",
   signature = "SQLiteConnection",
   definition = function(conn) {
      DBI::dbExecute(conn, "VACUUM")
   }
)


setMethod(
   f = "ReadTable",
   signature = c("SQLiteConnection", "character"),
   definition = function(conn, tableName, ...) {
      df <- callNextMethod()
      dateColNames <- colnames(df)[endsWith(colnames(df), "Date")]
      for (i in seq_along(dateColNames)) {
         eval(expr = parse(text = paste0("df$", dateColNames[i], "<- as.Date(df$", dateColNames[i], ")")))
      }
      return(df)
   }
)


setMethod(
   f = "WriteTable",
   signature = c("SQLiteConnection", "character", "ANY"),
   definition = function(conn, tableName, data, append = TRUE, colExpandable = FALSE) {

      dateColNames <- colnames(data)[endsWith(colnames(data), "Date")]
      for (i in seq_along(dateColNames)) {
         eval(expr = parse(text = paste0("data$", dateColNames[i], "<- as.character(as.Date(data$", dateColNames[i], "))")))
      }
      callNextMethod()
   }
)


setMethod(
   f = "CreateTable.Cov",
   signature = "SQLiteConnection",
   definition = function(conn) {
      DBI::dbWithTransaction(
         conn,
         {
            CreateTable(
               conn,
               tableName = "Cov",
               colSpec <- c(
                  Id = DBI::dbDataType(conn, character()),
                  PlanId = DBI::dbDataType(conn, character()),
                  IssDate = DBI::dbDataType(conn, character()),
                  IssAge = DBI::dbDataType(conn, integer()),
                  RiskClass = DBI::dbDataType(conn, character()),
                  FaceAmt = DBI::dbDataType(conn, numeric()),
                  PremMode = DBI::dbDataType(conn, integer()),
                  ModPrem = DBI::dbDataType(conn, numeric())
               ),
               primaryKey = c("Id")
            )
            CreateIndex(conn, tableName = "Cov", indexCol = "PlanId")
         }
      )
   }
)

