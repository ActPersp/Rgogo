setMethod(
   f = "CreateDb",
   signature = c("PqDriver", "character"),
   definition = function(object, args) {
      args <- gsub(" ", "", unlist(strsplit(args,",")))
      dbName <- eval(expr = parse(text = paste0("c(", args[startsWith(args, "dbname")], ")")))
      args <- paste(args[!startsWith(args, "dbname")], collapse = ", ")
      conn <- eval(expr = parse(text = paste0("DBI::dbConnect(object, ", args, ")")))
      DBI::dbExecute(conn, statement = paste0('CREATE DATABASE "', dbName, '"'))
      DisconnectDb(conn)
      return(paste("Database created:", dbName))
   }
)

setMethod(
   f = "CreateTable",
   signature = c("PqConnection", "character", "character"),
   definition = function(conn, tableName, dbColSpec, primaryKey = character(0L)) {
      dbColSpec <- paste0('"', names(dbColSpec), '"', dbColSpec)
      sql <- paste0('CREATE TABLE "', tableName, '" (', paste(dbColSpec, collapse = ', '))
      if (length(primaryKey) > 0) {
         sql <- paste(sql, ", PRIMARY KEY (", paste0('"', primaryKey, '"', collapse = ", "), ")")
      }
      sql <- paste(sql, ");")
      DBI::dbExecute(conn, sql)
   }
)

setMethod(
   f = "CreateIndex",
   signature = c("PqConnection", "character", "character"),
   definition = function(conn, tableName, indexCol) {
      idxName <- paste0(c("idx", tableName, indexCol), collapse = "_")
      sql <- paste0('CREATE INDEX ', idxName, ' ON "', tableName, '" (', paste0('"', indexCol, '"', collapse = ', ', ');'))
      DBI::dbExecute(conn, sql)
   }
)

setMethod(
   f = "WriteTable",
   signature = c("PqConnection", "character", "ANY"),
   definition = function(conn, tableName, data, append = TRUE, colExpandable = FALSE) {
      DBI::dbWithTransaction(
         conn,
         {
            if (!append) {
               sql <- paste0('DELETE FROM "', tableName, '";')
               DBI::dbExecute(conn, sql)
            }
            dfData <- as.data.frame(data)
            dataColNames <- colnames(dfData)
            newColNames <- dataColNames[!dataColNames %in% DBI::dbListFields(conn, tableName)]
            if (colExpandable & length(newColNames) > 0) {
               for (colName in newColNames) {
                  dataType <- eval(expr = parse(text = paste0("DBI::dbDataType(conn, dfData$", colName, ")")))
                  sql <- paste0('ALTER TABLE "', tableName, '" ADD COLUMN "', colName, '" ', dataType)
                  DBI::dbExecute(conn, sql)
               }
            }
            DBI::dbWriteTable(conn, tableName, data, append = TRUE)
         }
      )
   }
)

setMethod(
   f = "ReadTable",
   signature = c("PqConnection", "character"),
   definition = function(conn, tableName, ...) {
      selCond <- list(...)
      sql <- paste0('SELECT * FROM "', tableName, '"')
      if (length(selCond) > 0) {
         whereClause <- paste0(unlist(selCond), collapse = " AND ")
         sql <- paste0(sql, " WHERE ", whereClause)
      }
      sql <- paste0(sql, ";")
      df <- DBI::dbGetQuery(conn, sql)
      return(df)
   }
)

setMethod(
   f = "DeleteRows",
   signature = c("PqConnection", "character", "character"),
   definition = function(conn, tableName, where = character(0L)) {
      sql <- paste0('DELETE FROM "', tableName, '"')
      if (length(where) > 0) {
         sql <- paste(sql, "WHERE", where, ";")
      }
      DBI::dbExecute(conn, sql)
   }
)

setMethod(
   f = "CompactDb",
   signature = "PqConnection",
   definition = function(conn) {
      DBI::dbExecute(conn, "VACUUM FULL")
   }
)


