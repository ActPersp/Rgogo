setClass(Class = "Table.IA",
         contains = "ITable",
         slots = c(MinAge = "integer",
                   MaxAge = "integer",
                   TValue = "matrix"
         )
)

setValidity(
   Class = "Table.IA",
   method = function(object) {
      err <- New.SysMessage()
      # Validate @MinAge
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1, maxLen = 1),
            Validator.Range(minValue = 0)
         ),
         object@MinAge
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "@MinAge: Minimum age must be an integer and cannot be negative (@MinAge)."
      }
      # Validate @MaxAge
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1, maxLen = 1),
            Validator.Range(minValue = object@MinAge)
         ),
         object@MaxAge
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "@MaxAge: Mmaximum age must be an integer and cannot be less than the minimum age (@MaxAge)."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

Table.IA <- function(minAge, maxAge, tBase, tValue = NA,
                     source = character(0L), createdBy = character(0L),
                     id = character(0L), descrip = character(0L)) {
   tbl <- new(Class = "Table.IA")
   tbl@TValue <- matrix(data = tValue,
                        nrow = maxAge - minAge + 1,
                        ncol = 1,
                        dimnames = list(as.character(minAge:maxAge), NULL)
   )
   tbl@MinAge <- as.integer(minAge)
   tbl@MaxAge <- as.integer(maxAge)
   tbl@TBase <- as.numeric(tBase)
   tbl@Source <- as.character(source)
   tbl@CreatedBy <- as.character(createdBy)
   tbl@CreatedAt <- Sys.time()
   tbl@Id <- as.character(id)
   tbl@Descrip <- as.character(descrip)
   validObject(tbl)
   return(tbl)
}

setMethod(
   f = "GetMinAge",
   signature = "Table.IA",
   definition = function(object) {
      return(object@MinAge)
   }
)

setMethod(
   f = "GetMaxAge",
   signature = "Table.IA",
   definition = function(object) {
      return(object@MaxAge)
   }
)

setMethod(
   f = "LookUp",
   signature (tbl = "Table.IA", lookUpKey = "list"),
   definition = function(tbl, lookUpKey) {
      stopifnot(HasValue(lookUpKey$IssAge))
      issAge <- as.character(lookUpKey$IssAge)
      stopifnot(all(issAge %in% dimnames(tbl@TValue)[[1]]))
      v <- as.vector(tbl@TValue[issAge,1]) / tbl@TBase
      names(v) <- NULL
      return(v)
   }
)

setMethod(
   f = "LookUp",
   signature (tbl = "Table.IA", lookUpKey = "Cov"),
   definition = function(tbl, lookUpKey, len = NA_integer_) {
      issAge <- as.character(GetIssAge(lookUpKey))
      stopifnot(all(issAge %in% dimnames(tbl@TValue)[[1]]))
      v <- as.vector(tbl@TValue[issAge,1]) / tbl@TBase
      if(!is.na(len)) {
         v <- rep(v, length.out = len)
      }
      names(v) <- NULL
      return(v)
   }
)

setMethod(
   f = ".ExportToExcel.TValue",
   signature = "Table.IA",
   definition = function(object, wb, sheet, startRow, startCol, colWidth) {
      tbl <- data.frame(
         IssueAge = rownames(object@TValue),
         Value = object@TValue[,1],
         stringsAsFactors = FALSE
      )
      openxlsx::writeDataTable(wb = wb, sheet = sheet, startRow = startRow, startCol = startCol, x = tbl)
      openxlsx::setColWidths(wb, sheet, cols = 1:2, widths = colWidth)
      return(list(Workbook = wb, RowCount = dim(tbl)[1] + 1, ColCount = dim(tbl)[2]))
   }
)




