setClass(
   Class = "Table.PY",
   contains = "ITable",
   slots = c(MaxPolYear = "integer",
             TValue = "matrix"
   )
)

setValidity(
   Class = "Table.PY",
   method = function(object) {
      err <- New.SysMessage()
      # Validate @MaxPolYear
      isValid <- Validate(
         ValidatorGroup(
            Validator.Length(minLen = 1, maxLen = 1),
            Validator.Range(minValue = 1)
         ),
         object@MaxPolYear
      )
      if (isValid != TRUE) {
         AddMessage(err) <- "@MaxPolYear: Invalid maximum policy year.  It must be an integer value and cannot be less than 1."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

Table.PY <- function(maxPolYear, tBase, tValue = NA,
                     source = character(0L), createdBy = character(0L),
                     id = character(0L), descrip = character(0L)) {
   tbl <- new(Class = "Table.PY")
   tbl@TValue <- matrix(data = tValue,
                        nrow = maxPolYear,
                        ncol = 1,
                        dimnames = list(as.character(1:maxPolYear), NULL)
   )
   tbl@MaxPolYear <- as.integer(maxPolYear)
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
   f = "GetMaxPolYear",
   signature = "Table.PY",
   definition = function(object) {
      return(object@MaxPolYear)
   }
)

setMethod(
   f = "LookUp",
   signature (tbl = "Table.PY", lookUpKey = "list"),
   definition = function(tbl, lookUpKey) {
      stopifnot(HasValue(lookUpKey$PolYear))
      polYear <- as.character(lookUpKey$PolYear)
      stopifnot(all(polYear %in% dimnames(tbl@TValue)[[1]]))
      v <- as.vector(tbl@TValue[polYear,1]) / tbl@TBase
      names(v) <- NULL
      return(v)
   }
)

setMethod(
   f = "LookUp",
   signature (tbl = "Table.PY", lookUpKey = "Cov"),
   definition = function(tbl, lookUpKey, len = NA_integer_) {
      if (is.na(len)) {
         polYear <- as.character(1:tbl@MaxPolYear)
      } else {
         polYear <- as.character(1:len)
      }
      stopifnot(all(polYear %in% dimnames(tbl@TValue)[[1]]))
      v <- as.vector(tbl@TValue[polYear,1]) / tbl@TBase
      names(v) <- NULL
      return(v)
   }
)

setMethod(
   f = ".ExportToExcel.TValue",
   signature = "Table.PY",
   definition = function(object, wb, sheet, startRow, startCol, colWidth) {
      tbl <- data.frame(
         PolicyYear = rownames(object@TValue),
         Value = object@TValue[,1],
         stringsAsFactors = FALSE
      )
      openxlsx::writeDataTable(wb = wb, sheet = sheet, startRow = startRow, startCol = startCol, x = tbl)
      openxlsx::setColWidths(wb, sheet, cols = 1:2, widths = colWidth)
      return(list(Workbook = wb, RowCount = dim(tbl)[1] + 1, ColCount = dim(tbl)[2]))
   }
)




