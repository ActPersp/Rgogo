RepeatTail <- function(v, len) {
   if (length(v) < len) {
      v <- c(v, rep(v[length(v)], length.out = len - length(v)))
   } else {
      length(v) <- len
   }
   return(v)
}

HasValue <- function(x){
   return(!(is.null(x) || all(is.na(x)) || (length(x) == 0)))
}

IsEmptyString <- function(x) {
   return(!HasValue(x) || nchar(x)==0)
}

FillZeroIfNA <- function(v = "vector", len = NA){
   if (!is.na(len)) length(v) <- len
   v <- replace(v, is.na(v), 0)
   return(v)
}

FillTail <- function(v, filler, len){
   if (length(v) < len) {
      v <- c(v, rep(filler, length.out = len - length(v)))
   } else {
      length(v) <- len
   }
   return(v)
}

# GetMonthDiff <- function(date1, date2){
#    date1 <- as.Date(date1)
#    y1 <- as.integer(format(date1,'%Y'))
#    m1 <- as.integer(format(date1,'%m'))
#    d1 <- as.integer(format(date1,'%d'))
#    date2 <- as.Date(date2)
#    y2 <- as.integer(format(date2,'%Y'))
#    m2 <- as.integer(format(date2,'%m'))
#    d2 <- as.integer(format(date2,'%d'))
#    diff <- (y2 - y1) * 12 + (m2 - m1) + (d2 - d1) / 31
#    return(diff)
# }

Convert_qx_ud <- function(qx, m) {
   # Uniform distribution assumption
   years <- length(qx)
   k <- matrix(data = rep(0:(m-1), times = years), ncol = m, byrow = TRUE)
   qm <- as.vector(t((qx / m) / (1 - k * qx / m)))
   return(qm)
}

Convert_qx_cf <- function(qx, m) {
   # Constant force of mortality assumption
   years <- length(qx)
   k <- matrix(data = rep(1, times = years * m), ncol = m, byrow = TRUE)
   qm <- as.vector(t(1 - (1 - k * qx) ^ (1 / m)))
   return(qm)
}

Convert_qx_ba <- function(qx, m) {
   # Balducci assumption
   years <- length(qx)
   k <- matrix(data = rep(0:(m-1), times = years), ncol = m, byrow = TRUE)
   qm <- as.vector(t((qx / m) / (1 - (m - 1 - k) / m * qx)))
   return(qm)
}

Convert_qx <- function(qx, m, method) {
   stopifnot(m >= 1)
   if (m == 1) {
      return(qx)
   } else {
      qm <- switch(method,
                   "ud" = Convert_qx_ud(qx, m),
                   "cf" = Convert_qx_cf(qx, m),
                   "ba" = Convert_qx_ba(qx, m),
                   stop("Method must be one of the following: 'ud', 'cf', 'ba'")
      )
      return(qm)
   }
}


TidyUpList <- function(lst) {
   stopifnot(is.list(lst))
   toBeDeleted <- names(lst)[startsWith(names(lst), ".")]
   for (item in toBeDeleted) {
      eval(expr = parse(text = paste0("lst$", item, " <- NULL")))
   }
   return(lst)
}

Is.WholeNumber <- function(x) {
   return(as.integer(x) == x & x >= 0)
}

.GetTimeStampString <- function(tm = Sys.time()) {
   s <- paste0(
      sprintf("%02d", lubridate::year(tm)%/% 100),
      sprintf("%02d", lubridate::month(tm)),
      sprintf("%02d", lubridate::day(tm)),
      sprintf("%02d", lubridate::hour(tm)),
      sprintf("%02d", lubridate::minute(tm)),
      sprintf("%02d", as.integer(lubridate::second(tm)))
   )
   return(s)
}

GetPolMonth <- function(issDate, curDate, exact = FALSE, base = 1) {
   stopifnot(base == 1 | base == 0)
   pm <- lubridate::interval(issDate, curDate) / months(1) + base
   if (exact) {
      return(pm)
   } else {
      return(floor(pm))
   }
}

GetPolYear <- function(issDate, curDate, exact = FALSE, base = 1) {
   stopifnot(base == 1 | base == 0)
   py <- lubridate::interval(issDate, curDate) / lubridate::years(1) + base
   if (exact) {
      return(py)
   } else {
      return(floor(py))
   }
}

GetMonthversaryDates <- function(baseDate, monthlyPeriods) {
   return(baseDate %m+% months(monthlyPeriods))
}

GetAnniversaryDates <- function(baseDate, yearlyPeriods) {
   return(baseDate %m+% lubridate::years(yearlyPeriods))
}

Round.data.frame <- function(df, digits) {
   if (!is.data.frame(df)) {
      stop("df must an object of class data.frame.")
   }
   dfOutput <- as.data.frame(
      lapply(
         X = df,
         FUN = function(v, d) {
            if (is.numeric(v)) {
               return(round(v, digits = d))
            } else {
               return(v)
            }
         },
         digits
      )
   )
   return(dfOutput)
}

GetYearlyTotal <- function(monthlyAmounts) {
   l <- length(monthlyAmounts)
   len = ceiling(l / 12)
   v <- as.vector(matrix(c(monthlyAmounts,rep(0, len * 12 - l)), nrow = len, ncol = 12, byrow = TRUE) %*% matrix(1, nrow = 12, ncol = 1))
   return(v)
}

GetYearStartValue <- function(monthlyValue) {
   v <- monthlyValue[(1:length(monthlyValue)) %% 12 == 1]
   return(v)
}

GetYearEndValue <- function(monthlyValue) {
   v <- monthlyValue[(1:length(monthlyValue)) %% 12 == 0]
   return(v)
}

Shift <- function(v, positions, filler) {
   # Shift the elements of a vector by specified number of positions (positons).
   # Shift right if positions > 0; shift left if positions < 0.
   # Newly created positions are filled with the specified value (filler).
   if (positions > 0) {
      return (c(rep(filler, length.out = positions),v)[1:length(v)])
   } else {
      return (c(v, rep(filler, length.out = -positions))[(-positions + 1):(length(v) - positions)])
   }
}

ShiftRight <- function(v, positions, filler) {
   return(Shift(v, positions, filler))
}

ShiftLeft <- function(v, positions, filler) {
   return(Shift(v, -positions, filler))
}

LoanAmort <- function(loanAmt, amortMonths, intrRate, intrRateType) {
   lstResult <- list()
   if (as.numeric(intrRateType) == 1){     # Stated rate
      ir <- intrRate / 12
   } else {
      if (as.numeric(intrRateType) == 2) {     # Effective annual rate
         ir <- (1 + intrRate)^(1/12) - 1
      } else {
         stop("Unknown interest rate type.")
      }
   }
   i <- rep(ir, length.out = amortMonths)
   v <- cumprod((1 + i) ^ (-1))
   payment <- rep(loanAmt / sum(v), length.out = amortMonths)
   a1 <- cumprod(i + 1)
   # a0 <- c(1, a1[1:(amortMonths-1)])
   a0 <- ShiftRight(a1, positions = 1, filler = 1)
   bal <- loanAmt * a1 - cumsum(payment * a0)
   # principalPayment <- c(loanAmt, bal[1:(length(bal)-1)]) - bal
   principalPayment <- ShiftRight(bal, positions = 1, filler = loanAmt) - bal
   intPayment <- rep(payment, length.out = amortMonths) - principalPayment
   lstResult$LoanPayment <- payment      # Monthly loan payment
   lstResult$PrincipalPayment <- principalPayment
   lstResult$InterestPayment <- intPayment
   lstResult$LoanBalance <- c(loanAmt, bal)[1:amortMonths]     # Beginning-of-month loan balance
   return(lstResult)
}

CloneS4Object <- function(object){
   newObject <- new(Class = class(object))
   for (sn in slotNames(object)) {
      eval(parse(text = paste("newObject@",sn, " <- object@", sn, sep="")))
   }
   return(newObject)
}

IsListOfList <- function(x) {
   return(all(unlist(lapply(x, function(y) {is.list(y)}))))
}

IsListOf <- function(x, clsName) {
   return(all(unlist(lapply(x, function(y) {is(y, clsName)}))))
}

To.data.frame <- function(x, itemName, colNames = NA_character_) {
   stopifnot(IsListOfList(x))
   rslt <- list()
   # cols <- names(x[[1]][[itemName]])     # This will not work if the first element of x does not contain the specified item named itemName.
   for (i in 1:length(x)) {
      cols <- names(x[[i]][[itemName]])
      if (!is.null(cols)) break
   }
   for (col in cols) {
      eval(expr = parse(text = paste0("s", col, "<-c(", paste0("x[[", 1:length(x), "]][[itemName]]$", col, collapse = ","),")")))
      eval(expr = parse(text = paste0("rslt$", col, "<-s", col)))
   }
   df <- as.data.frame(rslt, stringsAsFactors = FALSE)
   if (!is.na(colNames)) {
      colnames(df) <- colNames
   }
   return(df)
}

fgsub <- function(strPattern, replacement, path = ".", fnPattern = NULL) {
   fileList <- dir(path = path, pattern = fnPattern, full.names = TRUE)
   for (f in fileList) {
      ftmp <- paste0(f, "_")
      s0 <- readLines(f)
      lines <- grep(strPattern, s0)
      if (length(lines) > 0) {
         s1 <- gsub(strPattern, replacement, s0)
         writeLines(s1, ftmp)
         file.remove(f)
         file.rename(ftmp, f)
      }
   }
}


`%df/%` <- function(df, x) {
   stopifnot(is.data.frame(df))
   stopifnot(is.numeric(x) & length(x) == 1)
   for (i in seq_along(colnames(df))) {
      v <- eval(expr = parse(text = paste0("df$`", colnames(df)[i], "`")))
      if (is.numeric(v)) {
         v <- v / x
      }
      eval(expr = parse(text = paste0("df$`", colnames(df)[i], "` <- v")))
   }
   return(df)
}



replace_string_in_r_files <- function(path, str1, str2) {
   # Find all .R files recursively
   r_files <- list.files(path = path, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)

   if (length(r_files) == 0) {
      message("No .R files found in the specified path.")
      return(invisible(NULL))
   }

   for (file in r_files) {
      tryCatch({
         # Read the file content
         content <- readLines(file, warn = FALSE)

         # Replace str1 with str2
         modified_content <- gsub(str1, str2, content, fixed = TRUE) # use fixed=TRUE for literal string replacement

         # Write the modified content back to the file
         writeLines(modified_content, file)

         message(paste("Replaced '", str1, "' with '", str2, "' in file:", file))
      }, error = function(e) {
         warning(paste("Error processing file:", file, "-", e$message))
      })
   }
}

# Example usage (replace with your actual path and strings)
# replace_string_in_r_files(path = "./my_r_project", str1 = "old_variable", str2 = "new_variable")

#Example usage with special characters.
#replace_string_in_r_files(path = "./my_r_project", str1 = "old.variable$", str2 = "new.variable")

#Example usage with regex special characters that are handled by fixed=TRUE.
#replace_string_in_r_files(path = "./my_r_project", str1 = "old.variable\\$", str2 = "new.variable")

#Example usage with regex, without fixed=TRUE.
#replace_string_in_r_files(path = "./my_r_project", str1 = "old\\.variable\\$", str2 = "new.variable")

# Explanation:
#    * replace_string_in_r_files(path, str1, str2):
#    * This defines the function with the specified arguments.
# * r_files <- list.files(...):
#    * list.files() is used to find all files within the given path.
# * pattern = "\\.R$" filters for files ending with ".R". The double backslash is crucial to escape the dot, as a single dot in regular expressions means "any character." The $ means the end of the string.
# * recursive = TRUE searches in all subfolders.
# * full.names = TRUE returns the full file paths.
# * if (length(r_files) == 0):
#    * Checks if any .R files were found and prints a message if none exist.
# * for (file in r_files):
#    * Loops through each found .R file.
# * tryCatch({...}, error = function(e) {...}):
#    * Handles potential errors during file reading or writing.
# * content <- readLines(file, warn = FALSE):
#    * Reads the content of the file into a character vector. warn = FALSE suppresses warnings about incomplete final lines.
# * modified_content <- gsub(str1, str2, content, fixed = TRUE):
#    * gsub() is used to replace all occurrences of str1 with str2 in the content.
# * fixed = TRUE is very important! It treats str1 and str2 as literal strings, meaning that any special regex characters within them are not interpreted as regex. If you want to use regex, remove fixed=TRUE and properly escape any regex characters in str1.
# * writeLines(modified_content, file):
#    * Writes the modified content back to the original file.
# * message(...) and warning(...):
#    * Provides feedback to the user about the progress or any errors encountered.
# * invisible(NULL):
#    * returns null, but does not print it to the console.
# Important considerations:
#    * Backup: Before running this function, make a backup of your .R files, as it will directly modify them.
# * Regex: If you need to use regular expressions for more complex string matching, remove fixed = TRUE from the gsub() call and ensure that str1 is a valid regular expression. You may need to escape special characters.
# * Permissions: Ensure that you have write permissions to the files and directories in the specified path.
# * Testing: Test the function on a small subset of files before running it on your entire project.



