CreateProject <- function(projId, loc = getwd(), msgPath = character(0L)) {
   cond <- tryCatch(
      {
         # Check project id naming rule: should contain only (ASCII) letters, numbers and dot, have at least two characters and start with a letter and not end in a dot.
         if (!grepl("^[a-zA-Z][a-zA-Z0-9.]{0,}[^-_.]$", projId)) {
            message("The project identifier is invlid.", appendLF = FALSE)
         }
         # Check if the location where the project is to be created exists.
         if (!dir.exists(loc)) {
            message("The location for the project does not exist.", appendLF = FALSE)
         }
         # Create project root directory:
         if (endsWith(loc, "/")) {
            projRoot <- paste0(loc, projId)
         } else {
            projRoot <- paste(loc, projId, sep = "/")
         }
         if (dir.exists(projRoot)) {
            message("The project root directory already exists.", appendLF = FALSE)
         }
         dir.create(projRoot)
         # Create DESCRIPTION file:
         descripFields <- list(
            Package = projId,
            Version = "0.0.1",
            License = "To be specified",
            Title = "To be specified",
            Description = "To be specified",
            Author = "To be specified",
            Maintainer = "To be specified <someone@somedomain.com>",
            Encoding = "UTF-8",
            LazyData = "true",
            Depends = "R (>= 4.0.0)",
            Imports = "Rgogo"
         )
         WriteDescripFile(descripFields, projRoot)
         # Create NAMESPACE file:
         content <- c(
            'exportPattern("^[[:alpha:]]+")',
            'import("Rgogo")'
         )
         WriteNamespaceFile(content, projRoot)
         # Create RProj file:
         projInfoFields <- list(
            Version = "1.0",
            RestoreWorkspace = "Default",
            SaveWorkspace = "Default",
            AlwaysSaveHistory = "Default",
            EnableCodeIndexing = "Yes",
            UseSpacesForTab = "Yes",
            NumSpacesForTab = "3",
            Encoding = "UTF-8",
            RnwWeave = "Sweave",
            LaTeX = "pdfLaTeX",
            BuildType = "Package",
            PackageUseDevtools = "Yes",
            PackageInstallArgs = "--no-multiarch --with-keep.source"
         )
         WriteProjFile(projId, projInfoFields, projRoot)
         # Create sub-folders under project root.
         for (subdir in c("R", "data", "batch", "export", "data-raw", "db")) {
            dir.create(file.path(projRoot, subdir))
         }
         paste0("'", projId, "' project is created in '", loc, "'")
      },
      message = function(c) {c},
      warning = function(c) {c},
      error = function(c) {c}
   )
   if (length(msgPath) > 0) {
      GenJsonMsgPacket(
         cond = cond,
         jsonPath =  msgPath,
         successMsg = paste0("Project '", projId, "' has been created successfully."),
         warningMsg = NA_character_,
         errorMsg = NA_character_,
         ProjectId = projId,
         Path = loc
      )
   } else {
      return(cond)
   }
}

ReadDescripFile <- function(projRoot = character()) {
   if (length(projRoot) > 0 & !endsWith(projRoot, "/")) {
      projRoot <- paste0(projRoot, "/")
   }
   path <- paste0(projRoot, "DESCRIPTION")
   if (!file.exists(path)) {
      return(NULL)
   }
   f <- file(path)
   txt <- readLines(f)
   close(f)
   content <- list()
   for (s in txt) {
      sep <- regexpr(":", s)
      if (sep > 0) {
         key <- trimws(substr(s, 1, sep - 1))
         value <- trimws(substr(s, sep + 1, nchar(s)))
         if (nchar(value) > 0) {
            content <- c(content, eval(expr = parse(text = paste0("list('", key, "' = '", value, "')"))))
         } else {
            content <- c(content, eval(expr = parse(text = paste0("list('", key, "' = character(0L))"))))
         }
      } else {
         if (nchar(trimws(s)) > 0) {
            content[[length(content)]] <- c(content[[length(content)]], trimws(s))
         }
      }
   }
   if (!is.null(content$Imports)) {
      content$Imports <- gsub(",", "", content$Imports)
   }
   if (!is.null(content$Suggests)) {
      content$Suggests <- gsub(",", "", content$Suggests)
   }
   return(content)
}

WriteDescripFile <- function(descripFields, projRoot = character()) {
   if (length(projRoot) > 0 & !endsWith(projRoot, "/")) {
      projRoot <- paste0(projRoot, "/")
   }
   path <- paste0(projRoot, "DESCRIPTION")
   f <- file(path)
   s <- character()
   for (key in names(descripFields)) {
      value <- eval(expr = parse(text = paste0("descripFields$'", key, "'")))
      if (key %in% c("Imports", "Suggests")) {
         sep = ",\n    "
      } else {
         sep = "\n    "
      }
      s <- c(s, paste0(key, ": ", paste0(value, collapse = sep)))
   }
   writeLines(s, f)
   close(f)
}

ReadNamespaceFile <- function(projRoot = character()) {
   if (length(projRoot) > 0 & !endsWith(projRoot, "/")) {
      projRoot <- paste0(projRoot, "/")
   }
   path <- paste0(projRoot, "NAMESPACE")
   if (!file.exists(path)) {
      return(NULL)
   }
   f <- file(path)
   txt <- readLines(f)
   close(f)
   return(txt)
}

WriteNamespaceFile <- function(content, projRoot = character()) {
   if (length(projRoot) > 0 & !endsWith(projRoot, "/")) {
      projRoot <- paste0(projRoot, "/")
   }
   path <- paste0(projRoot, "NAMESPACE")
   f <- file(path)
   writeLines(content, f)
   close(f)
}

WriteProjFile <- function(projId, projInfoFields, projRoot = character()) {
   if (length(projRoot) > 0 & !endsWith(projRoot, "/")) {
      projRoot <- paste0(projRoot, "/")
   }
   path <- paste0(projRoot, projId, ".Rproj")
   f <- file(path)
   s <- character()
   for (key in names(projInfoFields)) {
      value <- eval(expr = parse(text = paste0("projInfoFields$'", key, "'")))
      s <- c(s, paste0(key, ": ", paste0(value, collapse = "\n")))
   }
   writeLines(s, f)
   close(f)
}


DeployObject <- function(pkgName, objectType, overwrite = TRUE) {
   # # Identify all objects in the package which match the project type.
   funcList <- strsplit(eval(expr = parse(text = paste0("ls('package:", pkgName, "', pattern = '", paste0('^New.', objectType, '.'), "')"))), " ")
   if (length(funcList) == 0) return(paste0(objectType,": nothing to deploy."))
   # Save the identified objects as Rda data.
   lapply(funcList,
          function(funcName, pkg, ow) {
             if (is.function(eval(expr = parse(text = funcName)))) {
                objName <- substr(funcName, 5, nchar(funcName))
                objExists <- objName %in% data(package=pkg)$results[,"Item"]
                if(ow | !objExists) {
                   cat("-- Deploying", objName, "...")
                   eval(expr = parse(text = paste0("obj <- ", funcName, "()")))
                   if (length(GetId(obj)) == 0) {
                      SetId(obj) <- objName
                   } else if (GetId(obj) != objName) {
                      stop(paste0("Object deployed by function ", funcName, " has an inconsistent identifier ", GetId(obj)))
                   }
                   eval(expr = parse(text = paste0(objName, " <- obj" )))
                   eval(expr = parse(text = paste0("save(", objName, ", file = 'data/", objName, ".rda')")))
                   cat("done", "\n")
                }
             }
          }, pkgName, overwrite
   )
}


DeployProject <- function(pkgName, overwrite = TRUE) {
   cat("Start deploying project", pkgName, "...\n")
   devtools::load_all(export_all = FALSE)
   DeployObject(pkgName, "Plan", overwrite)
   DeployObject(pkgName, "MortAssump", overwrite)
   DeployObject(pkgName, "LapseAssump", overwrite)
   DeployObject(pkgName, "ExpnsAssump", overwrite)
   DeployObject(pkgName, "IntrAssump", overwrite)
   DeployObject(pkgName, "PremAssump", overwrite)
   DeployObject(pkgName, "IntrCredAssump", overwrite)
   DeployObject(pkgName, "PUA", overwrite)
   DeployObject(pkgName, "Rein", overwrite)
   DeployObject(pkgName, "ArgSet", overwrite)
   DeployObject(pkgName, "Model", overwrite)
   DeployObject(pkgName, "Const", overwrite)
   cat("Model components are deployed successfully.\n")
   cat("Remeber to install package before running models.")
   # cat("Installing package", pkgName, "\n")
   # devtools::install(quiet = TRUE)
   # cat(pkgName, "is deployed successfully.\n")
   # cat("Reloading package", pkgName, "\n")
}
