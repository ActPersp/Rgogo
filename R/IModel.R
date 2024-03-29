setClass(
   Class = "IModel",
   contains = c("IObject", "VIRTUAL"),
   slots = c(Args = "character_or_IArgSet")
)

setClassUnion(name = "character_or_IModel", members = c("character", "IModel"))

setValidity(
   Class = "IModel",
   method = function(object) {
      err <- New.SysMessage()
      if (length(object@Id) > 0) {
         if (!startsWith(object@Id, "Model.")) {
            AddMessage(err) <- "Invalid identifier.  It must contain the prefix 'Model.'"
         }
      }
      isValid = Validate(Validator.Length(minLen = 0, maxLen = 1), object@Args)
      if (isValid != TRUE) {
         AddMessage(err) <- "The length of slot value '@Args' cannot be greater than 1."
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

setMethod(
   f = "GetModelId",
   signature = "IModel",
   definition = function(object) {
      return(GetId(object))
   }
)

setMethod(
   f = "SetModelId<-",
   signature = c("IModel", "character"),
   definition = function(object, value) {
      if (length(value) == 0) return(object)
      if (!startsWith(value, "Model.")) {
         value <- paste0("Model.", value)
      }
      SetId(object) <- value
      return(object)
   }
)

setMethod(
   f = "GetArgs",
   signature = "IModel",
   definition = function(object) {
      if (is.character(object@Args)) {
         argSetId <- ifelse(startsWith(object@Args, "ArgSet."), object@Args, paste0("ArgSet.", object@Args))
         return(eval(expr = parse(text = argSetId)))
      } else {     # an object of class extending "IArgSet"
         return(object@Args)
      }
   }
)

setMethod(
   f = "SetArgs<-",
   signature = "IModel",
   definition = function(object, value) {
      object@Args <- value
      validObject(object)
      return(object)
   }
)

setMethod(
   f = "GetArgValue",
   signature = c("IModel", "character"),
   definition = function(object, argName) {
      return(GetArgValue(GetArgs(object), argName))
   }
)

setMethod(
   f = "SetArgValue",
   signature = "IModel",
   definition = function(object, ...) {
      # object@Args <- SetArgValue(object@Args, ...)
      object@Args <- SetArgValue(GetArgs(object), ...)
      return(object)
   }
)

setMethod(
   f = "Run",
   signature = "IModel",
   definition = function(object, ...) {
      stop("Method 'Run' must be implemented by a class extending 'IModel' virtual class.")
   }
)

