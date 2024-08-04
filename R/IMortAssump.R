setClass(Class = "IMortAssump", contains = c("IObject", "VIRTUAL"))

setClassUnion(name = "character_or_IMortAssump", members = c("character", "IMortAssump"))

setValidity(
   Class = "IMortAssump",
   method = function(object) {
      err <- New.SysMessage()
      if (length(object@Id) > 0) {
         if (!startsWith(object@Id, "MortAssump.")) {
            AddMessage(err) <- "Invalid identifier.  It must contain the prefix 'MortAssump.'"
         }
      }
      if (NoMessage(err)) {
         return(TRUE)
      } else {
         return(GetMessage(err))
      }
   }
)

setMethod(
   f = "GetAssumpId",
   signature = "IMortAssump",
   definition = function(object) {
      return(GetId(object))
   }
)

setMethod(
   f = "SetAssumpId<-",
   signature = c("IMortAssump", "character"),
   definition = function(object, value) {
      if (length(value) == 0) return(object)
      if (!startsWith(value, "MortAssump.")) {
         value <- paste0("MortAssump.", value)
      }
      SetId(object) <- value
      return(object)
   }
)

setMethod(
   f = "GetExpdAssump",
   signature = "IMortAssump",
   definition = function(object, cov, plan, assumpInfo, ...) {
      stop("Method 'GetExpdAssump' must be implemented by a class extending 'IMortAssump'.")
   }
)

setMethod(
   f = "GetPaddAssump",
   signature = "IMortAssump",
   definition = function(object, cov, plan, assumpInfo, ...) {
      stop("Method 'GetPaddAssump' must be implemented by a class extending 'IMortAssump'.")
   }
)

setMethod(
   f = "GetAssump",
   signature = "IMortAssump",
   definition = function(object, cov, plan, assumpInfo, ...) {
      stop("Method 'GetAssump' must be implemented by a class extending 'IMortAssump'.")
   }
)

