setClass(Class = "Model.ReinPfadAnlys", contains = "Model.PfadAnlys")

Model.ReinPfadAnlys <- function(args, id = character(0L), descrip = character(0L)) {
   model <- new(Class = "Model.ReinPfadAnlys", Args = args, Descrip = as.character(descrip))
   SetModelId(model) <- as.character(id)
   return(model)
}

setMethod(
   f = "Run",
   signature = "Model.ReinPfadAnlys",
   definition = function(object, var, result) {
      # PfAD analysis is to calculate reserves by removing provisions for adverse deviation in the following orders: mortality -> lapse -> interest -> expense -> premium
      model <- GetModel(GetArgs(object))
      args <- GetArgs(model)
      # Calculate base scenario reserve
      valuResult <- Run(model, var, list())
      reinRes0 <- ifelse(is.null(valuResult$Res$Res.Rein), 0, valuResult$Res$Res.Rein)
      # Step 1: calculate reserve by removing mortality pfad
      reinRes1 <- reinRes0
      if (all(Contains(args, c("MortAssump", "ApplyMortMargin")))) {
         if (ApplyMortMargin(args) & !is.null(GetMortAssump(args))) {
            ApplyMortMargin(args) <- FALSE
            SetArgs(model) <- args
            valuResult <- Run(model, var, list())
            reinRes1 <- ifelse(is.null(valuResult$Res$Res.Rein), 0, valuResult$Res$Res.Rein)
         }
      }
      # Step 2: calculate reserve by removing lapse pfad
      reinRes2 <- reinRes1
      if (all(Contains(args, c("LapseAssump", "ApplyLapseMargin")))) {
         if (ApplyLapseMargin(args) & !is.null(GetLapseAssump(args))) {
            ApplyLapseMargin(args) <- FALSE
            SetArgs(model) <- args
            valuResult <- Run(model, var, list())
            reinRes2 <- ifelse(is.null(valuResult$Res$Res.Rein), 0, valuResult$Res$Res.Rein)
         }
      }
      # Step 3: calculate reserve by removing interest pfad
      reinRes3 <- reinRes2
      if (all(Contains(args, c("IntrAssump", "ApplyIntrMargin")))) {
         if (ApplyIntrMargin(args) & !is.null(GetIntrAssump(args))) {
            ApplyIntrMargin(args) <- FALSE
            SetArgs(model) <- args
            valuResult <- Run(model, var, list())
            reinRes3 <- ifelse(is.null(valuResult$Res$Res.Rein), 0, valuResult$Res$Res.Rein)
         }
      }
      # Step 4: calculate reserve by removing expense pfad
      reinRes4 <- reinRes3
      if (all(Contains(args, c("ExpnsAssump", "ApplyExpnsMargin")))) {
         if (ApplyExpnsMargin(args) & !is.null(GetExpnsAssump(args))) {
            ApplyExpnsMargin(args) <- FALSE
            SetArgs(model) <- args
            valuResult <- Run(model, var, list())
            reinRes4 <- ifelse(is.null(valuResult$Res$Res.Rein), 0, valuResult$Res$Res.Rein)
         }
      }
      # Step 5: calculate reserve by removing premium pfad
      reinRes5 <- reinRes4
      if (all(Contains(args, c("PremAssump", "ApplyPremMargin")))) {
         if (ApplyPremMargin(args) & !is.null(GetPremAssump(args))) {
            ApplyPremMargin(args) <- FALSE
            SetArgs(model) <- args
            valuResult <- Run(model, var, list())
            reinRes5 <- ifelse(is.null(valuResult$Res$Res.Rein), 0, valuResult$Res$Res.Rein)
         }
      }
      result$Pfad <- list(
         CovId = GetId(var),
         PlanId = GetPlanId(var),
         ReinRes = reinRes0,
         MortPfad = reinRes0 - reinRes1,
         LapsePfad = reinRes1 - reinRes2,
         IntrPfad = reinRes2 - reinRes3,
         ExpnsPfad = reinRes3 - reinRes4,
         PremPfad = reinRes4 - reinRes5
      )
      return(result)
   }
)

