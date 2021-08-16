setClass(
   Class = "IPlan.LT.JL",
   contains = "IPlan.LT"
)

IPlan.LT.JL <- function(covYears = NA, covToAge = NA, premYears = NA, premToAge = NA,
                        premTable = character(0L), modFactor = c("1" = 1, "2" = 0.5, "4" = 0.25, "12" = 1/12),
                        polFee = numeric(0), premTaxRate = numeric(0L),
                        commSchd = numeric(0L), ovrdOnPremSchd = numeric(0L), ovrdOnCommSchd = numeric(0L),
                        rein = character(0L), id = character(0L), descrip = character(0L)) {
   plan <- IPlan.LT(covYears, covToAge, premYears, premToAge,
                    premTable, modFactor, polFee, premTaxRate,
                    commSchd, ovrdOnPremSchd, ovrdOnCommSchd,
                    rein, id, descrip)
   class(plan) <- "IPlan.LT.JL"
   return(plan)
}
