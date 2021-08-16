setClass(
   Class = "IPlan.DT.JL",
   contains = c(
      "IPlan.LT.JL",
      "IPlan.DT"
   )
)

IPlan.DT.JL <- function(covYears = NA, covToAge = NA, premYears = NA, premToAge = NA,
                        loanIntrRate, loanIntrRateType = 1L,
                        premTable = character(0L), modFactor = c("1" = 1, "2" = 0.5, "4" = 0.25, "12" = 1/12),
                        polFee = numeric(0), premTaxRate = numeric(0L),
                        commSchd = numeric(0L), ovrdOnPremSchd = numeric(0L), ovrdOnCommSchd = numeric(0L),
                        rein = character(0L), id = character(0L), descrip = character(0L)) {
   plan <- IPlan.DT(covYears, covToAge, premYears, premToAge,
                    loanIntrRate, loanIntrRateType,
                    premTable, modFactor, polFee, premTaxRate,
                    commSchd, ovrdOnPremSchd, ovrdOnCommSchd,
                    rein, id, descrip)
   class(plan) <- "IPlan.DT.JL"
   return(plan)
}
