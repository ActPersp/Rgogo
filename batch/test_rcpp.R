library(Rgogo)
library(MortTables.CA)
q <- LookUp(Mort.CIA8692L.M, lookUpKey = list(IssAge = 40))
i <- 0.05
n <- 25
microbenchmark::microbenchmark(CalcNSP.TermLife(q, i, n), times = 1000)
microbenchmark::microbenchmark(CalcNSP.WholeLife(q, i), times = 1000)
