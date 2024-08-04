setMethod(
   f = "Run.CF",
   signature = c("Model.CF", "IPlan.LT.JL", "Cov"),
   definition = function(object, plan, cov, result) {
      covMonths <- GetCovMonths(plan, cov)
      projStartDate <- GetArgValue(object, "ProjStartDate")
      result$Timeline <- GetProjTimelineInfo(projStartDate, cov, plan)
      projLen <- GetProjLen(result$Timeline)
      covProjLen <- GetCovProjLen(result$Timeline)
      projPolMonths <- GetProjPolMonths(result$Timeline)

      result$.ArgSet <- GetArgs(object)
      result <- Project(plan, cov, result)
      proj <- result$Proj

      # Get mortality assumption information
      mortAssump <- GetArgValue(object, "MortAssump")
      if (!is.null(mortAssump)) {
         result <- GetAssump(mortAssump, cov, plan, result)
         if (GetArgValue(object, "ApplyMortMargin")) {
            qRate_x <- result$q_x.Padd
            qRate_y <- result$q_y.Padd
         } else {
            qRate_x <- result$q_x.Expd
            qRate_y <- result$q_y.Expd
         }
         q_x <- Convert_qx(qRate_x, 12L, "ud")[1:covMonths]
         q_y <- Convert_qx(qRate_y, 12L, "ud")[1:covMonths]
      } else {
         q_y <- q_x <- rep(0, length.out = covMonths)
      }

      # Get lapse assumption information
      lapseAssump <- GetArgValue(object, "LapseAssump")
      if (!is.null(lapseAssump)) {
         result <- GetAssump(lapseAssump, cov, plan, result)
         if (GetArgValue(object, "ApplyLapseMargin")) {
            wRate <- result$w.Padd
         } else {
            wRate <- result$w.Expd
         }
         lapseMode <- ifelse(length(GetPremMode(cov)) == 0, 12L, GetPremMode(cov))
         wRate <- Convert_qx(wRate, lapseMode, "ud")
         w <- unlist(lapply(as.list(wRate), function(x){return(c(rep(0, length.out = 12/lapseMode - 1), x))}))
         w <- w[1:covMonths]
         if(NoLapseAfterPaidUp(lapseAssump)) {
            w <- w * ((1:covMonths) <= GetPremMonths(plan, cov))
         }
      } else {
         w <- rep(0, length.out = covMonths)
      }

      # Get expense assumption information
      expnsAssump <- GetArgValue(object, "ExpnsAssump")
      ae <- rep(0, length.out = projLen)
      me <- rep(0, length.out = projLen)
      if (!is.null(expnsAssump)) {
         result <- GetAssump(expnsAssump, cov, plan, result, projStartDate)
         if (GetArgValue(object, "ApplyExpnsMargin")) {
            if (!is.null(result$ae.PerPol.Padd)) {
               ae <- ae + result$ae.PerPol.Padd
            }
            if (!is.null(result$ae.PerFaceAmt.Padd)) {
               ae <- ae + result$ae.PerFaceAmt.Padd
            }
            if (!is.null(result$me.PerPol.Padd)) {
               me <- me + result$me.PerPol.Padd
            }
            if (!is.null(result$me.PerPrem.Padd)) {
               me <- me + result$me.PerPrem.Padd
            }
            if (!is.null(result$me.PerPremAmt.Padd)) {
               me <- me + result$me.PerPremAmt.Padd
            }
         } else {
            if (!is.null(result$ae.PerPol.Expd)) {
               ae <- ae + result$ae.PerPol.Expd
            }
            if (!is.null(result$ae.PerFaceAmt.Expd)) {
               ae <- ae + result$ae.PerFaceAmt.Expd
            }
            if (!is.null(result$me.PerPol.Expd)) {
               me <- me + result$me.PerPol.Expd
            }
            if (!is.null(result$me.PerPrem.Expd)) {
               me <- me + result$me.PerPrem.Expd
            }
            if (!is.null(result$me.PerPremAmt.Expd)) {
               me <- me + result$me.PerPremAmt.Expd
            }
         }
      }

      # Probability of survival and cashflow projection
      prob <- GetLifeProb.2L(q_x, q_y)
      q <- prob$jl.q_xy
      p <- 1 - q - w
      pn <- ShiftRight(cumprod(p), positions = 1, filler = 1)
      pn <- pn / pn[projPolMonths[1]]
      zeroCf <- rep(0, length.out = projLen - covProjLen)
      covProjTimeIndex <- GetCovProjTimeIndex(result$Timeline)[1:covMonths]
      IsBegPolMonth <- Is.WholeNumber(covProjTimeIndex[projPolMonths[1]])
      if (!is.null(proj$Prem)) {
         cfPrem <- proj$Prem[projPolMonths] * pn[projPolMonths]
         if (projLen > covProjLen) {
            cfPrem <- c(zeroCf, cfPrem)
         } else if (!IsBegPolMonth) {
            cfPrem <- ShiftLeft(cfPrem, positions = 1, filler = 0)
         }
      } else {
         cfPrem <- rep(0, length.out = projLen)
      }
      # Premium tax cash flow
      if (!is.null(proj$Prem.Tax)) {
         cfPremTax <- -proj$Prem.Tax[projPolMonths] * pn[projPolMonths]
         if (projLen > covProjLen) {
            cfPremTax <- c(zeroCf, cfPremTax)
         } else if (!IsBegPolMonth) {
             cfPremTax <- ShiftLeft(cfPremTax, positions = 1, filler = 0)
         }
      } else {
         cfPremTax <- rep(0, length.out = projLen)
      }
      # Commission and manager override cash flows
      if (!is.null(proj$Comm)) {
         cfComm <- -proj$Comm[projPolMonths] * pn[projPolMonths]
         if (projLen > covProjLen) {
            cfComm <- c(zeroCf, cfComm)
         } else if (!IsBegPolMonth) {
            cfComm <- ShiftLeft(cfComm, positions = 1, filler = 0)
         }
      } else {
         cfComm <- rep(0, length.out = projLen)
      }
      if (!is.null(proj$Comm.Ovrd)) {
         cfOvrd <- -proj$Comm.Ovrd[projPolMonths] * pn[projPolMonths]
         if (projLen > covProjLen) {
            cfOvrd <- c(zeroCf, cfOvrd)
         } else if (!IsBegPolMonth) {
            cfOvrd <- ShiftLeft(cfOvrd, positions = 1, filler = 0)
         }
      } else {
         cfOvrd <- rep(0, length.out = projLen)
      }
      # Death benefit cash flow
      if (!is.null(proj$Ben.Dth)) {
         cfDthBen <- -proj$Ben.Dth[projPolMonths] * pn[projPolMonths] * q[projPolMonths]
         if (projLen > covProjLen) {
            cfDthBen <- c(zeroCf, cfDthBen)
         }
      } else {
         cfDthBen <- rep(0, length.out = projLen)
      }
      # PUA Death benefit cash flow
      if (!is.null(proj$Ben.Dth.PUA)) {
         cfDthBenPUA <- -proj$Ben.Dth.PUA[projPolMonths] * pn[projPolMonths] * q[projPolMonths]
         if (projLen > covProjLen) {
            cfDthBenPUA <- c(zeroCf, cfDthBenPUA)
         }
      } else {
         cfDthBenPUA <- rep(0, length.out = projLen)
      }
      # Maturity benefit cash flow
      if (!is.null(proj$Ben.Mat)) {
         cfMatBen <- -proj$Ben.Mat[projPolMonths] * pn[projPolMonths] * p[projPolMonths]
         if (projLen > covProjLen) {
            cfMatBen <- c(zeroCf, cfMatBen)
         }
      } else {
         cfMatBen <- rep(0, length.out = projLen)
      }
      # PUA Maturity benefit cash flow
      if (!is.null(proj$Ben.Mat.PUA)) {
         cfMatBenPUA <- -proj$Ben.Mat.PUA[projPolMonths] * pn[projPolMonths] * p[projPolMonths]
         if (projLen > covProjLen) {
            cfMatBenPUA <- c(zeroCf, cfMatBenPUA)
         }
      } else {
         cfMatBenPUA <- rep(0, length.out = projLen)
      }
      # Surrender benefit cash flow
      if (!is.null(proj$Ben.Sur)) {
         cfSurBen <- -proj$Ben.Sur[projPolMonths] * pn[projPolMonths] * w[projPolMonths]
         if (projLen > covProjLen) {
            cfSurBen <- c(zeroCf, cfSurBen)
         }
      } else {
         cfSurBen <- rep(0, length.out = projLen)
      }
      # PUA Surrender benefit cash flow
      if (!is.null(proj$Ben.Sur.PUA)) {
         cfSurBenPUA <- -proj$Ben.Sur.PUA[projPolMonths] * pn[projPolMonths] * w[projPolMonths]
         if (projLen > covProjLen) {
            cfSurBenPUA <- c(zeroCf, cfSurBenPUA)
         }
      } else {
         cfSurBenPUA <- rep(0, length.out = projLen)
      }
      # Reinsurance cash flows.  Current reinsurance implementation is only for insurance only.  No reinsurance is assumed for annuity.
      if (!is.null(proj$Rein.Ben) & is.null(proj$Ben.Anu)) {
         cfReinBen <- proj$Rein.Ben[projPolMonths] * pn[projPolMonths] * q[projPolMonths]
         if (projLen > covProjLen) {
            cfReinBen <- c(zeroCf, cfReinBen)
         }
      } else {
         cfReinBen <- rep(0, length.out = projLen)
      }
      if (!is.null(proj$Rein.Prem) & is.null(proj$Ben.Anu)) {
         cfReinPrem <- -proj$Rein.Prem[projPolMonths] * pn[projPolMonths]
         if (projLen > covProjLen) {
            cfReinPrem <- c(zeroCf, cfReinPrem)
         } else if (!IsBegPolMonth) {
            cfReinPrem <- ShiftLeft(cfReinPrem, positions = 1, filler = 0)
         }
      } else {
         cfReinPrem <- rep(0, length.out = projLen)
      }
      if (!is.null(proj$Rein.Comm) & is.null(proj$Ben.Anu)) {
         cfReinComm <- proj$Rein.Comm[projPolMonths] * pn[projPolMonths]
         if (projLen > covProjLen) {
            cfReinComm <- c(zeroCf, cfReinComm)
         } else if (!IsBegPolMonth) {
            cfReinComm <- ShiftLeft(cfReinComm, positions = 1, filler = 0)
         }
      } else {
         cfReinComm <- rep(0, length.out = projLen)
      }
      if (!is.null(proj$Rein.Prem.Rfnd) & is.null(proj$Ben.Anu)) {
         cfReinPremRfnd <- proj$Rein.Prem.Rfnd[projPolMonths] * pn[projPolMonths] * w[projPolMonths]
         if (projLen > covProjLen) {
            cfReinPremRfnd <- c(zeroCf, cfReinPremRfnd)
         }
      } else {
         cfReinPremRfnd <- rep(0, length.out = projLen)
      }
      if (!is.null(proj$Rein.Comm.Rfnd) & is.null(proj$Ben.Anu)) {
         cfReinCommRfnd <- -proj$Rein.Comm.Rfnd[projPolMonths] * pn[projPolMonths] * w[projPolMonths]
         if (projLen > covProjLen) {
            cfReinCommRfnd <- c(zeroCf, cfReinCommRfnd)
         }
      } else {
         cfReinCommRfnd <- rep(0, length.out = projLen)
      }

      # Projected expenses and projected expense cashflows
      if (!is.null(result$.ProjEndPolMonth)) {
         v <- (seq_along(ae) +covMonths - covProjLen) <= result$.ProjEndPolMonth
         ae <- ae * v
         me <- me * v
      }
      result$Proj$Expns.Acq = c(rep(NA, covMonths - covProjLen), ae[(projLen - covProjLen + 1):projLen])
      result$Proj$Expns.Mnt = c(rep(NA, covMonths - covProjLen), me[(projLen - covProjLen + 1):projLen])
      cfAcqExpns <- -ae * c(rep(0, length.out = projLen - covProjLen), pn[projPolMonths])
      cfMntExpns <- -me * c(rep(0, length.out = projLen - covProjLen), pn[projPolMonths])
      if (projLen == covProjLen & !IsBegPolMonth) {
         cfAcqExpns <- ShiftLeft(cfAcqExpns, positions = 1, filler = 0)
         cfMntExpns <- ShiftLeft(cfMntExpns, positions = 1, filler = 0)
      }
      cfTotalGross <- cfPrem + cfPremTax + cfComm + cfOvrd + cfDthBen + cfMatBen + cfSurBen + cfDthBenPUA + cfMatBenPUA + cfSurBenPUA + cfAcqExpns + cfMntExpns
      cfTotalRein <- cfReinBen + cfReinPrem + cfReinComm + cfReinPremRfnd + cfReinCommRfnd

      result$Cf <- list(
         CovId = rep(ifelse(length(GetId(cov)) > 0, GetId(cov), NA), length.out = projLen),
         Timeline = GetProjTimeLabel(result$Timeline),
         Prem = cfPrem,
         Prem.Tax = cfPremTax,
         Comm = cfComm,
         Comm.Ovrd = cfOvrd,
         Ben.Dth = cfDthBen,
         Ben.Dth.PUA = cfDthBenPUA,
         Ben.Mat = cfMatBen,
         Ben.Mat.PUA = cfMatBenPUA,
         Ben.Sur = cfSurBen,
         Ben.Sur.PUA = cfSurBenPUA,
         Ben.Anu = rep(0, length.out = projLen),
         Expns.Acq = cfAcqExpns,
         Expns.Mnt = cfMntExpns,
         Rein.Ben = cfReinBen,
         Rein.Prem = cfReinPrem,
         Rein.Comm = cfReinComm,
         Rein.Prem.Rfnd = cfReinPremRfnd,
         Rein.Comm.Rfnd = cfReinCommRfnd,
         Total.Gross = cfTotalGross,
         Total.Rein = cfTotalRein,
         Total.Net = cfTotalGross + cfTotalRein
      )

      result$Assump <- list(
         PolMonth = projPolMonths,
         q = q[projPolMonths],
         w = w[projPolMonths],
         p = p[projPolMonths],
         pn = pn[projPolMonths],
         t = covProjTimeIndex[ceiling(covProjTimeIndex) >= 0]
      )

      return(result)
   }
)

