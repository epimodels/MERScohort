# Loading required libraries
library(ggplot2)
library(ez)
library(sm)

mersd.estimates = data.frame(
  var   = c("Age","Onset","Comorbidity","Animal Contact","Secondary Case","Female","HCW"),
  par = c(0.01673357,
          -0.003207127,
          0.6303083,
          0.3039488,
          -0.8643435,
          0.1146415,
          0.1131005),
  se = c(0.006311843,
         0.000424926,
         0.23327,
         0.4052165,
         0.5663609,
         0.214011,
         0.755714))
mersd.estimates$rr <- exp(mersd.estimates$par)
mersd.estimates$upper <- exp(mersd.estimates$par + (1.96*mersd.estimates$se))
mersd.estimates$lower <- exp(mersd.estimates$par - (1.96*mersd.estimates$se))

par(mar=c(2.1,5.1,2.1,5.1))
p2 <- ggplot(mersd.estimates, aes(var,rr, size=5)) + theme_bw() + theme(legend.position="none", axis.text=element_text(size=15), axis.title=element_text(size=20))
p2 + geom_point() +geom_errorbar(aes(x = var, ymin = lower, ymax = upper, size=1), width = 0.2) + scale_y_log10(limits=c(0.1, 10), breaks=c(0.1, 0.5, 1, 2, 5, 10)) + xlab("Risk Factor") + ylab("Relative Risk (Death)")

merss.estimates = data.frame(
  var   = c("Age","Onset","Comorbidity","HCW","Secondary Case","Female"),
  par = c(0.01229117,
          -0.001474409,
          0.4512598,
          -0.3774144,
          -0.2172952,
          -0.055447),
  se = c(0.002821169,
         0.000223982,
         0.1111523,
         0.2557504,
         0.2107082,
         0.09399749))
merss.estimates$rr <- exp(merss.estimates$par)
merss.estimates$upper <- exp(merss.estimates$par + (1.96*merss.estimates$se))
merss.estimates$lower <- exp(merss.estimates$par - (1.96*merss.estimates$se))

par(mar=c(2.1,5.1,2.1,5.1))
p2 <- ggplot(merss.estimates, aes(var,rr, size=5)) + theme_bw() + theme(legend.position="none", axis.text=element_text(size=15), axis.title=element_text(size=20))
p2 + geom_point() +geom_errorbar(aes(x = var, ymin = lower, ymax = upper, size=1), width = 0.2) + scale_y_log10(limits=c(0.25, 3), breaks=c(0.1, 0.5, 1, 2, 5, 10)) + xlab("Risk Factor") + ylab("Relative Risk (Severe Disease)")
