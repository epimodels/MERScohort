# Analysis of DICON PCR Timeseries Data Set
# Loading required libraries
library(ggplot2)
library(ez)

mersd.estimates = data.frame(
  var   = c("Age","Onset","Comorbidity","Animal Contact","HCW","Secondary Case","Female"),
  par = c(0.0141434,-0.002945124*10,0.6236935,0.4284723,0.1274188,-1.023623,0.160026),
  se = c(0.006021086,0.0004229502,0.2337606,0.377595,0.9802499,0.3652723,0.215024))
mersd.estimates$rr <- exp(mersd.estimates$par)
mersd.estimates$upper <- exp(mersd.estimates$par + (1.96*mersd.estimates$se))
mersd.estimates$lower <- exp(mersd.estimates$par - (1.96*mersd.estimates$se))

#postscript("hospitalspecific.eps", width=14, height=12)
par(mar=c(2.1,5.1,2.1,5.1))
p2 <- ggplot(mersd.estimates, aes(var,rr, size=5), xlab="Risk Factor", ylab="RR") + theme_bw(base_size=25) + opts(legend.position="none", axis.text.x=theme_text(size=15), axis.text.y=theme_text(size=20))
p2 + geom_point() +geom_errorbar(aes(x = var, ymin = lower, ymax = upper, size=1), width = 0.2) + scale_y_log10(limits=c(0.1, 10), breaks=c(0.1, 0.5, 1, 2, 5, 10)) + xlab("Risk Factor") + ylab("Relative Risk (Death)")
#dev.off()

merss.estimates = data.frame(
  var   = c("Age","Onset","Comorbidity","HCW","Secondary Case","Female"),
  par = c(0.01059699,-0.001355181*10,0.3861474,-0.8935945,-0.3927862,-0.08332688),
  se = c(0.002953264,0.0002339535,0.1245042,0.2564214,0.2033796,0.08943979))
merss.estimates$rr <- exp(merss.estimates$par)
merss.estimates$upper <- exp(merss.estimates$par + (1.96*merss.estimates$se))
merss.estimates$lower <- exp(merss.estimates$par - (1.96*merss.estimates$se))

#postscript("hospitalspecific.eps", width=14, height=12)
par(mar=c(2.1,5.1,2.1,5.1))
p2 <- ggplot(merss.estimates, aes(var,rr, size=5), xlab="Risk Factor", ylab="RR") + theme_bw(base_size=25) + opts(legend.position="none", axis.text.x=theme_text(size=15), axis.text.y=theme_text(size=20))
p2 + geom_point() +geom_errorbar(aes(x = var, ymin = lower, ymax = upper, size=1), width = 0.2) + scale_y_log10(limits=c(0.1, 10), breaks=c(0.1, 0.5, 1, 2, 5, 10)) + xlab("Risk Factor") + ylab("Relative Risk (Severe Disease)")
#dev.off()