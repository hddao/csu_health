# https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-020-01022-x#Sec15
# Parker 2020 Using multiple agreement methods for continuous repeated measures data: a tutorial for practitioners


require(lme4)
data <- read.table("copd.csv", sep = ",", fill = F, header = T)
y <- as.vector(c(data$RRox,data$RRcb))
subject <- as.vector(as.factor(c(data$PatientID,data$PatientID)))
device <- as.vector(as.factor(c(rep(1,385),rep(2,385))))
activity <- c(as.factor(data$Activity),as.factor(data$Activity))
copd <- data.frame(y, subject, device, activity)
copd$device <- as.factor(copd$device)
copd$activity <- as.factor(copd$activity)
copd$subject <- as.factor(copd$subject)
#Mixed effects model (1) in the main paper
res <- lmer(y ~ device + (1|subject) + (1|activity) +
(1|subject:activity) + (1|subject:device) + (1|activity:device),
data = copd,
control = lmerControl(optimizer = "bobyqa")
)
summary(res)
## Linear mixed model fit by REML ['lmerMod']
## Formula:
## y ~ device + (1 | subject) + (1 | activity) + (1 | subject:activity) +
## (1 | subject:device) + (1 | activity:device)
## Data: copd
## Control: lmerControl(optimizer = "bobyqa")
##
## REML criterion at convergence: 4330.4
##
## Scaled residuals:
## Min 1Q Median 3Q Max
## -4.6334 -0.4680 0.0167 0.5096 5.2893
##
## Random effects:
## Groups Name Variance Std.Dev.
## subject:activity (Intercept) 6.0046 2.4504
## subject:device (Intercept) 0.3774 0.6144
## activity:device (Intercept) 3.6937 1.9219
## subject (Intercept) 11.3869 3.3745
## activity (Intercept) 16.5660 4.0701
## Residual 10.4983 3.2401
## Number of obs: 770, groups:
## subject:activity, 223; subject:device, 42; activity:device, 22; subject, 21; activity, 11
##
## Fixed effects:
## Estimate Std. Error t value
## (Intercept) 22.1117 1.5718 14.068
## device2 -1.2826 0.8882 -1.444
##
## Correlation of Fixed Effects:
## (Intr)
## device2 -0.283
beta2.est <- coef(summary(res))[2]
sigma2.alpha.est <- as.numeric(summary(res)$varcor[4])
sigma2.gamma.est <- as.numeric(summary(res)$varcor[5])
sigma2.alpha.gamma.est <- as.numeric(summary(res)$varcor[1])
sigma2.alpha.beta.est <- as.numeric(summary(res)$varcor[2])
sigma2.beta.gamma.est <- as.numeric(summary(res)$varcor[3])
sigma2.epsilon.est <- as.numeric(summary(res)$sigma)^2
phi2.beta.est <- beta2.est^2
#Concordance correlation coefficient
num_ccc <- sigma2.alpha.est + sigma2.gamma.est + sigma2.alpha.gamma.est
den_ccc <- sigma2.alpha.est + phi2.beta.est + sigma2.gamma.est +
sigma2.alpha.gamma.est + sigma2.alpha.beta.est +
sigma2.beta.gamma.est + sigma2.epsilon.est
CCC <- num_ccc/den_ccc
CCC
## [1] 0.676822
#Mean squared deviation
MSD <- (beta2.est^2) + 2*(sigma2.alpha.beta.est+sigma2.beta.gamma.est+sigma2.epsilon.est)
MSD
## [1] 30.78389
#Total deviation index
p <- 0.95
TDI <- qnorm((1+p)/2)*sqrt(MSD)
TDI
## [1] 10.87451
#Coverage probability
delta <- 5
CP <- 1-2*(1-pnorm(delta/sqrt(MSD)))
CP
## [1] 0.6325038
#Coefficient of individual agreement
CIA <- 2*sigma2.epsilon.est/MSD
CIA
## [1] 0.6820637
#Limits of agreement (mixed model approach--modelling the differences)
d <- copd$y[386:770]-copd$y[1:385]
subject <- as.factor(copd$subject[1:385])
activity <- as.factor(copd$activity[1:385])
res.diff <- lmer(d ~ (1|subject) + (1|activity),
control = lmerControl(optimizer = "bobyqa")
)
summary(res.diff)
## Linear mixed model fit by REML ['lmerMod']
## Formula: d ~ (1 | subject) + (1 | activity)
## Control: lmerControl(optimizer = "bobyqa")
##
## REML criterion at convergence: 2231.1
##
## Scaled residuals:
## Min 1Q Median 3Q Max
## -4.2106 -0.4468 -0.0138 0.3886 5.8757
##
## Random effects:
## Groups Name Variance Std.Dev.
## subject (Intercept) 0.9602 0.9799
## activity (Intercept) 7.5652 2.7505
## Residual 17.3753 4.1684
## Number of obs: 385, groups: subject, 21; activity, 11
##
## Fixed effects:
## Estimate Std. Error t value
## (Intercept) -1.273 0.895 -1.422
totalsd <- sqrt(as.numeric(summary(res.diff)$varcor[1])+
as.numeric(summary(res.diff)$varcor[2])+
as.numeric(summary(res.diff)$sigma^2)
)
res.diff.1 <- lmer(d ~ 1 + (1|subject),
control = lmerControl(optimizer = "bobyqa")
)
summary(res.diff.1)
## Linear mixed model fit by REML ['lmerMod']
## Formula: d ~ 1 + (1 | subject)
## Control: lmerControl(optimizer = "bobyqa")
##
## REML criterion at convergence: 2297.7
##
## Scaled residuals:
## Min 1Q Median 3Q Max
## -4.7264 -0.4244 0.1472 0.3797 5.9935
##
## Random effects:
## Groups Name Variance Std.Dev.
## subject (Intercept) 0.67 0.8186
## Residual 22.36 4.7288
## Number of obs: 385, groups: subject, 21
##
## Fixed effects:
## Estimate Std. Error t value
## (Intercept) -1.5960 0.3001 -5.317
meanb <- coef(summary(res.diff.1))[1]
meanb
## [1] -1.595991
alpha <- 0.05
z <- qnorm(1-alpha/2)
lcl <- meanb - z*totalsd
ucl <- meanb + z*totalsd
lcl; ucl
## [1] -11.57078
## [1] 8.378797
#limits of agreement (mixed model approach--raw data)
ll_raw <- beta2.est - z*sqrt(2*sigma2.alpha.beta.est + 2*sigma2.beta.gamma.est + 2*sigma2.epsilon.est)
ul_raw <- beta2.est + z*sqrt(2*sigma2.alpha.beta.est + 2*sigma2.beta.gamma.est + 2*sigma2.epsilon.est)
ll_raw; ul_raw
## [1] -11.86257
## [1] 9.297345
beta2.est
## [1] -1.282612


####bootstrap procedure
set.seed(123)
n <- 21
B <- 500
resb <- resdb <- resd1b <- list()
for(l in 1:B){
ind <- sample(1:n, n, replace = TRUE)
subject_boot <- list()
for(j in 1:n){
subject_boot[[j]] <- data[data$PatientID==ind[j],c(1,3,5,12)]
}
datab <- rbind(subject_boot[[1]], subject_boot[[2]], subject_boot[[3]], subject_boot[[4]],
subject_boot[[5]], subject_boot[[6]], subject_boot[[7]], subject_boot[[8]],
subject_boot[[9]], subject_boot[[10]], subject_boot[[11]], subject_boot[[12]],
subject_boot[[13]], subject_boot[[14]], subject_boot[[15]], subject_boot[[16]],
subject_boot[[17]], subject_boot[[18]], subject_boot[[19]], subject_boot[[20]],
subject_boot[[21]]
)
yb <- as.vector(c(datab$RRox,datab$RRcb))
aux <- c(rep(1,nrow(subject_boot[[1]])),rep(2,nrow(subject_boot[[2]])),
rep(3,nrow(subject_boot[[3]])),rep(4,nrow(subject_boot[[4]])),
rep(5,nrow(subject_boot[[5]])),rep(6,nrow(subject_boot[[6]])),
rep(7,nrow(subject_boot[[7]])),rep(8,nrow(subject_boot[[8]])),
rep(9,nrow(subject_boot[[9]])),rep(10,nrow(subject_boot[[10]])),
rep(11,nrow(subject_boot[[11]])),rep(12,nrow(subject_boot[[12]])),
rep(13,nrow(subject_boot[[13]])),rep(14,nrow(subject_boot[[14]])),
rep(15,nrow(subject_boot[[15]])),rep(16,nrow(subject_boot[[16]])),
rep(17,nrow(subject_boot[[17]])),rep(18,nrow(subject_boot[[18]])),
rep(19,nrow(subject_boot[[19]])),rep(20,nrow(subject_boot[[20]])),
rep(21,nrow(subject_boot[[21]]))
)
subjectb <- as.vector(as.factor(c(aux,aux)))
deviceb <- as.vector(as.factor(c(rep(1,nrow(datab)),rep(2,nrow(datab)))))
activityb <- c(as.factor(datab$Activity),as.factor(datab$Activity))
db <- yb[(nrow(datab)+1):(2*nrow(datab))] - yb[1:nrow(datab)]
copdb <- data.frame(yb, subjectb, deviceb, activityb, db)
copdb$deviceb <- as.factor(copdb$deviceb)
copdb$activityb <- as.factor(copdb$activityb)
copdb$subjectb <- as.factor(copdb$subjectb)
resb[[l]] <- lmer(yb ~ deviceb+(1|subjectb)+(1|activityb)+
(1|subjectb:activityb)+(1|subjectb:deviceb)+(1|activityb:deviceb),
data = copdb,
control = lmerControl(optimizer = "bobyqa")
)
resdb[[l]] <- lmer(db ~ (1|subjectb) + (1|activityb),
data = copdb,
control = lmerControl(optimizer = "bobyqa")
)
resd1b[[l]] <- lmer(db ~ 1 + (1|subjectb),
data = copdb,
control = lmerControl(optimizer = "bobyqa")
)
}
beta2.est.b <- numeric(B)
sigma2.alpha.est.b <- sigma2.gamma.est.b <- numeric(B)
sigma2.alpha.gamma.est.b <- sigma2.alpha.beta.est.b <- numeric(B)
sigma2.beta.gamma.est.b <- sigma2.epsilon.est.b <- numeric(B)
phi2.beta.est.b <- numeric(B)
CCCb <- TDIb <- CPb <- CIAb <- MSDb <- numeric(B)
meanbb <- totalsdb <- numeric(B)
lclb <- uclb <- ll_rawb <- ul_rawb <- numeric(B)
for(l in 1:B){
beta2.est.b[l] <- coef(summary(resb[[l]]))[2]
sigma2.alpha.est.b[l] <- as.numeric(summary(resb[[l]])$varcor[4])
sigma2.gamma.est.b[l] <- as.numeric(summary(resb[[l]])$varcor[5])
sigma2.alpha.gamma.est.b[l] <- as.numeric(summary(resb[[l]])$varcor[1])
sigma2.alpha.beta.est.b[l] <- as.numeric(summary(resb[[l]])$varcor[2])
sigma2.beta.gamma.est.b[l] <- as.numeric(summary(resb[[l]])$varcor[3])
sigma2.epsilon.est.b[l] <- as.numeric(summary(resb[[l]])$sigma)^2
phi2.beta.est.b[l] <- beta2.est.b[l]^2
num_ccc.b <- sigma2.alpha.est.b[l] + sigma2.gamma.est.b[l] + sigma2.alpha.gamma.est.b[l]
den_ccc.b <- sigma2.alpha.est.b[l] + phi2.beta.est.b[l] +
sigma2.gamma.est.b[l] + sigma2.alpha.gamma.est.b[l] +
sigma2.alpha.beta.est.b[l] + sigma2.beta.gamma.est.b[l] +
sigma2.epsilon.est.b[l]
CCCb[l] <- num_ccc.b/den_ccc.b
MSDb[l] <- (beta2.est.b[l]^2) +
2*(sigma2.alpha.beta.est.b[l]+sigma2.beta.gamma.est.b[l]+sigma2.epsilon.est.b[l])
TDIb[l] <- qnorm((1+p)/2)*sqrt(MSDb[l])
CPb[l] <- 1-2*(1-pnorm(delta/sqrt(MSDb[l])))
CIAb[l] <- 2*sigma2.epsilon.est.b[l]/MSDb[l]
totalsdb[l] <- sqrt(as.numeric(summary(resdb[[l]])$varcor[1])+
as.numeric(summary(resdb[[l]])$varcor[2])+
as.numeric(summary(resdb[[l]])$sigma^2)
)
meanbb[l] <- coef(summary(resd1b[[l]]))[1]
lclb[l] <- meanbb[l] - z*totalsdb[l]
uclb[l] <- meanbb[l] + z*totalsdb[l]
ll_rawb[l] <- beta2.est.b[l] - z*sqrt(2*sigma2.alpha.beta.est.b[l] +
2*sigma2.beta.gamma.est.b[l] + 2*sigma2.epsilon.est.b[l])
ul_rawb[l] <- beta2.est.b[l] + z*sqrt(2*sigma2.alpha.beta.est.b[l] +
2*sigma2.beta.gamma.est.b[l] + 2*sigma2.epsilon.est.b[l])
}
MSD; quantile(MSDb, c(0.025,0.975))
## [1] 30.78389
## 2.5% 97.5%
## 22.98767 41.65718
CCC; quantile(CCCb, c(0.025,0.975))
## [1] 0.676822
## 2.5% 97.5%
## 0.5960483 0.7208343
TDI; quantile(TDIb, c(0.025,0.975))
## [1] 10.87451
## 2.5% 97.5%
## 9.397085 12.650072
CP; quantile(CPb, c(0.025,0.975))
## [1] 0.6325038
## 2.5% 97.5%
## 0.5614741 0.7029882
CIA; quantile(CIAb, c(0.025,0.975))
## [1] 0.6820637
## 2.5% 97.5%
## 0.5653724 0.7526850
lb <- quantile(lclb, c(0.025, 0.975))
lcl; lb
## [1] -11.57078
## 2.5% 97.5%
## -13.515309 -9.937825
ub <- quantile(uclb, c(0.025, 0.975))
ucl; ub
## [1] 8.378797
## 2.5% 97.5%
## 6.372063 10.690036
meanbbq <- quantile(meanbb, c(0.025, 0.975))
meanb; meanbbq
## [1] -1.595991
## 2.5% 97.5%
## -2.1307454 -0.9747718
lrawb <- quantile(ll_rawb, c(0.025, 0.975))
ll_raw; lrawb
## [1] -11.86257
## 2.5% 97.5%
## -13.73020 -10.33243
urawb <- quantile(ul_rawb, c(0.025, 0.975))
ul_raw; urawb
## [1] 9.297345
## 2.5% 97.5%
## 7.372611 11.421008
b2b <- quantile(beta2.est.b, c(0.025, 0.975))
beta2.est; b2b
## [1] -1.282612
## 2.5% 97.5%
## -1.8945034 -0.4927148
m <- (copd$y[1:385] + copd$y[386:770])/2
plot(m, d, xlab = "Average", ylab = "Difference", ylim = c(-30,30))
abline(h=ucl, lwd = 2, lty = 2)
abline(h=ub[1], lwd = 3, lty = 3)
abline(h=ub[2], lwd = 3, lty = 3)
abline(h=lcl, lwd = 2, lty = 2)
abline(h=lb[1], lwd = 3, lty = 3)
abline(h=lb[2], lwd = 3, lty = 3)
abline(h=0, lwd = 2)
abline(h=meanb, lwd = 2, lty = 2)
abline(h=meanbbq[1], lwd = 3, lty = 3)
abline(h=meanbbq[2], lwd = 3, lty = 3)
