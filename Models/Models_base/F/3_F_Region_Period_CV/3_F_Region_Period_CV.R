# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Packages
source('util.R')

#####################################################################################################

# Define directories
dat <- read.csv("new_data_council_model_revision.csv", as.is=T)
dat[!is.na(dat$F_recent) & dat$F_recent==0,]$F_recent <- NA
dat[!is.na(dat$F_old) & dat$F_old==0,]$F_old <- NA
dat[!is.na(dat$F_by_Fmsy_recent) & dat$F_by_Fmsy_recent==0,]$F_by_Fmsy_recent <- NA
dat[!is.na(dat$F_by_Fmsy_old) & dat$F_by_Fmsy_old==0,]$F_by_Fmsy_old <- NA
dat0 <- dat[!is.na(dat$F_recent) & !is.na(dat$F_old),]
dat0[dat0$num_assess==2,]$stockid_code <- as.numeric(as.factor(dat0[dat0$num_assess==2,]$stockid))
dat0[dat0$num_assess==3,]$stockid_code <- as.numeric(as.factor(dat0[dat0$num_assess==3,]$stockid))
dat0[dat0$num_assess==4,]$stockid_code <- as.numeric(as.factor(dat0[dat0$num_assess==4,]$stockid))
dat0[dat0$num_assess==5,]$stockid_code <- as.numeric(as.factor(dat0[dat0$num_assess==5,]$stockid))
dat0[dat0$num_assess==6,]$stockid_code <- as.numeric(as.factor(dat0[dat0$num_assess==6,]$stockid))
dat0[dat0$num_assess==7,]$stockid_code <- as.numeric(as.factor(dat0[dat0$num_assess==7,]$stockid))
dat0[dat0$num_assess==8,]$stockid_code <- as.numeric(as.factor(dat0[dat0$num_assess==8,]$stockid))
dd <- dat0[,c(3,28,8,9,11:26,30:36)]

data_2ass <- dd[dd$num_assess==2,]
data_3ass <- dd[dd$num_assess==3,]
data_4ass <- dd[dd$num_assess==4,]
data_5ass <- dd[dd$num_assess==5,]
data_6ass <- dd[dd$num_assess==6,]
data_7ass <- dd[dd$num_assess==7,]
data_8ass <- dd[dd$num_assess==8,]

## Stock number
stocknum_2ass <- length(unique(data_2ass$stockid_code))
stocknum_3ass <- length(unique(data_3ass$stockid_code))
stocknum_4ass <- length(unique(data_4ass$stockid_code))
stocknum_5ass <- length(unique(data_5ass$stockid_code))
stocknum_6ass <- length(unique(data_6ass$stockid_code))
stocknum_7ass <- length(unique(data_7ass$stockid_code))
stocknum_8ass <- length(unique(data_8ass$stockid_code))

#################################################################################################################

data = list(data_2ass = as.matrix(data_2ass),
            data_3ass = as.matrix(data_3ass),
            data_4ass = as.matrix(data_4ass),
            data_5ass = as.matrix(data_5ass),
            data_6ass = as.matrix(data_6ass),
            data_7ass = as.matrix(data_7ass),
            data_8ass = as.matrix(data_8ass),
            
            # Stock number
            stocknum_2ass = as.numeric(stocknum_2ass),
            stocknum_3ass = as.numeric(stocknum_3ass),
            stocknum_4ass = as.numeric(stocknum_4ass),
            stocknum_5ass = as.numeric(stocknum_5ass),
            stocknum_6ass = as.numeric(stocknum_6ass),
            stocknum_7ass = as.numeric(stocknum_7ass),
            stocknum_8ass = as.numeric(stocknum_8ass),
            
            # Stock_compare_num
            stock_compare_num_3ass = as.numeric(dim(data_3ass)[1]),
            stock_compare_num_4ass = as.numeric(dim(data_4ass)[1]),
            stock_compare_num_5ass = as.numeric(dim(data_5ass)[1]),
            stock_compare_num_6ass = as.numeric(dim(data_6ass)[1]),
            stock_compare_num_7ass = as.numeric(dim(data_7ass)[1]),
            stock_compare_num_8ass = as.numeric(dim(data_8ass)[1]),
            
            # ty number (for stocks with 2+ assesses)
            tynum_3ass = 2,
            tynum_4ass = 3,
            tynum_5ass = 4,
            tynum_6ass = 5,
            tynum_7ass = 6,
            tynum_8ass = 7,
            
            # number of regions
            rs = 20)


# Parameters monitored
params = c("cv_global", "cv_region", "cv", "nu_r", "nu_tp", "mu_2ass", "mu_3ass" ,"mu_4ass", "mu_5ass", "mu_6ass", "mu_7ass", "mu_8ass",
           "loglik_2ass", "loglik_3ass", "loglik_4ass", "loglik_5ass", "loglik_6ass", "loglik_7ass", "loglik_8ass") 

# Run jags model
system.time(fit <- run.jags(model="3_F_Region_Period_CV.txt", monitor=params, data=data, burnin=20000, sample=1000, adapt=20000, thin=20, n.chains=5, method = 'rjparallel'))
save(fit, file = '3_F_Region_Period_CV_fit.RData')
fit0 = add.summary(fit)
save(fit0, file = '3_F_Region_Period_CV_fit0.RData')
dic=extract(fit, 'dic')
save(dic, file='3_F_Region_Period_CV_dic.RData')

load('3_F_Region_Period_CV_dic.RData')
load('3_F_Region_Period_CV_fit.RData')
sum(dic$deviance)+sum(dic$penalty)
# -8283.67

# waic & loo
mcall <- rbind(fit$mcmc[[1]], fit$mcmc[[2]], fit$mcmc[[3]], fit$mcmc[[4]], fit$mcmc[[5]])
mc_ll <- mcall[,c(which(colnames(mcall)=="loglik_2ass[1,1]"):which(colnames(mcall)=="loglik_8ass[112,2]"))]
library(loo)
waic(mc_ll)  # 656.4
loo(mc_ll)  # 774.6

mcmc <- as.mcmc(fit)
pdf("traceplot.pdf") 
traceplot(mcmc)
dev.off()
