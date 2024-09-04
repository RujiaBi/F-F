dat <- read.csv("new_data_council_model_revision.csv", as.is=T)

dat[!is.na(dat$F_recent) & dat$F_recent==0,]$F_recent <- NA
dat[!is.na(dat$F_old) & dat$F_old==0,]$F_old <- NA
dat[!is.na(dat$F_by_Fmsy_recent) & dat$F_by_Fmsy_recent==0,]$F_by_Fmsy_recent <- NA
dat[!is.na(dat$F_by_Fmsy_old) & dat$F_by_Fmsy_old==0,]$F_by_Fmsy_old <- NA

dat$log_B_recent <- log(dat$B_recent)
dat$log_B_old <- log(dat$B_old)
dat$log_F_recent <- log(dat$F_recent)
dat$log_F_old <- log(dat$F_old)
dat$log_Bmsy_recent <- log(dat$Bmsy_recent)
dat$log_Bmsy_old <- log(dat$Bmsy_old)
dat$log_Fmsy_recent <- log(dat$Fmsy_recent)
dat$log_Fmsy_old <- log(dat$Fmsy_old)
dat$log_B_by_Bmsy_recent <- log(dat$B_by_Bmsy_recent)
dat$log_B_by_Bmsy_old <- log(dat$B_by_Bmsy_old)
dat$log_F_by_Fmsy_recent <- log(dat$F_by_Fmsy_recent)
dat$log_F_by_Fmsy_old <- log(dat$F_by_Fmsy_old)
dat$log_OFL_ty_recent <- log(dat$OFL_ty_recent)
dat$log_OFL_ty_old <- log(dat$OFL_ty_old)


# Add labels
dat$B_label <- dat$F_label <- dat$Bmsy_label <- dat$Fmsy_label <- 
  dat$B_Bmsy_label <- dat$F_Fmsy_label <- dat$OFL_label <-NA
dat$col_B_label <- dat$col_F_label <- dat$col_Bmsy_label <- dat$col_Fmsy_label <- 
  dat$col_B_Bmsy_label <- dat$col_F_Fmsy_label <- dat$col_OFL_label <- 1

# F
dat[!is.na(dat$log_F_recent) & !is.na(dat$log_F_old) & dat$log_F_recent < (-2) & 
      dat$log_F_recent > (-4) & dat$log_F_old < (-4) & dat$log_F_old > (-6) &
      dat$stockid=="HERRVIaVIIbc",]$F_label <- 2  # HERRVIaVIIbc in Europe (EU)
dat[!is.na(dat$log_F_recent) & !is.na(dat$log_F_old) & dat$log_F_recent < (-4) & 
      dat$log_F_recent > (-6) & dat$log_F_old < (0) & 
      dat$log_F_old > (-2),]$F_label <- 3 # SABluelineTilefish in US South Atlantic, managed by SAFMC

# Bmsy
dat[!is.na(dat$log_Bmsy_recent) & !is.na(dat$log_Bmsy_old) & dat$log_Bmsy_recent < 8 &
      dat$log_Bmsy_old > 9,]$Bmsy_label <- 1  # BIGHTREDSE in Australia

# Fmsy
dat[dat$stockid == "ALPLAICBSAI" ,]$Fmsy_label <- 4  # ALPLAICBSAI in US Alaska, managed by NPFMC
dat[!is.na(dat$log_Fmsy_recent) & !is.na(dat$log_Fmsy_old) & 
      dat$log_Fmsy_recent > (-2) & dat$log_Fmsy_recent < (-1) & 
      dat$log_Fmsy_old < (-3),]$Fmsy_label <- 5  # QUAHATLC in US East Coast, managed by MAFMC

# B/Bmsy
dat[!is.na(dat$log_B_by_Bmsy_recent) & !is.na(dat$log_B_by_Bmsy_old) & 
      dat$log_B_by_Bmsy_recent < (-0.9) & dat$log_B_by_Bmsy_old > (0) & 
      dat$log_B_by_Bmsy_old < 1,]$B_Bmsy_label <- 6  # CODBA2224 in Europe (EU)
dat[!is.na(dat$log_B_by_Bmsy_recent) & !is.na(dat$log_B_by_Bmsy_old) & 
      dat$log_B_by_Bmsy_recent < (-1.6) & dat$log_B_by_Bmsy_old > (-1.1) & 
      dat$log_B_by_Bmsy_old < 0,]$B_Bmsy_label <- 7  # WHITVIa in Europe (EU)

# F/Fmsy
dat[!is.na(dat$log_F_by_Fmsy_recent) & !is.na(dat$log_F_by_Fmsy_old) & 
      dat$log_F_by_Fmsy_recent < (-2) & dat$log_F_by_Fmsy_old  < (0) & 
      dat$log_F_by_Fmsy_old > (-1),]$F_Fmsy_label <- 5  # QUAHATLC in US East Atlantic, managed by MAFMC 
dat[!is.na(dat$log_F_by_Fmsy_recent) & !is.na(dat$log_F_by_Fmsy_old) & 
      dat$log_F_by_Fmsy_recent < (-1.5) & dat$log_F_by_Fmsy_old  > (0) & 
      dat$stockid=="CODIS",]$F_Fmsy_label <- 8  # CODIS in Europe (EU)
dat[!is.na(dat$log_F_by_Fmsy_recent) & !is.na(dat$log_F_by_Fmsy_old) & 
      dat$log_F_by_Fmsy_recent > (-2) & dat$log_F_by_Fmsy_old  < (-3) & 
      dat$stockid=="ALPLAICBSAI",]$F_Fmsy_label <- 4  # ALPLAICBSAI in US Alaska, managed by NPFMC
dat[!is.na(dat$log_F_by_Fmsy_recent) & !is.na(dat$log_F_by_Fmsy_old) & 
      dat$log_F_by_Fmsy_recent > (-2) & dat$log_F_by_Fmsy_recent > (-0.5) &
      dat$log_F_by_Fmsy_old  > (-3) & dat$log_F_by_Fmsy_old  < (-2),]$F_Fmsy_label <- 7  # WHITVIa in Europe (EU)

# OFL
dat[!is.na(dat$log_OFL_ty_recent) & !is.na(dat$log_OFL_ty_old) & 
      dat$log_OFL_ty_recent > 12 & dat$log_OFL_ty_old  > 10 &
      dat$council=="MAFMC" & dat$stockid=="QUAHATLC",]$OFL_label <- 5  # QUAHATLC in US East Coast, managed by MAFMC


# Set colours
# 1  # BIGHTREDSE in Australia
# 2  # HERRVIaVIIbc in Europe (EU)  (Changes in modeling method)
# 3  # SABluelineTilefish in US South Atlantic, managed by SAFMC  (Changes in modeling method)
# 4  # ALPLAICBSAI in US Alaska, managed by NPFMC  (Changes in natural mortality)
# 5  # QUAHATLC in US East Coast, managed by MAFMC  (Changes in definitions of reference points)
# 6  # CODBA2224 in Europe (EU)   (Changes in definitions of reference points)
# 7  # WHITVIa in Europe (EU)   (Changes in definitions of reference points)
# 8  # CODIS in Europe (EU)  (Combinations???)

# input data change = 2 (point 1), method change = 3 (points 2 & 3), 
# natural mortality change = 4 (point 4), definition change = 5 (points 5, 6 & 7), 
# estimation procedure change = 6 (point 8)
# F: 2, 3
dat[!is.na(dat$F_label) & dat$F_label==2,]$col_F_label <- 3
dat[!is.na(dat$F_label) & dat$F_label==3,]$col_F_label <- 3

# Bmsy: 1
dat[!is.na(dat$Bmsy_label) & dat$Bmsy_label==1,]$col_Bmsy_label <- 2

# Fmsy: 4, 5
dat[!is.na(dat$Fmsy_label) & dat$Fmsy_label==4,]$col_Fmsy_label <- 4
dat[!is.na(dat$Fmsy_label) & dat$Fmsy_label==5,]$col_Fmsy_label <- 5

# B/Bmsy: 3, 6, 7
dat[!is.na(dat$B_Bmsy_label) & dat$B_Bmsy_label==6,]$col_B_Bmsy_label <- 5
dat[!is.na(dat$B_Bmsy_label) & dat$B_Bmsy_label==7,]$col_B_Bmsy_label <- 5

# F/Fmsy: 3, 4, 5, 7, 8
dat[!is.na(dat$F_Fmsy_label) & dat$F_Fmsy_label==4,]$col_F_Fmsy_label <- 4
dat[!is.na(dat$F_Fmsy_label) & dat$F_Fmsy_label==5,]$col_F_Fmsy_label <- 5
dat[!is.na(dat$F_Fmsy_label) & dat$F_Fmsy_label==7,]$col_F_Fmsy_label <- 5
dat[!is.na(dat$F_Fmsy_label) & dat$F_Fmsy_label==8,]$col_F_Fmsy_label <- 6

# OFL: 5
dat[!is.na(dat$OFL_label) & dat$OFL_label==5,]$col_OFL_label <- 5


dat[!is.na(dat$log_B_by_Bmsy_recent) & !is.na(dat$log_B_by_Bmsy_old) & 
      dat$log_B_by_Bmsy_recent < (-0.9) & dat$log_B_by_Bmsy_old > (0) & 
      dat$log_B_by_Bmsy_old < 1,]$B_Bmsy_label <- NA  # CODBA2224 in Europe (EU)
dat[!is.na(dat$log_B_by_Bmsy_recent) & !is.na(dat$log_B_by_Bmsy_old) & 
      dat$log_B_by_Bmsy_recent < (-0.9) & dat$log_B_by_Bmsy_old > (0) & 
      dat$log_B_by_Bmsy_old < 1 & dat$comparison==11,]$B_Bmsy_label <- 6  # CODBA2224 in Europe (EU)

dat[!is.na(dat$log_F_by_Fmsy_recent) & !is.na(dat$log_F_by_Fmsy_old) & 
      dat$log_F_by_Fmsy_recent > (-2) & dat$log_F_by_Fmsy_recent > (-0.5) &
      dat$log_F_by_Fmsy_old  > (-3) & dat$log_F_by_Fmsy_old  < (-2),]$F_Fmsy_label <- NA   # WHITVIa in Europe (EU)
dat[!is.na(dat$log_F_by_Fmsy_recent) & !is.na(dat$log_F_by_Fmsy_old) & 
      dat$log_F_by_Fmsy_recent > (-2) & dat$log_F_by_Fmsy_recent > (-0.5) &
      dat$log_F_by_Fmsy_old  > (-3) & dat$log_F_by_Fmsy_old  < (-2) &
      dat$comparison==5,]$F_Fmsy_label <- 7 # WHITVIa in Europe (EU)

dat[!is.na(dat$log_F_recent) & !is.na(dat$log_F_old) & dat$log_F_recent < (-2) & 
      dat$log_F_recent > (-4) & dat$log_F_old < (-4) & dat$log_F_old > (-6) &
      dat$stockid=="HERRVIaVIIbc",]$F_label <- NA  # HERRVIaVIIbc in Europe (EU)
dat[!is.na(dat$log_F_recent) & !is.na(dat$log_F_old) & dat$log_F_recent < (-2) & 
      dat$log_F_recent > (-4) & dat$log_F_old < (-4) & dat$log_F_old > (-6) &
      dat$stockid=="HERRVIaVIIbc" & dat$comparison==6,]$F_label <- 2  # HERRVIaVIIbc in Europe (EU)

dat[!is.na(dat$log_B_by_Bmsy_recent) & !is.na(dat$log_B_by_Bmsy_old) & 
      dat$log_B_by_Bmsy_recent < (-1.6) & dat$log_B_by_Bmsy_old > (-1.1) & 
      dat$log_B_by_Bmsy_old < 0,]$B_Bmsy_label <- NA  # WHITVIa in Europe (EU)
dat[!is.na(dat$log_B_by_Bmsy_recent) & !is.na(dat$log_B_by_Bmsy_old) & 
      dat$log_B_by_Bmsy_recent < (-1.6) & dat$log_B_by_Bmsy_old > (-1.1) & 
      dat$log_B_by_Bmsy_old < 0 & dat$comparison==11,]$B_Bmsy_label <- 7  # WHITVIa in Europe (EU)

dd_Bty <- dat[,c(27,1,5,37,38,57,64)]
dd_Fty <- dat[,c(27,1,5,39,40,56,63)]
dd_Bmsy <- dat[,c(27,1,5,41,42,55,62)]
dd_Fmsy <- dat[,c(27,1,5,43,44,54,61)]
dd_B_Bmsy <- dat[,c(27,1,5,45,46,53,60)]
dd_F_Fmsy <- dat[,c(27,1,5,47,48,52,59)]
dd_OFLty <- dat[,c(27,1,5,49,50,51,58)]
colnames(dd_Bty) <- colnames(dd_Fty) <- colnames(dd_Bmsy) <- colnames(dd_Fmsy) <- colnames(dd_B_Bmsy) <- 
  colnames(dd_F_Fmsy) <- colnames(dd_OFLty) <- c("Region", "stockid", "stock_comparison", "Estimate_recent", "Estimate_old", "label", "col")
dd_Bty$type <- "B_ty"
dd_Fty$type <- "F_ty"
dd_Bmsy$type <- "B_MSY"
dd_Fmsy$type <- "F_MSY"
dd_B_Bmsy$type <- "B_ty/B_MSY"
dd_F_Fmsy$type <- "F_ty/F_MSY"
dd_OFLty$type <- "OFL_ty"

# Fit linear regressiions
library("R2jags")
library('rjags')
library('runjags')
library('rlecuyer')
library('snow')
library('snowfall')
require(parallel)

# B_ty
data = list(recent_log = as.vector(dd_Bty$Estimate_recent),
            old_log = as.vector(dd_Bty$Estimate_old),
            nrow = dim(dd_Bty)[1])
params = c("beta","intercept")
fit <- run.jags(model="Linear_model.txt", monitor=params, data=data, burnin=2000, sample=1000, adapt=2000, thin=5, n.chains=5, method = 'rjparallel')
mcmc <- as.mcmc(fit)
summary(mcmc)
#            Mean     2.5%      50%     97.5%
# beta      0.9876   0.97709   0.9876   0.9971
# intercept 0.1319   0.02569   0.1311   0.2492


# Bmsy
dd_Bmsy <- dd_Bmsy[!is.na(dd_Bmsy$Estimate_old) & !is.na(dd_Bmsy$Estimate_recent),]
data = list(recent_log = as.vector(dd_Bmsy$Estimate_recent),
            old_log = as.vector(dd_Bmsy$Estimate_old),
            nrow = dim(dd_Bmsy)[1])
params = c("beta","intercept")
fit <- run.jags(model="Linear_model.txt", monitor=params, data=data, burnin=2000, sample=1000, adapt=2000, thin=5, n.chains=5, method = 'rjparallel')
mcmc <- as.mcmc(fit)
summary(mcmc)
#             Mean    2.5%      50%     97.5% 
# beta      0.9801   0.9708    0.9802   0.9886
# intercept 0.2165   0.1239    0.2154   0.3180


# B_Bmsy
dd_B_Bmsy <- dd_B_Bmsy[!is.na(dd_B_Bmsy$Estimate_old) & !is.na(dd_B_Bmsy$Estimate_recent),]
data = list(recent_log = as.vector(dd_B_Bmsy$Estimate_recent),
            old_log = as.vector(dd_B_Bmsy$Estimate_old),
            nrow = dim(dd_B_Bmsy)[1])
params = c("beta","intercept")
fit <- run.jags(model="Linear_model.txt", monitor=params, data=data, burnin=2000, sample=1000, adapt=2000, thin=5, n.chains=5, method = 'rjparallel')
mcmc <- as.mcmc(fit)
summary(mcmc)
#             Mean     2.5%       50%      97.5% 
# beta      0.94765   0.92010     0.94754  0.974626
# intercept -0.02165  -0.04188    -0.02161 -0.001094


# F_ty
dd_Fty <- dd_Fty[!is.na(dd_Fty$Estimate_old) & !is.na(dd_Fty$Estimate_recent),]
data = list(recent_log = as.vector(dd_Fty$Estimate_recent),
            old_log = as.vector(dd_Fty$Estimate_old),
            nrow = dim(dd_Fty)[1])
params = c("beta","intercept")
fit <- run.jags(model="Linear_model.txt", monitor=params, data=data, burnin=2000, sample=1000, adapt=2000, thin=5, n.chains=5, method = 'rjparallel')
mcmc <- as.mcmc(fit)
summary(mcmc)
#            Mean     2.5%      50%     97.5%
# beta      0.8618   0.8358   0.8619    0.8873
# intercept -0.1915  -0.2415  -0.1913   -0.1412


# Fmsy
dd_Fmsy <- dd_Fmsy[!is.na(dd_Fmsy$Estimate_old) & !is.na(dd_Fmsy$Estimate_recent),]
data = list(recent_log = as.vector(dd_Fmsy$Estimate_recent),
            old_log = as.vector(dd_Fmsy$Estimate_old),
            nrow = dim(dd_Fmsy)[1])
params = c("beta","intercept")
fit <- run.jags(model="Linear_model.txt", monitor=params, data=data, burnin=2000, sample=1000, adapt=2000, thin=5, n.chains=5, method = 'rjparallel')
mcmc <- as.mcmc(fit)
summary(mcmc)
#             Mean    2.5%      50%     97.5% 
# beta      0.7874   0.7565    0.7874   0.8175
# intercept -0.3067  -0.3523   -0.3067  -0.2625


# F_Fmsy
dd_F_Fmsy <- dd_F_Fmsy[!is.na(dd_F_Fmsy$Estimate_old) & !is.na(dd_F_Fmsy$Estimate_recent),]
data = list(recent_log = as.vector(dd_F_Fmsy$Estimate_recent),
            old_log = as.vector(dd_F_Fmsy$Estimate_old),
            nrow = dim(dd_F_Fmsy)[1])
params = c("beta","intercept")
fit <- run.jags(model="Linear_model.txt", monitor=params, data=data, burnin=2000, sample=1000, adapt=2000, thin=5, n.chains=5, method = 'rjparallel')
mcmc <- as.mcmc(fit)
summary(mcmc)
#             Mean     2.5%       50%      97.5% 
# beta      0.81566  0.779633   0.81543   0.85294
# intercept 0.02677  -0.001045  0.02657   0.05403


# OFLty
dd_OFLty <- dd_OFLty[!is.na(dd_OFLty$Estimate_old) & !is.na(dd_OFLty$Estimate_recent),]
data = list(recent_log = as.vector(dd_OFLty$Estimate_recent),
            old_log = as.vector(dd_OFLty$Estimate_old),
            nrow = dim(dd_OFLty)[1])
params = c("beta","intercept")
fit <- run.jags(model="Linear_model.txt", monitor=params, data=data, burnin=2000, sample=1000, adapt=2000, thin=5, n.chains=5, method = 'rjparallel')
mcmc <- as.mcmc(fit)
summary(mcmc)
#             Mean     2.5%       50%      97.5% 
# beta      0.998964  0.9883   0.999046    1.0092
# intercept -0.003101 -0.1046  -0.003843   0.1019



# Combine data
dd <- rbind(dd_Bty, dd_Fty, dd_Bmsy, dd_Fmsy, dd_B_Bmsy, dd_F_Fmsy, dd_OFLty)

# Cause 
dd$Potential_causes <- NA
dd[dd$col==2,]$Potential_causes <-  "Changes in input data"
dd[dd$col==3,]$Potential_causes <-  "Changes in underlying model structure"
dd[dd$col==4,]$Potential_causes <-  "Changes in natural mortality"
dd[dd$col==5,]$Potential_causes <-  "Changes in definitions of reference points"
dd[dd$col==6,]$Potential_causes <-  "Changes in estimation procedure"
dd$Potential_causes <- factor(dd$Potential_causes, levels=c("Changes in input data", "Changes in underlying model structure", "Changes in natural mortality",
                                                            "Changes in definitions of reference points", "Changes in estimation procedure"))

dd$Potential_causes <- factor(dd$Potential_causes, labels = c(expression("Changes in input data"), 
                                                              expression("Changes in underlying model \n structure"), 
                                                              expression("Changes in natural mortality"), 
                                                              expression("Changes in definitions of \n reference points"),
                                                              expression("Changes in estimation procedure")))


dd$type <- factor(dd$type, levels = c("B_ty", "B_MSY", "B_ty/B_MSY", "F_ty", "F_MSY", "F_ty/F_MSY", "OFL_ty"))

dd$type2 <- factor(dd$type, labels = c(expression(bolditalic(B[ty])), expression(bolditalic(B[MSY])), expression(bolditalic(B[ty]/B[MSY])), 
                                       expression(bolditalic(F[ty])), expression(bolditalic(F[MSY])), expression(bolditalic(F[ty]/F[MSY])),
                                       expression(bold(OFL[bolditalic(ty)]))))

dd[dd$Region=="Canada East Coast",]$Region <- "CA East Coast"
dd[dd$Region=="Canada West Coast",]$Region <- "CA West Coast"

dd$Region <- factor(dd$Region, levels = c("GMFMC", "MAFMC", "NEFMC", "NPFMC", "PFMC", "SAFMC", "US non-federal",
                                          "Europe (EU)", "Europe (non-EU)",  "CA East Coast", "CA West Coast", 
                                          "Australia", "New Zealand", "Japan", "South Africa", "South America",
                                          "AOHS", "IOHS", "POHS", "MBS"))



library(ggplot2)
library(ggpmisc)

tag_facet <- function(p, open = "(", close = ")", tag_pool = letters, x = -Inf, y = Inf, 
                      hjust = -0.5, vjust = 1.5, fontface = 2, family = "", ...) {
  gb <- ggplot_build(p)
  lay <- gb$layout$layout
  tags <- cbind(lay, label = paste0(open, tag_pool[lay$PANEL], close), x = x, y = y)
  p + geom_text(data = tags, aes_string(x = "x", y = "y", label = "label"), ..., hjust = hjust, 
                vjust = vjust, fontface = fontface, family = family, inherit.aes = FALSE)
}


cbPalette1 <- c(1:20)
cbPalette2 <- c("#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

p1 <- ggplot(data=dd, aes(x=Estimate_old, y=Estimate_recent)) +
  geom_point(data = dd, aes(x=Estimate_old, y=Estimate_recent, shape=Region), position=position_dodge(0), colour = "#999999", fill = "white", size = 2.6, stroke = 1.5) + 
  facet_wrap(vars(type2), labeller = label_parsed, scales = "free", strip.position = "top", nrow = 3, dir = "v") + 
  geom_abline(intercept = 0, slope = 1, size = 1, col="red") +
  geom_point(data = subset(dd, !is.na(dd$Potential_causes)), aes(x=Estimate_old, y=Estimate_recent, colour=Potential_causes, shape=Region), position=position_dodge(0), fill = "white", size = 3, stroke = 2.2) + 
  stat_poly_line(size=1.1) +
  #stat_poly_eq(aes(label = after_stat(eq.label)), size=5.5) +
  #stat_poly_eq(label.y = 0.9, aes(label = paste(after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")), size=5.5) +
  scale_shape_manual(values=cbPalette1) +
  scale_colour_manual(name = "Potential causes", values=cbPalette2) +
  geom_text(data = subset(dd, dd$label!= 6 & dd$label!=7), aes(x=Estimate_old, y=Estimate_recent, label=label),hjust=-0.7, vjust=0, col="black",size=7, fontface = "bold") +
  geom_text(data = subset(dd, dd$label==6), aes(x=Estimate_old, y=Estimate_recent, label=label),hjust=-0.8, vjust=0.6, col="black",size=7, fontface = "bold") +
  geom_text(data = subset(dd, dd$label==7), aes(x=Estimate_old, y=Estimate_recent, label=label),hjust=-0.8, vjust=0.7, col="black",size=7, fontface = "bold") +
  xlab("Old estimate (log-transformed)") +
  ylab("Recent estimate (log-transformed)") +
  theme_bw() + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y   = element_text(size=20),
        axis.text.x   = element_text(size=20),
        axis.title.y  = element_text(size=22),
        axis.title.x  = element_text(size=22),
        legend.title = element_text(size=22),
        legend.text = element_text(size=20),
        legend.position = c(0.84, 0.3),
        legend.box="vertical", 
        legend.margin=margin(-60,0,0,0),
        legend.spacing.y = unit(0.8, "lines"),
        strip.text.x = element_text(size=22, face = "bold"),
        strip.text.y = element_text(size=22, face = "bold"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines")) +
 guides(colour = guide_legend(nrow = 10, byrow = T, override.aes=list(size=4))) +
 guides(shape = guide_legend(nrow = 10, byrow = T))

my_tag <- c(expression("y=0.132+0.988x,\nintercept[0.026,0.249],\nslope[0.977,0.997]"), 
            expression("y=0.217+0.980x,\nintercept[0.124,0.318],\nslope[0.971,0.989]"), 
            expression("y=-0.022+0.948x,\nintercept[-0.042,-0.001],\nslope[0.920,0.975]"),
            expression("y=-0.192+0.862x,\nintercept[-0.242,-0.141],\nslope[0.836,0.887]"),
            expression("y=-0.307+0.787x,\nintercept[-0.352,-0.263],\nslope[0.757,0.818]"),
            expression("y=0.027+0.816x,\nintercept[-0.001,0.054],\nslope[0.780,0.853]"),
            expression("y=-0.003+0.999x,\nintercept[-0.105,0.102],\nslope[0.988,1.009]"))

p2 <- tag_facet(p1, 
                x = -Inf, y = -Inf, 
                vjust = -4.2, hjust = -0.1,
                open = "", close = "",
                fontface = 4,
                size = 5.5,
                tag_pool = my_tag)

png("Visualization.png", height = 16, width = 16, units='in', res=600)
p2
dev.off()

