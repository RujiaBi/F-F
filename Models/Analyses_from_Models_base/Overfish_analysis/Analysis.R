# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Packages
library('readr')
library('tibble')
library('dplyr')
library('ggplot2')
library('ggpubr')

#####################################################################################################

# Define directories
dat <- read.csv("new_data_council_model.csv", as.is=T)
dat$F_Y <- dat$B_Y <- NA

dat[!is.na(dat$B_by_Bmsy_recent) & dat$B_by_Bmsy_recent >= 0.5,]$B_Y <- 0   # not overfished
dat[!is.na(dat$B_by_Bmsy_recent) & dat$B_by_Bmsy_recent < 0.5,]$B_Y <- 1  # overfished

dat[!is.na(dat$F_by_Fmsy_recent) & dat$F_by_Fmsy_recent <= 1,]$F_Y <- 0  # not overfishing
dat[!is.na(dat$F_by_Fmsy_recent) & dat$F_by_Fmsy_recent > 1,]$F_Y <- 1  # overfishing



## B/Bmsy ##
dd_b <- dat[!is.na(dat$B_by_Bmsy_recent) & !is.na(dat$B_by_Bmsy_old),]
plot(dd_b$B_by_Bmsy_old, dd_b$B_Y)
dd_b[dd_b$B_by_Bmsy_old>8,]  # HADVIIb-k, HADNEAR 

model1 <- glm(B_Y ~ B_by_Bmsy_old, family=binomial(link='logit'), data=dd_b)
summary(model1)

# grad the inverse link function
ilink <- family(model1)$linkinv

# add fit and se.fit on the **link** scale
dd_b <- bind_cols(dd_b, setNames(as_tibble(predict(model1, dd_b, se.fit = TRUE)[1:2]),
                                 c('fit_link','se_link')))

###################################################################

dd_b1 <- as.data.frame(1)
colnames(dd_b1) <- c("B_by_Bmsy_old")

dd_b1 <- bind_cols(dd_b1, setNames(as_tibble(predict(model1, dd_b1, se.fit = TRUE)[1:2]),
                                   c('fit_link','se_link')))
dd_b1 <- mutate(dd_b1,
                fit_resp  = ilink(fit_link),
                right_upr = ilink(fit_link + (2 * se_link)),
                right_lwr = ilink(fit_link - (2 * se_link)))

###################################################################

# create the interval and backtransform
dd_b <- mutate(dd_b,
                fit_resp  = ilink(fit_link),
                right_upr = ilink(fit_link + (2 * se_link)),
                right_lwr = ilink(fit_link - (2 * se_link)))

pb <- ggplot(data = dd_b[dd_b$B_by_Bmsy_old<=5,], aes(x = B_by_Bmsy_old, y = fit_resp)) +
  geom_line(colour="red", size=1.2) +
  geom_ribbon(data=dd_b[dd_b$B_by_Bmsy_old<=5,], aes(ymin = right_lwr, ymax = right_upr), alpha = 0.3) +
  geom_point(data=dd_b[dd_b$B_by_Bmsy_old<=5,], aes(x = B_by_Bmsy_old, y = B_Y), size=1.5) +
  geom_vline(xintercept = 0.5, linetype="dotted", color = "blue", size=1.1) +
  ggtitle("(a)") +
  xlab(expression(italic(B[ty]/B[MSY])~~~from~~older~~assessment)) +
  ylab("Overfished status of recent assessment") +
  ylim(0,1) +
  theme_bw() + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        plot.title = element_text(size=20,hjust=0.5,face = "bold"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"))



## F/Fmsy ##
dd_f <- dat[!is.na(dat$F_by_Fmsy_recent) & !is.na(dat$F_by_Fmsy_old),]
plot(dd_f$F_by_Fmsy_old, dd_f$F_Y)

model2 <- glm(F_Y ~ F_by_Fmsy_old, family=binomial(link='logit'), data=dd_f)
summary(model2)

# grad the inverse link function
ilink <- family(model2)$linkinv

# add fit and se.fit on the **link** scale
dd_f <- bind_cols(dd_f, setNames(as_tibble(predict(model2, dd_f, se.fit = TRUE)[1:2]),
                                 c('fit_link','se_link')))

# create the interval and backtransform
dd_f <- mutate(dd_f,
               fit_resp  = ilink(fit_link),
               right_upr = ilink(fit_link + (2 * se_link)),
               right_lwr = ilink(fit_link - (2 * se_link)))

###################################################################

dd_f1 <- as.data.frame(0.5)
colnames(dd_f1) <- c("F_by_Fmsy_old")

dd_f1 <- bind_cols(dd_f1, setNames(as_tibble(predict(model2, dd_f1, se.fit = TRUE)[1:2]),
                                   c('fit_link','se_link')))
dd_f1 <- mutate(dd_f1,
                fit_resp  = ilink(fit_link),
                right_upr = ilink(fit_link + (2 * se_link)),
                right_lwr = ilink(fit_link - (2 * se_link)))

###################################################################

pf <- ggplot(data = dd_f, aes(x = F_by_Fmsy_old, y = fit_resp)) +
  geom_line(colour="red", size=1.2) +
  geom_ribbon(data=dd_f, aes(ymin = right_lwr, ymax = right_upr), alpha = 0.3) +
  geom_point(data=dd_f, aes(x = F_by_Fmsy_old, y = F_Y), size=1.5) +
  geom_vline(xintercept = 1, linetype="dotted", color = "blue", size=1.1) +
  ggtitle("(b)") +
  xlab(expression(italic(F[ty]/F[MSY])~~~from~~older~~assessment)) +
  ylab("Overfishing status of recent assessment") +
  ylim(0,1) +
  theme_bw() + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y   = element_text(size=16),
        axis.text.x   = element_text(size=16),
        axis.title.y  = element_text(size=18),
        axis.title.x  = element_text(size=18),
        plot.title = element_text(size=20,hjust=0.5,face = "bold"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"))

p <- ggarrange(pb, pf, nrow=1)

png("Overfish_prob.png", height = 7.5, width = 14, units='in', res=600)
p 
dev.off()

