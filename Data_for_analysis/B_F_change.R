dat <- read.csv("new_data_council_model.csv", as.is=T)

dat[!is.na(dat$F_recent) & dat$F_recent==0,]$F_recent <- NA
dat[!is.na(dat$F_old) & dat$F_old==0,]$F_old <- NA
dat[!is.na(dat$F_by_Fmsy_recent) & dat$F_by_Fmsy_recent==0,]$F_by_Fmsy_recent <- NA
dat[!is.na(dat$F_by_Fmsy_old) & dat$F_by_Fmsy_old==0,]$F_by_Fmsy_old <- NA

dat$B_change <- log(dat$B_recent)/log(dat$B_old)
dat$F_change <- log(dat$F_recent)/log(dat$F_old)
dat$Bmsy_change <- log(dat$Bmsy_recent)/log(dat$Bmsy_old)
dat$Fmsy_change <- log(dat$Fmsy_recent)/log(dat$Fmsy_old)

library('readr')
library('tibble')
library('dplyr')
library('ggplot2')
library('ggpubr')

p1 <- ggplot() +
  geom_point(data=dat, aes(x=B_change, y=Bmsy_change), position=position_dodge(0), shape = 21, colour = "#999999", fill = "white", size = 2.6, stroke = 1.5) + 
  geom_abline(intercept = 0, slope = 1, size = 1, col="red") +
  xlab(expression(Recent~~log(italic(B[ty]))/Old~~log(italic(B[ty])))) +
  ylab(expression(Recent~~log(italic(B[MSY]))/Old~~log(italic(B[MSY])))) +
  ggtitle("(a)") +
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
        legend.position = c(0.84, 0.4),
        legend.box="vertical", 
        legend.margin=margin(),
        strip.text.x = element_text(size=22, face = "bold"),
        strip.text.y = element_text(size=22, face = "bold"),
        plot.title = element_text(size=22,hjust=0.5,face = "bold"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines")) +
  guides(colour = guide_legend(title='Potential causes', nrow = 5, byrow = T, override.aes=list(size=4))) +
  guides(shape = guide_legend(title='Potential causes', nrow = 5, byrow = T))

p2 <- ggplot() +
  geom_point(data=dat, aes(x=F_change, y=Fmsy_change), position=position_dodge(0), shape = 21, colour = "#999999", fill = "white", size = 2.6, stroke = 1.5) + 
  geom_abline(intercept = 0, slope = 1, size = 1, col="red") +
  xlab(expression(Recent~~log(italic(F[ty]))/Old~~log(italic(F[ty])))) +
  ylab(expression(Recent~~log(italic(F[MSY]))/Old~~log(italic(F[MSY])))) +
  ggtitle("(b)") +
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
        legend.position = c(0.84, 0.4),
        legend.box="vertical", 
        legend.margin=margin(),
        strip.text.x = element_text(size=22, face = "bold"),
        strip.text.y = element_text(size=22, face = "bold"),
        plot.title = element_text(size=22,hjust=0.5,face = "bold"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines")) +
  guides(colour = guide_legend(title='Potential causes', nrow = 5, byrow = T, override.aes=list(size=4))) +
  guides(shape = guide_legend(title='Potential causes', nrow = 5, byrow = T))

p <- ggarrange(p1, p2, nrow=1)

jpeg("B_F_change.jpeg", height = 7, width = 11, units='in', res=600)
p 
dev.off()

