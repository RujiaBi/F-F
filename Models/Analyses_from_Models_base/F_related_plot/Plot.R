load("dd_F.RData")
load("dd_F_new.RData")

load("dd_Fmsy.RData")
load("dd_Fmsy_new.RData")

load("dd_F_Fmsy.RData")
load("dd_F_Fmsy_new.RData")

library(Ipaper)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(grid)
cbPalette2 <- c("#0072B2", "#CC79A7")

# Data
dd_F_new$para <- "F_ty"
dd_Fmsy_new$para <- "Fmsy"
dd_F_Fmsy_new$para <- "F_Fmsy"

dd <- rbind(dd_F, dd_Fmsy, dd_F_Fmsy)
dd$para <- factor(dd$para, levels=c("F_ty", "Fmsy", "F_Fmsy"))
dd$para1 <- factor(dd$para, labels=c(expression(bolditalic(F[ty])), expression(bolditalic(F[MSY])), expression(bolditalic(F[ty]/F[MSY]))))

dd_new <- rbind(dd_F_new , dd_Fmsy_new , dd_F_Fmsy_new)
dd_new$para <- factor(dd_new$para, levels=c("F_ty", "Fmsy", "F_Fmsy"))
dd_new$para1 <- factor(dd_new$para, labels=c(expression(bolditalic(F[ty])), expression(bolditalic(F[MSY])), expression(bolditalic(F[ty]/F[MSY]))))

# set regions with 0 stocks as NAs
dd[dd$para=="F_ty" & dd$num==0,]$value <- 140
dd[dd$para=="F_ty" & dd$num==0,]$lim <- 140

dd[dd$para=="Fmsy" & dd$num==0,]$value <- 110
dd[dd$para=="Fmsy" & dd$num==0,]$lim <- 110

dd[dd$para=="F_Fmsy" & dd$num==0,]$value <- 120
dd[dd$para=="F_Fmsy" & dd$num==0,]$lim <- 120

p <- ggplot() +
  geom_boxplot2(data=dd, aes(x=Region, y=value, fill=Period), width = 0.6, width.errorbar = 0) +
  geom_text(data=dd_new, aes(x=Region, y=lim+sqrt(mean(lim))*1.8, col=Period, label= num), position = position_dodge(width = .6), size=6.6, show.legend=FALSE) +
  scale_fill_manual(values=cbPalette2) +
  scale_colour_manual(values=cbPalette2) +
  facet_wrap(vars(para1), labeller = label_parsed, scales = "free_y", nrow = 3, strip.position = "top") + 
  ylab("CV (%)") +
  ggtitle("(b)") +
  theme_bw() + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y   = element_text(size=20),
        axis.text.x   = element_text(size=20, angle = 90, vjust = 0.5, hjust=1),
        axis.title.y  = element_text(size=22),
        axis.title.x  = element_text(size=22),
        plot.title = element_text(size=22, face="bold"),
        legend.title=element_text(size=22),
        legend.text=element_text(size=20),
        legend.justification=c(1,1),
        legend.position=c(0.99,1.056),
        legend.direction = "horizontal",
        strip.text.x = element_text(size=22),
        strip.text.y = element_text(size=22),
        plot.margin = unit(c(0.5,2,0.5,0.5), "lines"),
        panel.spacing = unit(1, "lines")) +
  guides(fill = guide_legend(nrow = 1, byrow = T)) +
  guides(colour = guide_legend(nrow = 1, byrow = T))

jpeg("F_related.jpeg", height = 12, width = 18, units='in', res=600)
p
dev.off()



######################################################################################################

p <- ggplot() +
  geom_boxplot2(data=dd, aes(x=Region, y=value, fill=Period), width = 0.6, width.errorbar = 0) +
  geom_text(data=dd_new, aes(x=Region, y=lim+sqrt(mean(lim))*1.6, col=Period, label= num), position = position_dodge(width = .6), size=5.5, show.legend=FALSE) +
  scale_fill_manual(values=cbPalette2) +
  scale_colour_manual(values=cbPalette2) +
  facet_wrap(vars(para1), labeller = label_parsed, scales = "free_y", nrow = 3, strip.position = "top") + 
  ylab("CV (%)") +
  theme_bw() + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y   = element_text(size=20),
        axis.text.x   = element_text(size=20, angle = 90, vjust = 0.5, hjust=1),
        axis.title.y  = element_text(size=22),
        axis.title.x  = element_blank(),
        legend.title=element_text(size=22),
        legend.text=element_text(size=20),
        legend.justification=c(1,1),
        legend.position=c(0.995,1.08),
        legend.direction = "horizontal",
        strip.text.x = element_text(size=22),
        strip.text.y = element_text(size=22),
        plot.margin = unit(c(0.5,2,0.5,0.5), "lines"),
        panel.spacing = unit(1, "lines")) +
  guides(fill = guide_legend(nrow = 1, byrow = T)) +
  guides(colour = guide_legend(nrow = 1, byrow = T))

jpeg("F_related_AFS.jpeg", height = 9, width = 20, units='in', res=600)
p
dev.off()
