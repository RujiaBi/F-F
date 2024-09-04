load("dd_B.RData")
load("dd_B_new.RData")

load("dd_Bmsy.RData")
load("dd_Bmsy_new.RData")

load("dd_B_Bmsy.RData")
load("dd_B_Bmsy_new.RData")

library(Ipaper)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(grid)
cbPalette2 <- c("#0072B2", "#CC79A7")


# Data
dd_B_new$para <- "B_ty"
dd_Bmsy_new$para <- "Bmsy"
dd_B_Bmsy_new$para <- "B_Bmsy"

dd <- rbind(dd_B, dd_Bmsy, dd_B_Bmsy)
dd$para <- factor(dd$para, levels=c("B_ty", "Bmsy", "B_Bmsy"))
dd$para1 <- factor(dd$para, labels=c(expression(bolditalic(B[ty])), expression(bolditalic(B[MSY])), expression(bolditalic(B[ty]/B[MSY]))))

dd_new <- rbind(dd_B_new , dd_Bmsy_new , dd_B_Bmsy_new)
dd_new$para <- factor(dd_new$para, levels=c("B_ty", "Bmsy", "B_Bmsy"))
dd_new$para1 <- factor(dd_new$para, labels=c(expression(bolditalic(B[ty])), expression(bolditalic(B[MSY])), expression(bolditalic(B[ty]/B[MSY]))))

# set regions with 0 stocks as NAs
dd[dd$para=="B_ty" & dd$num==0,]$value <- 250
dd[dd$para=="B_ty" & dd$num==0,]$lim <- 250

dd[dd$para=="Bmsy" & dd$num==0,]$value <- 160
dd[dd$para=="Bmsy" & dd$num==0,]$lim <- 160

dd[dd$para=="B_Bmsy" & dd$num==0,]$value <- 70
dd[dd$para=="B_Bmsy" & dd$num==0,]$lim <- 70


p <- ggplot() +
  geom_boxplot2(data=dd, aes(x=Region, y=value, fill=Period), width = 0.6, width.errorbar = 0) +
  geom_text(data=dd_new, aes(x=Region, y=lim*1.2, col=Period, label= num), position = position_dodge(width = .6), size=6.6, show.legend=FALSE) +
  scale_fill_manual(values=cbPalette2) +
  scale_colour_manual(values=cbPalette2) +
  facet_wrap(vars(para1), labeller = label_parsed, scales = "free_y", nrow = 3, strip.position = "top") + 
  ylab("CV (%)") +
  ggtitle("(a)") +
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

png("B_related.png", height = 12, width = 18, units='in', res=600)
p
dev.off()


######################################################################################################

p <- ggplot() +
  geom_boxplot2(data=dd, aes(x=Region, y=value, fill=Period), width = 0.6, width.errorbar = 0) +
  geom_text(data=dd_new, aes(x=Region, y=lim*1.2, col=Period, label= num), position = position_dodge(width = .6), size=5.5, show.legend=FALSE) +
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

png("B_related_AFS.png", height = 9, width = 20, units='in', res=600)
p
dev.off()