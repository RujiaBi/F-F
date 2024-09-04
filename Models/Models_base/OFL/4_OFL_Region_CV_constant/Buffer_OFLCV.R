# qlnorm, inverse c. d. f.
# Pick four means of log(OFL) as examples: 0, 5, 10, 15

# 1st
cvlog <- seq(0,2.5,by=0.01)
sdlog <- sqrt(log(cvlog^2+1))
meanlog <- 0
abc1_0.4 <- qlnorm(p=0.4, meanlog=meanlog, sdlog=sdlog, lower.tail = TRUE, log.p = FALSE)
abc1_0.45 <- qlnorm(p=0.45, meanlog=meanlog, sdlog=sdlog, lower.tail = TRUE, log.p = FALSE)
buf1_0.4 <- (exp(meanlog)-abc1_0.4)/exp(meanlog)
buf1_0.45 <- (exp(meanlog)-abc1_0.45)/exp(meanlog)

# 2nd
cvlog <- seq(0,2.5,by=0.01)
sdlog <- sqrt(log(cvlog^2+1))
meanlog <- 5
abc2_0.4 <- qlnorm(p=0.4, meanlog=meanlog, sdlog=sdlog, lower.tail = TRUE, log.p = FALSE)
abc2_0.45 <- qlnorm(p=0.45, meanlog=meanlog, sdlog=sdlog, lower.tail = TRUE, log.p = FALSE)
buf2_0.4 <- (exp(meanlog)-abc2_0.4)/exp(meanlog)
buf2_0.45 <- (exp(meanlog)-abc2_0.45)/exp(meanlog)

# 3rd
cvlog <- seq(0,2.5,by=0.01)
sdlog <- sqrt(log(cvlog^2+1))
meanlog <- 10
abc3_0.4 <- qlnorm(p=0.4, meanlog=meanlog, sdlog=sdlog, lower.tail = TRUE, log.p = FALSE)
abc3_0.45 <- qlnorm(p=0.45, meanlog=meanlog, sdlog=sdlog, lower.tail = TRUE, log.p = FALSE)
buf3_0.4 <- (exp(meanlog)-abc3_0.4)/exp(meanlog)
buf3_0.45 <- (exp(meanlog)-abc3_0.45)/exp(meanlog)

# 4th
cvlog <- seq(0,2.5,by=0.01)
sdlog <- sqrt(log(cvlog^2+1))
meanlog <- 15
abc4_0.4 <- qlnorm(p=0.4, meanlog=meanlog, sdlog=sdlog, lower.tail = TRUE, log.p = FALSE)
abc4_0.45 <- qlnorm(p=0.45, meanlog=meanlog, sdlog=sdlog, lower.tail = TRUE, log.p = FALSE)
buf4_0.4 <- (exp(meanlog)-abc4_0.4)/exp(meanlog)
buf4_0.45 <- (exp(meanlog)-abc4_0.45)/exp(meanlog)

# combine results
res <- data.frame(matrix(NA, nrow=length(cvlog)*4*2, ncol=4))
colnames(res) <- c("cvlog", "group", "p", "buf")
res$cvlog <- rep(cvlog*100, times=4*2)
res$group <- rep(1:4, each=length(cvlog)*2)
res$p <- rep(c(0.4,0.45,0.4,0.45,0.4,0.45,0.4,0.45), each=length(cvlog))
res$buf  <- c(buf1_0.4, buf1_0.45, buf2_0.4, buf2_0.45, buf3_0.4, buf3_0.45, buf4_0.4, buf4_0.45)
res$buf <- res$buf*100
res$group <- as.factor(res$group)
res$p <- as.factor(res$p)

# plot
library(ggplot2)
library(ggpubr)
library(cowplot)
library(grid)
library(Ipaper)
cbPalette2 <- c("#0072B2", "#CC79A7")

# After plotting, mean value doesn't matter.
p <- ggplot(data=res[res$group==4,]) +
  geom_line(aes(x=cvlog, y=buf, color=p), size=1.3) +
  scale_colour_manual(name="P*", values=cbPalette2) +
  ylab("Relative difference between ABC and OFL (%)") +
  xlab("OFL CV (%)") +
  theme_bw() + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y   = element_text(size=20),
        axis.text.x   = element_text(size=20, vjust = 0.5, hjust=1),
        axis.title.y  = element_text(size=22),
        axis.title.x  = element_text(size=22),
        legend.title=element_text(size=22),
        legend.text=element_text(size=20),
        legend.justification=c(1,1),
        legend.position=c(0.98,0.1),
        legend.direction = "horizontal",
        strip.text.x = element_text(size=22),
        strip.text.y = element_text(size=22),
        plot.margin = unit(c(0.5,2,0.5,0.5), "lines"),
        panel.spacing = unit(1, "lines")) +
  guides(fill = guide_legend(nrow = 1, byrow = T))

png("Buffer_CV.png", height = 7.5, width = 12, units='in', res=600)
p
dev.off()


