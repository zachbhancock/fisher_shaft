#this file is R code for plotting the figures from Hancock & Cardinale (in prep)

library(ggplot2)
library(ggpubr)

#plot fig 1. Fisher's geometric model
fisher_geo_5 <- function(r){1-pnorm(r*(sqrt(1)/(2*0.05)))}
fisher_geo_25 <- function(r){1-pnorm(r*(sqrt(1)/(2*2.5)))}
fisher_geo_125 <- function(r){1-pnorm(r*(sqrt(1)/(2*1.25)))}
fisher_geo_625 <- function(r){1-pnorm(r*(sqrt(1)/(2*0.625)))}
curve(fisher_geo_625, from=0.01, to=5, lwd=2, xlab="mutation effect size", 
      ylab="Pr(beneficial)", col="red")
curve(fisher_geo_125, from=0.01, to=5, lwd=2, xlab="mutation effect size", 
      ylab="Pr(beneficial)", col="blue", add=TRUE)
curve(fisher_geo_25, from=0.01, to=5, lwd=2, xlab="mutation effect size", 
      ylab="Pr(beneficial)", col="green", add=TRUE)
curve(fisher_geo_5, from=0.01, to=5, lwd=2, xlab="mutation effect size", 
      ylab="Pr(beneficial)", col="purple", add=TRUE)

#plot fig 2. observed DFE and probability of fixation
GE_full <- read.table("GE_run_full.txt", header=TRUE)
GE_full$rep <- factor(GE_full$rep, levels=c("1", "2", "3", "4", "5"))
GE_full$fixation_prob <- 2*GE_full$effect / (1-exp(-4*800*GE_full$effect))

a <- ggplot(GE_full, aes(x=effect, y=frequency, color=mut_type)) + 
  geom_point(size=1, alpha=0.8) + xlim(c(-0.1,0.05)) + 
  geom_vline(xintercept=0, linetype="dashed") + theme_bw() + 
  xlab("fitness effect") + theme(axis.title=element_text(size=14), 
                                   axis.text=element_text(size=12),
                                 legend.position="none")

b <- ggplot(GE_full, aes(x=effect, y=fixation_prob, color=mut_type)) + geom_line(size=2) + 
  geom_hline(yintercept=0.000625, linetype="dashed") + xlim(c(-0.1,0.05)) +
  theme_bw() + 
  xlab("fitness effect") + theme(axis.title=element_text(size=14), 
                                 axis.text=element_text(size=12),
                                 legend.position="none") + ylab("Pr(fixation)")

ggarrange(a, b, labels=c("a", "b"), ncol=2, nrow=1)

#plot fig 3. population size through time
GE_1 <- read.table("GE_run1.txt", header=TRUE, sep=",")
GE_1$rep <- "1"
GE_2 <- read.table("GE_run2.txt", header=TRUE, sep=",")
GE_2$rep <- "2"
GE_3 <- read.table("GE_run3.txt", header=TRUE, sep=",")
GE_3$rep <- "3"
GE_4 <- read.table("GE_run4.txt", header=TRUE, sep=",")
GE_4$rep <- "4"
GE_5 <- read.table("GE_run5.txt", header=TRUE, sep=",")
GE_5$rep <- "5"

GE_all <- rbind(GE_1, GE_2, GE_3, GE_4, GE_5)
GE_all$rep <- factor(GE_all$rep, levels=c("1", "2", "3", "4", "5"))
ggplot(GE_all, aes(x=cycle, y=Population_size, color=rep)) + geom_line() + 
  geom_hline(yintercept=1000, linetype="dashed") + theme_bw() + 
  xlab("generation") + theme(axis.title=element_text(size=14), 
                                 axis.text=element_text(size=12),
                                 legend.position="none") + ylab("population size")