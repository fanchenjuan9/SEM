library(lavaan)
library(semPlot)
#东印度洋总SEM
data2 <- read.csv("C:/Users/范晨娟/Documents/R.SEM/total.csv")
modle19 = 'DMS ~ Temp + DMSPp + DMSOp + bact + Syn + DMSPd
DMSOp ~ SiO3 + PO4 + bact + DMS 
DMSPd ~ bact + DMSPp + Pro + a + SiO3
DMSPp ~ dino + Temp + bact + chrys + Sali + Syn
DMSOd ~ DMSOp + Syn + PO4 
a ~ Temp + Sali
chrys ~ PO4 + SiO3
dino ~ Temp
bact ~ Temp
Pro ~ PO4 + SiO3
Syn ~ Temp'
fit <- sem(modle19, data = data2)
summary(fit, standardized = TRUE)
fitmeasures(fit,c("chisq","df","pvalue","gfi","cfi","rmr","srmr","rmsea","cmin","nfi","tli","agfi","cmin"))
semPaths(fit,
         what = "std",
         layout = "tree3",
         fade=F,
         residuals = F)

rm(list = ls())

#5m处SEM
data5 <- read.csv("C:/Users/范晨娟/Documents/R.SEM/5-SEM.csv")
modle = 'DMSPp ~ bact + dino + chrys
DMSOd ~ PO4 + Euk
DMSOp ~ dino + DMS 
a ~ chrys + Euk
DMS ~ DMSPd + bact
DMSPd ~ DMSPp + bact
Euk ~ PO4'
#modle1 = 'DMSPp ~ bact + chrys 
#DMSOd ~ Euk + PO4 + a
#DMSOp ~ dino
#Euk ~ Syn + a 
#chrys ~ a + bact
#DMS ~ DMSPp 
#DMSPd ~ DMSPp'
fit <- sem(modle, data = data5)
summary(fit, standardized = TRUE)
fitmeasures(fit,c("chisq","df","pvalue","gfi","cfi","rmr","srmr","rmsea","cmin","nfi","tli","agfi","cmin"))
semPaths(fit,
         what = "std",
         layout = "tree3",
         fade=F,
         residuals = F)

rm(list = ls())

#25m处SEM
data25 <- read.csv("C:/Users/范晨娟/Documents/R.SEM/25-SEM.csv")
modle <- 'DMS ~ bact + DMSOd  + DMSPp
DMSPd ~ Syn + dino
DMSOd ~ DMSPp
bact ~ Temp + a
a ~ Syn
DMSPp ~ Syn 
DMSOp ~ Syn + DMS + DMSPd'
#modle1 <- 'DMS ~ bact + DMSOd
#DMSPd ~ dino 
#DMSOd ~ DMSPp + DMS
#bact ~ Temp + a
#a ~ Syn + Euk 
#Syn ~ Euk' 
fit <- sem(modle, data = data25)
summary(fit, standardized = TRUE)
fitmeasures(fit,c("chisq","df","pvalue","gfi","cfi","rmr","srmr","rmsea","cmin","nfi","tli","agfi","cmin"))
semPaths(fit,
         what = "std",
         layout = "tree3",
         fade=F,
         residuals = F)

rm(list = ls())

#75m处SEM
data75 <- read.csv("C:/Users/范晨娟/Documents/R.SEM/75-SEM.csv")
modle <- 'DMS ~ a + DMSPp + diat
a ~ PO4  
Temp ~ PO4
DMSPp ~ bact + Temp
DMSOd ~ dino'
fit <- sem(modle, data = data75)
summary(fit, standardized = TRUE)
fitmeasures(fit,c("chisq","df","pvalue","gfi","cfi","rmr","srmr","rmsea","cmin","nfi","tli","agfi","cmin"))
semPaths(fit,
         what = "std",
         layout = "tree3",
         fade=F,
         residuals = F)

rm(list = ls())

#150m处SEM
data150 <- read.csv("C:/Users/范晨娟/Documents/R.SEM/150-SEM.csv")
modle <- 'DMS ~ DMSOp + Syn + bact
DMSOp ~ Syn + bact
diat ~ Temp
bact ~ Temp
Euk ~ SiO3' 
fit <- sem(modle, data = data150)
summary(fit, standardized = TRUE)
fitmeasures(fit,c("chisq","df","pvalue","gfi","cfi","rmr","srmr","rmsea","cmin","nfi","tli","agfi","cmin"))
semPaths(fit,
         what = "std",
         layout = "tree3",
         fade=F,
         residuals = F)



rm(list = ls())