#load data, setup
options(digits = 3);options(scipen = 999);options(max.print = 5000)
#load datasets if needed
odf <- readRDS("uwlpuq.rds")
idf <- readRDS("uwlpui.rds")

#install.packages("psych")
#psych::describe(odf[,c(1:225)])
#sapply(odf[, c(1:225)], function(x) table(factor(x, levels = 0:5, exclude = NULL)))
#cbind(psych::describe(odf[,c(1:225)]), t(sapply(odf[, c(1:225)], function(x) table(factor(x, levels = 0:5, exclude = NULL)))))[,c(2,3,11,12,14:18)]

###univariate analyses for ordinal likert, freq, duration vars
#8:33 general env attitudes and behaviors
cbind(psych::describe(odf[,c(2,8:33)]), t(sapply(odf[, c(2, 8:33)], function(x) table(factor(x, levels = 1:5, exclude = NULL)))))[,c(2,3,11,12,14:18)]

#93:96 priority wetlands protection
cbind(psych::describe(odf[,c(93:96)]), t(sapply(odf[, c(93:96)], function(x) table(factor(x, levels = 1:5, exclude = NULL)))))[,c(2,3,11,12,14:18)]

#98:99 maria behety & tres puentes freq, duration visits
cbind(psych::describe(odf[,c(98:99)]), t(sapply(odf[, c(98:99)], function(x) table(factor(x, levels = 0:4, exclude = NULL)))))[,c(2,3,11,12,14:18)]
cbind(psych::describe(odf[,c(141:142)]), t(sapply(odf[, c(141:142)], function(x) table(factor(x, levels = 0:4, exclude = NULL)))))[,c(2,3,11,12,14:18)]
table(odf$mblvis);table(odf$tplvis)
cbind(psych::describe(odf[,c(98:99)]), t(sapply(odf[, c(98:99)], function(x) table(factor(x, levels = 0:4, exclude = NULL)))))[,c(2,3,11,12,14:18)]
cbind(psych::describe(odf[,c(141:142)]), t(sapply(odf[, c(141:142)], function(x) table(factor(x, levels = 0:4, exclude = NULL)))))[,c(2,3,11,12,14:18)]


#186:203 beh int, regid, place attachment
cbind(psych::describe(odf[,c(186:203)]), t(sapply(odf[, c(186:203)], function(x) table(factor(x, levels = 1:5, exclude = NULL)))))[,c(2,3,11,12,14:18)]



#mb acc state last

cbind(psych::describe(odf[,c(112,155)]), t(sapply(odf[, c(112,155)], function(x) table(factor(x, levels = 1:5, exclude = NULL)))))[,c(2,3,11,12,14:18)]
cbind(psych::describe(odf[,c(113,156)]), t(sapply(odf[, c(113,156)], function(x) table(factor(x, levels = 0:4, exclude = NULL)))))[,c(2,3,11,12,14:18)]
cbind(psych::describe(odf[,c(138,183)]), t(sapply(odf[, c(138,183)], function(x) table(factor(x, levels = 1:5, exclude = NULL)))))[,c(2,3,11,12,14:18)]




# Fix both odf[,125] and odf[,168]
# Fix both odf[,126] and odf[,169]
# Fix both odf[,151] and odf[,196]


odf[, c(98:99, 141:142)] <- odf[, c(98:99, 141:142)] + 1







write_xlsx(data.frame(Variable = rownames(rbind(cbind(psych::describe(odf[,c(2,8:33)]), t(sapply(odf[, c(2, 8:33)], function(x) table(factor(x, levels = 1:5, exclude = NULL)))))[,c(2,3,11,12,14:18)], cbind(psych::describe(odf[,c(93:96)]), t(sapply(odf[, c(93:96)], function(x) table(factor(x, levels = 1:5, exclude = NULL)))))[,c(2,3,11,12,14:18)], cbind(psych::describe(odf[,c(98:99, 141:142)]), t(sapply(odf[, c(98:99, 141:142)], function(x) table(factor(x, levels = 1:5, exclude = NULL)))))[,c(2,3,11,12,14:18)], cbind(psych::describe(odf[,c(186:203)]), t(sapply(odf[, c(186:203)], function(x) table(factor(x, levels = 1:5, exclude = NULL)))))[,c(2,3,11,12,14:18)])), rbind(cbind(psych::describe(odf[,c(2,8:33)]), t(sapply(odf[, c(2, 8:33)], function(x) table(factor(x, levels = 1:5, exclude = NULL)))))[,c(2,3,11,12,14:18)], cbind(psych::describe(odf[,c(93:96)]), t(sapply(odf[, c(93:96)], function(x) table(factor(x, levels = 1:5, exclude = NULL)))))[,c(2,3,11,12,14:18)], cbind(psych::describe(odf[,c(98:99, 141:142)]), t(sapply(odf[, c(98:99, 141:142)], function(x) table(factor(x, levels = 1:5, exclude = NULL)))))[,c(2,3,11,12,14:18)], cbind(psych::describe(odf[,c(186:203)]), t(sapply(odf[, c(186:203)], function(x) table(factor(x, levels = 1:5, exclude = NULL)))))[,c(2,3,11,12,14:18)]), vardesc = read.csv("vardesc.csv", header = TRUE)[match(rownames(rbind(cbind(psych::describe(odf[,c(2,8:33)]), t(sapply(odf[, c(2, 8:33)], function(x) table(factor(x, levels = 1:5, exclude = NULL)))))[,c(2,3,11,12,14:18)], cbind(psych::describe(odf[,c(93:96)]), t(sapply(odf[, c(93:96)], function(x) table(factor(x, levels = 1:5, exclude = NULL)))))[,c(2,3,11,12,14:18)], cbind(psych::describe(odf[,c(98:99, 141:142)]), t(sapply(odf[, c(98:99, 141:142)], function(x) table(factor(x, levels = 1:5, exclude = NULL)))))[,c(2,3,11,12,14:18)], cbind(psych::describe(odf[,c(186:203)]), t(sapply(odf[, c(186:203)], function(x) table(factor(x, levels = 1:5, exclude = NULL)))))[,c(2,3,11,12,14:18)])), read.csv("vardesc.csv", header = TRUE)$varname), "vardesc"]), "combined_results_table.xlsx")
