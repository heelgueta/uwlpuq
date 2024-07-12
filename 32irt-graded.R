idf <- readRDS("uwlpui.rds")

library(mirt)

# Specify the model structure
model <- ' F1 = 1-3'
model <- ' F1 = 1-4'
model <- ' F1 = 1-5'

model <- ' F1 = 1-6'
model <- ' F1 = 1-8'
model <- ' F1 = 1-8
           F2 = 9-12  '
model <- ' F1 = 1-4
           F2 = 5-8'


# Check data types of variables in the model
model_vars <- c("bint01", "bint02", "bint03", "bint04", "bint05", "bint06", "bint07", "bint08")
model_vars <- c("wlpri1", "wlpri2", "wlpri3", "wlpri4")
model_vars <- c("natcn1", "natcn2", "natcn3", "natcn4")
model_vars <- c("regid1", "regid2", "regid3", "regid4")
model_vars <- c("place1", "place2", "place3", "place4")
model_vars <- c("mbfreq", "mbleng", "tpfreq", "tpleng")
model_vars <- c("tpfreq", "tpleng","tpaccs")



model_vars <- c("wlpri1", "wlpri2", "wlpri3", "wlpri4",
                "regid1", "regid2", "regid3", "regid4")

model_vars <- c("bint01", "bint02", "bint03", "bint04", "bint05", "bint06", "bint07", "bint08",
                "regid1", "regid2", "regid3", "regid4")

model_vars <- c("wlim01", "wlim02", "wlim03", "wlim04", "wlim13")
model_vars <- c("wlim06", "wlim07", "wlim08", "wlim09", "wlim11")


#                ,
#                ,
#                "regid1", "regid2", "regid3", "regid4",
#                "place1", "place2", "place3", "place4",
#                "mbfreq", "mbleng", "tpfreq", "tpleng")

#print(sapply(idf[, model_vars], class))
#summary(idf[, model_vars])
cbind(psych::describe(idf[,model_vars]), t(sapply(idf[, model_vars], function(x) table(factor(x, levels = 1:5, exclude = NULL)))))[,c(2,3,11,12,14:18)]


# Fit the multi-factor IRT model
fit <- mirt(idf[, model_vars], model, method = 'QMCEM')

#fit <- mirt(idf[, model_vars], model, itemtype = 'graded', method = 'QMCEM')

# Examine the model results
summary(fit)
M2(fit, type = "C2")#nice but slow
itemfit(fit)
#plot(fit, type = "trace")
#plot(fit, type = "info")

mirt::itemplot(fit,item=1,)

fscores(fit)[,1];hist(fscores(fit)[,1])
fscores(fit)[,2];hist(fscores(fit)[,2])




# Custom colors for the ICC
#custom_colors <- c("red", "orange", "yellow", "green", "darkgreen")
library(RColorBrewer)
library(latticeExtra)
custom_colors <- brewer.pal(5, "RdBu")
custom_colors[3] <- "gray"  # Replace the middle color with gray

# Item plot for the first item with custom colors
itemplot(
  fit,
  item = 3,
  type = "trace",
  auto.key = list(points = FALSE, lines = TRUE, columns = 1),
  par.settings = list(
    superpose.line = list(col = custom_colors, lwd = 5, alpha = 1)
  )
)

plot(fit, type = "trace", par.settings = list(
  superpose.line = list(col = custom_colors, lwd = 2, alpha = 0.8)
))



plot(fit, type = "info",lwd=2)
plot(fit, type = "rxx",lwd=2)
plot(fit, type = "SE",lwd=2)



plot(fit, type = "infotrace",lwd=3)#nice gives info curve for each item
plot(fit, type = "infoSE",lwd=2)#test info + SE
#plot(fit, type = "trace",lwd=2)#ipf for each item
plot(fit, type = "itemscore",lwd=2)#expected item score vs theta (similar to difficulty?)
plot(fit, type = "score")#score vs theta
plot(fit, type = "EAPsum",lwd=2) #neat exp vs obs sumscores


plot(fit, type = "infocontour",lwd=2)#multidim
plot(fit, type = "scorecontour",lwd=2)#multidim
#plot(fit, type = "posteriorTheta",lwd=2) #dunno
#plot(fit, type = "empiricalhist",lwd=2)#nope
#plot(fit, type = "Davidian",lwd=2)#nope


#?mirt::plot()
#??mirt::plot
