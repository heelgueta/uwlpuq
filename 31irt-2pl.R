idf <- readRDS("uwlpui.rds")

library(mirt)

### 311 dichotomous 1 factor 2pl irts ------------------------------------------
# Specify the model structure
model <- ' F1 = 1-5'
model_vars <- c("wlim01", "wlim02", "wlim03", "wlim04", "wlim13")
model_vars <- c("wlim06", "wlim07", "wlim08", "wlim09", "wlim11")

cbind(psych::describe(idf[,model_vars]), t(sapply(idf[, model_vars], function(x) table(factor(x, levels = 0:1, exclude = NULL)))))[,c(2,3,11,12,14:15)]

fit <- mirt(idf[, model_vars], model, method = 'QMCEM')

summary(fit)
M2(fit, type = "C2")#nice but slow
itemfit(fit)
plot(fit, type = "trace")
plot(fit, type = "info")

mirt::itemplot(fit,item=1,)

### 312 nice plots -------------------------------------------------------------

# Item plot for the first item with custom colors
itemplot(
  fit,
  item = 1,
  type = "trace",
  auto.key = list(points = F, lines = TRUE, columns = 5),
  par.settings = list(
    superpose.line = list(col = "red", lwd = 5, alpha = 1)
  )
)

plot(fit, type = "trace", par.settings = list(
  superpose.line = list(col = "red", lwd = 2, alpha = 0.8),
))

### 313 dichotomous 2pl irt multidimensional (2factors) ------------------------
model <- ' F1 = 1-5
           F2 = 6-10'
model_vars <- c("wlim01", "wlim02", "wlim03", "wlim04", "wlim13",
                "wlim06", "wlim07", "wlim08", "wlim09", "wlim11")

cbind(psych::describe(idf[,model_vars]), t(sapply(idf[, model_vars], function(x) table(factor(x, levels = 0:1, exclude = NULL)))))[,c(2,3,11,12,14:15)]

fit <- mirt(idf[, model_vars], model, method = 'QMCEM')

summary(fit)
M2(fit, type = "C2")#nice but slow
itemfit(fit)


fscores(fit)[,1];hist(fscores(fit)[,1])
fscores(fit)[,2];hist(fscores(fit)[,2])



### 314 nice plots -------------------------------------------------------------
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
    superpose.line = list(col = "red", lwd = 5, alpha = 1)
  )
)

plot(fit, type = "trace", par.settings = list(
  superpose.line = list(col = "red", lwd = 2, alpha = 0.8)
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
