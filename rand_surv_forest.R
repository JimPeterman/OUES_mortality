
library(readxl)
library(dplyr)
library(randomForestSRC)


# Trying out some random survival forest analyses.

data <- read_xlsx(here::here("../CLEANED_OUES_dataset_5_27_2022.xlsx"))

data$OUES_tertile <- ordered(data$OUES_tertile, levels = c("Bad", "Ok", "Good"))

# Characters must be factors:
data <- data %>% mutate_at(vars(OUES_tertile), as.factor)

# R crashes when using sex in model as character. So, make numeric here.
data <- mutate(data, sex_male = ifelse(sex == "Male", 1, 0))

obj <- rfsrc(Surv(follow_up_yrs, mortality_status) ~ OUES + age + sex_male +
               obesity + hypertension + dyslipidemia + diabetes + inactivity + smoker +
               VO2_rel, 
             data = filter(data, sex_male == 0),
             ntree = 100, nodesize = 5, nsplit = 50, importance = TRUE)
obj
print(obj)


######################################
# Plot variable importance.
library(ggRandomForests)
library(ggplot2)


plot(gg_vimp(obj)) +
  theme(legend.position = c(.8, .2)) +
  labs(fill="VIMP > 0")

plot(jp)





######################################
# Plot variable importance (not as good).
jk.obj <- subsample(obj)
pdf("VIMPsur.pdf", width = 15, height = 20)
par(oma = c(0.5, 10, 0.5, 0.5))
par(cex.axis = 2.0, cex.lab = 2.0, cex.main = 2.0, mar = c(6.0,17,1,1), mgp = c(4, 1, 0))
plot(jk.obj, xlab = "Variable Importance (x 100)", cex = 1.2)
dev.off()

######################################
# Plot survival curve.
newdata <- data.frame(lapply(1:ncol(obj$xvar),function(i){median(obj$xvar[,i])}))
colnames(newdata) <- obj$xvar.names
newdata1 <- newdata2 <- newdata
newdata1[,which(obj$xvar.names == "OUES")] <- quantile(obj$xvar$OUES, 0.25)
newdata2[,which(obj$xvar.names == "OUES")] <- quantile(obj$xvar$OUES, 0.75)
newdata <- rbind(newdata1,newdata2)
y.pred <- predict(obj,newdata = rbind(newdata,obj$xvar)[1:2,], na.action="na.impute")





pdf("survival.pdf", width = 10, height = 8)
par(cex.axis = 2.0, cex.lab = 2.0, cex.main = 2.0, mar = c(6.0,6,1,1), mgp = c(4, 1, 0))
plot(round(y.pred$time.interest,2),y.pred$survival[1,], type="l", xlab="Time (Year)",   
     ylab="Survival", col=1, lty=1, lwd=2)
lines(round(y.pred$time.interest,2), y.pred$survival[2,], col=2, lty=2, lwd=2)
legend("topright", legend=c("Peak VO2=12.8","Peak VO2=19.3"), col=c(1:2), lty=1:2, cex=2, lwd=2)
dev.off() 


