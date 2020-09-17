data <- read.csv('AAG_levels_updated.csv', header = T)



data <- AAG_levels_updated


attach(AAG_levels_updated)
names(AAG_levels_updated)
plot(GFR, `Mean AAG`)


# check type of variables
str(data)
data$N <- as.numeric(as.character(data$N))
data$`Mean AAG` <- as.numeric(data$`Mean AAG`)
data$GFR <- as.numeric(as.character(data$GFR))


# linear regression model with no. of subjects as weights
model <- lm(data$`Mean AAG` ~ data$GFR, weights = data$N)

plot(model)

# summary of model


# plot and fit
plot(data$GFR, data$`Mean AAG`, xlab="GFR (mL/min/1.73m 2)", ylab= "AAG concentration (µmol/ L)")
abline(model)
summary(model)
mtext("y=30.07603x-0.06451", outer=FALSE,  cex=0.85, font=1, line=-1.5)
## printing of the equation


#Bubble Chart 
symbols(data$GFR, data$`Mean AAG`, circles=data$N, xlab="GFR (mL/min/1.73m 2)", ylab= "AAG concentration (µmol/ L)")
radius <- sqrt( data$N/ pi )
symbols(data$GFR, data$`Mean AAG`, circles=radius, xlab="GFR (mL/min/1.73m 2)", ylab= "AAG concentration (µmol/ L)")
symbols(data$GFR, data$`Mean AAG`, circles=radius, inches=0.35, fg="black", bg= data$N, xlab="GFR (mL/min/1.73m 2)", ylab= "AAG concentration (µmol/ L)")
text(data$GFR, data$`Mean AAG`, data$N, cex=0.5)

## Make a basic scatter plot :



library(flextable)

coefficients=coefficients(model)
formula=paste('AAG=',30.076, '-GFR*0.0645' )

ggplot(data, aes(x=data$GFR, y=data$`Mean AAG`)) +
  geom_point(aes( color=data$N, size=data$N)) + theme_bw() +
  geom_text_repel(aes(label=paste('size :' ,data$N),x=data$GFR, y=data$`Mean AAG`,color=data$N), size=3)+
  ggtitle("Scatter plot Mean AAG vs. GFR")+xlab("GFR (mL/min/1.73m 2)")+ylab("AAG concentration (µmol/ L)")+
  geom_smooth(method = "lm", mapping = aes(weight = data$N), 
              color = "purple", show.legend = TRUE, formula = y~x,   level = 0.99,alpha=0.1)+
  geom_label(aes(x=70, y=37,label=formula), size=8, color="grey")







#####Question 2######

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)


fu_predictions_ <- read_excel("C:/Users/hajji/Downloads/fu predictions .xlsx")
View(fu_predictions_)
new_fy<-fu_predictions_ %>% gather(category, value, colnames(fu_predictions_)[2]:colnames(fu_predictions_)[5]) %>%
  spread('Observed vs. Predicted', value)
        
str(new_fy)

reg<-lm(Predicted ~ Observed, data = new_fy)
reg
coeff=coefficients(reg)

ggplot(new_fy %>% filter(category != "Healthy"), aes(x=Observed, y=Predicted)) +
  geom_point(aes(color=category),size=3, alpha=0.5) + theme_bw() + theme(axis.text=element_text(size=12, face="bold"),legend.text=element_text(size=12, face="bold"))+
  ggtitle("Predicted vs. Observed Scatter plot per category") +
  geom_smooth(method="lm", se=F)+
  xlab('fu Observed')+ylab('fu Predicted')+
  geom_text_repel(aes(label=Observation, color=category), size=2.5)+
  geom_abline(intercept = 0.02, slope = 0.78, linetype="dashed")+
  geom_abline(intercept = -0.02, slope = 0.78, linetype="dashed")+
  geom_abline(intercept = 0.05, slope = 0.78, linetype="dashed", color="grey")+
  geom_abline(intercept = -0.05, slope = 0.78, linetype="dashed", color="grey")