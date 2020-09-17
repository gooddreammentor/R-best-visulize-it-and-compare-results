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

# summary of model
summary(model)
plot(model)

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

p<- plot_ly(data, x = ~data$GFR, y = ~data$`Mean AAG`, type="scatter", text = paste("Clarity: ", data$clarity),
          mode = "markers", color = ~data$GFR, size = ~data$N)


model2<- ggplot(data, aes(x=data$GFR, y=data$`Mean AAG`, color='rainbow', size=data$N)) +
  geom_point() +
  theme(legend.position="none")
model2<- text(data$GFR, data$`Mean AAG`, data$N, cex=0.5)  
abline (model2)
geom_text(label = paste("Adj R2 = ", adj.r.squared, "\n",
                                   "Intercept =",intercept, "\n",
                                   "Slope =", slope, "\n",
                                   "P =", pvalue))
