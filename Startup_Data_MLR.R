########## Packages Required #########

library(plyr)
library(corpcor)
install.packages("mvinfluence")
library(mvinfluence)
library(car)
library(corpcor)
library(MASS)


#######reading and understanding the data #######

Startups_Data<-read.csv(file.choose())
View(Startups_Data)
class(Startups_Data)
Startups_Data$State <- revalue(Startups_Data$State,
                          c("New York"="0", "California"="1", "Florida"="2"))

View(Startups_Data$State)

names(Startups_Data)

attach(Startups_Data)
Startups_Data <- cbind(RD_Spend=R.D.Spend,Administration,Marketing_Spend=Marketing.Spend,State,Profit)
?cbind


names(Startups_Data)
Startups_Data <- as.data.frame(Startups_Data)

attach(Startups_Data) # Basically to avoid reference of Data Set name(Startups) in this report.


summary(Startups_Data)

############ Steps of analysis for MLR #####
# Exploratory data analysis:
# 1. Measures of central tendency
# 2. Measures of dispersion
# 3. Third moment business decision
# 4. Fourth moment business decision
# 5. Probability distributions of variables 
# 6. Graphical representations (Histogram, Box plot, Dot plot, Stem & Leaf plot, Bar plot, etc.)



plot(R.D.Spend, Profit)


plot(Administration, Profit)




plot(Marketing_Spend, Profit)


plot(State, Profit)


windows()
# 7. Find the correlation between Output (Profit) & inputs (R.D_Spend, Administration, Marketing_Spend, State) - SCATTER DIAGRAM
pairs(Startups_Data)


# 8. Correlation coefficient - Strength & Direction of correlation
cor(Startups_Data)



# The Linear Model of interest
Model.Startups_Data <- lm(Profit~RD_Spend+Administration+Marketing_Spend+State)
summary(Model.Startups_Data)#Multiple R-squared:  0.9507


Model.Startups_Data1 <- lm(Profit~RD_Spend+log(Administration))
summary(Model.Startups_Data1)#Multiple R-squared:  0.9474


###Model.Startups_Data2 <- lm(Profit~RD_Spend+Administration+Marketing_Spend+log(State))
##summary(Model.Startups_Data2)


### Scatter plot matrix with Correlations inserted in graph
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  
  if(missing(cex.cor)) cex <- 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex)
}
pairs(Startups_Data, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")


### Partial Correlation matrix - Pure correlation between the variables


cor2pcor(cor(Startups_Data))





# It is better to delete a single observation rather than entire variable to get rid of collinearity problem
# Deletion Diagnostics for identifying influential variable

influence.measures(Model.Startups_Data)


influenceIndexPlot(Model.Startups_Data, id.n=3) # Index Plots of the influence measures


influencePlot(Model.Startups_Data, id.n=3) # A user friendly representation of the above






#############****** Logarthimic Transformation *******##############

Model.Startups_Data_Log<-lm(Profit~RD_Spend+log(Administration)+Marketing_Spend+log(State),data=Startups_Data[-c(49,50),]) 

summary(Model.Startups_Data_Log) #Adjusted R2 Value = 0.9591 ,Multiple R-squared:  0.9625 

confint(Model.Startups_Data_Log,level=0.95)

predict(Model.Startups_Data_Log,interval="predict")


Model.Startups_Data_Fin1<-lm(Profit~RD_Spend+Administration+Marketing_Spend+State,data=Startups_Data[-c(49,50),])
summary(Model.Startups_Data_Fin1) # Adjusted R2 Value is0.9593 ,Multiple R-squared:  0.9627





##########*********** Exponential Transformation ***********###########

Model.Startups_Data_exp<-lm(log(Profit)~RD_Spend+Administration+Marketing_Spend+State,data=Startups_Data[-c(49,50),])
summary(Model.Startups_Data_exp)  #Adjusted R2 Value is 0.9182,Multiple R-squared:  0.9252,


Model.Startups_Data_exp1<-lm(log(Profit)~RD_Spend+Marketing_Spend,data=Startups_Data[-c(49,50),])
summary(Model.Startups_Data_exp1) #Multiple R-squared:  0.9221,	Adjusted R-squared:  0.9187 





##################********** Quadratic Model  *******************##############

Model.Startups_Data_Quad <- lm(Profit~RD_Spend+I(RD_Spend^2)+Administration+I(Administration^2)
                          +Marketing_Spend+I(Marketing_Spend^2)+State+I(State^2),data=Startups_Data[-c(49,50),])
summary(Model.Startups_Data_Quad)  #Multiple R-squared:  0.9641,	Adjusted R-squared:  0.9567 

confint(Model.Startups_Data_Quad,level=0.95)

predict(Model.Startups_Data_Quad,interval="predict")


Model.Startups_Data_Quad1 <- lm(Profit~RD_Spend+I(RD_Spend^2)+Marketing_Spend+I(Marketing_Spend^2)
                           ,data=Startups_Data[-c(49,50),])
summary(Model.Startups_Data_Quad1)  #Multiple R-squared:  0.962,	Adjusted R-squared:  0.9585 







#################***** Poly Modal  **********################

Model.Startups_Data_Poly <- lm(Profit~RD_Spend+I(RD_Spend^2)+I(RD_Spend^3)+
                            Administration+I(Administration^2)+I(Administration^3)+
                            Marketing_Spend+I(Marketing_Spend^2)+I(Marketing_Spend^3)+
                            State+I(State^2)+I(State^3),data=Startups_Data[-c(49,50),])
summary(Model.Startups_Data_Poly) #Multiple R-squared:  0.967,	Adjusted R-squared:  0.9569 


Model.Startups_Data_Poly1 <- lm(Profit~RD_Spend+I(RD_Spend^2)+I(RD_Spend^3)+
                             Marketing_Spend+I(Marketing_Spend^2)+I(Marketing_Spend^3)
                           ,data=Startups_Data[-c(49,50),])
summary(Model.Startups_Data_Poly1) #Multiple R-squared:  0.9652,	Adjusted R-squared:  0.9601 


### Variance Inflation Factors is a formal way to check for collinearity

vif(Model.Startups_Data_Log)  # VIF is > 10 => collinearity


avPlots(Model.Startups_Data_Log, id.n=2, id.cex=0.7) # Added Variable Plots







################### Final Model ####################

FinalModel<-lm(Profit~RD_Spend+log(Administration)+Marketing_Spend+
                 log(State),data=Startups_Data[-c(49,50),])

summary(FinalModel) #Multiple R-squared:  0.9625,	Adjusted R-squared:  0.9591 


Profit_Predict <- predict(FinalModel,interval="predict")


Final <- cbind(Startups_Data$RD_Spend,Startups_Data$Administration,Startups_Data$Marketing_Spend,
               Startups_Data$State,Startups_Data$Profit,Profit_Predict)


View(Final)


################# Evaluate model LINE assumptions ###################

plot(FinalModel)# Residual Plots, QQ-Plos, Std. Residuals vs Fitted, Cook's distance


qqPlot(FinalModel, id.n=5) # QQ plots of studentized residuals, helps identify outliers


stepAIC(FinalModel) # backward
?stepAIC


# Lower the AIC value better is the model. AIC is used only if you build multiple models.
