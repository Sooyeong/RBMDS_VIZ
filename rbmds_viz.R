# RBMDS- Data visualization
# Author: Sooyeong Lim
# Date 2020/09/26
# Will use Shiny, ggplot, plotly() objects
# ggplot, plotly work station 


# load ToxicR package

library(ToxicR)
library(ggplot2)
library(dplyr)
library(plotly)

# Create an example dataset, single Dichotomous case
# X-axis- 
mData <- matrix(c(0, 1,10,
                  0.3, 4,10,
                  1, 4,10,
                  4, 7,10),nrow=4,ncol=3,byrow=T)

# declare it as datframe
head(mData)


# Model average dichotomous fit & single dichotomous fit result represent. 
# Not sure why other model is not working for BMD output 
H = single_dichotomous_fit(mData[,1],mData[,2],mData[,3],model_type = "weibull",fit_type = "mcmc")

View(H)

# D is dose level part, Y is response numbers in the declared dataset
H$data
H$bmd
H$prior

# replicate plot(H)'s report to ggplotly objects
plot(H)

# Added together with the plot 
H$fitted_model 
H$bmd
plot(H)

H_data<-data.frame(H$data)
H_data$prop<-H_data$Y/H_data$N

H$fitted_model

#Should I need to calculate 95$ 

model_fitted<-data.frame(H$fitted_model$bmd_dist)
# Tesst for model fitted
model_fitted$X2
# In Shiny app there should be a function added for Alpha values too.


# bmd fitted result

# Observed dataset;
# Predicted model line;

# BMDL, BMD, estimated doselevel

# What exactly the output suggest? BMDL, BMD for BMR=0.1? what exactly this value suggest?
bmd_result<-data.frame(H$bmd)


H$fitted_model$parameters

#dichotomous weibull - borrowed from Matt's github folder
.dich_weibull_f <-function(parms,d){
  g <- 1/(1+exp(-parms[1])); 
  a <- parms[2];
  b <- parms[3]; 
  rval <- g + (1-g)*(1-exp(-b*d^a))
  return (rval)
}

# I think d stands for dichotomous model's xaxis??? 
# increase value slgihtly from 0 to 4


x<-seq(from=0, to = 5, length.out=1000)


output<-data.frame(x,.dich_weibull_f(H$fitted_model$parameters,x))


H$fitted_model
bmd_dots<-data.frame(H$bmd,.dich_weibull_f(H$fitted_model$parameters,H$bmd))

bmd_dots

test_obj<-ggplot()+
  geom_point(data=H_data, aes(x=D, y=prop))+
  geom_line(data=output, aes(x=x, y=.dich_weibull_f.H.fitted_model.parameters..x.),color="green")+
  geom_segment(data=bmd_dots, aes(x=H.bmd, y=.dich_weibull_f.H.fitted_model.parameters..H.bmd., xend=H.bmd, yend=0), color="Red")+
  xlim(c(0,4))+ylim(c(0,1))+labs(x="Dose", y="Proportion",title="Model: Weibull")+theme_bw()



test_obj_plotly<-ggplotly(test_obj)
test_obj_plotly

H$bmd
p_test<-ggplot()+geom_density(data=model_fitted, aes(x=X1))

p_test
# Q1) Is there any way that I can add title automatically based on the dataset?

