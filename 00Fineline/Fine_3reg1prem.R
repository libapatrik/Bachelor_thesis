library(data.table)
library(ggplot2)
library(lubridate)
library(dyn)
library(reshape2)
library(readxl)
library(randomForest)
library(MASS)
library(lars)

#####################
# All the work is replication of 
# https://christophj.github.io/replicating/r/replicating-goyal-welch-2008/
# Then try randFor and XGB;

# import data
annual = read_excel("I:/Data_work/PredictorData2018.xlsx", 
                    sheet = "Annual")
#View(annual)

#setwd("/Users/lukaslaffers/Dropbox/liba bc thesis/code v1")
#monthly = read_excel("PredictorData2018.xlsx", sheet = "Monthly")
#annual = read_excel("PredictorData2018.xlsx", sheet = "Annual")

# as.table data
annual  <- as.data.table(annual)
# Data manipulation z Githubu replicating G/W
annual <- annual[, IndexDiv := Index + D12]
annual <- annual[, dp := log(D12) - log(Index)]
annual <- annual[, ep := log(E12) - log(Index)]
vec_dy <- c(NA, annual[2:nrow(annual), log(D12)] - annual[1:(nrow(annual)-1), log(Index)])
annual <- annual[, dy := vec_dy]
annual <- annual[, logret   :=c(NA,diff(log(Index)))]
vec_logretdiv <- c(NA, annual[2:nrow(annual), log(IndexDiv)] - annual[1:(nrow(annual)-1), log(Index)])
vec_logretdiv <- c(NA, log(annual[2:nrow(annual), IndexDiv]/annual[1:(nrow(annual)-1), Index]))
annual <- annual[, logretdiv:=vec_logretdiv]
annual <- annual[, logRfree := log(Rfree + 1)]
annual <- annual[, rp_div   := logretdiv - logRfree]

#Put it in time series (is needed in function get_statistics)
ts_annual <- ts(annual, start=annual[1, Datum], end=annual[nrow(annual), Datum])
#plot(ts_annual[, c("rp_div", "dp", "dy")])

set.seed(123)
ts_df <- ts_annual
indep <- "dp"
indep2 <- "dy"
indep3 <- "ep"
dep <- "rp_div"
h <- 1 
start <- 1872
end <- 2018 
est_periods_OOS <- 20


#### OOS ANALYSIS ####  
# Create error
OOS_error_Nlm1 <- numeric(end - start - est_periods_OOS)  
OOS_error_Alm1 <- numeric(end - start - est_periods_OOS) 

OOS_error_Nlm2 <- numeric(end - start - est_periods_OOS)  
OOS_error_Alm2 <- numeric(end - start - est_periods_OOS)  

OOS_error_Nlm3 <- numeric(end - start - est_periods_OOS)  
OOS_error_Alm3 <- numeric(end - start - est_periods_OOS)

# For loop per model
### LIN #######
j <- 0   # counter 
for (i in (start + est_periods_OOS):(end-1)) {  
  j <- j + 1    # add
  
  #Get the actual ERP that you want to predict
  actual_ERP <- as.numeric(window(ts_df, i+1, i+1)[, dep])
  
  #1. Historical mean model
  OOS_error_Nlm1[j] <- actual_ERP - mean(window(ts_df, start, i)[, dep], na.rm=TRUE)
  
  ###################
  datasetI <- window(ts_df, start, i)
  y <- as.numeric(datasetI[,dep])
  X <- as.numeric(lag(datasetI[,indep],-1))
  
  X2 <- as.numeric(lag(datasetI[,indep2],-1))
  X3 <- as.numeric(lag(datasetI[,indep3],-1))
  
  yy <- y[2:length(y)] 
  XX <- X[1:(length(y)-1)]
  XX2 <- X2[1:(length(y)-1)]
  XX3 <- X3[1:(length(y)-1)]
  
  
  # LinModel
  #reg_OOS <- lm(yy ~ XX)
  lm_OOS1 <- lm(yy ~ XX)
  
  ## Pre linModel a RF s 3 premennymi
  newdat_lm1  <- data.frame(XX=as.numeric(window(ts_df, i, i)[, indep]))
  
  
  #### Tu zvolit predict - 1. moznost pre LM a RF 2. moznost pre LASSO
  ####
  pred_ERP_lm1 <- predict(lm_OOS1, newdata = newdat_lm1) # pre lm a randForest vector predicted
  
  
  OOS_error_Alm1[j] <-  pred_ERP_lm1 - actual_ERP
}


### LIN #######
j <- 0   # counter 
for (i in (start + est_periods_OOS):(end-1)) {  
  j <- j + 1    # add
  
  #Get the actual ERP that you want to predict
  actual_ERP <- as.numeric(window(ts_df, i+1, i+1)[, dep])
  
  #1. Historical mean model
  OOS_error_Nlm2[j] <- actual_ERP - mean(window(ts_df, start, i)[, dep], na.rm=TRUE)
  
  ###################
  datasetI <- window(ts_df, start, i)
  y <- as.numeric(datasetI[,dep])
  X <- as.numeric(lag(datasetI[,indep],-1))
  
  X2 <- as.numeric(lag(datasetI[,indep2],-1))
  X3 <- as.numeric(lag(datasetI[,indep3],-1))
  
  yy <- y[2:length(y)] 
  XX <- X[1:(length(y)-1)]
  XX2 <- X2[1:(length(y)-1)]
  XX3 <- X3[1:(length(y)-1)]
  
  
  # LinModel

  lm_OOS2 <- lm(yy ~ XX2)
  
  ## Pre linModel a RF s 3 premennymi
  newdat_lm2  <- data.frame(XX2=as.numeric(window(ts_df, i, i)[, indep2]))
  
  
  #### Tu zvolit predict - 1. moznost pre LM a RF 2. moznost pre LASSO
  ####
  pred_ERP_lm2 <- predict(lm_OOS2, newdata = newdat_lm2) # pre lm a randForest vector predicted
  
  
  OOS_error_Alm2[j] <-  pred_ERP_lm2 - actual_ERP
}
### LIN #######
j <- 0   # counter 
for (i in (start + est_periods_OOS):(end-1)) {  
  j <- j + 1    # add
  
  #Get the actual ERP that you want to predict
  actual_ERP <- as.numeric(window(ts_df, i+1, i+1)[, dep])
  
  #1. Historical mean model
  OOS_error_Nlm3[j] <- actual_ERP - mean(window(ts_df, start, i)[, dep], na.rm=TRUE)
  
  ###################
  datasetI <- window(ts_df, start, i)
  y <- as.numeric(datasetI[,dep])
  X <- as.numeric(lag(datasetI[,indep],-1))
  
  X2 <- as.numeric(lag(datasetI[,indep2],-1))
  X3 <- as.numeric(lag(datasetI[,indep3],-1))
  
  yy <- y[2:length(y)] 
  XX <- X[1:(length(y)-1)]
  XX2 <- X2[1:(length(y)-1)]
  XX3 <- X3[1:(length(y)-1)]
  
  
  # LinModel
  #reg_OOS <- lm(yy ~ XX)
  lm_OOS3 <- lm(yy ~ XX3)
  
  ## Pre linModel a RF s 3 premennymi
  newdat_lm3  <- data.frame(XX3=as.numeric(window(ts_df, i, i)[, indep3]))
  
  
  #### Tu zvolit predict - 1. moznost pre LM a RF 2. moznost pre LASSO
  ####
  pred_ERP_lm3 <- predict(lm_OOS3, newdata = newdat_lm3) # pre lm a randForest vector predicted
  
  
  OOS_error_Alm3[j] <-  pred_ERP_lm3 - actual_ERP
}
#Compute statistics; 
#MSE_N <- mean(OOS_error_N^2)
#MSE_A <- mean(OOS_error_A^2)
#MSE_A <- length(!is.na(ts_df[, dep]))
#OOS_R2  <- 1 - MSE_A/MSE_N
#Is the -1 enough (maybe -2 needed because of lag)?
#OOS_oR2 <- OOS_R2 - (1-OOS_R2)*(reg$df.residual)/(T - 1) 
#dRMSE <- sqrt(MSE_N) - sqrt(MSE_A)
####


#### CREATE PLOT
OOSlm1 <- cumsum(OOS_error_Nlm1^2) - cumsum(OOS_error_Alm1^2)
OOSlm2 <- cumsum(OOS_error_Nlm2^2) - cumsum(OOS_error_Alm2^2)
OOSlm3 <- cumsum(OOS_error_Nlm3^2) - cumsum(OOS_error_Alm3^2)

df  <- data.frame(x=seq.int(from=start + 1 + est_periods_OOS, to=end),
                  OOSdp=OOSlm1, OOSdy=OOSlm2, OOSep=OOSlm3) 


#df$IS <- df$IS - df$IS[1] 
df  <- melt(df, id.var="x") 
plotGG <- ggplot(df) + 
  geom_line(aes(x=x, y=value, color=variable)) + 
  geom_line(aes(x=x, y=value, color=variable)) +
  geom_line(aes(x=x, y=value, color=variable)) +
  scale_y_continuous('Cumulative SSE Difference') + 
  scale_x_continuous('Year')
####
return(list(IS_error_N = IS_error_N,
            #IS_error_A = reg$residuals,
            OOS_error_N = OOS_error_N,
            OOS_error_A = OOS_error_A,
            #IS_R2 = summary(reg)$r.squared, 
            #IS_aR2 = summary(reg)$adj.r.squared, 
            #OOS_R2  = OOS_R2,
            #OOS_oR2 = OOS_oR2,
            #dRMSE = dRMSE,
            plotGG = plotGG))
#}
plotGG

