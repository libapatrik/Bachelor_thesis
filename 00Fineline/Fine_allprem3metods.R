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
annual = read_excel("I:/Data_work/PredictorData2018short.xlsx", 
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

# Treba prerobit strukturu
ts_df <- ts_annual
indep <- "dp"
indep2 <- "dy"
indep3 <- "ep"
indep4 <- "D12"
indep5 <- "E12"
indep6 <- "tbl"
indep7 <- "AAA"
indep8 <- "BAA"
indep9 <- "lty"
indep10 <- "b/m"
indep11 <- "ntis"
#indep12 <- "Rfree"
indep13 <- "infl"
indep14 <- "eqis"
indep15 <- "ltr"
indep16 <- "Index"
indep17 <- "corpr"
indep18 <- "svar"
indep19 <- "CRSP_SPvw"
dep <- "rp_div"
h <- 1 
start <- 1930
end <- 2018 
est_periods_OOS <- 20


#### OOS ANALYSIS ####  
# Create error
OOS_error_Nlm <- numeric(end - start - est_periods_OOS)  
OOS_error_Alm <- numeric(end - start - est_periods_OOS) 

OOS_error_Nlas <- numeric(end - start - est_periods_OOS)  
OOS_error_Alas <- numeric(end - start - est_periods_OOS)  

OOS_error_Nrf <- numeric(end - start - est_periods_OOS)  
OOS_error_Arf <- numeric(end - start - est_periods_OOS)  

# For loop per model
### LIN #######
j <- 0   # counter 
for (i in (start + est_periods_OOS):(end-1)) {  
  j <- j + 1    # add
  
  #Get the actual ERP that you want to predict
  actual_ERP <- as.numeric(window(ts_df, i+1, i+1)[, dep])
  
  #1. Historical mean model
  OOS_error_Nlm[j] <- actual_ERP - mean(window(ts_df, start, i)[, dep], na.rm=TRUE)
  
  ###################
  ###################
  datasetI <- window(ts_df, start, i)
  y <- as.numeric(datasetI[,dep])
  X <- as.numeric(lag(datasetI[,indep],-1))
  
  #X2 <- as.numeric(lag(datasetI[,indep2],-1))
  X3 <- as.numeric(lag(datasetI[,indep3],-1))
  X4 <- as.numeric(lag(datasetI[,indep4],-1))
  X5 <- as.numeric(lag(datasetI[,indep5],-1))
  X6 <- as.numeric(lag(datasetI[,indep6],-1))
  X7 <- as.numeric(lag(datasetI[,indep7],-1))
  X8 <- as.numeric(lag(datasetI[,indep8],-1))
  X9 <- as.numeric(lag(datasetI[,indep9],-1))
  X10 <- as.numeric(lag(datasetI[,indep10],-1))
  X11 <- as.numeric(lag(datasetI[,indep11],-1))
  X13 <- as.numeric(lag(datasetI[,indep13],-1))
  X14 <- as.numeric(lag(datasetI[,indep14],-1))
  X15 <- as.numeric(lag(datasetI[,indep15],-1))
  X16 <- as.numeric(lag(datasetI[,indep16],-1))
  X17 <- as.numeric(lag(datasetI[,indep17],-1))
  X18 <- as.numeric(lag(datasetI[,indep18],-1))
  X19 <- as.numeric(lag(datasetI[,indep19],-1))
  #X12 <- as.numeric(lag(datasetI[,indep12],-1))
  yy <- y[2:length(y)] 
  XX <- X[1:(length(y)-1)]
  #XX2 <- X2[1:(length(y)-1)]
  XX3 <- X3[1:(length(y)-1)]
  XX4 <- X4[1:(length(y)-1)]
  XX5 <- X5[1:(length(y)-1)]
  XX6 <- X6[1:(length(y)-1)]
  XX7 <- X7[1:(length(y)-1)]
  XX8 <- X8[1:(length(y)-1)]
  XX9 <- X9[1:(length(y)-1)]
  XX10 <- X10[1:(length(y)-1)]
  XX11 <- X11[1:(length(y)-1)]
  #XX12 <- X12[1:(length(y)-1)]
  XX13 <- X13[1:(length(y)-1)]
  XX14 <- X14[1:(length(y)-1)]
  XX15 <- X15[1:(length(y)-1)]
  XX16 <- X16[1:(length(y)-1)]
  XX17 <- X17[1:(length(y)-1)]
  XX18 <- X18[1:(length(y)-1)]
  XX19 <- X19[1:(length(y)-1)]
  
  
  # LinModel
  #reg_OOS <- lm(yy ~ XX)
  lm_OOS <- lm(yy ~ XX+XX3+XX4+XX5+XX6+XX7+XX8+XX9+XX10+XX11+XX13+XX14+XX15+XX16+XX17+XX18+XX19)
  
  ## Pre linModel a RF s 3 premennymi
  newdat_lm  <- data.frame(XX=as.numeric(window(ts_df, i, i)[, indep]),
                        XX2=as.numeric(window(ts_df, i, i)[, indep2]),
                        XX3=as.numeric(window(ts_df, i, i)[, indep3]),
                        XX4=as.numeric(window(ts_df, i, i)[, indep4]),
                        XX5=as.numeric(window(ts_df, i, i)[, indep5]),
                        XX6=as.numeric(window(ts_df, i, i)[, indep6]),
                        XX7=as.numeric(window(ts_df, i, i)[, indep7]),
                        XX8=as.numeric(window(ts_df, i, i)[, indep8]),
                        XX9=as.numeric(window(ts_df, i, i)[, indep9]),
                        XX10=as.numeric(window(ts_df, i, i)[, indep10]),
                        XX11=as.numeric(window(ts_df, i, i)[, indep11]),
                        XX13=as.numeric(window(ts_df, i, i)[, indep13]),
                        XX14=as.numeric(window(ts_df, i, i)[, indep14]),
                        XX15=as.numeric(window(ts_df, i, i)[, indep15]),
                        XX16=as.numeric(window(ts_df, i, i)[, indep16]),
                        XX17=as.numeric(window(ts_df, i, i)[, indep17]),
                        XX18=as.numeric(window(ts_df, i, i)[, indep18]),
                        XX19=as.numeric(window(ts_df, i, i)[, indep19]))
  
  
  #### Tu zvolit predict - 1. moznost pre LM a RF 2. moznost pre LASSO
  ####
  pred_ERP_lm <- predict(lm_OOS, newdata = newdat_lm) # pre lm a randForest vector predicted
  
  
  OOS_error_Alm[j] <-  pred_ERP_lm - actual_ERP
}


### LASSO ################
j <- 0   # counter 
for (i in (start + est_periods_OOS):(end-1)) {  
  j <- j + 1    # add
  actual_ERP <- as.numeric(window(ts_df, i+1, i+1)[, dep])
  #1. Historical mean model
  OOS_error_Nlas[j] <- actual_ERP - mean(window(ts_df, start, i)[, dep], na.rm=TRUE)
  
  ###################
  ###################
  datasetI <- window(ts_df, start, i)
  y <- as.numeric(datasetI[,dep])
  X <- as.numeric(lag(datasetI[,indep],-1))
  
  #X2 <- as.numeric(lag(datasetI[,indep2],-1))
  X3 <- as.numeric(lag(datasetI[,indep3],-1))
  X4 <- as.numeric(lag(datasetI[,indep4],-1))
  X5 <- as.numeric(lag(datasetI[,indep5],-1))
  X6 <- as.numeric(lag(datasetI[,indep6],-1))
  X7 <- as.numeric(lag(datasetI[,indep7],-1))
  X8 <- as.numeric(lag(datasetI[,indep8],-1))
  X9 <- as.numeric(lag(datasetI[,indep9],-1))
  X10 <- as.numeric(lag(datasetI[,indep10],-1))
  X11 <- as.numeric(lag(datasetI[,indep11],-1))
  X13 <- as.numeric(lag(datasetI[,indep13],-1))
  X14 <- as.numeric(lag(datasetI[,indep14],-1))
  X15 <- as.numeric(lag(datasetI[,indep15],-1))
  X16 <- as.numeric(lag(datasetI[,indep16],-1))
  X17 <- as.numeric(lag(datasetI[,indep17],-1))
  X18 <- as.numeric(lag(datasetI[,indep18],-1))
  X19 <- as.numeric(lag(datasetI[,indep19],-1))
  #X12 <- as.numeric(lag(datasetI[,indep12],-1))
  yy <- y[2:length(y)] 
  XX <- X[1:(length(y)-1)]
  #XX2 <- X2[1:(length(y)-1)]
  XX3 <- X3[1:(length(y)-1)]
  XX4 <- X4[1:(length(y)-1)]
  XX5 <- X5[1:(length(y)-1)]
  XX6 <- X6[1:(length(y)-1)]
  XX7 <- X7[1:(length(y)-1)]
  XX8 <- X8[1:(length(y)-1)]
  XX9 <- X9[1:(length(y)-1)]
  XX10 <- X10[1:(length(y)-1)]
  XX11 <- X11[1:(length(y)-1)]
  #XX12 <- X12[1:(length(y)-1)]
  XX13 <- X13[1:(length(y)-1)]
  XX14 <- X14[1:(length(y)-1)]
  XX15 <- X15[1:(length(y)-1)]
  XX16 <- X16[1:(length(y)-1)]
  XX17 <- X17[1:(length(y)-1)]
  XX18 <- X18[1:(length(y)-1)]
  XX19 <- X19[1:(length(y)-1)]
  set.seed(123)
  cvreg = cv.lars(as.matrix(cbind(XX,XX3,XX4,XX5,XX6,XX7,XX8,XX9,XX10,XX11,XX13,XX14,XX15,XX16,XX17,XX18,XX19)), as.matrix(yy), type="lasso", plot.it=FALSE)
  reg <- lars(as.matrix(cbind(XX,XX3,XX4,XX5,XX6,XX7,XX8,XX9,XX10,XX11,XX13,XX14,XX15,XX16,XX17,XX18,XX19)), as.matrix(yy), type="lasso")
  
  ## newdat pre Lasso;
  newdat_las <- cbind(as.numeric(window(ts_df, i+1, i+1)[, indep]),
                  as.numeric(window(ts_df, i+1, i+1)[, indep3]),
                  as.numeric(window(ts_df, i+1, i+1)[, indep4]),
                  as.numeric(window(ts_df, i+1, i+1)[, indep5]),
                  as.numeric(window(ts_df, i+1, i+1)[, indep6]),
                  as.numeric(window(ts_df, i+1, i+1)[, indep7]),
                  as.numeric(window(ts_df, i+1, i+1)[, indep8]),
                  as.numeric(window(ts_df, i+1, i+1)[, indep9]),
                  as.numeric(window(ts_df, i+1, i+1)[, indep10]),
                  as.numeric(window(ts_df, i+1, i+1)[, indep11]),
                  as.numeric(window(ts_df, i+1, i+1)[, indep13]),
                  as.numeric(window(ts_df, i+1, i+1)[, indep14]),
                  as.numeric(window(ts_df, i+1, i+1)[, indep15]),
                  as.numeric(window(ts_df, i+1, i+1)[, indep16]),
                  as.numeric(window(ts_df, i+1, i+1)[, indep17]),
                  as.numeric(window(ts_df, i+1, i+1)[, indep18]),
                  as.numeric(window(ts_df, i+1, i+1)[, indep19]))
  pred_ERP_las <- predict.lars(object = reg, newx = newdat_las, s=cvreg$index[which.min(cvreg$cv)],mode="fraction")$fit
  
  OOS_error_Alas[j] <-  pred_ERP_las - actual_ERP
}

### RF ####
j <- 0   # counter 
for (i in (start + est_periods_OOS):(end-1)) {  
  j <- j + 1    # add
  actual_ERP <- as.numeric(window(ts_df, i+1, i+1)[, dep])
  #1. Historical mean model
  OOS_error_Nrf[j] <- actual_ERP - mean(window(ts_df, start, i)[, dep], na.rm=TRUE)
  
  ###################
  ###################
  datasetI <- window(ts_df, start, i)
  y <- as.numeric(datasetI[,dep])
  X <- as.numeric(lag(datasetI[,indep],-1))
  
  #X2 <- as.numeric(lag(datasetI[,indep2],-1))
  X3 <- as.numeric(lag(datasetI[,indep3],-1))
  X4 <- as.numeric(lag(datasetI[,indep4],-1))
  X5 <- as.numeric(lag(datasetI[,indep5],-1))
  X6 <- as.numeric(lag(datasetI[,indep6],-1))
  X7 <- as.numeric(lag(datasetI[,indep7],-1))
  X8 <- as.numeric(lag(datasetI[,indep8],-1))
  X9 <- as.numeric(lag(datasetI[,indep9],-1))
  X10 <- as.numeric(lag(datasetI[,indep10],-1))
  X11 <- as.numeric(lag(datasetI[,indep11],-1))
  X13 <- as.numeric(lag(datasetI[,indep13],-1))
  X14 <- as.numeric(lag(datasetI[,indep14],-1))
  X15 <- as.numeric(lag(datasetI[,indep15],-1))
  X16 <- as.numeric(lag(datasetI[,indep16],-1))
  X17 <- as.numeric(lag(datasetI[,indep17],-1))
  X18 <- as.numeric(lag(datasetI[,indep18],-1))
  X19 <- as.numeric(lag(datasetI[,indep19],-1))
  #X12 <- as.numeric(lag(datasetI[,indep12],-1))
  yy <- y[2:length(y)] 
  XX <- X[1:(length(y)-1)]
  #XX2 <- X2[1:(length(y)-1)]
  XX3 <- X3[1:(length(y)-1)]
  XX4 <- X4[1:(length(y)-1)]
  XX5 <- X5[1:(length(y)-1)]
  XX6 <- X6[1:(length(y)-1)]
  XX7 <- X7[1:(length(y)-1)]
  XX8 <- X8[1:(length(y)-1)]
  XX9 <- X9[1:(length(y)-1)]
  XX10 <- X10[1:(length(y)-1)]
  XX11 <- X11[1:(length(y)-1)]
  #XX12 <- X12[1:(length(y)-1)]
  XX13 <- X13[1:(length(y)-1)]
  XX14 <- X14[1:(length(y)-1)]
  XX15 <- X15[1:(length(y)-1)]
  XX16 <- X16[1:(length(y)-1)]
  XX17 <- X17[1:(length(y)-1)]
  XX18 <- X18[1:(length(y)-1)]
  XX19 <- X19[1:(length(y)-1)]
  
  
  # RandFor Model
  rf_OOS <- randomForest(yy ~ XX+XX3+XX4+XX5+XX6+XX7+XX8+XX9+XX10+XX11+XX13+XX14+XX15+XX16+XX17+XX18+XX19)
  
  
  ## Pre linModel a RF s 3 premennymi
  newdat_rf  <- data.frame(XX=as.numeric(window(ts_df, i, i)[, indep]),
                        XX2=as.numeric(window(ts_df, i, i)[, indep2]),
                        XX3=as.numeric(window(ts_df, i, i)[, indep3]),
                        XX4=as.numeric(window(ts_df, i, i)[, indep4]),
                        XX5=as.numeric(window(ts_df, i, i)[, indep5]),
                        XX6=as.numeric(window(ts_df, i, i)[, indep6]),
                        XX7=as.numeric(window(ts_df, i, i)[, indep7]),
                        XX8=as.numeric(window(ts_df, i, i)[, indep8]),
                        XX9=as.numeric(window(ts_df, i, i)[, indep9]),
                        XX10=as.numeric(window(ts_df, i, i)[, indep10]),
                        XX11=as.numeric(window(ts_df, i, i)[, indep11]),
                        XX13=as.numeric(window(ts_df, i, i)[, indep13]),
                        XX14=as.numeric(window(ts_df, i, i)[, indep14]),
                        XX15=as.numeric(window(ts_df, i, i)[, indep15]),
                        XX16=as.numeric(window(ts_df, i, i)[, indep16]),
                        XX17=as.numeric(window(ts_df, i, i)[, indep17]),
                        XX18=as.numeric(window(ts_df, i, i)[, indep18]),
                        XX19=as.numeric(window(ts_df, i, i)[, indep19]))
  
  
  pred_ERP_rf <- predict(rf_OOS, newdata = newdat_rf) # pre lm a randForest vector predicted
  
  
  
  OOS_error_Arf[j] <-  pred_ERP_rf - actual_ERP
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
OOSlm <- cumsum(OOS_error_Nlm^2) - cumsum(OOS_error_Alm^2)
OOSlas <- cumsum(OOS_error_Nlas^2) - cumsum(OOS_error_Alas^2)
OOSrf <- cumsum(OOS_error_Nrf^2) - cumsum(OOS_error_Arf^2)

df  <- data.frame(x=seq.int(from=start + 1 + est_periods_OOS, to=end),
                  OOSlm=OOSlm, OOSlas=OOSlas, OOSrf = OOSrf) 


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

