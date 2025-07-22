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
monthly = read_excel("I:/Data_work/PredictorData2018.xlsx", 
                     sheet = "Monthly")
#View(PredictorData2018)
annual = read_excel("I:/Data_work/PredictorData2018.xlsx", 
                    sheet = "Annual")
#View(annual)

#setwd("/Users/lukaslaffers/Dropbox/liba bc thesis/code v1")
#monthly = read_excel("PredictorData2018.xlsx", sheet = "Monthly")
#annual = read_excel("PredictorData2018.xlsx", sheet = "Annual")

# as.table data
monthly <- as.data.table(monthly)
annual  <- as.data.table(annual)
#View(annual)
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


ts_df <- ts_annual
indep <- "dp"
indep2 <- "dy"
indep3 <- "ep"
dep <- "rp_div"
h <- 1 
start <- 1872
end <- 2018 
est_periods_OOS <- 20


  #### IS ANALYSIS
  
  #1. Historical mean model
  avg   <- mean(window(ts_df, start, end)[, dep], na.rm=TRUE)
  IS_error_N <- (window(ts_df, start, end)[, dep] - avg)   # IN_Sample error for mean
  
  ###################
  datasetIS <- window(ts_df, start,end)
  #y <- as.numeric(datasetIS[,dep])
  #X <- as.numeric(lag(datasetIS[,indep],-1))
  ly <- length(datasetIS[,dep])
  y <- as.numeric(datasetIS[2:ly,dep])
  X <- as.numeric(lag(datasetIS[1:(ly-1),indep],-1))
  #
  X2 <- as.numeric(lag(datasetIS[1:(ly-1),indep2],-1))
  X3 <- as.numeric(lag(datasetIS[1:(ly-1),indep3],-1))
  
  ####
  #### Vybrat model pre IS
  ####    
          # LinModel
  #reg <- lm(y ~ X)
  #reg <- lm(y ~ X + X2 + X3)
  
          # RandFor Model
  set.seed(123)
  #reg <- randomForest(y ~ X)
  reg <- randomForest(y ~ X + X2 + X3)
  
          # LassoModel
  #set.seed(123)
  #cvreg = cv.lars(as.matrix(cbind(X,X2,X3)), as.matrix(y), type="lasso",plot.it=TRUE)
  #cvreg$index[which.min(cvreg$cv)]
  #reg <- lars(as.matrix(cbind(X,X2,X3)), as.matrix(y), type="lasso")
  #beta <- coef.lars(reg, s=cvreg$index[which.min(cvreg$cv)], mode="fraction")
  #predicted_lasso <-predict.lars(object = reg, newx = as.matrix(cbind(X,X2,X3)), s=cvreg$index[which.min(cvreg$cv)],mode="fraction")$fit

  ####
  #### Vybrat error pre IS, podla modelu
  ####    
  # LinModel, RandFor Model - prva moznost, druha moznost pre Lasso
  IS_error_A <- (predict(reg) - y) # lm and randomForest
  #IS_error_A = (predicted_lasso - y) # lasso 


  #### OOS ANALYSIS ####  
  
  OOS_error_N <- numeric(end - start - est_periods_OOS)  
  OOS_error_A <- numeric(end - start - est_periods_OOS)  
  
  #Only use information that is available up to the time at which the forecast is made
  j <- 0   # counter 
  for (i in (start + est_periods_OOS):(end-1)) {  
    ### start is a set date,
    ### est_periods_OOS is set to 20 / decides how many periods are used to compute the out-of-sample statistics
    ### end is a set date (2005 or 2018 as end of data)
    
    j <- j + 1    # add
    
    #Get the actual ERP that you want to predict
    actual_ERP <- as.numeric(window(ts_df, i+1, i+1)[, dep])
    
    #1. Historical mean model
    OOS_error_N[j] <- actual_ERP - mean(window(ts_df, start, i)[, dep], na.rm=TRUE)
    
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

    #### Tu zvolit model pre OOS; podla modelu pre IS
    ####
    # LinModel
    #reg_OOS <- lm(yy ~ XX)
    #reg_OOS <- lm(yy ~ XX + XX2 + XX3)
    
    # RandFor Model
    #reg_OOS = randomForest(yy ~ XX)
    reg_OOS <- randomForest(yy ~ XX + XX2 + XX3)
    
    ## LASSO
    # CV
    ### 
    #set.seed(123)
    #cvreg = cv.lars(as.matrix(cbind(XX,XX2,XX3)), as.matrix(yy), type="lasso", plot.it=TRUE)
    #reg <- lars(as.matrix(cbind(XX,XX2,XX3)), as.matrix(yy), type="lasso")
    #pred_ERP <-predict.lars(object = reg, newx = newdat, s=cvreg$index[which.min(cvreg$cv)],mode="fraction")$fit

    #### Vybrat newdat pre OOS, podla modelu
    ####
    ## newdat pre lm a RF s 1 premennou
    #newdat  <- data.frame(XX=as.numeric(window(ts_df, i, i)[, indep])) 
    
    ## Pre linModel a RF s 3 premennymi
    newdat  <- data.frame(XX=as.numeric(window(ts_df, i, i)[, indep]),
                         XX2=as.numeric(window(ts_df, i, i)[, indep2]),
                        XX3=as.numeric(window(ts_df, i, i)[, indep3]))
    
    ## newdat pre Lasso;
    #newdat <- cbind(as.numeric(window(ts_df, i+1, i+1)[, indep]),
    #                as.numeric(window(ts_df, i+1, i+1)[, indep2]),
    #                as.numeric(window(ts_df, i+1, i+1)[, indep3]))

    #### Tu zvolit predict - 1. moznost pre LM a RF 2. moznost pre LASSO
    ####
    pred_ERP <- predict(reg_OOS, newdata = newdat) # pre lm a randForest vector predicted
    #pred_ERP <-predict.lars(object = reg, newx = newdat, s=cvreg$index[which.min(cvreg$cv)], mode="fraction")$fit # Pre Lasso predicted


    OOS_error_A[j] <-  pred_ERP - actual_ERP
 }

  #Compute statistics; 
  MSE_N <- mean(OOS_error_N^2)
  MSE_A <- mean(OOS_error_A^2)
  MSE_A <- length(!is.na(ts_df[, dep]))
  OOS_R2  <- 1 - MSE_A/MSE_N
  #Is the -1 enough (maybe -2 needed because of lag)?
  #OOS_oR2 <- OOS_R2 - (1-OOS_R2)*(reg$df.residual)/(T - 1) 
  dRMSE <- sqrt(MSE_N) - sqrt(MSE_A)
  ####
  #### CREATE PLOT
  IS  <- cumsum(IS_error_N[2:length(IS_error_N)]^2) - cumsum(IS_error_A^2)
  OOS <- cumsum(OOS_error_N^2) - cumsum(OOS_error_A^2)
  df  <- data.frame(x=seq.int(from=start + 1 + est_periods_OOS, to=end), 
                    IS=IS[(1+est_periods_OOS):length(IS)], 
                    OOS=OOS) #Because you lose one observation due to the lag
  #Shift IS errors vertically, so that the IS line begins 
  # at zero on the date of first OOS prediction. (see Goyal/Welch (2008, p. 1465))
  df$IS <- df$IS - df$IS[1] 
  df  <- melt(df, id.var="x") 
  plotGG <- ggplot(df) + 
    geom_line(aes(x=x, y=value,color=variable)) + 
    geom_rect(data=data.frame(),#Needed by ggplot2, otherwise not transparent
              #aes(xmin=1973, xmax=1975,ymin=min(IS,OOS),ymax=max(IS,OOS)), 
              aes(xmin=1973, xmax=1975,ymin=-0.2,ymax=0.2), 
              fill='red',
              alpha=0.1) + 
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
  
  
  
  
  
#dp_stat <- get_statistics2(ts_annual, "dp", "rp_div", start=1872)
#dp_stat$plotGG
#dp_stat

  
  
#plot(1893:2018,cumsum(dp_stat$OOS_error_A),'l',col = 'red')
#lines(1893:2018,cumsum(dp_stat$OOS_error_N),'l',col = 'blue')


#dy_stat <- get_statistics2(ts_annual, "dy", "rp_div", start=1872)
#dy_stat$plotGG
#dy_stat

#ep_stat <- get_statistics2(ts_annual, "ep", "rp_div", start=1872)
#ep_stat$plotGG
#ep_stat

#####
