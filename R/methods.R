
# Our inherited functions, from coef, residuals and predict 
print.linreg <- function(x){
  
  temp <- rownames(x$B_hat)
  B_hat <- as.vector(x$B_hat)
  names(B_hat) <- temp
  
  lista <- list(Call=x$formula, Coefficients=B_hat)
  
  return(lista)
}

# print(a)



plot.linreg<-function(x){
  
  sresid_temp <- x$res/sqrt(x$sigma_2[1,1])
  df<-data.frame(Residuals=x$res,"Fitted values"=x$y_hat, "Standardized residuals"=sresid_temp)
  
  p1<-ggplot(data=df) + aes(x=Fitted.values,y=Residuals) + geom_point(shape=21)+
    geom_hline(yintercept = 0,)+ggtitle("Residuals vs Fitted")  
  
  p2 <- ggplot(data=df) + aes(x=Fitted.values,y=Standardized.residuals)+ geom_point(shape=21)+
    ggtitle("Scale-Location")
  
  plot(p1)
  plot(p2)
  
}

# plot(a)



coef.linreg<-function(x){
  
  temp <- rownames(x$B_hat)
  B_hat <- as.vector(x$B_hat)
  names(B_hat) <- temp
  
  return(B_hat)
  
}

# coefficients(a)




residuals.linreg<-function(x){
  return(x$res)
}

# resid(a)



summary.linreg<-function(x){
  Out_data<-data.frame(Estimate=x$B_hat,"Std Error"=sqrt(x$Var_B_hat),"t value"=x$t_B,"P-Values"=x$p_values)
  sigma <- as.vector(sqrt(x$sigma_2))
  
  
  Output_lista<-list(Coefficients=Out_data, "Residual standard error"=sigma, "Degrees of freedom" = x$df)
  
  
  return(Output_lista)
}

# summary(a)



predict.linreg <- function(x){
  return(x$y_hat)
}

# predict(a)













