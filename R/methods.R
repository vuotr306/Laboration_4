
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
  
  sresid <- x$res/sqrt(x$sigma_2[1,1])
  scale_res <- sqrt(abs(sresid))
  df<-data.frame(x$res, x$y_hat, sresid, scale_res)
  colnames(df) <- c("Residuals", "Fittedvalues", "Standardized residuals", "scale_res")
  
#   browser()
  
  
  
  p1 <- ggplot(data=df) + theme_bw() + theme(panel.grid.major = element_blank(), axis.text.y = element_text(angle=90)) + aes(x=Fittedvalues,y=Residuals) + geom_point(shape=21,size=8)+
    ggtitle("Residuals vs Fitted") + geom_hline(yintercept = 0, linetype=3, colour="lightgrey", size=1)
  
  p2 <- ggplot(data=df) + theme_bw() + theme(panel.grid.major = element_blank(), axis.text.y = element_text(angle=90)) + aes(x=Fittedvalues,y=scale_res) + geom_point(shape=21,size=8)+
    ggtitle("Scale-Location") + labs(x = paste("Fitted values\n ", x$formula), y = expression("|Strandardized residuals|"^.5))
  
  
  plot(p1)
  plot(p2)
  
}

#paste("linreg(", paste(all.vars(a$formula), collapse="~"), ")", sep="", collapse="")

plot(a)



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




rstudent.linreg<-function(x){
  se<-sqrt(x$sigma_2*(1-x$H_ii))
  Sres=x$res/se
  return(Sres)
}
rstudent(a)


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













