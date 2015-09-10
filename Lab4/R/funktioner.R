
linreg<-function(formula,data){
   #browser()
  
  X<-model.matrix(formula)  
  y<-as.matrix(get(all.vars(formula)[1]),ncol=1)
  
  QR <- qr((X))
  Q <- qr.Q(QR)
  R <- qr.R(QR)  
  B_hat <- qr.solve(R) %*% t(Q) %*% as.matrix(y)
    
#   B_hat<-solve(t(X)%*%X)%*%t(X)%*%y
  y_hat<-X%*%B_hat
  res<-as.matrix(y-y_hat,ncol=1)
  n<-nrow(X)
  p<-ncol(X)
  df<-n-p
  sigma_2<-(t(res)%*%res)/df
  Var_B_hat<-diag(sigma_2[1,1]*(solve(t(X)%*%X)))
  t_B<-B_hat/((Var_B_hat)^0.5)
  p_values<-2 * pt(q = -abs(t_B), df=df)
  
  lista <- list(formula=formula, X=X, y=y, B_hat=B_hat, y_hat=y_hat, res=res, n=n, p=p, df=df,
                sigma_2=sigma_2, Var_B_hat=Var_B_hat, t_B=t_B, p_values=p_values)

  returnerad_objekt_i_linregklass <- structure(lista, class="linreg")
  
  return(returnerad_objekt_i_linregklass)
  
}

a<-linreg(y~x2+x1)
PL <- iris$Petal.Length
Spe <- iris$Species
a <- linreg(PL~Spe)


a <- linreg(iris$Petal.Length~iris$Species)


# Our three inherited functions, from coef, residuals and predict 
coef.linreg<-function(x){
  
  temp <- rownames(x$B_hat)
  B_hat <- as.vector(x$B_hat)
  names(B_hat) <- temp
  
  return(B_hat)
  
}

coefficients(a)

residuals.linreg<-function(x){
  return(x$res)
}

resid(a)


print.linreg <- function(x){

  temp <- rownames(x$B_hat)
  B_hat <- as.vector(x$B_hat)
  names(B_hat) <- temp
  
  lista <- list(Call=x$formula, Coefficients=B_hat)
  
  return(lista)
}


print(a)

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

plot(a)



ggplot(a$hat,a$res)







summary.linreg<-function(x){
  Out_data<-data.frame(Estimate=x$B_hat,"Std Error"=sqrt(x$Var_B_hat),"t value"=x$t_B,"P-Values"=x$p_values)
  sigma <- as.vector(sqrt(x$sigma_2))
  
  
  Output_lista<-list(Coefficients=Out_data, "Residual standard error"=sigma, "Degrees of freedom" = x$df)
  
  
  return(Output_lista)
}

summary(a)





predict.linreg <- function(x){
  return(x$y_hat)
}

predict(a)





x1 <- iris$Sepal.Width
x2 <- iris$Sepal.Length
y <- iris$Petal.Length

b <- lm(y~x1+x2)
lm()


model.matrix(y~x1+x2)


# 1  linalg	S3	Theme
# 2	linalg	RC	ej theme
# 3	QR	S3	ej theme


# Get the package to work
# Write tests
# Write stopifnots and more.
# Finish the plot function
# Calculate standardized residual...
# and detect outliers?
# Create a vignette









