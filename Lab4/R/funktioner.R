linreg<-function(formula,data){
   #browser()
  
  X<-model.matrix(formula)
  y<-as.matrix(get(all.vars(formula)[1]),ncol=1)
  B_hat<-solve(t(X)%*%X)%*%t(X)%*%y
  y_hat<-X%*%B_hat
  res<-as.matrix(y-y_hat,ncol=1)
  n<-nrow(X)
  p<-ncol(X)
  df<-n-p
  sigma_2<-(t(res)%*%res)/df
  Var_B_hat<-diag(sigma_2[1,1]*(solve(t(X)%*%X)))
  t_B<-B_hat/((Var_B_hat)^0.5)
  p_values<-2 * pt(q = -abs(t_B), df=df)
  
  lista <- list(X=X, y=y, B_hat=B_hat, y_hat=y_hat, res=res, n=n, p=p, df=df,
                sigma_2=sigma_2, Var_B_hat=Var_B_hat, t_B=t_B, p_values=p_values)

  returnerad_objekt_i_linregklass <- structure(lista, class="linreg")
  
  return(returnerad_objekt_i_linregklass)
  
}

a<-linreg(y~x2+x1)

# Our three inherited functions, from coef, residuals and predict 
coef.linreg<-function(x){
  return(x$B_hat)
}

coefficients(a)

residuals.linreg<-function(x){
  return(x$res)
}

resid(a)


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


