# Testar fraan laptop.
linreg<-function(formula, data){
#    browser()


   stopifnot(!is.null(data))
   stopifnot(is.data.frame(data))
   stopifnot(sum(1-c(all.vars(formula)) %in% colnames(data)) == 0)
#    stopifnot(is.numeric(data$all.vars(formula)[1]))

  X<-model.matrix(formula, data=data)
  y<-as.matrix(data[all.vars(formula)[1]],ncol=1)

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
  H_ii <-diag(X %*% solve(t(X) %*% X) %*% t(X))
  linreg_list <- list(formula=formula, data=data, X=X, y=y, B_hat=B_hat, y_hat=y_hat, res=res, n=n, p=p, df=df,
                sigma_2=sigma_2, Var_B_hat=Var_B_hat, t_B=t_B, p_values=p_values, H_ii=H_ii)

  linreg_object <- structure(linreg_list, class="linreg")

  return(linreg_object)

}



# the linreg function
 a <- linreg(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
 a <- linreg(Petal.Length~Species, data=iris)

 # the lm function
 b <- lm(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
 b <- lm(Petal.Length~Species, data=iris)
 summary(b)


# Anteckningar ----
# 1  linalg	S3	Theme
# 2	linalg	RC	ej theme
# 3	QR	S3	ej theme


# Fix the bug !!!
# More tests?
# Write stopifnots and more.
# ??? Finish the plot function (outliers)?
# ??? More vignette ?









