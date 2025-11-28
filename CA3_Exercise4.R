## Exercise 3  
### 1.   

df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))


### 2. 

nll_lm <- function(data,par){
  beta <- par[1:4]
  sigma <- par[5]
  
  x <- as.matrix(cbind(1, data[, c("x1", "x2", "x3")]))
  
  yhat <- x %*% beta
  
  res <- data$y - yhat
  
  -sum(dnorm(res, 0, sigma, log = TRUE))
}

est <- lm(mpg~disp+hp+wt, data=mtcars)
beta <- coef(est)
sigma <- summary(est)$sigma
parvals <- c(unname(beta), sigma)

nll_lm(df, parvals)



### 3.


inits <- c(mean(df$y),0,0,0,1)

fit <- optim(inits, nll_lm, 
             data=df,
             method="L-BFGS-B",
             hessian=TRUE)

options(scipen = 999)
beta_o <- fit$par
beta_o 


### 4.  

# Negative likelihood is necessary because optim can only minimise functions. The mle maximises the log likelihood so by minimising the negative log likelihood we get the same parameter estimates. 


### 5.  


X <- as.matrix(cbind(1, df[,c("x1","x2","x3")]))

Y <- df$y

beta_m <- solve(crossprod(X),crossprod(X,Y))

## very similar 
beta_m <- as.numeric(beta_m)
beta_o <- as.numeric(fit$par[1:4])

all.equal(beta_m, beta_o)



### 6. 


sigma_o <- as.numeric(fit$par[5])

res_m <- Y - X %*% beta_m 
sigma_m <- sqrt(sum(res_m^2)/nrow(X))

all.equal(sigma_m, sigma_o)



### 7. 

#The beta values are very similar as both optim and matrix codes compute the same mle for beta.   

#Sigma values arent too different in my code, but they do differ more than beta values,
#this is because the same mle arent computed. 


### 8.  

fit <- optim(inits, nll_lm, 
             data=df,
             method="L-BFGS-B",
             hessian=TRUE)

matr <- solve(fit$hessian)
sqrt(diag(matr)[1:4])



fit2 <- lm(y~ x1 + x2 + x3, data=df)
betah <- coef(fit2)
sigmah <- summary(fit2)$sigma
betah
sigmah

