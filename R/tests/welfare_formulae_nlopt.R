### Welfare effect extrapolations
## Marginal and non-marginal changes
## New formulae August 27


#### Objective Functions ----------------

### Marginal changes

# Normalization function for bernstein polynomial
normalize <- function(p, min, max) {
  (p - min)/(max - min)
}

# Bernstein polynomial basis 
bernstein <- function(p, t, sigma, k, K, min, max){
  p <- normalize(p +t*sigma, min, max)
  choose(K, k) * p^k * (1 - p)^(K - k)
}

# Elasticity
log.elas <- function(mu, p, t, sigma, min, max) {
  t <- log(1+t)
  K <- length(mu)
  j <- 0:(K-1)
  b_k <- sapply(j, function (j, p, t, sigma, k, K, min, max) bernstein(p = p, t = t, sigma = sigma, k = j, K = K, min = min, max = max), 
                p = p, t = t, sigma = sigma, K = K - 1, min = min, max = max)
  return(-(sum(mu*b_k)))
}

# Scaling by sigma salience
A.sigma <- function(t, sigma) sigma*((1+t) - (1+t)^sigma)/(1+t)

# Scalar in the numerator
M.theta <- function(t, theta)  (1+theta+t)/(1+t)

# Scaling to calculate FE
B.sigma <- function(t, sigma) sigma*t/(1+t)


# Marginal change function 
marginal.change <- function(mu, data, pp, tau, theta, sigma, w, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  
  # Take producer price
  p <- data[[pp]] 

  # Get tax rate (t not log(1+t))
  t <- exp(data[[tau]]) - 1
  
  # Put together and transform to list to apply
  X <- rbind(p, t)
  X <- lapply(seq_len(ncol(X)), function(i) X[,i])
  
  # Numerator
  numer.vector <- sapply(X, function(x, mu, theta, sigma, min, max) 
                                  M.theta(x["t"], theta) - A.sigma(x["t"], sigma)*log.elas(mu, x["p"], x["t"], sigma, min, max),
                                  mu = mu, theta = theta, sigma = sigma, min = min, max = max)
  
  # Denominator
  denom.vector <- sapply(X, function(x, mu, theta, sigma, min, max) 
                                  B.sigma(x["t"], sigma)*log.elas(mu, x["p"], x["t"], sigma, min, max),
                                  mu = mu, theta = theta, sigma = sigma, min = min, max = max)
  # Get weighted average
  w <- data[[w]]
  return(weighted.mean(numer.vector, w = w)/(1-weighted.mean(denom.vector, w = w)))
}

max.marginal.change <- function(mu, data, pp, tau, theta, sigma, w, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  return(-marginal.change(mu, data, pp, tau, theta, sigma, w, min, max, constr_mat, IV_mat, min.crit, elas))
}
  
### Non-Marginal changes

# Integral of bernstein
int.bernstein <- function(p, t, sigma, k, K, min, max) {
  
  j <- (k+1):(K+1)
  b_k <- sapply(j, function (j, p, t, sigma, k, K, min, max) bernstein(p = p, t = t, sigma = sigma, k = j, K = K, min = min, max = max), 
                p = p, t = t, sigma = sigma, K = K + 1, min = min, max = max)
  
  return(sum(b_k)/(K+1))
}
# log demand
log.demand <- function(p, t, sigma, mu, mu0 = NULL, min, max) {
  # If we provide a constant then use it do retrieve the implied demand
  t <- log(1+t)
  K <- length(mu)
  if (!is.null(mu0)) {
    mu.p <- mu0
    for (k in 1:(K+1)) {
      if (k < (K+1)) mu.p[k+1] <- mu.p[k] + mu[k]
    }
    j <- 0:K
    b_k <- sapply(j, function (j, p, t, sigma, k, K, min, max) bernstein(p = p, t = t, sigma = sigma, k = j, K = K, min = min, max = max), 
                  p = p, t = t, sigma = sigma, K = K, min = min, max = max)
    return((sum(mu.p*b_k)))    
  }
  # If mu0 is not provided, then use integrated bernstein polynomial and omit constant
  if (is.null(mu0)) {
    polynomial <- rep(0,K)
    for (k in 1:K) {
      polynomial[k] <- int.bernstein(p, t, sigma, k, K-1, min, max)
    }
    return((sum(mu*polynomial)))    
  }  
}


# Demand
demand <- function(p, t, sigma, mu, mu0 = NULL, min, max) {
  return(exp(log.demand(p, t, sigma, mu, mu0, min, max)))
}

# Integrand numerator
integrand.num <- function(t, p, sigma, theta, mu, min, max, mu0 = NULL) {
  return(demand(p, t, sigma, mu, mu0, min, max)*(M.theta(t,theta)-A.sigma(t, sigma)*log.elas(mu, p, t, sigma, min, max))/100)
}
# Apply integral to every value of t (this so the integral function can use vectors)
int.apply.num <- function(x, mu, p, sigma, theta, mu0, min, max) {
  sapply(x, integrand.num, p = p, sigma = sigma, theta = theta, mu=mu, mu0=mu0, min=min, max=max)
}

#New: Integral in denominator too
integrand.den <- function(t, p, sigma, mu, min, max, mu0 = NULL) {
  return(demand(p, t, sigma, mu, mu0, min, max)*(1-B.sigma(t, sigma)*log.elas(mu, p, t, sigma, min, max))/100)
}
int.apply.den <- function(x, mu, p, sigma, mu0, min, max) {
  sapply(x, integrand.den, p = p, sigma = sigma, mu=mu, mu0=mu0, min=min, max=max)
}


# Funtion to extract mu0
implied.mu0 <- function(p, t, sigma, mu, min, max, n.value) {
  num <- demand(p = p, t = t, sigma = sigma, mu = mu, min = min, max = max)
  c <- rep(1, length(mu) + 1)
  den <- log.elas(p = p, t = t, sigma = sigma, mu = c, min = min, max = max)
  return((n.value - num)/den)
}

# Non-Marginal change function
non.marginal.change <- function(mu, data, pp, t0, t1, theta, sigma, w, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  
  # Normalize to get a value of \mu_0: set to NULL we know is the same
  mu0 <- NULL
  
  # Use vectors 
  ll <- exp(data[[t0]])-1
  ul <- exp(data[[t1]])-1
  p <- data[[pp]]
  
  
  # Put together and transform to list
  X <- rbind(ll, ul, p)
  X <- lapply(seq_len(ncol(X)), function(i) X[,i])
  
  # Calculate initial demand
  init.dem <- sapply(X, function(x, mu, sigma, min, max, mu0) 
                     demand(p = x["p"], t = x["ll"], sigma = sigma, 
                            mu = mu, mu0 = mu0, min = min, max = max), 
                     mu = mu, sigma = sigma, min = min, max = max, mu0 = mu0)

  # sapply from list to numerator
  int.num <- sapply(X, function(x, p, mu, sigma, theta, min, max, mu0) 
    integrate(int.apply.num, 
              lower = x["ll"], 
              upper = x["ul"], 
              p = x["p"],
              mu = mu,
              sigma = sigma,
              theta = theta,
              min = min, 
              max = max,
              mu0 = mu0)$value, mu = mu, sigma = sigma, theta = theta, min = min, max = max, mu0 = mu0)
  
    
  ## OLD
  # # Denominator
  # denom.vector <- ul*(demand(p, ul, sigma, mu, mu0, min, max)/demand(p, ll, sigma, mu, mu0, min, max)) - ll
  
  ## NEW denominator
  int.den <- sapply(X, function(x, p, mu, sigma, theta, min, max, mu0) 
    integrate(int.apply.den, 
              lower = x["ll"], 
              upper = x["ul"], 
              p = x["p"],
              mu = mu,
              sigma = sigma,
              min = min, 
              max = max,
              mu0 = mu0)$value, mu = mu, sigma = sigma, min = min, max = max, mu0 = mu0)
  
  # divide numerator and denominator by initial demand
  numer.vector <- int.num/init.dem
  denom.vector <- int.den/init.dem

  # Get weighted average
  w <- data[[w]] 
  w <- w/sum(w)

  numer <- weighted.mean(numer.vector, w = w)
  denom <- weighted.mean(denom.vector, w = w)
  # Sometimes we get Infinite as result as R can't handle large precission numbers. In such case we return 1
  res <-numer/denom
  if(is.infinite(numer) | is.infinite(denom)) res <- 1
    
  return(res)
}



max.non.marginal.change <- function(mu, data, pp, t0, t1, theta, sigma, w, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  return(-non.marginal.change(mu, data, pp, t0, t1, theta, sigma, w, min, max, constr_mat, IV_mat, min.crit, elas))
}


# Average across states

# Marginal change function 
av.marginal.change <- function(mu, data, pp, tau, theta, sigma, w, st.code, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  
  # Take producer price
  p <- data[[pp]] 
  
  # Get tax rate (t not log(1+t))
  t <- exp(data[[tau]]) - 1
  
  # Put together and transform to list to apply
  X <- rbind(p, t)
  X <- lapply(seq_len(ncol(X)), function(i) X[,i])
  
  # Numerator
  numer.vector <- sapply(X, function(x, mu, theta, sigma, min, max) 
    M.theta(x["t"], theta) - A.sigma(x["t"], sigma)*log.elas(mu, x["p"], x["t"], sigma, min, max),
    mu = mu, theta = theta, sigma = sigma, min = min, max = max)
  
  # Denominator
  denom.vector <- sapply(X, function(x, mu, theta, sigma, min, max) 
    B.sigma(x["t"], sigma)*log.elas(mu, x["p"], x["t"], sigma, min, max),
    mu = mu, theta = theta, sigma = sigma, min = min, max = max)
  # Put together info: vectors are organized as provided
  w <- data[[w]]
  st.code <- data[[st.code]]
  data.final <- data.table(numer.vector, denom.vector, w, st.code)
  # Get weighted average by state
  data.final <- data.final[, .(MVPF.state = weighted.mean(numer.vector, w = w)/(1-weighted.mean(denom.vector, w = w))), by =.(st.code)]  
  # Average across states
  
  return(mean(data.final$MVPF.state))
}

max.av.marginal.change <- function(mu, data, pp, tau, theta, sigma, w, st.code, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  return(-av.marginal.change(mu, data, pp, tau, theta, sigma, w, st.code, min, max, constr_mat, IV_mat, min.crit, elas))
}

av.non.marginal.change <- function(mu, data, pp, t0, t1, theta, sigma, w, st.code, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  
  # Normalize to get a value of \mu_0: set to NULL we know is the same
  mu0 <- NULL
  
  # Use vectors 
  ll <- exp(data[[t0]])-1
  ul <- exp(data[[t1]])-1
  p <- data[[pp]]
  
  
  # Put together and transform to list
  X <- rbind(ll, ul, p)
  X <- lapply(seq_len(ncol(X)), function(i) X[,i])
  
  # Calculate initial demand
  init.dem <- sapply(X, function(x, mu, sigma, min, max, mu0) 
    demand(p = x["p"], t = x["ll"], sigma = sigma, 
           mu = mu, mu0 = mu0, min = min, max = max), 
    mu = mu, sigma = sigma, min = min, max = max, mu0 = mu0)
  
  # sapply from list to numerator
  int.num <- sapply(X, function(x, p, mu, sigma, theta, min, max, mu0) 
    integrate(int.apply.num, 
              lower = x["ll"], 
              upper = x["ul"], 
              p = x["p"],
              mu = mu,
              sigma = sigma,
              theta = theta,
              min = min, 
              max = max,
              mu0 = mu0)$value, mu = mu, sigma = sigma, theta = theta, min = min, max = max, mu0 = mu0)
  
  ## OLD
  # # Denominator
  # denom.vector <- ul*(demand(p, ul, sigma, mu, mu0, min, max)/demand(p, ll, sigma, mu, mu0, min, max)) - ll
  
  ## NEW denominator
  int.den <- sapply(X, function(x, p, mu, sigma, theta, min, max, mu0) 
    integrate(int.apply.den, 
              lower = x["ll"], 
              upper = x["ul"], 
              p = x["p"],
              mu = mu,
              sigma = sigma,
              min = min, 
              max = max,
              mu0 = mu0)$value, mu = mu, sigma = sigma, min = min, max = max, mu0 = mu0)

  # divide numerator and denominator by initial demand
  numer.vector <- int.num/init.dem
  denom.vector <- int.den/init.dem
  

  
  w <- data[[w]]
  w <- w/sum(w)
  st.code <- data[[st.code]]
  data.final <- data.table(numer.vector, denom.vector, w, st.code)
  # Get weighted average by state
  data.final <- data.final[, .(MVPF.state = weighted.mean(numer.vector, w = w)/(weighted.mean(denom.vector, w = w))), by =.(st.code)]  
  # Average across states
  return(mean(data.final$MVPF.state))
}

max.av.non.marginal.change <- function(mu, data, pp, t0, t1, theta, sigma, w, st.code, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  return(-(av.non.marginal.change(mu, data, pp, t0, t1, theta, sigma, w, st.code, min, max, constr_mat, IV_mat, min.crit, elas)))
}

av.non.marginal.change.parallel <- function(mu, data, pp, t0, t1, theta, sigma, w, st.code, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  
  # Normalize to get a value of \mu_0: set to NULL we know is the same
  mu0 <- NULL
  
  # Use vectors 
  ll <- exp(data[[t0]])-1
  ul <- exp(data[[t1]])-1
  p <- data[[pp]]
  
  
  # Put together and transform to list
  X <- rbind(ll, ul, p)
  X <- lapply(seq_len(ncol(X)), function(i) X[,i])
  
  # Calculate initial demand
  init.dem <- sapply(X, function(x, mu, sigma, min, max, mu0) 
    demand(p = x["p"], t = x["ll"], sigma = sigma, 
           mu = mu, mu0 = mu0, min = min, max = max), 
    mu = mu, sigma = sigma, min = min, max = max, mu0 = mu0)
  
  # sapply from list to numerator
  int.num <- foreach(i=1:length(p),.combine="c") %dopar% {
                  integrate(int.apply.num, 
                            lower = ll[i], 
                            upper = ul[i], 
                            p = p[i],
                            mu = mu,
                            sigma = sigma,
                            theta = theta,
                            min = min, 
                            max = max,
                            mu0 = mu0)$value
      }

  ## OLD
  # # Denominator
  # denom.vector <- ul*(demand(p, ul, sigma, mu, mu0, min, max)/demand(p, ll, sigma, mu, mu0, min, max)) - ll
  
  ## NEW denominator
  int.den <- foreach(i=1:length(p),.combine="c") %dopar% {
                integrate(int.apply.den, 
                          lower = ll[i], 
                          upper = ul[i], 
                          p = p[i],
                          mu = mu,
                          sigma = sigma,
                          theta = theta,
                          min = min, 
                          max = max,
                          mu0 = mu0)$value
  }
  
  # divide numerator and denominator by initial demand
  numer.vector <- int.num/init.dem
  denom.vector <- int.den/init.dem
  
  
  
  w <- data[[w]]
  w <- w/sum(w)
  st.code <- data[[st.code]]
  data.final <- data.table(numer.vector, denom.vector, w, st.code)
  # Get weighted average by state
  data.final <- data.final[, .(MVPF.state = weighted.mean(numer.vector, w = w)/(weighted.mean(denom.vector, w = w))), by =.(st.code)]  
  # Average across states
  return(mean(data.final$MVPF.state))
}

max.av.non.marginal.change.parallel <- function(mu, data, pp, t0, t1, theta, sigma, w, st.code, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  return(-(av.non.marginal.change.parallel(mu, data, pp, t0, t1, theta, sigma, w, st.code, min, max, constr_mat, IV_mat, min.crit, elas)))
}

#### Constraints functions ----------

## Now, we put together functions that create the restrictions for the problem and will be used in the NLOPT program

# The final function takes 5 main inputs:
# mu: the control variables
# constr_mat = the matrix of constraint
# IV_mat = the vector of IVs, used for the RHS of the restriction
# elas = T (default) indicates we are imposing shape constraint on the elasticity (F is for demand)
# min.crit = (default 0) indicates the value of the minimum criterion for the problem (as solved earlier). If NULL the problem is on an equality

## Function without min.criterion
constraint <- function(mu, constr_mat, IV_mat) {
  
  if (dim(constr_mat)[1] != length(IV_mat)) { stop("constr_mat and IV_mat dimensions must match") }
  if (length(mu) != dim(constr_mat)[2]) { stop("constr_mat and mu dimensions must match") }
  
  constraints <- NULL
  for (r in 1:dim(constr_mat)[1]) {
    constraints <- rbind(constraints, c(sum(constr_mat[r,]*mu) - IV_mat[r]))
  }
  return(constraints)
}
## Function to add min.cretrion = value
constr.min.crit <- function(mu, constr_mat, IV_mat, min.crit) {
  
  if(length(min.crit) > 1){ stop("minimum criterion should be a value") }
  
  return(
    rbind(
      constraint(mu, constr_mat, IV_mat+min.crit),
      constraint(mu, -constr_mat, -IV_mat+min.crit)
    )
  )
}
## Function to create the shape constraint
shape.constr<-function(mu, elas) {
  constr.mono <- NULL
  if (elas) {
    for (k in 1:K) {
      
      constr.mono <- rbind(constr.mono,
                           c(mu[k]))
    }
  } else {
    for (k in 1:(K-1)) {
      
      constr.mono <- rbind(constr.mono,
                           c( -mu[k] + mu[k+1]))
    }
  }
  return(constr.mono)
}
## Function for constraint: includes the arguments from evaluation function even if not needed so it runs
eval_restrictions_marg <- function(mu, data, pp, tau, theta, sigma, w, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  
  return(
    as.matrix(
      #rbind(
      constr.min.crit(mu, constr_mat, IV_mat, min.crit) #,
      #shape.constr(mu, elas)
      #)
    )
  )
}
eval_restrictions_marg_av <- function(mu, data, pp, tau, theta, sigma, w, st.code, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  
  return(
    as.matrix(
      #rbind(
      constr.min.crit(mu, constr_mat, IV_mat, min.crit) #,
      #shape.constr(mu, elas)
      #)
    )
  )
}

## Function for jacobian
eval_restrictions_j_marg <- function(mu, data, pp, tau, theta, sigma, w, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  
  constr.jac <- NULL
  for (k in 1:K) {
    
    constr.jac <- cbind(
      constr.jac,
      #rbind(
      constr.min.crit(c(rep(0,k-1),1,rep(0,K-k)), constr_mat, rep(0, dim(constr_mat)[1]), 0) #,
      #shape.constr(c(rep(0,k-1),1,rep(0,K-k)), elas)
      #)
    )
    
  }
  return(as.matrix(constr.jac))
}
## Function for constraint: includes the arguments from evaluation function even if not needed so it runs
eval_restrictions_nmarg <- function(mu, data, pp, t0, t1, theta, sigma, w, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  
  return(
    as.matrix(
      #rbind(
      constr.min.crit(mu, constr_mat, IV_mat, min.crit) #,
      #shape.constr(mu, elas)
      #)
    )
  )
}
eval_restrictions_nmarg_av <- function(mu, data, pp, t0, t1, theta, sigma, w, st.code, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  
  return(
    as.matrix(
      #rbind(
      constr.min.crit(mu, constr_mat, IV_mat, min.crit) #,
      #shape.constr(mu, elas)
      #)
    )
  )
}
## Function for jacobian
eval_restrictions_j_nmarg <- function(mu, data, pp, t0, t1, theta, sigma, w, min, max, constr_mat, IV_mat, min.crit = 0, elas = T) {
  
  constr.jac <- NULL
  for (k in 1:K) {
    
    constr.jac <- cbind(
      constr.jac,
      #rbind(
      constr.min.crit(c(rep(0,k-1),1,rep(0,K-k)), constr_mat, rep(0, dim(constr_mat)[1]), 0) #,
      #shape.constr(c(rep(0,k-1),1,rep(0,K-k)), elas)
      #)
    )
    
  }
  return(as.matrix(constr.jac))
}







## Small function to get an initial values for optimization -----------
get.init.val <- function(A, b, min.c, max = 1000) {
  
  init <- as.vector(ginv(A) %*% b)
  if (sum(init> 0) == 0) {
    return(init)
  } 
  else {
    srv <- (sum(init> 0) > 0)
    d <- dim(A)[2]
    kernel <- null(A)
    if (is.null(kernel)) kernel <- t(t(rep(0, length(init)))) # When it has a solution then is going to be null: use only min.criterion
    for (d in 1:dim(kernel)[2]) {
      ker <- as.vector(kernel[,d])
      i <- 0
      while (srv & i < max) {
        i <- i + 1
        s <- sign(ker[which(init == max(init))])
        rat <- abs( ker[which(init == max(init))]/min(init))
        if (min.c == 0) {
          init <- init - s*rat*ker
        }
        else {
          init <- init - s*rat*ker -
            (init > 0)*rep(sum(init < 0)*min.c/(d), length(init)) + 
            (init < 0)*rep(sum(init < 0)*min.c/(d), length(init))          
        }
        if (i < 11 & round(5*i/max) == 5*i/max) {
        } 
        srv <- (sum(init> 0) > 0)
      }
    }
    if (i == max) {
      # Set 1 of them to 0
      m <- dim(A)[2] -1
      if (m > 1){
        A <- A[,1:m]
        init <- c(get.init.val(A, b, min.c, max), 0)
        
      }
      else {
        stop("Algorithm Failed") 
      }
    }
    else {
      print(init)
      return(init)
    }
  }
}
