poisson_reg <- function(X, Y, tol=1e-5, iter_max=25){
  q=ncol(X)
  beta=rep(0,q)
  iter=0
  while (epsilon > tol & ite <= ite_max){
    eta = X %*% beta
    mu = exp(eta)
    nu = exp(eta)
    V = diag(as.vector(nu))
    y_star = eta + solve(V)  %*% (Y-mu)
    beta_new = solve(X %*% V %*% t(X)) %*% t(X) %*% V %*% y_star
    epsilon = sqrt(t(beta_new-beta)%*%(beta_new-beta))
    beta = beta_new
    iter = iter + 1
  } 
  return(beta)
}

