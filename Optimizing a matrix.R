#Base matrix code
S <- 25
C <- 0.1
sigma = 0.5
L <- matrix(0, nr = S, nc = S)
int <- matrix(rnorm(S^2, mean = 0, 
                    sd = sigma), nr = S, nc = S)
rand <- matrix(runif(S^2,0,1),
               nr=S,nc=S)
L[rand<C] = int[rand<C]

#Function to compute the largest eigen value(real part)
eigen_FUNction <- function(L) max(Re(eigen(L)$values))

#Function to change OG function
other_FUNction <- function(L) {
  
  not_zero <- which(abs(L) > .01, arr.ind = TRUE )
  L[not_zero]
  
  #Choose a value
  pick_one <- function(L) matrix(L[sample(1:nrow(L), 1),], ncol = 2)
  
  #choose a zero
  zero <- which(abs(L) < .01, arr.ind = TRUE )
  
  targ_zero <- pick_one(zero)
  targ_non <- pick_one(not_zero)
  
  #Swap zero and non-zero cell
  L[targ_zero] <- L[targ_non]
  L[targ_non] <- 0
  return(L)
}

#Optimizing the function
Optimal_FUN = function(L, T0, a, nsteps) {
  
  superior_L <- L
  
  #Storing data
  Keep <- matrix(nrow=nsteps+1, ncol=2)
  Keep[1,1] <- 1
  Keep[1,2] <- eigen_FUNction(superior_L)
  
  #Loop
  for(step in 2:(nsteps+1)) {
    potential_L <- other_FUNction(superior_L)
    
    dif <- eigen_FUNction(superior_L) - eigen_FUNction(potential_L)
    
    if(dif>0) superior_L <- potential_L
    
    else {
      if(dif<0) {
        p=exp(dif/T0/exp(a*step))  
        if(runif(1)<p) superior_L <- potential_L
      }
      
    }  
    #Record values
    Keep[step,1] = step
    Keep[step,2] = eigen_FUNction(superior_L)
    
  }
  
  #Storage
  R <- list(Keep=Keep, superior_L=superior_L)
  return(R)
  
}

#Run and save
res <- Optimal_FUN(L=L,T0=5,a= -0.01,nsteps =1000)
Keep <- res$Keep

#Plot
plot(Keep, type = "l", xlab = "Steps", ylab = "Stability",cex = 2, log = "x")