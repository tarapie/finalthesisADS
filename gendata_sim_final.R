gendata <- function(n, # sample size, i.e., size of the dataset
                    model_type = "main only" # How to generate Y?
){
  
  # Step 1: Generate X and M 
  X <- rnorm(n,0,1)
  M <- rnorm(n,0,1)
  
  # Step 2: Generate Y
  if(model_type == "main only"){
    Y <-  1*X + 2*M + rnorm(n,0,1)
  }else if(model_type == "int only"){
    Y <-  2*X*M + rnorm(n,0,1) 
  }else if(model_type == "weak main"){
    Y  <-  0.2*X + 0.1*M + 0.5*X*M + rnorm(n,0,1)
  }
  data.frame(X, M, Y) 
}