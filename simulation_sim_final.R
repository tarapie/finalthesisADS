# Load required files
source("libraries_sim_final.R")
source("methods_sim_final.R")
source("gendata_sim_final.R")

truedag <- matrix(c(0,0,1,
                    0,0,1,
                    0,0,0),3,3,byrow=T)
colnames(truedag) <- rownames(truedag)<- c("X","M","Y")

# We have a 3 x 3 design in terms of simulation conditions
# 3 choices of data generating model (Main only, Interaction only, Weak Main + Interaction)
# 3 choices of sample size
# We evaluate 2 methods, namely the PC- and kPC-algorithm
n_cond <- c(100,200,500)
dgm_cond <- c("main only", "int only", "weak main")

# The number of iterations is how many times we will evaluate each condition 
iter <- 1000
# So we will generate a dataset (of size 200) 1000 times with the data-generating model
# of Y being produced by interaction effects only ("int only") 
# and fit both the PC-algorithm and kPC-algorithm to that data. 
# This is to ensure the samples are random enough, 
# so that the data is generated "randomly" in every iteration


# The alpha level is set it to .05
a = .05

# ---------------------------------------------------------------
# --------------------------- Simulation ------------------------
# ---------------------------------------------------------------

# So, we have 3x3 = 9 conditions to run through, for 2 methods, iter times (1000)

# The lists used will store ALL results for the kPC and PC-algorithm respectively

# Creating empty lists with fitting names, before starting the loops
models <-list(n100 = NA, n200 = NA, n500= NA)

# This is the important pair of lists where results will be stored
kpc_res <- list(model1 = models, model2=models, model3 = models)
pc_res <- list(model1 = models, model2=models, model3 = models)

# Every time you fit a model to a dataset you want to store only 
# "True" or "False" based on whether it recoversthe DAG or not

# Creating lists similar to the lists above
kpc_recover <- list(model1 = models, model2=models, model3 = models)
pc_recover <- list(model1 = models, model2=models, model3 = models)

# We want to know how long the simulation takes
begintime <- Sys.time()

# Start Model loop
for(m in 1:length(dgm_cond)){
  
  # Start Sample Size Loop
  for (n in 1:length(n_cond)){
    
    # To ensure reproducibility, we set a seed
    set.seed(123) 
    
    # Before starting the iteration loop for this model at this sample size, 
    # we create arrays to store the results of all of these iterations
    # Each iteration is saved in each "slice" (3rd dimension)
    kpc_arr <- array(data = NA, dim = c(3,3,iter), dimnames = list(c("X","M","Y"), c("X","M","Y"), NULL))
    pc_arr <- array(data = NA, dim = c(3,3,iter), dimnames = list(c("X","M","Y"), c("X","M","Y"), NULL))
    
    # We make a "temporary" storage for the performance metric,  
    # which temporarily stores whether the algorithm recovers 
    # the true dag for each iteration
    kpc_recover_tmp <- vector(mode = "logical", length = iter)
    pc_recover_tmp <- vector(mode = "logical", length = iter)
    
    for(i in 1:iter){
      
      data <- gendata(n=n_cond[n], model_type = dgm_cond[m])
      pc_est <- pc_function(dataframe = data, a = a)
      kpc_est <- kpc_function(dataframe = data, a = a)
      
      # Save the results of each iteration of this data generating model
      # at this sample size
      kpc_arr[,,i] <- kpc_est
      pc_arr[,,i] <- pc_est
      
      # Here we evaluate whether the method recovered the DAG or not in each iteration
      kpc_recover_tmp[i] <- all(truedag == kpc_est)
      pc_recover_tmp[i] <- all(truedag == pc_est)
    }
    
    # These are all of the iteration results for this condition
    # We temporarily save those somewhere to analyse later
   
    # Now, check the other conditions, using 
    # For this we use the lists we created earlier
    kpc_res[[m]][[n]] <- kpc_arr
    pc_res[[m]][[n]] <- pc_arr
    
    # Now we save the performance for this condition across all iterations in the list
    kpc_recover[[m]][[n]] <- kpc_recover_tmp
    pc_recover[[m]][[n]] <- pc_recover_tmp
    
  }
  endtime <- Sys.time()
}

(duration <- endtime - begintime)


##### SAVING THE RESULTS ############
## Results PC
pc_model1_n100_mean <- mean(pc_recover$model1$n100)
pc_model2_n100_mean <- mean(pc_recover$model2$n100)
pc_model3_n100_mean <- mean(pc_recover$model3$n100)

pc_model1_n200_mean <- mean(pc_recover$model1$n200)
pc_model2_n200_mean <- mean(pc_recover$model2$n200)
pc_model3_n200_mean <- mean(pc_recover$model3$n200)

pc_model1_n500_mean <- mean(pc_recover$model1$n500)
pc_model2_n500_mean <- mean(pc_recover$model2$n500)
pc_model3_n500_mean <- mean(pc_recover$model3$n500)

## Results kPC
kpc_model1_n100_mean <- mean(kpc_recover$model1$n100)
kpc_model2_n100_mean <- mean(kpc_recover$model2$n100)
kpc_model3_n100_mean <- mean(kpc_recover$model3$n100)

kpc_model1_n200_mean <- mean(kpc_recover$model1$n200)
kpc_model2_n200_mean <- mean(kpc_recover$model2$n200)
kpc_model3_n200_mean <- mean(kpc_recover$model3$n200)

kpc_model1_n500_mean <- mean(kpc_recover$model1$n500)
kpc_model2_n500_mean <- mean(kpc_recover$model2$n500)
kpc_model3_n500_mean <- mean(kpc_recover$model3$n500)

# Save these objects as .RDS files
# Boolean vectors
boolean_results_pc <- saveRDS(pc_recover, paste0("boolean_results_pc_June2021.RDS"))
boolean_results_kpc <- saveRDS(kpc_recover, paste0("boolean_results_kpc_June2021.RDS"))

# Returned DAGs
dags_results_pc <- saveRDS(pc_res, paste0("dags_results_pc_June2021.RDS"))
dags_results_kpc <- saveRDS(kpc_res, paste0("dags_results_kpc_June2021.RDS"))


#### SESSION INFO
# No new libraries are loaded after this
sink("sessionInfo.txt")
sessionInfo()
sink()
