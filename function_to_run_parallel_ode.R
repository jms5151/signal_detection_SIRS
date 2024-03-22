library(doParallel)
library(foreach)

runParallelODE <- function(beta_list, xstart, times, model_function, params) {
  # Set up parallel processing with doParallel on PC
  cl <- makeCluster(3) # detectCores()
  # for cluster, needs to match #SBATCH cpus per task
  # cl <- makeCluster(detectCores(25))
  registerDoParallel(cl)
  
  # Define the inner function for parallel execution
  runODE <- function(beta, params, model_function) {
    params$beta <- beta
    as.data.frame(
      ode(
        xstart,
        times,
        model_function,
        params
      )
    )
  }
  
  # Run the parallel foreach loop with explicit export of necessary variables
  out_list <- foreach(
    beta = beta_list,
    .packages = c("deSolve")
  ) %dopar% {
    runODE(beta, params, model_function)
  }
  
  stopCluster(cl)  # Stop the parallel processing cluster
  
  # add list element names
  names(out_list) <- names(beta_list)
  
  # return result
  return(out_list)
}
