###################################
##### Run MCMC for Base Model ##### 
###################################

# This program was run on DEAC Clusters (https://is.wfu.edu/services/high-performance-computing/)

#-------------------------------------------------------------------------------

library(iterators)
library(doParallel)
library(foreach)
library(doRNG)


model <- ""



for (MM in 1:4) {
        filename = paste("base.", toString(MM), ".Rda", sep="")
        outfile = paste("base_log.",toString(MM),".txt",sep="")
        Mrep = ((MM-1)*25+1):(MM*25) ### identify the set of 25 simulated data sets that are run in this code
        
        cl <- makePSOCKcluster(25) ## this code is designed to use 25 cores of a cluster
        registerDoParallel(cl)
        
        # Setup ----------------------------------------------------------------
        
        writeLines(c(""), outfile)
        set.seed(4122022)
        
        SimMCMC<-foreach(kk=1:25) %dorng% {
                
                k=Mrep[kk]
                library(nimble) 
                library(coda) 
                
                sink(outfile, append=TRUE)
                cat(paste("Starting iteration",k,"\n"))
                sink()
                
                ############################################################ 
                ###########             Load data               ############
                ############################################################ 
                
                ### load in data
                load("simul-data.Rda")
                y <- y[, k]
                
                ############################################################ 
                ###########            Write Model              ############
                ############################################################ 
                
                library(nimble)
                library(coda)
                
                # Measurement Error Model
                
                code <- nimbleCode({
                        
                        ## Data Layer
                        
                        for (t in 1:1) {
                                for (i in 1:N) {
                                        f[(t-1)*N+i] <- u[(t-1)*N+i]
                                }
                                u[((t-1)*N+1):((t-1)*N+N)] ~ dcar_normal(adj = adj[], num = num[], tau = tau, zero_mean = 1)
                        }
                        
                        for (t in 2:10) {
                                for (i in 1:N) {
                                        f[(t-1)*N+i] <- phi * f[(t-2)*N+i] + u[(t-1)*N+i]
                                }
                                u[((t-1)*N+1):((t-1)*N+N)] ~ dcar_normal(adj = adj[], num = num[], tau = tau, zero_mean = 1)
                        }
                        
                        
                        for (t in 1:10) {
                                # linear regression
                                for (i in 1:N) {
                                        y[(t-1)*N+i] ~ dnorm(mean = (beta0 + inprod(X[(t-1)*N+i, 1:4], beta[1:4]) + f[(t-1)*N+i]),
                                                             var = sigmasq)
                                }
                        } 
                        
                        ## Prior
                        
                        beta0 ~ dflat()
                        
                        for (p in 1:4) {
                                beta[p] ~ dflat()
                        }
                        
                        sigmasq ~ dinvgamma(0.5, 0.5)
                        phi ~ dunif(0, 1)
                        tau ~ dinvgamma(0.5, 0.5)
                })
                
                ## Run MCMC
                
                constants <- list(N = N, adj = adj, num = num)
                
                data <- list(y = as.numeric(y),
                             X = as.matrix(X5[401:1400, ]))
                
                inits <- list(beta0 = 0, beta = c(rep(0, 4)), sigmasq = 1,
                              f = rep(0, N*10), u = rep(0, N*10), tau = 1, phi = 0)
                
                # Build the model
                nimble_mod <- nimbleModel(code, constants, data, inits)
                compiled_mod <- compileNimble(nimble_mod, resetFunctions = T,
                                              showCompilerOutput = T)
                
                # Set up samplers
                mcmc_conf <- configureMCMC(nimble_mod, monitors = c("beta0", "beta"))
                nimble_mcmc <- buildMCMC(mcmc_conf)
                compiled_mcmc <- compileNimble(nimble_mcmc, project = nimble_mod, resetFunctions = T)
                
                # Run the model
                MCS <- 200000
                samples <- runMCMC(compiled_mcmc, inits = inits, nburnin = MCS/2, thin = 10,
                                   nchains = 1, niter = MCS, samplesAsCodaMCMC = T,
                                   WAIC = F, progressBar = T)
                
                # Organize output
                list(colMeans(samples),apply(samples,2,quantile,probs=c(.025,.975)),apply(samples,2,sd), apply(samples, 2, median))
                
        }
        
        save(SimMCMC,file=filename)
}
stopCluster(cl)
