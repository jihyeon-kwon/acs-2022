################################################
##### Run MCMC for Measurement Error Model ##### 
################################################

# This program was run on DEAC Clusters (https://is.wfu.edu/services/high-performance-computing/)

#-------------------------------------------------------------------------------

library(iterators)
library(doParallel)
library(foreach)
library(doRNG)


model <- ""



for (MM in 1:4) {
        filename = paste("me.", toString(MM), ".Rda", sep="")
        outfile = paste("me_log.",toString(MM),".txt",sep="")
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
                        
                        # for t = 2014
                        for (t in 2014:2014) {
                                
                                for(i in 1:N) {
                                        # Data Layer
                                        y[(t-2014)*N+i] ~ dnorm(mean = (beta0 + inprod(X[(t-2014)*N+i, 1:4], beta[1:4])
                                                                        + f[(t-2014)*N+i]), 
                                                                var = sigmasq)
                                        
                                        # Process Layer
                                        for (p in 1:4) {
                                                X5[(t-2014)*N+i, p] ~ dnorm(X[(t-2014)*N+i, p], sd = se5[(t-2014)*N+i, p])
                                        }
                                        
                                        for (p in 1:2) {
                                                
                                                X[(t-2014)*N+i, p] ~ dnorm(theta.i[i, p], tau = tausq.t[((t-2014)+1), p])
                                                X[(t-2014)*N+i, p+2] ~ dnorm(theta.t[((t-2014)+1), p] + g[(t-2014)*N+i, p], tau = tausq.i[i, p])
                                                
                                        }
                                        
                                        f[(t-2014)*N+i] <- u[(t-2014)*N+i]
                                        
                                }
                                
                                # Process Layer
                                
                                for (p in 1:2) {
                                        
                                        # Prior Layer
                                        theta.t[((t-2014)+1), p] ~ dflat()
                                        tausq.t[((t-2014)+1), p] ~ dgamma(0.5, 0.5)
                                        g[((t-2014)*N+1):((t-2014)*N+N), p] ~ dcar_normal(adj = adj[], num = num[], tau = tau2[p], zero_mean = 1)
                                        
                                }
                                
                                u[((t-2014)*N+1):((t-2014)*N+N)] ~ dcar_normal(adj = adj[], num = num[], tau = tau, zero_mean = 1)
                                
                        }
                        
                        
                        # for t = 2015-2023
                        for (t in 2015:2023) {
                                
                                for(i in 1:N) {
                                        # Data Layer
                                        y[(t-2014)*N+i] ~ dnorm(mean = (beta0 + inprod(X[(t-2014)*N+i, 1:4], beta[1:4])
                                                                        + f[(t-2014)*N+i]), 
                                                                var = sigmasq)
                                        
                                        # Process Layer
                                        for (p in 1:4) {
                                                X5[(t-2014)*N+i, p] ~ dnorm(X[(t-2014)*N+i, p], sd = se5[(t-2014)*N+i, p])
                                        }
                                        
                                        for (p in 1:2) {
                                                
                                                X[(t-2014)*N+i, p] ~ dnorm(theta.i[i, p], tau = tausq.t[((t-2014)+1), p])
                                                X[(t-2014)*N+i, p+2] ~ dnorm(theta.t[((t-2014)+1), p] + g[(t-2014)*N+i, p], tau = tausq.i[i, p])
                                        }
                                        
                                        f[(t-2014)*N+i] <- phi * f[(t-2015)*N+i] + u[(t-2014)*N+i]
                                        
                                }
                                
                                # Process Layer
                                
                                for (p in 1:2) {
                                        
                                        # Prior Layer
                                        theta.t[((t-2014)+1), p] ~ dflat()
                                        tausq.t[((t-2014)+1), p] ~ dgamma(0.5, 0.5)
                                        g[((t-2014)*N+1):((t-2014)*N+N), p] ~ dcar_normal(adj = adj[], num = num[], tau = tau2[p], zero_mean = 1)
                                        
                                }
                                
                                u[((t-2014)*N+1):((t-2014)*N+N)] ~ dcar_normal(adj = adj[], num = num[], tau = tau, zero_mean = 1)
                                
                        }
                        
                        for (i in 1:N) {
                                
                                for (p in 1:2) {
                                        
                                        # Prior Layer
                                        theta.i[i, p] ~ dflat()
                                        tausq.i[i, p] ~ dgamma(0.5, 0.5)
                                        
                                }
                        }
                        
                        
                        for (p in 1:4) {
                                beta[p] ~ dflat()
                        }
                        
                        for (p in 1:2) {
                                tau2[p] ~ dgamma(0.5, 0.5)
                        }
                        
                        # Prior Layer
                        tau ~ dgamma(0.5, 0.5)
                        sigmasq ~ dinvgamma(0.5, 0.5)
                        phi ~ dunif(0, 1)
                        beta0 ~ dflat() 
                        
                })
                
                ## Run MCMC
                
                constants <- list(N = N, N1 = N1, oyInd = oyInd, adj = adj, num = num,
                                  se5 = as.matrix(se5[401:1400, ]))
                
                data <- list(y = as.numeric(y),
                             X5 = as.matrix(X5[401:1400, ]))
                
                inits <- list(beta0 = 0, beta = c(rep(0, 4)),
                              X = as.matrix(X5[401:1400, ]),
                              theta.i = matrix(0, nrow = N, ncol = 2),
                              theta.t = matrix(0, nrow = 2023-2014+1, ncol = 2),
                              tausq.i = matrix(1, nrow = N, ncol = 2),
                              tausq.t = matrix(1, nrow = 2023-2014+1, ncol = 2),
                              f = rep(0, N*10), u = rep(0, N*10),
                              g = matrix(0, nrow = N*(2023-2014+1), ncol = 2),
                              tau = 1, tau2 = rep(1, 2), sigmasq = 1, phi = 0)
                
                # Build the model
                nimble_mod <- nimbleModel(code, constants, data, inits)
                compiled_mod <- compileNimble(nimble_mod, resetFunctions = T)
                
                # Set up samplers
                mcmc_conf <- configureMCMC(nimble_mod, monitors = c("beta0", "beta", "X", "theta.i", "theta.t"))
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
