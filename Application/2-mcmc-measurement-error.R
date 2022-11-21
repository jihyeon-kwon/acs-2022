######################################
# 2 - MCMC - Measurement Error Model #
######################################

# This program was run on DEAC Clusters (https://is.wfu.edu/services/high-performance-computing/)

load("Model1data.Rda")
library(nimble)
library(coda)

MCS <- 1000000

code <- nimbleCode({
        
        # for t = 2014
        for (t in 2014:2014) {
                
                for(i in 1:N) {
                        # Data Layer
                        y[(t-2014)*N+i] ~ dnorm(mean = (beta0 + inprod(X.cent[(t-2010)*N+i, 1:8], beta[1:8]) 
                                                        + inprod(X.rucc[(t-2014)*N+i, 1:2], betaInd[1:2])
                                                        + f[(t-2014)*N+i]), 
                                                var = sigmasq)
                        
                        # Process Layer
                        for (p in 1:8) {
                                X5[(t-2010)*N+i, p] ~ dnorm(X[(t-2010)*N+i, p], sd = se5[(t-2010)*N+i, p])
                        }
                        
                        for (p in 1:4) {
                                
                                X[(t-2010)*N+i, p] ~ dnorm(theta.i[i, p], tau = tausq.t[((t-2010)+1), p])
                                X[(t-2010)*N+i, p+4] ~ dnorm(theta.t[((t-2010)+1), p] + g[(t-2010)*N+i, p], tau = tausq.i[i, p])
                                
                        }
                        
                        f[(t-2014)*N+i] <- u[(t-2014)*N+i]
                        
                }
                
                # Process Layer
                for (p in 1:8) {
                        
                        X.cent[((t-2010)*N+1):((t-2010)*N+N), p] <- (X[((t-2010)*N+1):((t-2010)*N+N), p] 
                                                                     - mean(X[((t-2010)*N+1):((t-2010)*N+N), p])) / sd(X[((t-2010)*N+1):((t-2010)*N+N), p])
                        
                }
                
                for (p in 1:4) {
                        
                        # Prior Layer
                        theta.t[((t-2010)+1), p] ~ dflat()
                        tausq.t[((t-2010)+1), p] ~ dgamma(0.5, 0.5)
                        g[((t-2010)*N+1):((t-2010)*N+N), p] ~ dcar_normal(adj = adj[], num = num[], tau = tau2[p], zero_mean = 1)
                        
                }
                
                u[((t-2014)*N+1):((t-2014)*N+N)] ~ dcar_normal(adj = adj[], num = num[], tau = tau, zero_mean = 1)
                
        }
        
        
        # for t = 2015-2018
        for (t in 2015:2018) {
                
                for(i in 1:N) {
                        # Data Layer
                        y[(t-2014)*N+i] ~ dnorm(mean = (beta0 + inprod(X.cent[(t-2010)*N+i, 1:8], beta[1:8]) 
                                                        + inprod(X.rucc[(t-2014)*N+i, 1:2], betaInd[1:2])
                                                        + f[(t-2014)*N+i]), 
                                                var = sigmasq)
                        
                        # Process Layer
                        for (p in 1:8) {
                                X5[(t-2010)*N+i, p] ~ dnorm(X[(t-2010)*N+i, p], sd = se5[(t-2010)*N+i, p])
                        }
                        
                        for (p in 1:4) {
                                
                                X[(t-2010)*N+i, p] ~ dnorm(theta.i[i, p], tau = tausq.t[((t-2010)+1), p])
                                X[(t-2010)*N+i, p+4] ~ dnorm(theta.t[((t-2010)+1), p] + g[(t-2010)*N+i, p], tau = tausq.i[i, p])
                        }
                        
                        f[(t-2014)*N+i] <- phi * f[(t-2015)*N+i] + u[(t-2014)*N+i]
                        
                }
                
                # Process Layer
                for (p in 1:8) {
                        
                        X.cent[((t-2010)*N+1):((t-2010)*N+N), p] <- (X[((t-2010)*N+1):((t-2010)*N+N), p] 
                                                                     - mean(X[((t-2010)*N+1):((t-2010)*N+N), p])) / sd(X[((t-2010)*N+1):((t-2010)*N+N), p])
                        
                }
                
                for (p in 1:4) {
                        
                        # Prior Layer
                        theta.t[((t-2010)+1), p] ~ dflat()
                        tausq.t[((t-2010)+1), p] ~ dgamma(0.5, 0.5)
                        g[((t-2010)*N+1):((t-2010)*N+N), p] ~ dcar_normal(adj = adj[], num = num[], tau = tau2[p], zero_mean = 1)
                        
                }
                
                u[((t-2014)*N+1):((t-2014)*N+N)] ~ dcar_normal(adj = adj[], num = num[], tau = tau, zero_mean = 1)
                
        }
        
        for (i in 1:N) {
                
                for (p in 1:4) {
                        
                        # Prior Layer
                        theta.i[i, p] ~ dflat()
                        tausq.i[i, p] ~ dgamma(0.5, 0.5)
                        
                }
        }
        
        
        for (p in 1:8) {
                beta[p] ~ dflat()
        }
        
        for (p in 1:2) {
                betaInd[p] ~ dflat()
        }
        
        for (p in 1:4) {
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
                  se5 = as.matrix(se5.t), se1 = as.matrix(se1.t), 
                  NCse1 = as.matrix(NC1.se.t))

data <- list(y = as.numeric(y.t),
             X.rucc = as.matrix(X.rucc),
             X5 = as.matrix(X5.t))

inits <- list(beta0 = 0, beta = c(rep(0, 8)), betaInd = c(rep(0, 2)),
              X = as.matrix(X5.t),
              theta.i = matrix(0, nrow = N, ncol = 4),
              theta.t = matrix(0, nrow = 2018-2010+1, ncol = 4),
              tausq.i = matrix(1, nrow = N, ncol = 4),
              tausq.t = matrix(1, nrow = 2018-2010+1, ncol = 4),
              f = rep(0, N*5), u = rep(0, N*5),
              g = matrix(0, nrow = N*9, ncol = 4),
              tau = 1, tau2 = rep(1, 4), sigmasq = 1, phi = 0)

# Build the model
nimble_mod <- nimbleModel(code, constants, data, inits)
compiled_mod <- compileNimble(nimble_mod, resetFunctions = T)

# Set up samplers
mcmc_conf <- configureMCMC(nimble_mod, monitors = c("beta0", "beta", "betaInd", "X", "theta.i", "theta.t"))
nimble_mcmc <- buildMCMC(mcmc_conf)
compiled_mcmc <- compileNimble(nimble_mcmc, project = nimble_mod, resetFunctions = T)

# Run the model

samples <- runMCMC(compiled_mcmc, inits = inits, nburnin = MCS/2, thin = 100,
                   nchains = 1, niter = MCS, samplesAsCodaMCMC = T,
                   WAIC = F, progressBar = T)



save(samples, file = "app-me.Rda")

