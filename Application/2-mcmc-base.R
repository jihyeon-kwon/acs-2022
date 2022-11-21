#########################
# 2 - MCMC - Base Model #
#########################

# This program was run on DEAC Clusters (https://is.wfu.edu/services/high-performance-computing/)

load("Model1data.Rda")
library(nimble)
library(coda)

MCS <- 1000000

code <- nimbleCode({
        
        ## Data Layer
        
        for (t in 2014:2014) {
                for (i in 1:N) {
                        y[(t-2014)*N+i] ~ dnorm(mean = (beta0 + inprod(X.cent[(t-2010)*N+i, 1:8], beta[1:8]) 
                                                        + inprod(X.rucc[(t-2010)*N+i, 1:2], betaInd[1:2])
                                                        + f[(t-2014)*N+i]),
                                                var = sigmasq)
                        f[(t-2014)*N+i] <- u[(t-2014)*N+i]
                }
                u[((t-2014)*N+1):((t-2014)*N+N)] ~ dcar_normal(adj = adj[], num = num[], tau = tau, zero_mean = 1)
        }
        
        for (t in 2015:2018) {
                for (i in 1:N) {
                        y[(t-2014)*N+i] ~ dnorm(mean = (beta0 + inprod(X.cent[(t-2010)*N+i, 1:8], beta[1:8]) 
                                                        + inprod(X.rucc[(t-2010)*N+i, 1:2], betaInd[1:2])
                                                        + f[(t-2014)*N+i]),
                                                var = sigmasq)
                        f[(t-2014)*N+i] <- phi * f[(t-2015)*N+i] + u[(t-2014)*N+i]
                }
                u[((t-2014)*N+1):((t-2014)*N+N)] ~ dcar_normal(adj = adj[], num = num[], tau = tau, zero_mean = 1)
        }
        
        
        for (t in 2014:2018) {
                # standardize
                for (p in 1:8) {
                        X.cent[((t-2010)*N+1):((t-2010)*N+N), p] <- (X[((t-2010)*N+1):((t-2010)*N+N), p] 
                                                                     - mean(X[((t-2010)*N+1):((t-2010)*N+N), p])) / sd(X[((t-2010)*N+1):((t-2010)*N+N), p])
                }
        } 
        
        ## Prior
        
        beta0 ~ dflat()
        
        for (p in 1:8) {
                beta[p] ~ dflat()
        }
        
        for (p in 1:2) {
                betaInd[p] ~ dflat()
        }
        
        sigmasq ~ dinvgamma(0.5, 0.5)
        phi ~ dunif(0, 1)
        tau ~ dinvgamma(0.5, 0.5)
})

## Run MCMC

constants <- list(oyInd = oyInd, N = N, adj = adj, num = num)

data <- list(y = as.numeric(y.t),
             X = as.matrix(X5.t), X.rucc = as.matrix(X.rucc))

inits <- list(beta0 = 0, beta = c(rep(0, 8)), betaInd = c(rep(0, 2)), sigmasq = 1,
              f = rep(0, N*5), u = rep(0, N*5),
              X.cent = matrix(0, N*9, 8), tau = 1, phi = 0)

# Build the model
nimble_mod <- nimbleModel(code, constants, data, inits)
compiled_mod <- compileNimble(nimble_mod, resetFunctions = T)

# Set up samplers
mcmc_conf <- configureMCMC(nimble_mod, monitors = c("beta0", "beta", "betaInd", "sigmasq"))
nimble_mcmc <- buildMCMC(mcmc_conf)
compiled_mcmc <- compileNimble(nimble_mcmc, project = nimble_mod, resetFunctions = T)

# Run the model
samples1 <- runMCMC(compiled_mcmc, inits = inits, nburnin = MCS/2, thin = 100,
                    nchains = 1, niter = MCS, samplesAsCodaMCMC = T,
                    WAIC = F, progressBar = T)

save(samples1, file = "app-base.Rda")