## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(ramp.xds)
library(MASS)
suppressMessages(library(expm))
library(deSolve)
library(data.table)
library(ggplot2)
library(viridisLite)

## ----echo=FALSE---------------------------------------------------------------
#devtools::load_all()

## -----------------------------------------------------------------------------
nPatches <- 3
nStrata <- nPatches
nHabitats <- nPatches
residence <- c(1:nStrata)
membership <- c(1:3)
HPop <- rpois(n = nPatches, lambda = 1000)

## -----------------------------------------------------------------------------
# human parameters
b <- 0.55
c <- 0.15
r <- 1/200
Xo = list(b=b,c=c,r=r)

## -----------------------------------------------------------------------------
wf <- rep(1, nStrata)

pfpr <- runif(n = nStrata, min = 0.25, max = 0.35)
X <- rbinom(n = nStrata, size = HPop, prob = pfpr)

searchWtsH = rep(1,3) 

TaR <- matrix(
  data = c(
    0.9, 0.05, 0.05,
    0.05, 0.9, 0.05,
    0.05, 0.05, 0.9
  ), nrow = nStrata, ncol = nPatches, byrow = T
)
TaR <- t(TaR)

## -----------------------------------------------------------------------------
f <- rep(0.3, nPatches)
q <- rep(0.9, nPatches) 
g <- rep(1/10, nPatches)  
mu <- rep(0, nPatches)  
sigma <- rep(1/100, nPatches)  
nu <- rep(1/2, nPatches)  
eggsPerBatch <- 30
eip <- 11
MYZo = list(f=f, q=q, g=g, sigma=sigma, mu=mu, 
            nu=nu, eggsPerBatch=eggsPerBatch, eip=eip) 

## -----------------------------------------------------------------------------
calK = create_calK_herethere(nPatches)
calK

## -----------------------------------------------------------------------------
# derived EIR to sustain equilibrium pfpr
EIR <- diag(1/b, nStrata) %*% ((r*X) / (HPop - X))

# ambient pop
W <- TaR %*% HPop

# biting distribution matrix
beta <- diag(wf) %*% t(TaR) %*% diag(1/as.vector(W), nPatches)

# kappa
kappa <- t(beta) %*% (X*c)

Omega <- compute_Omega_xde(g, sigma, mu, calK)
Omega_inv <- solve(Omega)
Upsilon <- expm::expm(-Omega * eip)
Upsilon_inv <- expm::expm(Omega * eip)

# equilibrium solutions
Z <- diag(1/(f*q), nPatches) %*% ginv(beta) %*% EIR
MY <- diag(1/as.vector(f*q*kappa), nPatches) %*% Upsilon_inv %*% Omega %*% Z
Y <- Omega_inv %*% (diag(as.vector(f*q*kappa), nPatches) %*% MY)
M <- MY + Y
P <- solve(diag(f, nPatches) + Omega) %*% diag(f, nPatches) %*% M
Lambda <- Omega %*% M

## -----------------------------------------------------------------------------
# aquatic habitat membership matrix (assume 1 habitat per patch)
calN <- matrix(0, nPatches, nHabitats)
diag(calN) <- 1

# egg dispersal matrix (assume 1 habitat per patch)
calU <- matrix(0, nHabitats, nPatches)
diag(calU) <- 1

psi <- 1/10
phi <- 1/12
eta <- as.vector(calU %*% (M * nu * eggsPerBatch))

alpha <- as.vector(solve(calN) %*% Lambda)
L <- alpha/psi
theta <- (eta - psi*L - phi*L)/(L^2)


## -----------------------------------------------------------------------------
# adult mosquito parameters

Lo = list(psi=psi, phi=phi, theta=theta, L=L)

MYZo = list(f=f, q=q, g=g, sigma=sigma, 
            nu=nu, eggsPerBatch=eggsPerBatch, 
            M=M, Y=Y, Z=Z)

## -----------------------------------------------------------------------------
xds_setup(MYZname="SI", Xname="SIS", Lname="basicL", 
          nPatches=3, HPop=HPop, membership=membership, 
          MYZopts=MYZo, calK=calK,
          Xopts=Xo, residence=1:3, searchB=rep(1,3), 
          TimeSpent =TaR, searchQ=rep(1,3), Lopts=Lo) -> itn_mod

## -----------------------------------------------------------------------------
itn_mod <- xds_solve(itn_mod, Tmax=1830, dt=15)
itn_mod <- last_to_inits(itn_mod)

## -----------------------------------------------------------------------------
cov_opts <- list(
  mean = 0.5, 
  F_season = function(t)
    {ifelse(t < 0, 0, (sin(2*pi*(t-365/4) / 365) + 1))}
)

## -----------------------------------------------------------------------------
itn_mod <- xds_setup_bednets(itn_mod,
     coverage_name = "func", coverage_opts = cov_opts, 
     effectsizes_name = "lemenach")


## -----------------------------------------------------------------------------
tt = seq(0, 1830, by = 15) 
with(cov_opts, plot(tt, mean*F_season(tt), type ="l"))

## ----solve--------------------------------------------------------------------
xds_solve(itn_mod, 1830, dt=15) -> itn_mod

## ----fig.height=7, fig.width=6------------------------------------------------
par(mfrow = c(2,1))
xds_plot_Y(itn_mod, clrs=turbo(3), llty=2)
xds_plot_Z(itn_mod, clrs=turbo(3), add=T)
xds_plot_aEIR(itn_mod)

## ----fig.height=5, fig.width=8------------------------------------------------
xds_plot_PR(itn_mod)

