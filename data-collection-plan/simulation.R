

pylcr_lambda <- c(
    0.786, -0.002,  0.115, -0.003,  0.010,  0.008,
    0.746,  0.024,  0.158,  0.035,  0.038, -0.001,
    0.716,  0.071,  0.189, -0.009, -0.016, -0.008,
    0.646, -0.012, -0.011, -0.133, -0.050,  0.321,
    0.606,  0.044, -0.036,  0.002,  0.023,  0.368,
    0.581,  0.113, -0.038,  0.097,  0.061, -0.105,
    0.416, -0.091,  0.028,  0.120, -0.013,  0.368,
    0.111,  0.800, -0.048, -0.037,  0.198,  0.024,
    -0.016,  0.753,  0.186,  0.063,  0.004,  0.129,
    0.036,  0.661,  0.040,  0.251, -0.012, -0.048,
    0.121, -0.056,  0.638,  0.118,  0.045,  0.065,
    0.033,  0.039,  0.633, -0.073,  0.153, -0.021,
    0.014,  0.189,  0.620,  0.042,  0.007, -0.018,
    -0.016,  0.103,  0.080,  0.695, -0.181, -0.038,
    0.086, -0.111, -0.126,  0.623,  0.133,  0.124,
    -0.169,  0.058,  0.004,  0.593,  0.104,  0.025,
    0.058,  0.120,  0.066,  0.418, -0.033, -0.107,
    -0.022, -0.042,  0.142,  0.151,  0.805, -0.025,
    0.049,  0.114, -0.012,  0.045,  0.704, -0.038,
    -0.008,  0.069,  0.208, -0.045,  0.695,  0.085,
    0.017,  0.007,  0.111,  0.031, -0.060,  0.728,
    -0.019,  0.031, -0.065, -0.003,  0.085,  0.692,
    0.002,  0.185,  0.036, -0.051,  0.044,  0.559)

pyli_lambda <- matrix(pyli_lambda,ncol = 6, byrow = T)

pyli_mean <- c(4.78,
               4.90,
               4.95,
               4.49,
               4.62,
               4.57,
               3.73,
               5.14,
               5.21,
               4.98,
               4.94,
               5.12,
               4.96,
               4.30,
               4.37,
               4.77,
               4.57,
               5.12,
               5.13,
               5.14,
               4.54,
               4.32,
               4.26)


R_imp <- pyli_lambda %*% t(pyli_lambda)
diag(R_imp) <- 1
eigen(R_imp)$values

library(mvtnorm)

dat <- rmvnorm(1000,mean = pyli_mean,sigma = R_imp)

library(GPArotation)
factanal(dat, 6, rotation = "geominT")


pyli_lambda


slc_CR <- c(.64,NA,NA,NA,
            .57,NA,NA,NA,
            .57,NA,NA,NA,
            .68,NA,NA,NA,
            .63,NA,NA,NA,
            .44,NA,NA,NA,
            .75,NA,NA,NA,
            .80,NA,NA,NA,
            .82,NA,NA,NA,
            NA,NA,.58,NA,
            NA,NA,.92,NA,
            NA,NA,.88,NA,
            NA,NA,NA,.56,
            NA,NA,NA,.66,
            NA,.42,NA,.40,
            NA,.96,NA,NA,
            NA,.94,NA,NA,
            NA,.85,NA,NA)

slc_CR <- matrix(slc_CR,ncol = 4, byrow = T)


slc_C <- c(.72,NA,NA,NA,NA,
           .78,NA,NA,NA,NA,
           .66,NA,NA,NA,NA,
           .59,NA,NA,NA,NA,
           .60,NA,NA,NA,NA,
           NA,NA,NA,-.80,NA,
           NA,NA,NA,-.90,NA,
           NA,NA,NA,-.82,NA,
           NA,NA,NA,-.63,NA,
           NA,-.95,NA,NA,NA,
           NA,-.94,NA,NA,NA,
           NA,-.90,NA,NA,NA,
           NA,NA,.61,NA,NA,
           NA,NA,.54,NA,NA,
           NA,NA,.77,NA,NA,
           NA,NA,.71,NA,NA,
           NA,NA,.63,NA,NA,
           NA,NA,.57,NA,NA,
           NA,NA,NA,NA,-.52,
           NA,NA,NA,NA,-.86,
           NA,NA,NA,NA,-.79)

slc_C <- matrix(slc_C,ncol = 5, byrow = T)

slc_GD <- c(.83,NA,NA,
            .85,NA,NA,
            .85,NA,NA,
            .81,NA,NA,
            .74,NA,NA,
            .65,NA,NA,
            NA,.89,NA,
            NA,.91,NA,
            NA,.87,NA,
            NA,NA,.92,
            NA,NA,.90,
            NA,NA,.52)

slc_GD <- matrix(slc_GD,ncol = 3, byrow = T)



slc_II <- c(NA,NA,NA,.49,NA,NA,
            NA,NA,NA,NA,NA,NA,
            NA,NA,NA,.59,NA,NA,
            NA,NA,NA,.69,NA,NA,
            NA,NA,NA,.70,NA,NA,
            NA,NA,NA,.65,NA,NA,
            .79,NA,NA,NA,NA,NA,
            .67,NA,NA,NA,NA,NA,
            .77,NA,NA,NA,NA,NA,
            .62,NA,NA,NA,NA,NA,
            .49,NA,NA,NA,NA,NA,
            .50,NA,NA,NA,NA,NA,
            NA,NA,NA,NA,NA,.73,
            NA,NA,NA,NA,NA,.70,
            NA,NA,NA,NA,NA,.55,
            NA,-.77,NA,NA,NA,NA,
            NA,-.76,NA,NA,NA,NA,
            NA,-.67,NA,NA,NA,NA,
            NA,-.68,NA,NA,NA,NA,
            NA,-.77,NA,NA,NA,NA,
            NA,-.70,NA,NA,NA,NA,
            NA,NA,.42,NA,NA,NA,
            NA,NA,.87,NA,NA,NA,
            NA,NA,.81,NA,NA,NA,
            NA,NA,NA,NA,NA,.79,
            NA,NA,NA,NA,NA,.71,
            NA,NA,NA,NA,NA,NA,
            NA,NA,NA,NA,-.47,NA,
            NA,NA,NA,NA,-.58,NA,
            NA,NA,NA,NA,-.54,NA,
            NA,NA,.51,NA,NA,NA,
            NA,NA,NA,NA,-.42,NA,
            NA,NA,.84,NA,NA,NA)

slc_II  <- matrix(slc_II,ncol = 6, byrow = T)



slc_LR <- c(NA,NA,NA,NA,.73,NA,
            NA,NA,NA,NA,.68,NA,
            NA,NA,NA,NA,.65,NA,
            NA,NA,NA,.59,NA,NA,
            NA,NA,NA,.56,NA,NA,
            NA,NA,NA,.69,NA,NA,
            NA,NA,NA,NA,NA,-.86,
            NA,NA,NA,NA,NA,-.86,
            NA,NA,NA,NA,NA,-.80,
            NA,NA,NA,NA,NA,NA,
            .52,NA,NA,NA,NA,NA,
            .43,NA,NA,NA,NA,NA,
            .61,NA,NA,NA,NA,NA,
            .40,NA,NA,NA,NA,NA,
            .71,NA,NA,NA,NA,NA,
            .69,NA,NA,NA,NA,NA,
            .72,NA,NA,NA,NA,NA,
            .78,NA,NA,NA,NA,NA,
            NA,NA,.61,NA,NA,NA,
            NA,NA,.74,NA,NA,NA,
            NA,NA,.64,NA,NA,NA,
            NA,-.88,NA,NA,NA,NA,
            NA,-.85,NA,NA,NA,NA,
            NA,-.89,NA,NA,NA,NA,
            NA,-.44,NA,NA,NA,NA,
            NA,-.49,NA,NA,NA,NA,
            NA,-.46,NA,NA,NA,NA,
            NA,NA,.53,NA,NA,NA,
            NA,NA,.53,NA,NA,NA,
            NA,NA,.49,NA,NA,NA)

slc_LR  <- matrix(slc_LR,ncol = 6, byrow = T)


slc_SA <- c(.63,NA,NA,NA,NA,
            .52,NA,NA,NA,NA,
            .53,NA,NA,NA,NA,
            .83,NA,NA,NA,NA,
            .80,NA,NA,NA,NA,
            .72,NA,NA,NA,NA,
            NA,NA,NA,.77,NA,
            NA,NA,NA,.84,NA,
            NA,NA,NA,.85,NA,
            NA,.64,NA,NA,NA,
            NA,.82,NA,NA,NA,
            NA,.84,NA,NA,NA,
            NA,NA,NA,NA,-.74,
            NA,NA,NA,NA,-.81,
            NA,NA,NA,NA,-.71,
            NA,NA,-.62,NA,NA,
            NA,NA,-.68,NA,NA,
            NA,NA,-.66,NA,NA)

slc_SA  <- matrix(slc_SA,ncol = 5, byrow = T)

slc_SP <- c(NA,-.74,NA,
            NA,-.85,NA,
            NA,-.83,NA,
            NA,-.79,NA,
            NA,-.88,NA,
            NA,-.80,NA,
            .83,NA,NA,
            .80,NA,NA,
            .83,NA,NA,
            .75,NA,NA,
            .70,NA,NA,
            .80,NA,NA,
            NA,NA,.82,
            NA,NA,.91,
            NA,NA,.90)

slc_SP  <- matrix(slc_SP,ncol = 3, byrow = T)

slc <- list(slc_C = list(lambda = slc_C),
            slc_CR = list(lambda = slc_CR),
            slc_GD = list(lambda = slc_GD),
            slc_II = list(lambda = slc_II),
            slc_LR = list(lambda = slc_LR),
            slc_SA = list(lambda = slc_SA),
            slc_SP = list(lambda = slc_SP))


slc <- lapply(slc,function(x){
    n_facets <- nrow(x$lambda)/3
    return(list(lambda = x$lambda, n_facets = n_facets))
})


