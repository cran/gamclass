simreg <-
function(formula, data, n=1000){
        obj <- lm(formula, data=data)
        res <- resid(obj)
        sd <- summary(obj)$sigma
        hat <- fitted(obj)
        m <- length(coef(obj))
        regmat <- matrix(0, nrow=n, ncol=m)
        nvar <- length(hat)
        simform <- as.formula(paste("ySim", deparse(delete.response(terms(formula)))))
        for(i in 1:n){
            resSim <- rnorm(nvar, 0, sd)
            ySim <- hat+resSim
            regmat[i,] <- coef(lm(simform, data=data))
        }
        colnames(regmat) <- names(coef(obj))
        regmat
    }
