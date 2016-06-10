GWASone <- function(geno,pheno,cov,i){
        
        X <- geno
        y <- pheno
        w <- cov
        
        fit <- vector()
        sumfit <- vector() 
        logp <- vector()
        
    
        fit <- lm(y~X[,i]+as.factor(w[,1])+as.factor(w[,2])) ## y ~ X[,i] + (confounders)
        sumfit <- summary(fit)$coefficients  
        
        
        return(sumfit)     
        
}
