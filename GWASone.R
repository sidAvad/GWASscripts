GWASone <- function(geno,pheno,cov,i){
        
        X <- geno
        y <- pheno
        w <- cov
        
        fit <- vector()
        sumfit <- vector() 
        logp <- vector()
        
        mod<- model.matrix(~as.factor(w[,1])+ as.factor(w[,2]))
        
    
        fit <- lm(y~X[,i]+mod[,2]+mod[,3]+mod[,4]+mod[,5]+mod[,6]+mod[,7]+mod[,8]+mod[,9]) ## y ~ X[,i] + (confounders)
        sumfit <- summary(fit)$coefficients  
        
        
        return(sumfit)     
        
}
