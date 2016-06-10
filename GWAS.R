GWAS1 <- function(geno,pheno,cov){
        
X <- geno
y <- pheno
w <- cov

fit <- vector()
sumfit <- matrix(nrow=20000, ncol=10) 
logp <- vector()


for (i in 1:ncol(X)){
     fit <- lm(y~X[,i]+as.factor(w[,1])+as.factor(w[,2])) ## y ~ X[,i] + (confounders ## y ~ X[,i] + (confounders)
     sumfit[i,] <- summary(fit)$coefficients[,4][2]    
     logp[i] <- -log10(summary(fit)$coefficients[,4][2])
}
 
return(logp)     

}
