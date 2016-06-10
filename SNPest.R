SNPest <- function(X,X2,y){
        
        ## Return a list of parameter estimate, error variance and standardError 
        ## for SNPs one at a time ( efficient method using cholesky decomp)
        
        X2 <- model.matrix(~as.factor(X2[,1])+as.factor(X2[,2]))
        X2 <- X2[,-1]
        X2tX2 <- crossprod(X2)
        X2ty <- crossprod(X2,y)
        BetaHat <- vector()
        errorvar <- vector()
        standardError <- vector()
        ret <- matrix(nrow=10346,ncol=5)
        
        
        for (i in 1:ncol(X)){
                X1 <- X[,i]
                ones <- rep(1,length(X1))
                X1 <- cbind(ones,X1)   ## Appends column of ones to X1 before passing to updateC or updateRHS
                X1X2 <- cbind(X1,X2)
                X1 <- as.matrix(X1)
                Cinv <- updateC(X1,X2,X2tX2)
                RHS <- updateRHS(X1,X2ty,y)
                temp <- Cinv%*%RHS
                BetaHat[i] <- temp[2]
                errorvar[i] <- sum((y - X1X2%*%temp)^2)/(length(y) - ncol(X1X2))
                standardError[i] <- sqrt(errorvar[i]*Cinv[2,2])
                t <- BetaHat[i]/standardError[i]
                p <- 2*pt(-abs(t),length(y)-ncol(X1X2))
                ret[i,] <- c(BetaHat[i],errorvar[i],standardError[i],p,-log10(p))
                
        }
        
return(ret)


}