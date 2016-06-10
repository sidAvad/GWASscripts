updateC <- function(X1,X2,X2tX2){
        
        ## returns updated C inverse using cholesky decomposition 
 
        
        UR <- crossprod(X1,X2)
        LL <- crossprod(X2,X1)
        UL <- crossprod(X1)
        
        R <- rbind(UR,X2tX2)
        L <- rbind(UL,LL)
        
        C <- cbind(L,R)
       
        sol <- chol2inv(chol(C))
}
