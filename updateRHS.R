updateRHS <- function(X1,X2ty,y){
        
        ## Returns updated RHS 
 
        
        ret <- rbind(crossprod(X1,y),X2ty)
        
}