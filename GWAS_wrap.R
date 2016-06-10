GWAS_wrap <- function(task,nTasks=4,pheno=y,Xsnp=X,W=w){
temp <- rep(1:nTasks,each=ceiling(ncol(Xsnp)/nTasks))[1:ncol(Xsnp)]
j <- which(temp==task)
out <- GWAS1(Xsnp[,j],pheno,w)
return(out)
}

