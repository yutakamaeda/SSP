#####TLR and SC ESTMATOR ####

#no adjustment in s.e cal (details)
pml<-function(formula,data_comp,data_sup,method="TLR",
              weight_comp=NULL,weight_sup=NULL,
              share=NULL,tol=1e-6,boot=FALSE,bootnum=1000){

 options(warn = 0)
  
 DC <- model.matrix(formula,data_comp)
 ds <- model.matrix(formula,data_sup)
 
 if(!is.null(weight_comp)){
   weight_comp<-weight_comp[as.numeric(dimnames(DC)[[1]])]
   DC<-DC*weight_comp
 }
 
 if(!is.null(weight_sup)){
   weight_sup<-weight_sup[as.numeric(dimnames(ds)[[1]])]
   ds<-ds*weight_sup
 }
 
 if(!method == "TLR"){
   
   weight <- share*nrow(ds)/nrow(DC)
   
   LL <- function(beta) {
     sum(log(1+exp(ds%*%beta)))-
       weight*sum(dc%*%beta)
   }
   
 }else{
   
   N<-nrow(DC)
   
   LL <- function(beta) {
     p1<-sum(dc%*%beta-
                  log(1+exp(dc%*%beta)))
     
     p2<-log(mean(exp(ds%*%beta)/
                       (1+exp(ds%*%beta))))
     
     -p1+p2*N
   }
 }
 
 if(boot == TRUE){
   
   HAKO<-NULL
   j<-0
   n <- nrow(DC)
  
   for(j in 1:bootnum){
     dc<-DC[sample(1:n,n,replace =T),]
     kekka<-nlm(LL, p = rep(0, ncol(ds)),hessian = T,gradtol = tol)
     HAKO<-rbind(HAKO,kekka$estimate)
   }
   
   options(warn = 0)
   
   coef <- apply(HAKO,2,mean)
   s.e <- apply(HAKO,2,function(x)sd(x)*sqrt(length(x)-1)/sqrt(length(x)))
   CI.05 <- apply(HAKO,2,function(x){quantile(x,probs = 0.05)})
   CI.95 <- apply(HAKO,2,function(x){quantile(x,probs = 0.95)})
   p.value <- round(sapply(abs(coef/s.e),function(x){2*(1-pnorm(x))}),4)
   
   result<-cbind(coef,s.e,CI.05,CI.95,p.value)
   dimnames(result)[[1]]<-dimnames(ds)[[2]]
   
 }else{
     
   options(warn = -1)
   
   DC->dc
   
   kekka<-nlm(LL, p = rep(0, ncol(ds)),hessian = T,gradtol = tol)
   
   coef <- kekka$estimate
   s.e <- sqrt(diag(solve(kekka$hessian)))
   p.value <- round(sapply(abs(coef/s.e),function(x){2*(1-pnorm(x))}),4)
   
   result<-cbind(coef,s.e,p.value)
   dimnames(result)[[1]]<-dimnames(dc)[[2]]

    }
 
 result
 
}

