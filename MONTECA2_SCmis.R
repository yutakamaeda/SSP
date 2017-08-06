##シミュレーション 連続版-------------
set.seed(12345)

# source("KEISAN.R")

#set true parameters
b0<-0.5
b1<-1
b2<-1*-1

##600のとき-------

N1<-3000 #data_sup
N2<-600 #data_comp

HAKO<-NULL

for(k in seq(from = -0.2,to = 0.2,by = 0.01)){
  
  hako<-NULL
  
  for(i in 1:1000){
    
    #complete data
    data_choice<-NULL
    j<-0
    
    while(j<=N2){
      
      X1<-rnorm(1,0,1)
      X2<-rnorm(1,0,1)
      
      Y_ast<-b0+X1*b1+X2*b2
      prob_Y<-exp(Y_ast)/(1+exp(Y_ast))
      
      Y<-sample(c(1,0),size = 1,prob = c(prob_Y,1-prob_Y))
      
      if(Y==1){
        h<-sample(c(T,F),1,prob = c(0.5,0.5))
        if(h==T){
          data_choice<-rbind(data_choice,c(Y,Y,X1,X2))
          j<-j+1
        }
      }
    }
    
    #supplement data
    
    X1<-rnorm(N1,0,1)
    X2<-rnorm(N1,0,1)
    
    Y_ast<-b0+X1*b1+X2*b2
    prob_Y<-exp(Y_ast)/(1+exp(Y_ast))
    
    Y<-sapply(prob_Y,function(x)sample(c(1,0),size = 1,prob = c(x,1-x)))
    s<-mean(Y) 
    data_sup<-cbind(Y,NA,X1,X2)
    
    #計算
    as.data.frame(data_choice[,3:4])->data_choice
    as.data.frame(data_sup[,3:4])->data_sup
    
    names(data_sup)<-names(data_choice)<-c("X1","X2")
    
    s_mis<-s+k
    SC_kekka<-pml(formula = ~X1+X2,data_comp = data_choice,data_sup = data_sup,method = "SC",share = s_mis)
    hako<-rbind(hako,SC_kekka[,1])
  }
  
  res <- c(apply(hako,2,mean),k)
  HAKO <-rbind(HAKO,res)
  
}


  
  
  
   