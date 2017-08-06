##シミュレーション 連続版-------------
set.seed(12345)

# source("KEISAN.R")

#set true parameters
b0<-0.5
b1<-1
b2<-1*-1

TLR_all<-NULL
SC_all<-NULL
IF_all<-NULL

N1<-3000 #data_sup

#200のとき---------
HAKO_tlr<-NULL
HAKO_sc<-NULL
HAKO_if<-NULL
N2<-200 #data_comp

for(k in 1:1000){
  
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
  
  ifdata <- cbind.data.frame(
            "Y"=c(rep(1,nrow(data_choice)),rep(0,nrow(data_sup))),
            rbind.data.frame(data_choice,data_sup)
  )
  
  
  TLR_kekka<-pml(formula = ~X1+X2,data_comp = data_choice,data_sup = data_sup,method = "TLR")
  SC_kekka<-pml(formula = ~X1+X2,data_comp = data_choice,data_sup = data_sup,method = "SC",share = s)
  IF_kekka<-glm(Y ~ X1+X2,family = "binomial",data = ifdata)
  
  HAKO_tlr<-rbind(HAKO_tlr,TLR_kekka[,1])
  HAKO_sc<-rbind(HAKO_sc,SC_kekka[,1])
  HAKO_if<-rbind(HAKO_if,coef(IF_kekka))
}

HAKO_tlr<-cbind(HAKO_tlr,N2)
HAKO_sc<-cbind(HAKO_sc,N2)
HAKO_if<-cbind(HAKO_if,N2)

TLR_all<-rbind(TLR_all,HAKO_tlr)
SC_all<-rbind(SC_all,HAKO_sc)
IF_all<-rbind(IF_all,HAKO_if)

##400のとき-------
HAKO_tlr<-NULL
HAKO_sc<-NULL
HAKO_if<-NULL

N2<-400 #data_comp

for(k in 1:1000){
  
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
  
  ifdata <- cbind.data.frame(
    "Y"=c(rep(1,nrow(data_choice)),rep(0,nrow(data_sup))),
    rbind.data.frame(data_choice,data_sup)
  )
  
  
  TLR_kekka<-pml(formula = ~X1+X2,data_comp = data_choice,data_sup = data_sup,method = "TLR")
  SC_kekka<-pml(formula = ~X1+X2,data_comp = data_choice,data_sup = data_sup,method = "SC",share = s)
  IF_kekka<-glm(Y ~ X1+X2,family = "binomial",data = ifdata)
  
  HAKO_tlr<-rbind(HAKO_tlr,TLR_kekka[,1])
  HAKO_sc<-rbind(HAKO_sc,SC_kekka[,1])
  HAKO_if<-rbind(HAKO_if,coef(IF_kekka))
}

HAKO_tlr<-cbind(HAKO_tlr,N2)
HAKO_sc<-cbind(HAKO_sc,N2)
HAKO_if<-cbind(HAKO_if,N2)

TLR_all<-rbind(TLR_all,HAKO_tlr)
SC_all<-rbind(SC_all,HAKO_sc)
IF_all<-rbind(IF_all,HAKO_if)

##600のとき-------
HAKO_tlr<-NULL
HAKO_sc<-NULL
HAKO_if<-NULL

N2<-600 #data_comp

for(k in 1:1000){
  
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
  
  ifdata <- cbind.data.frame(
    "Y"=c(rep(1,nrow(data_choice)),rep(0,nrow(data_sup))),
    rbind.data.frame(data_choice,data_sup)
  )
  
  
  TLR_kekka<-pml(formula = ~X1+X2,data_comp = data_choice,data_sup = data_sup,method = "TLR")
  SC_kekka<-pml(formula = ~X1+X2,data_comp = data_choice,data_sup = data_sup,method = "SC",share = s)
  IF_kekka<-glm(Y ~ X1+X2,family = "binomial",data = ifdata)
  
  HAKO_tlr<-rbind(HAKO_tlr,TLR_kekka[,1])
  HAKO_sc<-rbind(HAKO_sc,SC_kekka[,1])
  HAKO_if<-rbind(HAKO_if,coef(IF_kekka))
}

HAKO_tlr<-cbind(HAKO_tlr,N2)
HAKO_sc<-cbind(HAKO_sc,N2)
HAKO_if<-cbind(HAKO_if,N2)

TLR_all<-rbind(TLR_all,HAKO_tlr)
SC_all<-rbind(SC_all,HAKO_sc)
IF_all<-rbind(IF_all,HAKO_if)

#
write.csv(TLR_all,"TLR_all.csv")
write.csv(SC_all,"SC_all.csv")
write.csv(IF_all,"IF_all.csv")

###########
# #SC
# cbind(
# "mean"=apply(HAKO_sc,2,mean),
# "median"=apply(HAKO_sc,2,median),
# "sd"=apply(HAKO_sc,2,function(x)sd(x)*sqrt(length(x)/(length(x)-1))),
# "RMSE"=apply((c(b0,b1,b2)-HAKO_sc)^2,2,function(x){sqrt(sum(x)/length(x))})
# )
# 
# #TLR
# cbind(
#   "mean"=apply(HAKO_tlr,2,mean),
#   "median"=apply(HAKO_tlr,2,median),
#   "sd"=apply(HAKO_sc,2,function(x)sd(x)*sqrt(length(x)/(length(x)-1))),
#   "RMSE"=apply((c(b0,b1,b2)-HAKO_tlr)^2,2,function(x){sqrt(sum(x)/length(x))})
# )
# 
# #IF
# cbind(
#   "mean"=apply(HAKO_if,2,mean),
#   "median"=apply(HAKO_if,2,median),
#   "sd"=apply(HAKO_sc,2,function(x)sd(x)*sqrt(length(x)/(length(x)-1))),
#   "RMSE"=apply((c(b0,b1,b2)-HAKO_if)^2,2,function(x){sqrt(sum(x)/length(x))})
# )
# 
# ###結果###
