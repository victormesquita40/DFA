
DFA<-function(file,scale = 2^(1/8),box_size = 4,m=1){

  if(class(file)=="data.frame")
  {
    file <- file[,1]
  }

  N<-length(file)

  if(scale != "F")
  {

  box_size<-NULL;n=1;n_aux<-0;box_size[1]<-4

  for(n in 1:N){
    while (n_aux<round(N/4)) {
      n_aux<-box_size[1]
      n = n + 1
      box_size[n]<-ceiling(scale*box_size[n-1])
      n_aux<- box_size[n]+4
    }
  }

  }else{
    box_size <- box_size
  }

    ninbox2<- NULL

    for(j in 1:length(box_size))
    {
      ninbox2[j] <- N%/%box_size[j]

    }

    aux_j<-NULL;aux_j[1]<-box_size[1];Log_n<-NULL;DFA<- NULL;yn_k<-NULL;y_k<-NULL
    coef_alpha<-NULL;coef_beta<-NULL;aux_yk<-NULL;aux_coef_alpha<-NULL;aux_coef_beta<-NULL
    Results<-NULL;j=1

    aux_j<-numeric(box_size[j])
    aux_list <- lapply(seq_along(ninbox2), function(j){
      aux_j <- numeric(box_size[j]*ninbox2[j])

      for(i in 2:(box_size[j]*ninbox2[j])){
        y_k[1]  <-file[1] - mean(file)
        y_k[i]  <-y_k[i-1] + file[i] - mean(file)
      }

        for(i in seq_len(box_size[j]*ninbox2[j])){
        if(i==1){
          i<-1
          aux_j[1]<- box_size[j]

          aux_coef_alpha[i]<-coefficients(lm(y_k[i:aux_j[i]]~poly(c(i:aux_j[i]),m,raw=TRUE)))[2]
          aux_coef_beta[i]<-coefficients(lm(y_k[i:aux_j[i]]~poly(c(i:aux_j[i]),m,raw=TRUE)))[1]

          coef_alpha[i:(aux_j[i])]<-aux_coef_alpha[i]
          coef_beta [i:(aux_j[i])] <-aux_coef_beta[i]

          yn_k[i:(aux_j[i])]<-coef_alpha[i:(aux_j[i])]*c(i:(aux_j[i])) + coef_beta[i:(aux_j[i])]

        }

        if(i>=2){

          aux_j[i] <- aux_j[i-1]+ box_size[j]

          aux_coef_alpha[i]<-coefficients(lm(y_k[(aux_j[i-1]+1):aux_j[i]]~poly(c((aux_j[i-1]+1):aux_j[i]),m,raw=TRUE)))[2]
          aux_coef_beta[i]<-coefficients(lm(y_k[(aux_j[i-1]+1):aux_j[i]]~poly(c((aux_j[i-1]+1):aux_j[i]),m,raw=TRUE)))[1]

          coef_alpha[(aux_j[i-1]+1):(aux_j[i])]<-aux_coef_alpha[i]
          coef_beta [(aux_j[i-1]+1):(aux_j[i])] <-aux_coef_beta[i]

          yn_k[(aux_j[i-1]+1):(aux_j[i])]<-coef_alpha[(aux_j[i-1]+1):(aux_j[i])]*c((aux_j[i-1]+1):(aux_j[i])) + coef_beta[(aux_j[i-1]+1):(aux_j[i])]
          DFA<- sqrt((1/N)*sum((y_k[1:(box_size[j]*ninbox2[j])]-yn_k[1:(box_size[j]*ninbox2[j])])^2))
          Results<- c(round(box_size[j],digits = 0),round(DFA,digits=6))

        }

        if(i>=ninbox2[j]){
          aux_j[i] <- 0
        }
      }
      Results

    })


    aux_list<-matrix(unlist(aux_list),nrow=length(box_size),byrow=TRUE)

    colnames(aux_list)<- c("boxe","DFA")

    print(list(aux_list)[[1]])

}

DeltaDFA<-function(file,file2,scale = 2^(1/8),box_size = 4,m=1){

  if(class(file)=="data.frame" || class(file2)=="data.frame"){
    file <- file[,1]
    file2<- file2[,1]
  }

  if(length(file)==length(file2)){

    N<-length(file)

    if(scale != "F")
    {

    box_size<-NULL;n=1;n_aux<-0;box_size[1]<-4

    for(n in 1:N){
      while (n_aux<round(N/4)) {
        n_aux<-box_size[1]
        n = n + 1
        box_size[n]<-ceiling(scale*box_size[n-1])
        n_aux<- box_size[n]+4
      }
    }

    }else{
      box_size <- box_size
    }

    ninbox2<- NULL

    for(j in 1:length(box_size))
    {
      ninbox2[j] <- N%/%box_size[j]

    }


    aux_j<-NULL;aux_j[1]<-box_size[1];Log_n<-NULL;Log_Fn<- NULL;Log_Fn2<-NULL;yn_k<-NULL
    y_k<-NULL;coef_alpha<-NULL;coef_beta<-NULL;aux_yk<-NULL;aux_coef_alpha<-NULL;aux_coef_beta<-NULL
    Results<-NULL;yn_k2<-NULL;y_k2<-NULL;coef_alpha2<-NULL;coef_beta2<-NULL;aux_coef_alpha2<-NULL
    aux_coef_beta2<-0;DeltaDFA<- 0

    j<-1

    aux_j<-numeric(box_size[j])

    aux_list <- lapply(seq_along(ninbox2), function(j){
      aux_j <- numeric(box_size[j]*ninbox2[j])

      for(i in 2:(box_size[j]*ninbox2[j])){
        y_k[1]  <-file[1] - mean(file)
        y_k[i]  <-y_k[i-1] + file[i] - mean(file)
        y_k2[1] <-file2[1] - mean(file2)
        y_k2[i] <-y_k2[i-1] + file2[i]-mean(file2)

      }

      for(i in seq_len(box_size[j]*ninbox2[j])){

          if(i==1){
          i<-1

          aux_j[1]<- box_size[j]

          aux_coef_alpha[i]<-coefficients(lm(y_k[i:aux_j[i]]~poly(c(i:aux_j[i]),m,raw=TRUE)))[2]
          aux_coef_beta[i]<-coefficients(lm(y_k[i:aux_j[i]]~poly(c(i:aux_j[i]),m,raw=TRUE)))[1]

          aux_coef_alpha2[i]<-coefficients(lm(y_k2[i:aux_j[i]]~poly(c(i:aux_j[i]),m,raw=TRUE)))[2]
          aux_coef_beta2[i]<-coefficients(lm(y_k2[i:aux_j[i]]~poly(c(i:aux_j[i]),m,raw=TRUE)))[1]

          coef_alpha[i:(aux_j[i])]<-aux_coef_alpha[i]
          coef_beta [i:(aux_j[i])] <-aux_coef_beta[i]

          coef_alpha2[i:(aux_j[i])]<-aux_coef_alpha2[i]
          coef_beta2 [i:(aux_j[i])] <-aux_coef_beta2[i]

          yn_k[i:(aux_j[i])]<-coef_alpha[i:(aux_j[i])]*c(i:(aux_j[i])) + coef_beta[i:(aux_j[i])]
          yn_k2[i:(aux_j[i])]<-coef_alpha2[i:(aux_j[i])]*c(i:(aux_j[i])) + coef_beta2[i:(aux_j[i])]


        }

        if(i>=2){

          aux_j[i] <- aux_j[i-1]+ box_size[j]

          aux_coef_alpha[i]<-coefficients(lm(y_k[(aux_j[i-1]+1):aux_j[i]]~poly(c((aux_j[i-1]+1):aux_j[i]),m,raw=TRUE)))[2]
          aux_coef_beta[i]<-coefficients(lm(y_k[(aux_j[i-1]+1):aux_j[i]]~poly(c((aux_j[i-1]+1):aux_j[i]),m,raw=TRUE)))[1]

          aux_coef_alpha2[i]<-coefficients(lm(y_k2[(aux_j[i-1]+1):aux_j[i]]~poly(c((aux_j[i-1]+1):aux_j[i]),m,raw=TRUE)))[2]
          aux_coef_beta2[i]<-coefficients(lm(y_k2[(aux_j[i-1]+1):aux_j[i]]~poly(c((aux_j[i-1]+1):aux_j[i]),m,raw=TRUE)))[1]

          coef_alpha[(aux_j[i-1]+1):(aux_j[i])]<-aux_coef_alpha[i]
          coef_beta [(aux_j[i-1]+1):(aux_j[i])] <-aux_coef_beta[i]

          coef_alpha2[(aux_j[i-1]+1):(aux_j[i])]<-aux_coef_alpha2[i]
          coef_beta2[(aux_j[i-1]+1):(aux_j[i])] <-aux_coef_beta2[i]

          yn_k[(aux_j[i-1]+1):(aux_j[i])]<-coef_alpha[(aux_j[i-1]+1):(aux_j[i])]*c((aux_j[i-1]+1):(aux_j[i])) + coef_beta[(aux_j[i-1]+1):(aux_j[i])]
          yn_k2[(aux_j[i-1]+1):(aux_j[i])]<-coef_alpha2[(aux_j[i-1]+1):(aux_j[i])]*c((aux_j[i-1]+1):(aux_j[i])) + coef_beta2[(aux_j[i-1]+1):(aux_j[i])]

          Log_Fn<- log10(sqrt((1/N)*sum((y_k[1:(box_size[j]*ninbox2[j])]-yn_k[1:(box_size[j]*ninbox2[j])])^2)))
          Log_Fn2<- log10(sqrt((1/N)*sum((y_k2[1:(box_size[j]*ninbox2[j])]-yn_k2[1:(box_size[j]*ninbox2[j])])^2)))

          DeltaDFA<- Log_Fn - Log_Fn2

          Results<- c(round(box_size[j],digits = 0),round(DeltaDFA,digits=6))


        }

        if(i>=ninbox2[j]){
          aux_j[i] <- 0

        }

      }
      Results

    })


    aux_list<-matrix(unlist(aux_list),nrow=length(box_size),byrow=TRUE)

    colnames(aux_list)<- c("boxe","DeltaDFA")

    print(list(aux_list)[[1]])

  }

}

DCCA<-function(file,file2,scale = 2^(1/8),box_size = 4,m=1){

  if(class(file)=="data.frame" || class(file2)=="data.frame"){
    file <- file[,1]
    file2<- file2[,1]
  }

  if(length(file)==length(file2)){

    N<-length(file)

    if(scale != "F")
    {

    box_size<-NULL;n=1;n_aux<-0;box_size[1]<-4

    for(n in 1:N){
      while (n_aux<round(N/4)) {
        n_aux<-box_size[1]
        n = n + 1
        box_size[n]<-ceiling(scale*box_size[n-1])
        n_aux<- box_size[n]+4
      }
    }

    }else{
      box_size <- box_size
    }

    j=1
    aux_list <- lapply(seq_along(box_size), function(j){


      aux_j<-NULL;aux_j<-numeric(box_size[j]);coef_alpha<-NULL;coef_beta<-NULL
      Rn_k<-NULL;Rn_k2<-NULL;coef_alpha2<-NULL;coef_beta2<-NULL;f2DCCA<- NULL
      DCCA<-NULL;Results2<-NULL;f2DFA1<-NULL;f2DFA2<-NULL;DFA1<-NULL;DFA2<-NULL
      R_k<-NULL;R_k2<-NULL;aux_r<-0

      aux_r[1]<-box_size[j]+1;R_k[1:aux_r[1]]<- file[1:aux_r[1]] - mean(file[1:aux_r[1]])
      R_k2[1:aux_r[1]]<- file2[1:aux_r[1]] - mean(file2[1:aux_r[1]]);intervals<-N%/%(box_size[j]+1)

      for(l in 2:intervals){
        aux_r[l]<-aux_r[l-1]+box_size[j]+1

        R_k[(aux_r[l-1]+1):aux_r[l]]<- file[(aux_r[l-1]+1):aux_r[l]] - mean(file[(aux_r[l-1]+1):aux_r[l]])

        R_k2[(aux_r[l-1]+1):aux_r[l]]<- file2[(aux_r[l-1]+1):aux_r[l]] - mean(file2[(aux_r[l-1]+1):aux_r[l]])

      }

      for(i in seq_len(max(aux_r)-box_size[j]))
      {
        if(i==1){

          aux_j[i]<- box_size[j]+1

          coef_alpha[i]<-coefficients(lm(c(R_k[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[2]
          coef_beta[i]<-coefficients(lm(c(R_k[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[1]

          coef_alpha2[i]<-coefficients(lm(c(R_k2[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[2]
          coef_beta2[i]<-coefficients(lm(c(R_k2[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[1]


          Rn_k[1]<-coef_alpha[1]*1 +coef_beta[1]
          Rn_k2[1]<-coef_alpha2[1]*1 +coef_beta2[1]

        }

        if(i>=2){

          aux_j[i]<- aux_j[i-1]+1

          coef_alpha[i]<-coefficients(lm(c(R_k[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[2]
          coef_beta[i]<-coefficients(lm(c(R_k[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[1]

          coef_alpha2[i]<-coefficients(lm(c(R_k2[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[2]
          coef_beta2[i]<-coefficients(lm(c(R_k2[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[1]

          Rn_k[i]<-coef_alpha[i]*i +coef_beta[i]
          Rn_k2[i]<-coef_alpha2[i]*i + coef_beta2[i]

          f2DFA1[1]<- (1/(box_size[j]+1))*sum((R_k[1]-Rn_k[1])**2)
          f2DFA2[1]<- (1/(box_size[j]+1))*sum((R_k2[1]-Rn_k2[1])**2)

          f2DFA1[i]<- (1/(box_size[j]+1))*sum((R_k[i]-Rn_k[i])**2)
          f2DFA2[i]<- (1/(box_size[j]+1))*sum((R_k2[i]-Rn_k2[i])**2)

          f2DCCA[1] <- (1/(box_size[j]+1))*sum((R_k[1]-Rn_k[1])*(R_k2[1]-Rn_k2[1]))
          f2DCCA[i] <- (1/(box_size[j]+1))*sum((R_k[i]-Rn_k[i])*(R_k2[i]-Rn_k2[i]))

          DFA1 <- sqrt((1/(N+box_size[j]))*sum(f2DFA1))
          DFA2 <- sqrt((1/(N+box_size[j]))*sum(f2DFA2))
          DCCA <- (1/(N+box_size[j]))*sum(f2DCCA)

          aux_Result<- c(DFA1,DFA2)
          Results<- c(round(box_size[j],digits=0),round(DFA1,digits=6),round(DFA2,digits=6))
          Results2<- DCCA

        }

      }



      c(Results,round(Results2,digits=6))

    })

    aux_list<-matrix(unlist(aux_list),nrow=length(box_size),byrow=TRUE)

    colnames(aux_list)<- c("boxe","DFA","DFA2","DCCA")

    print(list(aux_list)[[1]])

  }

}

rhoDCCA<-function(file,file2,scale = 2^(1/8),box_size = 4,m=1){

  if(class(file)=="data.frame" || class(file2)=="data.frame"){
    file <- file[,1]
    file2<- file2[,1]
  }

  if(length(file)==length(file2)){

    N<-length(file)

    if(scale != "F")
    {

    box_size<-NULL;n=1;n_aux<-0;box_size[1]<-4

    for(n in 1:N){
      while (n_aux<round(N/4)) {
        n_aux<-box_size[1]
        n = n + 1
        box_size[n]<-ceiling(scale*box_size[n-1])
        n_aux<- box_size[n]+4
      }
    }

    }else{
      box_size <- box_size
    }

    j=1
    aux_list <- lapply(seq_along(box_size), function(j){


      aux_j<-NULL;aux_j<-numeric(box_size[j]);coef_alpha<-NULL;coef_beta<-NULL
      Rn_k<-NULL;Rn_k2<-NULL;coef_alpha2<-NULL;coef_beta2<-NULL;f2DCCA<- NULL
      DCCA<-NULL;Results2<-NULL;f2DFA1<-NULL;f2DFA2<-NULL;DFA1<-NULL;DFA2<-NULL
      R_k<-NULL;R_k2<-NULL;aux_r<-0

      aux_r[1]<-box_size[j]+1;R_k[1:aux_r[1]]<- file[1:aux_r[1]] - mean(file[1:aux_r[1]])
      R_k2[1:aux_r[1]]<- file2[1:aux_r[1]] - mean(file2[1:aux_r[1]]);intervals<-N%/%(box_size[j]+1)

      for(l in 2:intervals){
        aux_r[l]<-aux_r[l-1]+box_size[j]+1

        R_k[(aux_r[l-1]+1):aux_r[l]]<- file[(aux_r[l-1]+1):aux_r[l]] - mean(file[(aux_r[l-1]+1):aux_r[l]])

        R_k2[(aux_r[l-1]+1):aux_r[l]]<- file2[(aux_r[l-1]+1):aux_r[l]] - mean(file2[(aux_r[l-1]+1):aux_r[l]])

      }

      for(i in seq_len(max(aux_r)-box_size[j]))
      {
        if(i==1){

          aux_j[i]<- box_size[j]+1

          coef_alpha[i]<-coefficients(lm(c(R_k[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[2]
          coef_beta[i]<-coefficients(lm(c(R_k[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[1]

          coef_alpha2[i]<-coefficients(lm(c(R_k2[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[2]
          coef_beta2[i]<-coefficients(lm(c(R_k2[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[1]


          Rn_k[1]<-coef_alpha[1]*1 +coef_beta[1]
          Rn_k2[1]<-coef_alpha2[1]*1 +coef_beta2[1]

        }

        if(i>=2){

          aux_j[i]<- aux_j[i-1]+1

          coef_alpha[i]<-coefficients(lm(c(R_k[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[2]
          coef_beta[i]<-coefficients(lm(c(R_k[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[1]

          coef_alpha2[i]<-coefficients(lm(c(R_k2[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[2]
          coef_beta2[i]<-coefficients(lm(c(R_k2[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[1]

          Rn_k[i]<-coef_alpha[i]*i +coef_beta[i]
          Rn_k2[i]<-coef_alpha2[i]*i + coef_beta2[i]

          f2DFA1[1]<- (1/(box_size[j]+1))*sum((R_k[1]-Rn_k[1])**2)
          f2DFA2[1]<- (1/(box_size[j]+1))*sum((R_k2[1]-Rn_k2[1])**2)

          f2DFA1[i]<- (1/(box_size[j]+1))*sum((R_k[i]-Rn_k[i])**2)
          f2DFA2[i]<- (1/(box_size[j]+1))*sum((R_k2[i]-Rn_k2[i])**2)

          f2DCCA[1] <- (1/(box_size[j]+1))*sum((R_k[1]-Rn_k[1])*(R_k2[1]-Rn_k2[1]))
          f2DCCA[i] <- (1/(box_size[j]+1))*sum((R_k[i]-Rn_k[i])*(R_k2[i]-Rn_k2[i]))

          DFA1 <- sqrt((1/(N+box_size[j]))*sum(f2DFA1))
          DFA2 <- sqrt((1/(N+box_size[j]))*sum(f2DFA2))
          DCCA <- (1/(N+box_size[j]))*sum(f2DCCA)

          aux_Result<- c(DFA1,DFA2)
          Results<- c(round(box_size[j],digits=0),round(DFA1,digits=6),round(DFA2,digits=6))
          Results2<- DCCA

        }

      }



      c(Results,round(Results2,digits=6),round(Results2/(aux_Result[1]*aux_Result[2]),digits = 3))

    })

    aux_list<-matrix(unlist(aux_list),nrow=length(box_size),byrow=TRUE)

    colnames(aux_list)<- c("boxe","DFA1","DFA2","DCCA","rhoDCCA")

    print(list(aux_list)[[1]])

  }

}

Deltarho<-function(file,file2,file3,file4,scale = 2^(1/8),box_size = 4,m=1){

  if(class(file)=="data.frame" || class(file2)=="data.frame" || class(file3)=="data.frame"|| class(file4)=="data.frame"){
    file <- file[,1]
    file2<- file2[,1]
    file3<- file3[,1]
    file4<- file4[,1]

  }

  if(length(file)==length(file2) || length(file)==length(file3) ||length(file)==length(file4)){

    N<-length(file)

    if(scale != "F")
    {

    box_size<-NULL;n=1;n_aux<-0;box_size[1]<-4

    for(n in 1:N){
      while (n_aux<round(N/4)) {
        n_aux<-box_size[1]
        n = n + 1
        box_size[n]<-ceiling(scale*box_size[n-1])
        n_aux<- box_size[n]+4
      }
    }

    }else{
      box_size <- box_size
    }

    j=1
    aux_list <- lapply(seq_along(box_size), function(j){


      aux_j<-NULL;aux_j<-numeric(box_size[j]);coef_alpha<-NULL;coef_beta<-NULL
      Rn_k<-NULL;Rn_k2<-NULL;coef_alpha2<-NULL;coef_beta2<-NULL;f2DCCA<- NULL
      DCCA<-NULL;Results2<-NULL;f2DFA1<-NULL;f2DFA2<-NULL;DFA1<-NULL;DFA2<-NULL
      R_k<-NULL;R_k2<-NULL;aux_r<-0

      coef_alpha3<-NULL;coef_beta3<-NULL
      Rn_k3<-NULL;Rn_k4<-NULL;coef_alpha4<-NULL;coef_beta4<-NULL;f2DCCA2<- NULL
      DCCA2<-NULL;f2DFA3<-NULL;f2DFA4<-NULL;DFA3<-NULL;DFA4<-NULL
      R_k3<-NULL;R_k4<-NULL;rhoDCCA1<- NULL;rhoDCCA2<- NULL


      aux_r[1]<-box_size[j]+1

      R_k[1:aux_r[1]]<- file[1:aux_r[1]] - mean(file[1:aux_r[1]])
      R_k2[1:aux_r[1]]<- file2[1:aux_r[1]] - mean(file2[1:aux_r[1]])
      R_k3[1:aux_r[1]]<- file3[1:aux_r[1]] - mean(file3[1:aux_r[1]])
      R_k4[1:aux_r[1]]<- file4[1:aux_r[1]] - mean(file4[1:aux_r[1]])


      intervals<-N%/%(box_size[j]+1)

      for(l in 2:intervals){
        aux_r[l]<-aux_r[l-1]+box_size[j]+1

        R_k[(aux_r[l-1]+1):aux_r[l]]<- file[(aux_r[l-1]+1):aux_r[l]] - mean(file[(aux_r[l-1]+1):aux_r[l]])

        R_k2[(aux_r[l-1]+1):aux_r[l]]<- file2[(aux_r[l-1]+1):aux_r[l]] - mean(file2[(aux_r[l-1]+1):aux_r[l]])

        R_k3[(aux_r[l-1]+1):aux_r[l]]<- file3[(aux_r[l-1]+1):aux_r[l]] - mean(file3[(aux_r[l-1]+1):aux_r[l]])

        R_k4[(aux_r[l-1]+1):aux_r[l]]<- file4[(aux_r[l-1]+1):aux_r[l]] - mean(file4[(aux_r[l-1]+1):aux_r[l]])


      }

      for(i in seq_len(max(aux_r)-box_size[j]))
      {
        if(i==1){

          aux_j[i]<- box_size[j]+1

          coef_alpha[i]<-coefficients(lm(c(R_k[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[2]
          coef_beta[i]<-coefficients(lm(c(R_k[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[1]

          coef_alpha2[i]<-coefficients(lm(c(R_k2[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[2]
          coef_beta2[i]<-coefficients(lm(c(R_k2[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[1]

          coef_alpha3[i]<-coefficients(lm(c(R_k3[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[2]
          coef_beta3[i]<-coefficients(lm(c(R_k3[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[1]

          coef_alpha4[i]<-coefficients(lm(c(R_k4[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[2]
          coef_beta4[i]<-coefficients(lm(c(R_k4[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[1]


          Rn_k[1]<-coef_alpha[1]*1 +coef_beta[1]
          Rn_k2[1]<-coef_alpha2[1]*1 +coef_beta2[1]
          Rn_k3[1]<-coef_alpha3[1]*1 +coef_beta3[1]
          Rn_k4[1]<-coef_alpha4[1]*1 +coef_beta4[1]


        }

        if(i>=2){

          aux_j[i]<- aux_j[i-1]+1

          coef_alpha[i]<-coefficients(lm(c(R_k[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[2]
          coef_beta[i]<-coefficients(lm(c(R_k[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[1]

          coef_alpha2[i]<-coefficients(lm(c(R_k2[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[2]
          coef_beta2[i]<-coefficients(lm(c(R_k2[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[1]

          coef_alpha3[i]<-coefficients(lm(c(R_k3[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[2]
          coef_beta3[i]<-coefficients(lm(c(R_k3[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[1]

          coef_alpha4[i]<-coefficients(lm(c(R_k4[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[2]
          coef_beta4[i]<-coefficients(lm(c(R_k4[i:aux_j[i]])~poly(c(i:aux_j[i]),m,raw=TRUE)))[1]

          Rn_k[i]<-coef_alpha[i]*i +coef_beta[i]
          Rn_k2[i]<-coef_alpha2[i]*i + coef_beta2[i]
          Rn_k3[i]<-coef_alpha3[i]*i + coef_beta3[i]
          Rn_k4[i]<-coef_alpha4[i]*i + coef_beta4[i]

          f2DFA1[1]<- (1/(box_size[j]+1))*sum((R_k[1]-Rn_k[1])**2)
          f2DFA2[1]<- (1/(box_size[j]+1))*sum((R_k2[1]-Rn_k2[1])**2)
          f2DFA3[1]<- (1/(box_size[j]+1))*sum((R_k3[1]-Rn_k3[1])**2)
          f2DFA4[1]<- (1/(box_size[j]+1))*sum((R_k4[1]-Rn_k4[1])**2)

          f2DFA1[i]<- (1/(box_size[j]+1))*sum((R_k[i]-Rn_k[i])**2)
          f2DFA2[i]<- (1/(box_size[j]+1))*sum((R_k2[i]-Rn_k2[i])**2)
          f2DFA3[i]<- (1/(box_size[j]+1))*sum((R_k3[i]-Rn_k3[i])**2)
          f2DFA4[i]<- (1/(box_size[j]+1))*sum((R_k4[i]-Rn_k4[i])**2)

          f2DCCA[1] <- (1/(box_size[j]+1))*sum((R_k[1]-Rn_k[1])*(R_k2[1]-Rn_k2[1]))
          f2DCCA[i] <- (1/(box_size[j]+1))*sum((R_k[i]-Rn_k[i])*(R_k2[i]-Rn_k2[i]))

          f2DCCA2[1] <- (1/(box_size[j]+1))*sum((R_k3[1]-Rn_k3[1])*(R_k4[1]-Rn_k4[1]))
          f2DCCA2[i] <- (1/(box_size[j]+1))*sum((R_k3[i]-Rn_k3[i])*(R_k4[i]-Rn_k4[i]))


          DFA1 <- sqrt((1/(N+box_size[j]))*sum(f2DFA1))
          DFA2 <- sqrt((1/(N+box_size[j]))*sum(f2DFA2))
          DFA3 <- sqrt((1/(N+box_size[j]))*sum(f2DFA3))
          DFA4 <- sqrt((1/(N+box_size[j]))*sum(f2DFA4))


          DCCA <- (1/(N+box_size[j]))*sum(f2DCCA)
          DCCA2 <- (1/(N+box_size[j]))*sum(f2DCCA2)

          aux_Result<- c(DFA1,DFA2,DFA3,DFA4)
          Results<- c(round(box_size[j],digits=0),round(DFA1,digits=6),
                      round(DFA2,digits=6),round(DFA3,digits=6),round(DFA4,digits=6))
          Results2<- c(DCCA, DCCA2)

        }

      }

      rhoDCCA1<- c(round(Results2[1]/(aux_Result[1]*aux_Result[2]),digits = 3))
      rhoDCCA2<- c(round(Results2[2]/(aux_Result[3]*aux_Result[4]),digits = 3))

      c(Results,round(Results2,digits=6),rhoDCCA1,rhoDCCA2,rhoDCCA2-rhoDCCA1)

    })

    aux_list<-matrix(unlist(aux_list),nrow=length(box_size),byrow=TRUE)

    colnames(aux_list)<- c("boxe","DFA","DFA2","DFA3"
                           ,"DFA4","DCCA","DCCA2","rhoDCCA1","rhoDCCA2","DeltaRho")

    print(list(aux_list)[[1]])

  }

}

AUC<-function(x,data){


  aux_list <- lapply(seq_along(data), function(j){


    s<- data[,j][which.min(data[,j])];area<- NULL;B<-NULL
    b<- NULL;h<-NULL;Area<-NULL;Results<-NULL;B[1]<-x[2] - x[1]

    for(i in 1:length(x))
    {
      h[i]<- abs(data[,j][i]-s)
    }

    for(i in 2:length(x)){
      B[i]<-x[i+1] - x[i]
      B<- B[1:(length(x)-1)]
      area[1]<- B[1]*h[1]+ (abs(h[2]-h[1]))*B[1]/2
      area[i]<- B[i]*h[i]+ (abs(h[i+1]-h[i]))*B[i]/2
      area<- area[1:(length(x)-1)]

      Area<- sum(area)
      Results<-Area
    }
    Results

  })

  (Final_Results<-list(position = which.max(aux_list),Area = aux_list[which.max(aux_list)][[1]]))


}

secant <- function(x,y,npoint,size_fit){

  for(j in 1:npoint){
    if(j == 1){


      n_4<- NULL;n_4[j]<-size_fit

      xx<- NULL; xx[[j]] <- list(seq(0,x[round(length(x)/4)],length.out = n_4[j]))

      tn_3<- NULL;tn_3[j]<-round(length(x)-n_4[j])

      xx2<-NULL;xx2[[j]] <- list(seq(x[tn_3[j]],x[length(x)], length = length(x)-tn_3[j]))

      mean_slope<- NULL;mean_slope[j] <- mean(c(lm(y[1:n_4[j]] ~ x[1:n_4[j]])$coefficients[2],
                                                lm(y[(tn_3[j]+1):length(x)] ~ x[(tn_3[j]+1):length(x)])$coefficients[2]))

      aux_ajuste<-NULL;cont<-0

      for(i in 1:length(x))
      {
        if(cont<=length(x)){
          cont<- cont+1
          aux_ajuste[i]<- lm(y[c(cont:(cont+1))]~x[c(cont:(cont+1))])$coefficients[2]

        }
      }

      aux_ajuste<-aux_ajuste[round(length(x)/4):round(length(x)-length(x)/4)]

      posit<-NULL;posit[j]<-round(length(x)/4)+which.max(aux_ajuste<mean_slope[[j]][[j]])-1


    }


    if(j > 1){
      x2<- list(); x2[[1]]<- 0;y2<- list(); y2[[1]]<- 0
      x2[[j]]<- list(x[(round(length(x)/4)+ which.max(aux_ajuste<mean_slope[j-1])):round(length(x)-length(x)/4)])
      y2[[j]]<- list(y[(round(length(x)/4)+ which.max(aux_ajuste<mean_slope[j-1])):round(length(x)-length(x)/4)])


      xx[[j]]<-seq(x2[[j]][[1]][length(x2[[j]][[1]])-round(length(x2[[j]][[1]])/4)],x2[[j]][[1]][length(x2[[j]][[1]])], length = round(length(x2[[j]][[1]])/4))

      xx2[[j]] <- seq(x2[[j]][[1]][1],x2[[j]][[1]][round(length(x2[[j]][[1]])/4)], length = round(length(x2[[j]][[1]])/4))


      mean_slope[j] <- mean(c(lm(y2[[j]][[1]][1:round(length(x2[[j]][[1]])/4)] ~ x2[[j]][[1]][1:round(length(x2[[j]][[1]])/4)])$coefficients[2],
                              lm(y2[[j]][[1]][(length(x2[[j]][[1]])-round(length(x2[[j]][[1]])/4)+1):length(x2[[j]][[1]])] ~ x2[[j]][[1]][(length(x2[[j]][[1]])-round(length(x2[[j]][[1]])/4)+1):length(x2[[j]][[1]])])$coefficients[2]))

      aux_ajuste<-0;cont<-0

      for(i in 1:length(x2[[j]][[1]]))
      {
        if(cont<=length(x2[[j]][[1]])){
          cont<- cont+1
          aux_ajuste[i]<- lm(y2[[j]][[1]][c(cont:(cont+1))]~x2[[j]][[1]][c(cont:(cont+1))])$coefficients[2]

        }
      }


      aux_ajuste<-aux_ajuste[round(length(x2[[j]][[1]])/4):round(length(x2[[j]][[1]])-length(x2[[j]][[1]])/4)]

      which.max(aux_ajuste<mean_slope[j])

      posit[j]<-posit[j-1]+round(length(x2[[j]][[1]])/4)+which.max(aux_ajuste<mean_slope[j])

    }

  }

  (results = list(position=c(posit)))


}

euclidean<-function(x,y,npoint){
  if(npoint<=2){
    for(j in 1:npoint){
      if(j == 1){

        distance_after<- NULL;distance_after[[j]]<- list(0);distance_after[[j]][length(x)]<-0

        for(i in 2:(length(x)-1))
        {
          distance_after[[j]][[i]] <- abs(((y[length(y)] - y[1])*x[i]-(x[length(x)]-x[1])*y[i] + x[length(x)]*y[1]-y[length(y)]*y[1]))/sqrt((y[length(y)]-y[1])**2+(x[length(x)]-x[1])**2)
        }


        position<-NULL;position[j] = which.max(distance_after[[j]])
      }

      if(j > 1){

        distance_after[[j]]<- 0;distance_after[[j]][length(x)-position[j-1]]<-0
        distance_before<- NULL;distance_before[[j]]<- list(0)
        distance_before[[j]]<- 0;distance_before[[j]][length(x)-position[j-1]]<-0

        sugestion<-NULL;sugestion=list(0)

        for(i in 2:(length(x)-1))
        {

          if(i<=(position[j-1]-1)){
            distance_before[[j]][[i]] <- abs(((y[length(y)] - y[1])*x[i]-(x[length(x)]-x[1])*y[i] + x[length(x)]*y[1]-y[length(y)]*y[1]))/sqrt((y[length(y)]-y[1])**2+(x[length(x)]-x[1])**2)
          }


          if((position[j-1]+1)<=i)
          {
            distance_after[[j]][[i]] <- abs(((y[length(y)] - y[1])*x[i]-(x[length(x)]-x[1])*y[i] + x[length(x)]*y[1]-y[length(y)]*y[1]))/sqrt((y[length(y)]-y[1])**2+(x[length(x)]-x[1])**2)
          }

        }

        sugestion[[j]] = c(sugestion_before=which.max(distance_before[[j]]),sugestion_after=which.max(distance_after[[j]]))

      }
    }
    if(npoint==2)
    {
      c(position=list(position)[[1]],sugestion[[2:j]])
    }
    else
    {
      c(position=list(position)[[1]])
    }
  }
  else
  {
    paste("This amount of crossover points requires user iterativity and specific function. More information: victormesquita40@hotmail.com ")
  }
}
