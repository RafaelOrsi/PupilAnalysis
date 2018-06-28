###########################################################################################
#                                                                                         #  
# Title: "PupilAnalysis"                                                                  #
# Version: 2.0                                                                            #
# Date of last update: "June 27, 2018"                                                    #
# Author: "Rafael Nobre Orsi"                                                             #
# Maintainer: Rafael Nobre Orsi <rafaelnobre21@terra.com.br>                              #
# License: Free (but don't forget to cite the reference)                                  #
#                                                                                         #
###########################################################################################
#                                                                                         #
# Description: Algorithm for pupillary signal processing                                  #
# This algorithm has 7 steps                                                              #
#                                                                                         #    
# In step 1 the original database is loaded                                               #
# In step 2 the database is filtered and only the signal of interest is extracted (base.0)#
#         Additional to this step:                                                        #
#         1) Signal loss analysis                                                         #
#         2) Analysis of variance                                                         #
# In step 3 is done the pre-processing of the signal (base.1)                             #
#         Pre-processing steps                                                            #                      
#         1) Remove outliers (base.2)                                                     #
#         2) Data interpolation to fill signal loss (base.3)                              #
#         3) Smoothing filter to remove noise (base.4)                                    #
# In step 4 is calculate the PCA (base.5)                                                 #
# In step 5 is calculate the MLDA (base.6)                                                #
# In step 6 is done the crossvalidation                                                   #
# In step 7 presents the final results (final report)                                     #
#                                                                                         #
###########################################################################################

# Libraries require (uncomment the lines below to use) 
# install.packages("vegan") # Library to calculate the PCA
# library("vegan")


###########################################################################################
#                                                                                         #
#                            Step 1: Loading original database                            #
#                                                                                         #
###########################################################################################

getwd() # To view the current working directory
setwd('C:/Users/Rafa/Desktop/CognitiveCounting') # Set working directory

# In this work two databases were used
pupil30 <- read.csv("pupila30.csv", header = TRUE, sep = "\t") # Read the database 1
pupil3 <- read.csv("pupila3.csv", header = TRUE, sep = "\t") # Read the database 2  
pupil = rbind.data.frame(pupil30,pupil3) #  Join the databases
write.csv(pupil,"pupila33.csv") # Record new database

###########################################################################################
#                                                                                         #
#             Step 2 - filter to extract only the signal of interest (base.0)             #
#                                                                                         #
###########################################################################################

base.0 <- pupil
# First it is evaluated which eye has greater signal loss
dim(base.0) #  = 5977680 5
length(base.0[!is.na(base.0$PupilLeft),][,3]) # = 5576167
length(base.0[!is.na(base.0$PupilRight),][,4]) # = 5572621

# In this case the PupilRight has greater loss, then it was only the PupilLeft  
base.0$PupilRight <- NULL # Remove PupilRigth to decrease processing
base.0$X <- NULL #Remove the X column that dos not use
base.0 <- base.0[!is.na(base.0$MediaName),] #Filters MediaName = NA
base.0 <- base.0[base.0$MediaName!="", ] #Filters MediaName = empty
base.0 <- base.0[base.0$MediaName!="6x6.jpg", ] #Filters Slide 6x6.jpg (slide test)
names(base.0)[1:1] <- c("RecordingName") # Rename the column because the load  changed the name
names(pupil)[1:1] <- c("RecordingName") # Rename the column because the load  changed the name

# Individual
rec <- c("Rec 02","Rec 03","Rec 04","Rec 05","Rec 06",
         "Rec 07","Rec 08","Rec 09","Rec 10","Rec 11",
         "Rec 12","Rec 13","Rec 14","Rec 15","Rec 16",
         "Rec 17","Rec 18","Rec 19","Rec 20","Rec 22", # Rec 21 was excluded
         "Rec 23","Rec 24","Rec 25","Rec 26","Rec 27",
         "Rec 28","Rec 29","Rec 30","Rec 31","Rec 32",
         "Rec 33","Rec 34","Rec 35")

# Test application sequence
slidet <- c("7x8a.JPG","4x5a.JPG","7x8b.JPG","12x13a.JPG","4x5b.JPG","7x12a.JPG",
            "7x8c.JPG","8x11a.JPG","4x5c.JPG","12x13b.JPG","7x12c.JPG","12x13c.JPG",
            "11x14a.JPG","8x11b.JPG","7x12b.JPG","8x11c.JPG","11x14b.JPG","11x14c.JPG")

# Sequence by quantity
slideq <- c("4x5a.JPG","4x5b.JPG","4x5c.JPG","7x8a.JPG","7x8b.JPG","7x8c.JPG",
            "7x12a.JPG","7x12b.JPG","7x12c.JPG","8x11a.JPG","8x11b.JPG","8x11c.JPG",
            "11x14a.JPG","11x14b.JPG","11x14c.JPG","12x13a.JPG","12x13b.JPG","12x13c.JPG")

# Sequence by pattern A / B / C
slidep <- c("4x5a.JPG","7x8a.JPG","7x12a.JPG","8x11a.JPG","11x14a.JPG","12x13a.JPG",
          "4x5b.JPG","7x8b.JPG","7x12b.JPG","8x11b.JPG","11x14b.JPG","12x13b.JPG",
          "4x5c.JPG","7x8c.JPG","7x12c.JPG","8x11c.JPG","11x14c.JPG","12x13c.JPG")

# Legend of the axis of the abscissa by quantity
ableg <- c("4x5a","4x5b","4x5c","7x8a","7x8b","7x8c","7x12a","7x12b","7x12c","8x11a",
         "8x11b","8x11c","11x14a","11x14b","11x14c","12x13a","12x13b","12x13c")


# Individuals in order according to Volk classification (without 33, 34, 35)
recvolk <- c("Rec 25","Rec 28","Rec 14","Rec 19","Rec 03",
             "Rec 02","Rec 30","Rec 27","Rec 24","Rec 29",
             "Rec 13","Rec 05","Rec 18","Rec 06","Rec 26",
             "Rec 22","Rec 23","Rec 20","Rec 07","Rec 16",
             "Rec 15","Rec 10","Rec 32","Rec 04","Rec 09",
             "Rec 12","Rec 11","Rec 31","Rec 17","Rec 08")

recvolk2 <- c("Rec 25","Rec 28","Rec 14","Rec 19","Rec 03", # 01 - 05
              "Rec 02","Rec 33","Rec 30","Rec 27","Rec 34", # 06 - 10
              "Rec 24","Rec 29","Rec 13","Rec 05","Rec 18", # 11 - 15
              "Rec 06","Rec 26","Rec 22","Rec 23","Rec 20", # 16 - 20
              "Rec 07","Rec 16","Rec 15","Rec 04","Rec 09", # 21 - 25
              "Rec 35","Rec 12","Rec 11","Rec 31","Rec 17") # 26 - 30

# Individuals in order according to quantity of hits
recquant <- c("Rec 06","Rec 03","Rec 14","Rec 24","Rec 28",
              "Rec 33","Rec 23","Rec 25","Rec 02","Rec 27",
              "Rec 29","Rec 34","Rec 13","Rec 18","Rec 22",
              "Rec 05","Rec 19","Rec 20","Rec 30","Rec 16",
              "Rec 26","Rec 07","Rec 09","Rec 15","Rec 04",
              "Rec 11","Rec 32","Rec 35","Rec 12","Rec 17") 

###########################################################################################
#                                                                                         #
#                                 SIGNAL LOSS ANALYSIS                                    #
#                                                                                         #
###########################################################################################

validsignal <- base.0[!is.na(base.0$PupilLeft),] #Filters PupilLeft = NA
signallength <- matrix(0,length(rec),3) #Matrix of signal length
index = 1
for (j in c(rec)){
  x1 <- base.0[base.0$RecordingName==j,] # Filters individual base.0
  x2 <- validsignal[validsignal$RecordingName==j,] # Filters individual original base
  signallength[index,1] = length(x1$PupilLeft) # Measures the length of the signal
  signallength[index,2] = length(x2$PupilLeft) # Measures the length of the original signal
  signallength[index,3] = length(x2$PupilLeft)/length(x1$PupilLeft) # Valid signal
  index = index + 1
}

###########################################################################################
#                                                                                         #
#                                 ANALYSIS OF VARIANCE                                    #
#                                                                                         #
###########################################################################################

# Function to calculate the variance per task
# Input 1: base<-("database")
# Input 2: RecordingName <- Column used to filter
# Input 3: MediaName <- Column used to filter
# Input 4: diameter <- Column used to filter
# Input 5: rec<-("set of indiduals")
# Input 6: slide<-("set of tasks")
# Output: List of variance per task
coef.var = function(base,RecordingName,MediaName,PupilLeft,rec,slide){
  base <- base[!is.na(base$PupilLeft),] # Remove NA
  lr <- length(rec) # Quantity of samples of the same individual
  ls <- length(slide) # Quantity of tasks
  list <- matrix(0,lr,ls) # Defines matrix according to set length
  r <- 1
  for (j in c(rec)){
    c <- 1
    for(i in c(slide)){
      x1 <- base[base$RecordingName==j,] # Filters individual.
      x2 <- x1[x1$MediaName==i,] # Filters task
      list[r,c] <- var(x2$PupilLeft) # Compute the variance of the pupillary diameter
      c <- c+1
    }
    r <- r+1
  }
  return(list) # Return a list of coefficients of variance per task
}

variance.task = coef.var(base.0,RecordingName,MediaName,PupilLeft,rec,slidet)
# Pre-view of the result
boxplot(variance.task)

# Teste of statistical significance
test.report <- t.test(variance.task[,1],variance.task[,2]) # Comparative (col1 x col2)
# Pre-view of the result (note if the p-value must be less than 0.05 )
test.report

###########################################################################################
#                                                                                         #
#                       FUNCTION TO EXTRACT INTERVAL OF INTEREST                          #
#                                                                                         #
###########################################################################################
#      # This step also serves to assemble the matrix of individuals (rows)               #
# Note # by variables (columns), so if the number of samples in the database is           #
#      # the same for all individuals, simply indicate the number of samples.             #
###########################################################################################
# Input 1: base<-("database")
# Input 2: RecordingName <- Column used to filter
# Input 3: MediaName <- Column used to filter
# Input 4: PupilLeft <- Column used to filter
# Input 5: rec<-c("set of individual")
# Input 6: slide<-c("set of slides")
# Input 7: window.cut <-("quantity of samples before decision making")
# Input 8: position <- receive the position of the cut - "pre" or "end" 
#          (write end if you want to observe the decision-making process)
# Output: clipping of database with same size signal
f.window.cut = function(base,RecordingName,MediaName,PupilLeft,rec,slide,window.cut,position){
  lr <- length(rec) 
  lc <- length(slide) 
  list <- matrix(0,lr,(lc*window.cut)) # Sets the size of the clipping matrix
  r <- 1
  for (j in c(rec)){
    d <- 0
    for(i in c(slide)){
      x1 <- base[base$RecordingName==j & base$MediaName==i,] # Filters signal base.1
      s2 <- length(x1$PupilLeft)
      if(position=="pre"){
        if(s2 < window.cut){
          window.seg <- x1$PupilLeft[ (1:s2)]
          for(k in c(1:s2)){
            list[r,d+k] <- window.seg[k]
          }
        }
        else{
          window.seg <- x1$PupilLeft[ (1:window.cut)]
          for(k in c(1:window.cut)){
            list[r,d+k] <- window.seg[k]
          }
        }
      }
      if(position=="end"){
        if(s2 < window.cut){
          s1 <- window.cut-s2+1
          window.seg <- matrix(NA,1,length(window.cut))
          window.seg[s1:window.cut] <- x1$PupilLeft[ (1:s2)]
          for(k in c(1:window.cut)){
            list[r,d+k] <- window.seg[k]
          }
        }
        else{
          s1 <- s2-window.cut+1
          window.seg <- x1$PupilLeft[ (s1:s2)]
          for(k in c(1:window.cut)){
            list[r,d+k] <- window.seg[k]
          }
        }
      }
      d <- d+window.cut
    }
    r <- r+1
  }
  return(list)
}

# base.1 - data matrix with clipping of interest
base.1 <- f.window.cut(base.0,RecordingName,MediaName,PupilLeft,recvolk,slidet,600,"pre") 

###########################################################################################
#                                                                                         #
#                                Step 3 - Pre-processing                                  #
#                                                                                         #
###########################################################################################

###########################################################################################
#                                                                                         #
#                               REMOVE OUTILIER (base.2)                                  #
#                                                                                         #
###########################################################################################
 
base.2 = base.1
factor = 0.03
base.2[is.na(base.2)] <- 0 # Replace NA to 0
lr <- nrow(base.2)
lc <- ncol(base.2)
for(j in 1:lr){
  for(i in 2:lc-1){
    if(abs(base.2[j,i] - base.2[j,i+1]) > factor){
      base.2[j,i] = 0
    }
  }
  for(k in lc:2){
    if(abs(base.2[j,i] - base.2[j,i+1]) > factor){
      base.2[j,i+1] = 0
    }
  }
  # for(t in 1:lc){
  #   if(base.1[j,t]==0){
  #     base.1[j,t]<-NA
  #   }
  # }
}
# Pre-view of the result
plot(base.2[1,],type="l")

###########################################################################################
#                                                                                         #
#   RECONSTRUCTION OF THE SIGNAL LOSS INTERVAL BY DATA LINEAR INTERPOLATION (base.3)      #
#                                                                                         #
###########################################################################################

base.3 = base.2
for(t in 1:lr){
  if(base.3[t,1]==0){
    base.3[t,1] <- mean(base.3[t,])
  }
  if(base.3[t,length(base.3[t,])]==0){
    base.3[t,length(base.3[t,])] <- mean(base.3[t,])
  }
  for(i in 1:length(base.3[t,])){
    if(base.3[t,i]== 0){
      va = base.3[t,i-1]
      pa = i-1
      pi = i
      for(k in i:length(base.3[t,])){
        if(base.3[t,k]!=0){
          vp = base.3[t,k]
          pp = k
          while(i<k){
            base.3[t,i] = va*(1-(i-pa)/(pp-pa))+vp*((1-(pp-i)/(pp-pa)))
            i = i+1
          }
          break
        }
      }
    }
  }
}
# Pre-view of the result
lines(base.3[1,],type="l", col = "red")

###########################################################################################
#                                                                                         #
#                     SMOOTHING FILTER TO REMOVE NOISE (base.4)                           #
#                                                                                         #
###########################################################################################

base.4 = base.3
# Smoothing filter
for(i in 1:lr){
  x = smooth.spline(base.4[i,])
  base.4[i,]<-x$y
}
# Pre-view of the result
# Signal before
plot(base.3[1,1:400],type="l") # Zoom to see detail (base.3)
# Signal after
lines(base.4[1,1:400],type="l",col="red") # Signal Smoothed (base.4)


###########################################################################################
#                                                                                         #
#                   PCA - PRINCIPAL COMPONENT ANALYSIS (base.5)                           #
#                                                                                         #
###########################################################################################

# Input 1: base<-("database")
# Input 2: N <- Number of principal components (to use with MLDA the N must be iqual the n-1)
# Output: Matrix with reduced dimensionality
PCA <- function(base,N){
  res.pca <- prcomp(base, scale = TRUE) # Computes the PCA
  basepca <- base%*%res.pca$rotation[,1:N] # projection of the PCA in the database
  return(basepca)
}

base.5 <- PCA(base.4,29)

###########################################################################################
#                                                                                         #
#                   MLDA - MAXIMUM LINEAR DISCRIMINANT ANALYSIS (base.6)                  #
#                                                                                         #
#                                  Two Classes                                            #
#                                                                                         #
###########################################################################################

# Input 1: x<-("database")
# Input 2: LABEL <- label of the classes
# Output: MLDA report
MLDA <- function(x,LABEL) {
  x <- t(x) # x is the predictors matrix (Nsamples x Nfeatures) 
  INDEX <- sort(as.numeric(LABEL),index.return=T)$ix
  x <- x[,INDEX]
  LABEL <- LABEL[INDEX]
  
  x1 <- x[,1:as.numeric(table(LABEL)[1])]
  x2 <- x[,(as.numeric(table(LABEL)[1])+1):(as.numeric(table(LABEL)[1])+as.numeric(table(LABEL)[2]))]
  
  numGenes <- nrow(x1)
  numSample1 <- ncol(x1)
  numSample2 <- ncol(x2)
  
  xMean <- matrix(0,numGenes,1)
  x1Mean <- matrix(0,numGenes,1)
  x2Mean <- matrix(0,numGenes,1)
  
  x <- matrix(0,numGenes,numSample1+numSample2)
  x[,1:numSample1] <- x1
  x[,(numSample1+1):(numSample1+numSample2)] <- x2
  
  #   for(i in 1:numGenes) {
  #       x[i,] <- x[i,] - mean(x[i,])
  #   }
  
  x1 <- x[,1:numSample1]   
  x2 <- x[,(numSample1+1):(numSample1+numSample2)]   
  
  for (i in 1:numGenes) {
    x1Mean[i,1] <- mean(x1[i,])
    x2Mean[i,1] <- mean(x2[i,])
    xMean[i,1] <- (sum(x1[i,]) + sum(x2[i,]))/(numSample1+numSample2)
  }
  
  Sb <- ( numSample1 * (x1Mean[,1] - xMean[,1]) %*% t((x1Mean[,1] - xMean[,1]))) + ( numSample2 * (x2Mean[,1] - xMean[,1]) %*% t((x2Mean[,1] - xMean[,1])))
  
  Sw <- matrix(0, numGenes, numGenes)
  
  for (i in 1:numSample1) {
    Sw <- Sw + (x1[,i] - x1Mean[,1]) %*% (t(x1[,i] - x1Mean[,1]))
  }
  
  for (i in 1:numSample2) {
    Sw <- Sw + (x2[,i] - x2Mean[,1]) %*% (t(x2[,i] - x2Mean[,1]))
  }
  
  Sp <- Sw/(numSample1+numSample2-2)
  
  Eigen <- eigen(Sp) 
  
  lambdaMean <- sum(diag(Sp))/numGenes
  
  SpEigenValueNew <- matrix(0,numGenes,numGenes)
  Eigen$values=Re(Eigen$values)
  Eigen$vectors=Re(Eigen$vectors)
  
  for(i in 1:numGenes) {
    SpEigenValueNew[i,i] <- max(Eigen$values[i], lambdaMean)
  }
  
  SwNew <- (Eigen$vectors %*% SpEigenValueNew %*% t(Eigen$vectors)) * (numSample1+numSample2-2)
  
  SwNew_1 <- qr.solve(SwNew)
  Z=eigen(SwNew_1%*%Sb)
  pMLDA <- Z$vectors
  
  x1Trans <- pMLDA[,1]%*%x1
  x2Trans <- pMLDA[,1]%*%x2
  
  meanx1Trans <- mean(x1Trans)
  meanx2Trans <- mean(x2Trans)
  #sdx1Trans <- sd(as.real(x1Trans)) #AS.REAL É FUNÇÃO OBSOLETA
  #sdx2Trans <- sd(as.real(x2Trans)) #AS.REAL É FUNÇÃO OBSOLETA
  sdx1Trans <- sd(as.double(x1Trans)) #SOLUÇÃO PARA CORRIGIR FUNÇÃO OBSOLETA ACIMA
  sdx2Trans <- sd(as.double(x2Trans))
  
  #    pMLDA[,1]=pMLDA[,1]*sign(as.real(meanx1Trans))
  #    x1Trans <- pMLDA[,1]%*%x1
  #    x2Trans <- pMLDA[,1]%*%x2
  #    meanx1Trans <- mean(x1Trans)
  #    meanx2Trans <- mean(x2Trans)
  
  class <- list()
  class$mlda <- pMLDA[,1]
  class$m1=meanx1Trans
  class$m2=meanx2Trans
  class$sd1=sdx1Trans
  class$sd2=sdx2Trans
  class$project1=x1Trans
  class$project2=x2Trans
  
  return(class)
}

# Input 1: MODEL<-("result of the step MLDA")
# Input 2: x <- ("reduced database")
# Output: Eigenvector discriminant
predict.MLDA <- function(MODEL,x){
  x <- t(x) #x is a matrix not an array (Nsamples x Npredictors)
  xTrans <- MODEL$mlda%*%x # Projection of the MLDA in the database
  numSample <- length(xTrans)
  label <- array(0,numSample)
  
  for (i in 1:numSample) {
    if(abs(xTrans[i]-MODEL$m1) < abs(xTrans[i]-MODEL$m2)){ #EUCLIDIAN
      label[i] <- 1
    }
    else {
      label[i] <- 2
    }
  }
  return(label)
}

# Input 1: class1 <- number of the individuals of the class 1
# Input 2: class2 <- number of the individuals of the class 2
# Output: Matrix with labels of each individual
flabel <- function(class1,class2){
  label <- matrix(0,class1+class2,1)
  for(i in 1:class1){
    label[i] <- 1
  }
  for(i in (class1+1):(class1+class2)){
    label[i] <- 2
  }
  return(label)
}

label <- flabel(15,15)

result.mlda = MLDA(base.5,label) # Compute the MLDA

classifier = predict.MLDA(result.mlda,base.5) 

ac = 0 
for(i in 1:nrow(label)){
  if(label[i]==classifier[i]){
    ac = ac + 1
  }
}
ac/nrow(label)
accuracy = ac/nrow(label)

###########################################################################################
#                                                                                         #
#                                     REPORT OF RESULTS                                   #
#                                                                                         #
###########################################################################################

pdf("Variance per task.pdf",width = 5, height = 3.5)
par(mfrow=c(1,1),mar=c(4,4,1,1),bty="l",cex.axis=0.4,cex.lab=0.7,cex.main=0.7)
boxplot(x = as.list(as.data.frame(variance.task)),ylim=c(0,.22),xaxt = "n",
        main="Variance per task", 
        xlab="Tasks", ylab="Variance of the pupil diameter (cm)")
axis(1,at=1:18,labels=c(1:18))
dev.off()

variance.task

# CONTINUE
