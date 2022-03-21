## Ly Nguyen 
## Illinois Institute of Technology 
## ME/CFS Prediction Using Cytokines Data
## Stage 1 (N = 105) , Stage 2 (N = 121), Stage 3 (N = 115)
## 3 outcomes ( Control, M-ME/CFS, S-ME/CFS)

library(tidyverse) #for filter, add_row
library(rstatix) #for cor_mat, cor.test, cor
library(psych) #for Jennrich test
library(corrplot) # for Correlation Matrices Plot
library(PerformanceAnalytics) # For ggplot for Random Forest ( in this experiment, it can be deleted)
library(plyr) # For splitting data
library(dplyr) # For manipulating the data set
library(readxl) # for excel file import
library(stats) # for T - test 

# Importing the data  (Stage 1,2, and 3), remove the "#" when running the chosen stage

#data_Co <- read_excel("Downloads/stage_1_cytokines_Co.xlsx") 
#data_Co <- read_excel("Downloads/stage_2_cytokines_Co.xlsx")
#data_Co <- read_excel("Downloads/stage_3_cytokines_Co.xlsx") 

attach(data_Co)

# Log base 10 transformation

data_Co_trans <- log10(data_Co[,c(6:21)]) # Only Cytokines

Dx_extract <- data_Co[,-c(6:21)] # Extract the rest of data except cytokines

merge_data <- add_column(Dx_extract, data_Co_trans) # Merging back all columns, with transformed cytokines

data_Co <- merge_data

attach(data_Co)

## Log x+1 Transformation

data_Co_trans <- log1p(data_Co[,c(6:21)]) # Only Cytokines

Dx_extract <- data_Co[,-c(6:21)] # Extract the rest of data except cytokines

merge_data <- add_column(Dx_extract, data_Co_trans) # Merging back all columns, with transformed cytokines

data_Co <- merge_data

attach(data_Co)


# ---------------------------------------

## Separating Dx levels Stage 1

control <- subset(data_Co, Dx_3_outcomes == 0 , select = c(6:21))  # Selecting only cytokines data from control group
control_size <- nrow(control)

cfs.m <- subset(data_Co, Dx_3_outcomes == 1 , select = c(6:21))  # Selecting only cytokines data from m-cfs group
cfs_m_size <- nrow(cfs.m)

cfs.s <- subset(data_Co, Dx_3_outcomes == 2 , select = c(6:21))  # Selecting only cytokines data from s-cfs group
cfs_s_size <- nrow(cfs.s)

testing_data <- subset(data_Co, select = c(1,5:21)) # testing data is the one that used to add patients, contain Dx_3_outcomes level and cytokines

## Creates a data.frame for control, this is a blank table that have the same column names
newdata_control <- data_frame()
columns= colnames(testing_data)
newdata_control = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(newdata_control) = columns
print(newdata_control)

## Creates a data.frame for cfs_m, this is a blank table that have the same column names
newdata_cfs.m <- data_frame()
columns= colnames(testing_data)
newdata_cfs.m = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(newdata_cfs.m) = columns
print(newdata_cfs.m)

## Creates a data.frame for cfs_s, this is a blank table that have the same column names
newdata_cfs.s <- data.frame()
columns = colnames(testing_data)
newdata_cfs.s = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(newdata_cfs.s) = columns
print(newdata_cfs.s)


## New Mission, New Experiement 

convec = c()
cfs.m.vec = c()
cfs.s.vec = c()
accuracy_final = c()


for (k in 1:10){
  counter = 1
  correctness = 0
  true_control = 0 # True Negative
  false_control = 0 # False Negative
  true_cfs = 0 # True Positive 
  false_cfs = 0 # False Positive
 # for k loop
for (i in 1:nrow(testing_data)){ 
  
  newdata_control<- control
  newdata_cfs.m<- cfs.m
  newdata_cfs.s<- cfs.s
  
  for (j in 1:k){ # For j loop
    
  newdata_control <-add_row(newdata_control, testing_data[i,-c(1:2)]) ## Adding patient one at a time to control group from stage 1 data
  newdata_cfs.m <- add_row(newdata_cfs.m, testing_data[i,-c(1:2)]) ## Adding patient one at a time to cfs_m group from stage 1 data
  newdata_cfs.s <- add_row(newdata_cfs.s, testing_data[i,-c(1:2)]) ## Adding patient one at a time to cfs_s group from stage 1 data

  
  if (testing_data[i,'participant_id'] == "MG2567" && k == 10) {
    
    cor_control_id <- cor(newdata_control, method = c("spearman")) # Compute added_control correlation matrix
    cor_controlorg_id <- cor(control, method = c("spearman")) # Compute original control  correlation matrix
    jenn_test_control_id <-cortest.jennrich(cor_control_id,cor_controlorg_id, n1=control_size + i , n2= control_size) # Perform Jennrich test to get Chi2 values
    
    cor_cfs_m_id <- cor(newdata_cfs.m, method = c("spearman")) # Compute added_cfs correlation matrix
    cor_cfsorg_m_id <- cor(cfs.m, method = c("spearman")) # Compute original control  correlation matrix
    jenn_test_cfs_m_id <- cortest.jennrich(cor_cfs_m_id,cor_cfsorg_m_id, n1= cfs_m_size + i , n2 = cfs_m_size) # Perform Jennrich test to get Chi2 values
    
    cor_cfs_s_id <- cor(newdata_cfs.s, method = c("spearman")) # Compute added_cfs correlation matrix
    cor_cfsorg_s_id <- cor(cfs.s, method = c("spearman")) # Compute original control  correlation matrix
    jenn_test_cfs_s_id <- cortest.jennrich(cor_cfs_s_id,cor_cfsorg_s_id, n1= cfs_s_size + i, n2 = cfs_s_size) # Perform Jennrich test to get Chi2 values
    
    
    control_chi2_id <- jenn_test_control_id$chi2
    cfs_m_chi2_id <- jenn_test_cfs_m_id$chi2
    cfs_s_chi2_id <- jenn_test_cfs_s_id$chi2
    
    
    convec <- append(convec,control_chi2_id)
    cfs.m.vec <- append(cfs.m.vec,cfs_m_chi2_id)
    cfs.a.vec <- append(cfs.s.vec,cfs_s_chi2_id)
    
    
  } # for if statement 
  
  } # for j loop
  
  cor_control <- cor(newdata_control, method = c("spearman")) # Compute added_control correlation matrix
  cor_controlorg <- cor(control, method = c("spearman")) # Compute original control  correlation matrix
  jenn_test_control <-cortest.jennrich(cor_control,cor_controlorg, n1=control_size + i , n2=control_size) # Perform Jennrich test to get Chi2 values
  
  cor_cfs_m <- cor(newdata_cfs.m,method = c("spearman")) # Compute added_cfs correlation matrix
  cor_cfsorg_m <- cor(cfs.m,method = c("spearman")) # Compute original control  correlation matrix
  jenn_test_cfs_m <- cortest.jennrich(cor_cfs_m,cor_cfsorg_m, n1=cfs_m_size + i, n2 = cfs_m_size) # Perform Jennrich test to get Chi2 values
  
  cor_cfs_s <- cor(newdata_cfs.s,method = c("spearman")) # Compute added_cfs correlation matrix
  cor_cfsorg_s <- cor(cfs.s,method = c("spearman")) # Compute original control  correlation matrix
  jenn_test_cfs_s <- cortest.jennrich(cor_cfs_s,cor_cfsorg_s, n1=cfs_s_size + i , n2 =cfs_s_size) # Perform Jennrich test to get Chi2 values
  
  # Print results
  print(paste0("Chi2_Control: ",format(round(jenn_test_control$chi2, 4), nsmall = 2))) # Print chi2 values of original control and added_control group 
  print(paste0("Chi2_CFS_m: ",format(round(jenn_test_cfs_m$chi2, 4), nsmall = 2))) # Print chi2 values of original cfs and added_cfs group
  print(paste0("Chi2_CFS_s: ",format(round(jenn_test_cfs_s$chi2, 4), nsmall = 2))) # Print chi2 values of original cfs and added_cfs group
  print(paste0("Actual_Dx: ",testing_data$Dx_3_outcomes[i])) # print the Dx level to compare
  
  print(paste0("Patient #: ",i) ) # print the patient#
  
  ## If else statment to calculate accuracy 
  
  # IF Chi2_control <  Chi2_cfs_m && Chi2_control<Chi2_cfs_s %% Patient = 0  =>>> TRUE
  # IF Chi2_cfs_m < Chi2_control && Chi2_cfs_m < Chi2_cfs_s %% Patient = 1  =>>> TRUE
  # IF Chi2_cfs_s < Chi2_control && Chi2_cfs_s < Chi2_cfs_m %% Patient = 2  =>>> TRUE
  # Else =>>>> FALSE 
  # Accuracy = Number of TRUE / Number of Patient 
  
  if (jenn_test_control$chi2 <= jenn_test_cfs_m$chi2 && jenn_test_control$chi2 <= jenn_test_cfs_s$chi2 && testing_data$Dx_3_outcomes[i] == 0) {
    print("Matched")
    correctness = correctness + 1
  }
  else if ( jenn_test_cfs_m$chi2 <= jenn_test_control$chi2 && jenn_test_cfs_m$chi2 <= jenn_test_cfs_s$chi2   && testing_data$Dx_3_outcomes[i] == 1){
    print("Matched")
    correctness = correctness + 1
  }
  else if ( jenn_test_cfs_s$chi2 <= jenn_test_control$chi2 && jenn_test_cfs_s$chi2 <= jenn_test_cfs_m$chi2   && testing_data$Dx_3_outcomes[i] == 2){
    print("Matched")
    correctness = correctness + 1
  }
     else {
    print("Not Matched")
  }
  
  print('---------')
  counter = counter+1 # For j loop
} # for i loop
  accurracy = (correctness / (counter)) * 100 # For j times
  accuracy_final <- c(accuracy_final,accurracy)
  
} # for k loop
accuracy_final

# This plot is for j-loop
par(mfrow=c(1,2)) 
# Plotting a slected patient's chi square values
plot(1:k,convec,
     main="Patient Plot",
     ylab="Chi Square Values",
     xlab="Index",
     type="l",
     col="blue")
lines(1:k,cfs.m.vec, col="orange")
lines(1:k,cfs.s.vec, col="red")
legend("bottomright",
       c("CONTROL","CFS_m","CFS_s"),
       fill=c("blue","orange", "red")
)
# Plot the accuracy list over j times.
plot (1:k,accuracy_final,
      main="Accuracy Plot (S3, kendall)",
      type ='l',
      ylab="Accuracy Rate",
      xlab="Index",
      col="black")







