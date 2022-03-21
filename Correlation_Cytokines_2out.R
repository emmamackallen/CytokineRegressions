## Ly Nguyen 
## Illinois Institute of Technology 
## ME/CFS Prediction Using Cytokines Data
## Stage 1 (N = 105) , Stage 2 (N = 121), Stage 3 (N = 115)
## 2 outcomes ( control and ME/CFS)


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
data_Co <- read_excel("Downloads/stage_2_cytokines_Co.xlsx")
#data_Co <- read_excel("Downloads/stage_3_cytokines_Co.xlsx")

attach(data_Co) 


# Log10(x) transformation

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


# --------------------------------

## Experiment starts from here

## Separating Dx levels Stage 1

control <- subset(data_Co, Dx_2_outcomes == 0 , select = c(6:21)) # Selecting only cytokines data from control group
control_size <- nrow(control)

cfs<- subset(data_Co, Dx_2_outcomes == 1 , select = c(6:21)) # Selecting only cytokines data from cfs group
cfs_size <- nrow(cfs)

## Crearting data.frame

testing_data <- subset(data_Co, select = c(1,4,6:21)) # Dx_2_outcomes and Cytokines

# Creating a new data.frame for new control group ( new control group is the one that being added patients)
newdata_control <- data_frame() # Assign a data frame for newdata_control, this newdata control will contain the control group plus added patients
columns= colnames(testing_data) # Get the column names
newdata_control = data.frame(matrix(nrow = 0, ncol = length(columns))) # Adding columns name  
colnames(newdata_control) = columns
print(newdata_control)

# Creating a new data.frame for new cfs group ( new cfs group is the one that being added patients)
newdata_cfs <- data_frame() # Assign a data frame for newdata_cfs, this newdata cfs will contain the cfs group plus added patients
columns= colnames(testing_data) # Get the column names
newdata_cfs = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(newdata_cfs) = columns
print(newdata_cfs)

 
# New Mission
# For Loop 


convec = c() # Create a vector for control group
cfsvec = c() # Create a vector for cfs group
accuracy_final = c() # Create a vector for accuracy of j times

for (k in 1:20){ 
  counter = 1
  #i = 0
  correctness = 0
  true_control = 0 # True Negative
  false_control = 0 # False Negative
  true_cfs = 0 # True Positive 
  false_cfs = 0 # False Positive
  
for (i in 1:nrow(testing_data)){ 
  
  newdata_control<- control # Duplicate control group into newdata control
  newdata_cfs<- cfs # Duplicate cfs group into newdata cfs
  
  for (j in 1:k){ # For j loop

  newdata_control <-add_row(newdata_control, testing_data[i,-c(1:2)]) ## Adding patient one at a time to control group from stage 1 data
  newdata_cfs <- add_row(newdata_cfs, testing_data[i,-c(1:2)]) ## Adding patient one at a time to cfs group from stage 1 data

  # Print out 1 patient chi2_control, chi2_cfs 
  if (testing_data[i,'participant_id'] == "MG71177" && k == 20) { # looking at one specific patient to see how their chi square values change as j times increase
    
    cor_control_id <- cor(newdata_control, method = c("kendall")) # Compute added_control correlation matrix
    cor_controlorg_id <- cor(control,method = c("kendall")) # Compute original control  correlation matrix
    jenn_test_control_id <-cortest.jennrich(cor_control_id,cor_controlorg_id, n1=(control_size + i), n2=control_size) # Perform Jennrich test to get Chi2 values
    
    
    cor_cfs_id <- cor(newdata_cfs,method = c("kendall")) # Compute added_cfs correlation matrix
    cor_cfsorg_id <- cor(cfs,method = c("kendall")) # Compute original control  correlation matrix
    jenn_test_cfs_id <- cortest.jennrich(cor_cfs_id,cor_cfsorg_id, n1=(cfs_size + i), n2 =cfs_size) # Perform Jennrich test to get Chi2 values
    
    control_chi2_id <- jenn_test_control_id$chi2 # Get the chi square of control
    cfs_chi2_id <- jenn_test_cfs_id$chi2 # Get the chi square of cfs
    
    convec <- append(convec,control_chi2_id) # attach the chi square values of control to convec
    cfsvec <- append(cfsvec,cfs_chi2_id) # attach the chi square values of cfs to cfsvec
    
  }
  # Plot the j times vs chi2 values
  # Calculate the accuracy from 1:j times 
  
   } #for j loop
  
  cor_control <- cor(newdata_control, method = c("kendall")) # Compute added_control correlation matrix
  cor_controlorg <- cor(control,method = c("kendall")) # Compute original control  correlation matrix
  jenn_test_control <-cortest.jennrich(cor_control,cor_controlorg, n1=(control_size + i), n2=control_size) # Perform Jennrich test to get Chi2 values
  
  
  cor_cfs <- cor(newdata_cfs,method = c("kendall")) # Compute added_cfs correlation matrix
  cor_cfsorg <- cor(cfs,method = c("kendall")) # Compute original control  correlation matrix
  jenn_test_cfs <- cortest.jennrich(cor_cfs,cor_cfsorg, n1=(cfs_size + i), n2 =cfs_size) # Perform Jennrich test to get Chi2 values
  
  
  print(paste0("Chi2_Control: ",format(round(jenn_test_control$chi2, 4), nsmall = 2))) # Print chi2 values of original control and added_control group 
  print(paste0("Chi2_CFS: ",format(round(jenn_test_cfs$chi2, 4), nsmall = 2))) # Print chi2 values of original cfs and added_cfs group
  print(paste0("Actual_Dx: ",testing_data$Dx_2_outcomes[i])) # print the Dx level to compare
  print(paste0("Patient #: ",i) ) # print the patient#
  print(counter) # For j times
  
  ## If else statment to calculate accuracy 
  # IF Chi2_control <  Chi2_cfs && Patient = 0  =>>> TRUE
  # IF Chi2_cfs < Chi2_control && Patient = 1  =>>> TRUE
  # Else =>>>> FALSE 
  # Accuracy = Number of TRUE / Number of Patient 
  
  if ((jenn_test_control$chi2 <= jenn_test_cfs$chi2) && (testing_data$Dx_2_outcomes[i] == 0)) {
    print("Correctly Classified")
    correctness = correctness + 1
    true_control = true_control + 1 # True Negative
  }
  else if ( (jenn_test_cfs$chi2 <= jenn_test_control$chi2) && (testing_data$Dx_2_outcomes[i] == 1)){
    print("Correctly Classified")
    correctness = correctness + 1
    true_cfs = true_cfs + 1 # True positive
  }
  else {
    if ((jenn_test_control$chi2 <= jenn_test_cfs$chi2) && (testing_data$Dx_2_outcomes[i] == 1 )){
      false_cfs = false_cfs + 1 # False Negative
    }
    else if ((jenn_test_cfs$chi2 <= jenn_test_control$chi2) && (testing_data$Dx_2_outcomes[i] == 0)){
      false_control = false_control + 1 # False Positive
    }
    print("INCorrectly Classified")
  }
  
  print('---------')
  counter = counter+1 # For j loop
} # for i loop
  accurracy = (correctness / (counter)) * 100 # For j times 
  accuracy_final <- c(accuracy_final,accurracy) # For the list of accuracy when j times added
} # for k loop

#accurracy = (correctness / nrow(testing_data)) * 100

#### For T-test loop, comparing the accuracy of Stage 2 and Stage 3 to see if they are significant different, run each stage separately 
#accuracy_final_S3 = accuracy_final 
#accuracy_final_S2 = accuracy_final
#t_test <- t.test(accuracy_final_S3,accuracy_final_S2)



# This plot is for j-loop

par(mfrow=c(1,2)) 
# Plot selected patient Chi square values within the interval of j times
plot(1:k,convec,
     main="Participant Plot",
     ylab="Chi Square Values",
     xlab="Adding one participant j times",
     type="l",
     col="blue")
lines(1:k,cfsvec, col="red")
legend("topleft",
       c("CONTROL","CFS"),
       fill=c("blue","red")
)
 plot (1:k,accuracy_final,
      main="Accuracy Plot (S1, kendall)",
      type ='l',
      ylab="Accuracy Rate",
      xlab="Index",
      col="black")

sensitivity = (true_cfs)/cfs_size
specificity = (true_control)/control_size












