
library(readxl)
AF3 <- read_excel("C:/Users/Hult/OneDrive - Hult Students/Escritorio/Hult MBA/R/Air France Case Spreadsheet Supplement_2.xls", 
                                                       sheet = "DoubleClick (2)")
View(AF3)


# Renaming each variable for easy access for double_click dataset
colnames(AF3) <- c("publisher_id","publisher_name","keyword_id", 
                     "keyword","match_type", "campaign", "keyword_group","category",
                     "bid_strategy", "keyword_type", "status", "SEB",
                     "clicks","clicks_charger","avg_cpc", "impressions",
                     "Engine_Click_P", "Avg_Pos", "Trans_Conv_P","cost_per_trans",
                     "amount",
                     "total_cost", "total_boking", "bid_str", 
                     "net_revenue", "ROA",
                     "conversion_rate", "binary_booking", "campaign_bin",
                     "match_type_bin", "status_bin", "publisher_name_bin")


# Converting the keyword ID from character to numeric
AF3$`keyword_id` <- as.numeric(AF3$`keyword_id`)

#Checking the data for null values
anyNA(AF3) #The data does have some null values

#We need to explore the NAs values further
map(AF3, ~sum(is.na(.))) #counting null values for each variables

#making new subset of rows with NAs values #
#'*(WHY ARE WE SUBSETTING)*
#my_af_na <- my_af[rowSums(is.na(my_af)) == 1,] 

#Creating a for loop to get unique values in  all variables
for(i in 1:ncol(AF3)){
  print(unique(AF3[,i]))
} 
#After carefully observing the console we can see that there are a 
#    few N/As under "Match Type" & "Bid Strategy"

#replacing N/A character values for comprehensibility
AF3$`match_type` <- replace(AF3$`match_type`,
                              which(AF3$`match_type`=="N/A"),"Unmatched")

# Checking to confirm if the N/As got replaced with unmatched
unique(AF3$`match_type`) 

#replacing N/A character values for comprehensibility
AF3$`bid_strategy` <- replace(AF3$`bid_strategy`,
                                which(is.na(AF3$`bid_strategy`)),"Unpositioned")

#Creating a whole new variable and assigning a new name  
AF3$bid_str <- AF3$`bid_strategy`
AF3$bid_str<-gsub("Postiion 1-4 Bid Strategy","Position 1-4 Bid Strategy",AF3$bid_str)
AF3$bid_str<-gsub("Position 1 -2 Target","Position 1-2 Target",AF3$bid_str)

#Creating a revenue variable
AF3$net_revenue <- AF3$amount - AF3$`total_cost`

#Creating a Return on Advertisement Variable
AF3$ROA <- AF3$amount/ AF3$`total_cost`

#Creating a conversion rate variable 
AF3$conversion_rate <- AF3$`Trans_Conv_P/ AF3$clicks





# regressions
colnames(AF3)

train_index <- sample(1:nrow(AF3), size = 0.8*nrow(AF3))

train_data <- AF3[train_index, ]
test_data <- AF3[-train_index, ]
#linear complete
linear_r <- lm(ROA ~ amount + SEB + clicks + clicks_charger + avg_cpc + 
                 impressions + Engine_Click_P + Avg_Pos + Trans_Conv_P + cost_per_trans + 
                 total_cost + total_boking + bid_str , data = train_data)
summary(linear_r)
### dropping the variables
linear_r_2 <- lm(ROA ~ amount  + clicks  + avg_cpc + Avg_Pos + Trans_Conv_P + total_boking  , data = train_data)
summary(linear_r_2)

#normalize the variables 


normalize <- function(x){
  percent <- (x-min(x))/(max(x)-min(x))
  return(percent)
}

ROA_norm <- normalize(AF3$ROA)
Amount_norm <- normalize(AF3$amount)
clicks_norm <- normalize(AF3$clicks)
avg_cpc_norm <- normalize(AF3$avg_cpc)
Avg_Pos_norm <- normalize(AF3$Avg_Pos)
Trans_Conv_P_norm <- normalize(AF3$Trans_Conv_P)
total_boking_P_norm <- normalize(AF3$total_boking)

#normalized regression

linear_norm <- lm(ROA ~ Amount_norm + clicks_norm + avg_cpc_norm + Avg_Pos_norm +
                    Trans_Conv_P_norm + total_boking_P_norm, data=train_data )
summary(linear_norm)
++++++++++++++++++


# builiding logistic regression with net revenue as binary variable #########################################


# convert roa in binary with median

mean(AF3$ROA)
print(ROA_median)
type(AF3$ROA)
AF3$ROA <- as.numeric(AF3$ROA)
ROA_Mean <- mean(AF3$ROA)
summary(AF3$ROA)

# creating new variables
AF3$Net_revenue <- AF3$amount - AF3$total_cost
AF3$ROA <- AF3$amount / AF3$total_cost
AF3$keyword_binary <- AF3$keyword_group

# repalcing in keyword group
AF3$keyword_binary <- gsub("Unassigned", "0", AF3$keyword_group)

for (i in 1:nrow(AF3)){
  if (AF3$keyword_binary[i] != "0"){
    AF3$keyword_binary[i] <- 1
  }
}

AF3$keyword_binary <- as.numeric(AF3$keyword_binary)

# labeling as missing value all the infinite observations
is.na(AF3) <- sapply(AF3, is.infinite)

# replacing all the missing values with zero

AF3$ROA[is.na(AF3$ROA)] <- 0
AF3$Net_revenue[is.na(AF3$Net_revenue)] <- 0

# defining the new variable 'binary'
# which will be used as dependent variable in the logistic regression
AF3[, 'binary'] <- NA

# defining the binary variable for the logistic regression
for (i in 1:nrow(AF3)){
  if(AF3$Net_revenue[i] > 0){
    AF3$binary[i] <- 1
  } else {
    AF3$binary[i] <- 0
  }
}

colnames(AF3)

train_index <- sample(1:nrow(AF3), size = 0.8*nrow(AF3))

train_data <- AF3[train_index, ]
test_data <- AF3[-train_index, ]

# building the regresion 

my_logit_rev <- glm(binary ~  keyword_binary +  SEB + clicks  +  clicks_charger + avg_cpc + Engine_Click_P + 
                      impressions + Avg_Pos + Trans_Conv_P + cost_per_trans  + total_boking, data = train_data, family = "binomial")
summary(my_logit_rev)

colnames(AF3)

# droping the variables not relevant

my_logit_rev_2 <- glm(binary ~  + SEB +  clicks_charger  + Trans_Conv_P + cost_per_trans  + total_boking, data = train_data, family = "binomial")
summary(my_logit_rev_2)
# normalize the data

normalize <- function(x){
  percent <- (x-min(x))/(max(x)-min(x))
  return(percent)
}

amount_norm <- normalize(AF3$amount)
clicks_norm <- normalize(AF3$clicks)
clicks_charge_nomr <- normalize(AF3$clicks_charger)
avg_cpc_norm <- normalize(AF3$avg_cpc)
impressions_norm <- normalize(AF3$impressions)
Avg_Pos_norm <- normalize(AF3$Avg_Pos)
Trans_Conv_P_norm <- normalize(AF3$Trans_Conv_P)
cost_per_trans_norm <- normalize(AF3$cost_per_trans)
total_boking_norm <- normalize(AF3$total_boking)

#building the normalization regression

my_logit_nomr <- glm(binary ~ amount_norm +  clicks_norm + clicks_charge_nomr + avg_cpc_norm + 
                       impressions_norm + Avg_Pos_norm + Trans_Conv_P_norm + cost_per_trans_norm +
                           total_boking_norm, data = train_data , family = "binomial"  )
                               
#building the confusion matrix
install.packages("ggplot2")
library(ggplot2)

install.packages("lattice")
library(lattice)

install.packages("caret")
library(caret)

# predicting with testing data

my_logit_rev_2 <- glm(binary ~ amount + clicks + clicks_charger + avg_cpc + 
                        impressions  + Avg_Pos + Trans_Conv_P + cost_per_trans  + total_boking , data = test_data)
summary(my_logit_rev_2)

my_prediction <- predict(my_logit_rev_2, test_data , type = "response")
my_prediction
confusionMatrix(data = as.factor(as.numeric(my_prediction>0.5)) , reference = as.factor(as.numeric(test_data$binary)))




# testing  with training data

my_logit_rev_2 <- glm(binary ~ amount + clicks + clicks_charger + avg_cpc + 
                        impressions  + Avg_Pos + Trans_Conv_P + cost_per_trans  + total_boking , data = train_data)
summary(my_logit_rev_2)

my_prediction <- predict(my_logit_rev_2, train_data, type = "response")
my_prediction
confusionMatrix(data = as.factor(as.numeric(my_prediction>0.5)) , reference = as.factor(as.numeric(train_data$binary)))


###creating AUC RUC or lift and gains for logistic regression
install.packages("ROCR")
library(ROCR)


pred_val_logit <- prediction(my_prediction , test_data$binary )
perf_logit <- performance(pred_val_logit,"tpr", "fpr")
perf_logit
plot(perf_logit)

## desinging a gini tree with test data base

library(rpart)
library(rpart.plot)

my_logit_rev_2 <- glm(binary ~ amount + clicks + clicks_charger + avg_cpc + 
                        impressions  + Avg_Pos + Trans_Conv_P + cost_per_trans  + total_boking , data = test_data)
summary(my_logit_rev_2)

my_Tree <- rpart(binary ~  amount + clicks + clicks_charger + avg_cpc + 
                   impressions  + Avg_Pos + Trans_Conv_P + cost_per_trans  + total_boking, data = test_data, method = "class",
                 cp = 0.02)

rpart.plot(my_Tree, type = 1, extra = 1)


## desinging a gini tree with test data train

my_logit_rev_2 <- glm(binary ~ amount + clicks + clicks_charger + avg_cpc + 
                        impressions  + Avg_Pos + Trans_Conv_P + cost_per_trans  + total_boking , data = train_data)
summary(my_logit_rev_2)

my_Tree <- rpart(binary ~  amount + clicks + clicks_charger + avg_cpc + 
                   impressions  + Avg_Pos + Trans_Conv_P + cost_per_trans  + total_boking, data = train_data, method = "class",
                 cp = 0.0002)

rpart.plot(my_Tree, type = 1, extra = 1)








