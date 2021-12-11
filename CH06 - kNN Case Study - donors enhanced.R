## Chapter 6 - K-NN model, Case Study using Donors data, include categorical variables

library(tidyverse)
donors <- read_csv("donors.csv", col_type = "nnnnnnnnnnnnffffffffff")
glimpse(donors)

summary(donors)

## On checking the original data, there are missing data (NAs) in those numeric variables. These will be 
## dealt with similarly as before, Then those categorical variables with NAs include: state, urbanicity, socioEconmicStatus, isHomeowner, gender

## Deal with missing data for numerical variables
## age, replace missing data with mean age
donors <- donors %>%
  mutate(age = ifelse(is.na(age), mean(age, na.rm = TRUE), age))

summary(select(donors, age))

## numberChildren: replace with median
donors <- donors %>%
  mutate(numberChildren = ifelse(is.na(numberChildren), median(numberChildren, na.rm = TRUE), numberChildren))

summary(select(donors, numberChildren))

## exclude missing incomeRating and wealthRating, also exclude wealthRating = 0

donors <- donors %>%
  filter(!is.na(incomeRating) & !is.na(wealthRating) & wealthRating > 0)
summary(select(donors, incomeRating, wealthRating))

## transform data with true/false into numeric 1/0
donors <- donors %>%
  mutate(inHouseDonor = as.factor(ifelse(inHouseDonor==TRUE,1,0)))
donors <- donors %>%
  mutate(plannedGivingDonor = as.factor(ifelse(plannedGivingDonor==TRUE,1,0)))
donors <- donors %>%
  mutate(sweepstakesDonor = as.factor(ifelse(sweepstakesDonor==TRUE,1,0)))
donors <- donors %>%
  mutate(P3Donor = as.factor(ifelse(P3Donor==TRUE,1,0)))
donors <- donors %>%
  mutate(isHomeowner = as.factor(ifelse(isHomeowner==TRUE,1,0)))

summary(select(donors, inHouseDonor, plannedGivingDonor, sweepstakesDonor, P3Donor, isHomeowner))

## check each variable if they indeed contain NA for missing data

colSums(is.na(donors))

## These checks revealed that each of variable having "missing data" are actually not NAs

## Replace the "NA" by "UNK"

donors <- donors %>%
  mutate(urbanicity = as.character(urbanicity)) %>%
  mutate(urbanicity = str_replace(urbanicity, "town","1")) %>%
  mutate(urbanicity = str_replace(urbanicity, "suburb","2")) %>%
  mutate(urbanicity = str_replace(urbanicity, "rural","3")) %>%
  mutate(urbanicity = str_replace(urbanicity, "urban","4")) %>%
  mutate(urbanicity = str_replace(urbanicity, "city","5")) %>%
  mutate(urbanicity = str_replace(urbanicity, "NA","6")) %>%
  mutate(urbanicity = as.factor(urbanicity))
donors <- donors %>%
  mutate(socioEconomicStatus = as.character(socioEconomicStatus)) %>%
  mutate(socioEconomicStatus = str_replace(socioEconomicStatus, "highest","1")) %>%
  mutate(socioEconomicStatus = str_replace(socioEconomicStatus, "average","2")) %>%
  mutate(socioEconomicStatus = str_replace(socioEconomicStatus, "lowest","3")) %>%
  mutate(socioEconomicStatus = str_replace(socioEconomicStatus, "NA","4")) %>%
  mutate(socioEconomicStatus = as.factor(socioEconomicStatus))
donors <- donors %>%
  mutate(gender = as.character(gender)) %>%
  mutate(gender = str_replace(gender, "female","1")) %>%
  mutate(gender = str_replace(gender, "male","2")) %>%
  mutate(gender = str_replace(gender, "joint","3")) %>%
  mutate(gender = str_replace(gender, "NA","4")) %>%
  mutate(gender = as.factor(gender))

summary(donors)

## Alternatively, use levels to remap the level values to numeric...

levels(donors$gender) # display as "female", "male", "NA", "joint"
levels(donors$gender) <- c("1", "2", "4", "3")  # replace the value to numeric

levels(donors$urbanicity) # display as "town", "suburb", "rural", "urban", "city", "NA
levels(donors$urbanicity) <- c("1", "2", "3", "4", "5", "6")

levels(donors$socioEconomicStatus) # display as "average", "highest", "lowest", "NA"
levels(donors$socioEconomicStatus) <- c("1", "2", "3", "4")

summary(select(donors, gender, urbanicity, socioEconomicStatus))


## exclude the state variable as values are non-numeric (unless a replacement is performed)

donors <- data.frame(select(donors, -state))

## normalise data
## create the normalize function

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

## normalize all numeric data 

donors <- donors %>%
  mutate(age = normalize(age)) %>%
  mutate(numberChildren = normalize(numberChildren)) %>%  
  mutate(incomeRating = normalize(incomeRating)) %>%
  mutate(wealthRating = normalize(wealthRating)) %>%
  mutate(mailOrderPurchases = normalize(mailOrderPurchases)) %>%
  mutate(totalGivingAmount = normalize(totalGivingAmount)) %>%
  mutate(numberGifts = normalize(numberGifts)) %>%
  mutate(smallestGiftAmount = normalize(smallestGiftAmount)) %>%
  mutate(largestGiftAmount = normalize(largestGiftAmount)) %>%
  mutate(averageGiftAmount = normalize(averageGiftAmount)) %>%
  mutate(yearsSinceFirstDonation = normalize(yearsSinceFirstDonation)) %>%
  mutate(monthsSinceLastDonation = normalize(monthsSinceLastDonation))

summary(donors)

## split the sample set into training set and testing set

set.seed(1234)
sample_index <- sample(nrow(donors), round(nrow(donors)*.75), replace = FALSE)
donors_train <- donors[sample_index, ]
donors_test <- donors[-sample_index, ]

## verify if the samples into training and testing sets retain the original data's class distributions
## respondMailing is the dependent variable
## round(prop.table(table(select(donors, respondedMailing), exclude = NULL),4) * 100)

rm_table <- table(donors$respondedMailing)
prop_table_rm <- prop.table(rm_table)
prop_table_rm   ## classification distributions of dependent variable

## compare with the training set
rm_train <- table(donors_train$respondedMailing)
prop_table_train <- prop.table(rm_train)
prop_table_train

## compare with the testing set
rm_test <- table(donors_test$respondedMailing)
prop_table_test <- prop.table(rm_test)
prop_table_test

## the class distributions are similar, but there is a strong imbalance
## The DMwR package is required for SWOTE function

library(DMwR)

## make use of the SMOTE function in library DMwR, to generate double numbers of the miniority (False) when selecting
## double the no. of majority (True) using the perc.over = 100, and perc.under = 200 parameters


set.seed(1234)

donors_train2 <- SMOTE(respondedMailing ~ .,data.frame(donors_train), perc.over=100, perc.under=200)
new_train_data <- donors_train2$respondedMailing
prop.table(table(new_train_data))

## split off the class labels (respondedMailing) from the training set, also to the testing set
## the class labels are factors

donors_train_labels <- as.factor(pull(donors_train2, respondedMailing))
donors_test_labels <- as.factor(pull(donors_test, respondedMailing))

## new training and testing sets without the class labels
train_no_labels <- data.frame(select(donors_train2, -respondedMailing))
test_no_labels <- data.frame(select(donors_test, -respondedMailing))

## Build the Model, set k = 5
library(class)
donors_pred1 <- 
  knn(
    train = train_no_labels,
    test = test_no_labels,
    cl = donors_train_labels,
    k = 5
  )

head(donors_pred1)

## Evaluate the model accuracy by confusion table. Here, we just use the table function, not using confusionMatrix function

donors_pred1_table <- table(donors_test_labels, donors_pred1)
donors_pred1_table
sum(diag(donors_pred1_table)) / nrow(donors_test)

## The result shows TP and TN total = 243 + 7545 = 7788. Over 12067 test entries, accuracy = 0.6454
## Thus, include the categorical variables improves the accuracy from 0.531 to 0.646





  
