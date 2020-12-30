##Compare time 

#### syptoms and gender

Sym_data <- read.csv("Sym_data_1560.csv", colClasses = "factor", header = T)
str(Sym_data)
Sym_sex_only <- select(Sym_data, -1,-2,-3, -5,-6, -7,-8, -35, -36)
str(Sym_sex_only) ##data.frame'##	1560 obs. of  27 variables:
Sym_sex_only <- subset.data.frame(Sym_sex_only,Sym_sex_only$sex!="MissingNotAvailable")

## make data into transaction format
Sym_sex_only[Sym_sex_only == 0] <- NA

Sym_sex_only[] <- lapply(Sym_sex_only, function(x) levels(x)[x])

w <- which(Sym_sex_only == 1, arr.ind = TRUE)

Sym_sex_only[w] <- names(Sym_sex_only)[w[,"col"]]

write.csv(Sym_sex_only, 'Sym_sex_only_trans.csv', row.names=FALSE)

### convert Sym_data into transaction format ## this file contain all systomps data with demographi
Sym_sex_only_tData <- read.transactions('Sym_sex_only_trans.csv', 
                                        format = "basket", sep = ",", 
                                        header=TRUE)

summary(Sym_sex_only_tData)


### apply rules function 
start_time <- Sys.time()
Sym_sex_only_rules <- apriori(Sym_sex_only_tData, 
                              parameter = list(supp = 0.005,  ## Support (0.1%)
                                               conf=0.9,       ## Confidence (90%)
                                               minlen=3,      ## At least 3 items in rule
                                               maxlen=5))     ## At most 5 items in rule
end_time <- Sys.time()
total_time <- end_time - start_time
total_time #3  0.033
# start_time
# end_time

####  Now apply FP growth formula 
train <- sapply(Sym_sex_only,as.factor)
str(train)
train <- data.frame(train, check.names=FALSE)
txns <- as(train,"transactions")
start_time <- Sys.time()
rules = rCBA::fpgrowth(txns, support=0.001, confidence=0.9, maxLength=5, consequent="sex",
                       parallel=TRUE)

total_time <- Sys.time() - start_time
total_time ## 0.05 
# start_time
# end_time

### Eclat function 
start_time <- Sys.time()
itemsets <- eclat(Sym_sex_only_tData,
                  parameter = list(supp = 0.001, maxlen = 5))
## Create rules from the itemsets
rules <- ruleInduction(itemsets, Sym_sex_only_tData, confidence = .9)
end_time <- Sys.time()
total_time <- end_time - start_time
total_time## 0.038

# start_time
# end_time


#########
library(ggplot2)


Algorithm <- c ("Apriori", "FP Growth", "Eclat")
time <- c (0.03, 0.05, 0.04)

DF <- data.frame( Algorithm, time)
str(DF)
barplot(height=data$time, names=data$Algorithm)

DF$Algorithm <- as.factor(DF$Algorithm)
barplot(time ~ Algorithm, data = DF)

ggplot(data, aes(x=time, y=Algorithm)) + 
  geom_bar(stat="identity" )


data <- data.frame(
  Algorithm=c("Apriori", "FP Growth", "Eclat") ,  
  time=c(0.03, 0.05, 0.04)
)




## chronic condition

Sym_data <- read.csv("Sym_data_1560.csv", colClasses = "factor", header = T)
str(Sym_data)
index <- c(0,1)
values <- c("No-Chronic_DZs", "Yes-Chronic_DZs")
Sym_data$ChronicDZ_YN <- values[match(Sym_data$ChronicDZ_YN, index)]
Sym_data$ChronicDZ_YN <- as.factor(Sym_data$ChronicDZ_YN)
table(Sym_data$ChronicDZ_YN)
Sym_ChronicY_N <- select(Sym_data, -1,-2,-3,-4,-5, -6, -8, -35, -36)
str(Sym_ChronicY_N) ##data.frame'##	1560 obs. of  28 variables

## make data into transaction format
Sym_ChronicY_N[Sym_ChronicY_N == 0] <- NA

Sym_ChronicY_N[] <- lapply(Sym_ChronicY_N, function(x) levels(x)[x])

w <- which(Sym_ChronicY_N == 1, arr.ind = TRUE)

Sym_ChronicY_N[w] <- names(Sym_ChronicY_N)[w[,"col"]]

write.csv(Sym_ChronicY_N, 'Sym_ChronicY_N_trans.csv', row.names=FALSE)

### convert Sym_data into transaction format ## this file contain all systomps data with demographi
Sym_ChronicY_N_tData <- read.transactions('Sym_ChronicY_N_trans.csv', 
                                          format = "basket", sep = ",", 
                                          header=TRUE)
summary(Sym_ChronicY_N_tData)

### apply rules function 
start_time <- Sys.time()
Sym_ChronicY_N_rules <- apriori(Sym_ChronicY_N_tData, 
                                parameter = list(supp = 0.001,  ## Support (0.1%)
                                                 conf=0.9,       ## Confidence (90%)
                                                 minlen=3,      ## At least 3 items in rule
                                                 maxlen=5))     ## At most 5 items in rule
end_time <- Sys.time()
total_time <- end_time - start_time
total_time
#start_time
#end_time
####  Now apply FP growth formula 
start_time <- Sys.time()
rules = rCBA::fpgrowth(Sym_ChronicY_N_tData, support=0.001, confidence=0.9, maxLength=5, consequent="ChronicDZ_YN",
                       parallel=TRUE)

end_time <- Sys.time()

total_time <- end_time - start_time
total_time
# start_time
# end_time

### Eclat function 
start_time <- Sys.time()
itemsets <- eclat(Sym_ChronicY_N_tData,
                  parameter = list(supp = 0.001, maxlen = 5))
## Create rules from the itemsets

rules <- ruleInduction(itemsets, Sym_ChronicY_N_tData, confidence = .9)
end_time <- Sys.time()
total_time <- end_time - start_time
total_time

# start_time
# end_time


inspect(rules)
