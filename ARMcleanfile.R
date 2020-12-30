

####### rule mining for only data with symptoms ony which means excluding demograhic 
### load libraries first
library(dplyr)
#install.packages("plyr")
library(plyr)
#install.packages("arulesCBA")
library(arulesCBA)
library(arules)
library(arulesViz)
#install.packages("visNetwork")
library(visNetwork)
#install.packages("RColorBrewer")
library(RColorBrewer)
##import data
Sym_data <- read.csv("Sym_data_1560.csv", colClasses = "factor", header = T)
str(Sym_data)
Sym_data_only <- select(Sym_data, -1,-2,-3,-4,-5,-6, -7,-8, -34)

str(Sym_data_only) ##data.frame'##	1560 obs. of  27 variables:

## make data into transaction format
Sym_data_only[Sym_data_only == 0] <- NA

Sym_data_only[] <- lapply(Sym_data_only, function(x) levels(x)[x])

w <- which(Sym_data_only == 1, arr.ind = TRUE)

Sym_data_only[w] <- names(Sym_data_only)[w[,"col"]]

write.csv(Sym_data_only, 'Sym_data_only_trans.csv', row.names=FALSE)

### convert Sym_data into transaction format ## this file contain all systomps data with demographi
Sym_only_tData <- read.transactions('Sym_data_only_trans.csv', 
                               format = "basket", sep = ",", 
                               header=TRUE)
summary(Sym_only_tData)
Sym_only_tData@itemsetInfo
#View(itemFrequency(Sym_only_tData)) ## copy it to excel and make chart there. 


### plot frequency of symptoms in the transaction 

##itemFrequencyPlot(Sym_only_tData, topN= 27, col=brewer.pal(3,'Pastel2'), 
                  main='Relative Symptoms Frequency',type="relative", 
                  ylab="Symptoms Frequency (Relative)",
                  xlab= "Name of Symptoms") ## This did not work so we did bar plot

#p <- round(sort(itemFrequency(Sym_only_tData),decreasing = T, topN= 22),3)
#n <- barplot(sort(itemFrequency(Sym_only_tData), decreasing=TRUE))

#text(x = n, y = p, label = p, pos =2, cex = 0.5, col = "red")

## check data 
inspect(head(Sym_only_tData))


### apply rules function 
sym_only_rules <- apriori(Sym_only_tData, 
                 parameter = list(supp = 0.001,  ## Support (0.1%)
                                  conf=0.9,       ## Confidence (90%)
                                  minlen=3,      ## At least 3 items in rule
                                  maxlen=5))     ## At most 5 items in rule

## Check range of lift of generated rules
 summary(sym_only_rules)### 283 rules established
 
 ## Filter out redundant rules
 Sym_only_nonr_rules <- sym_only_rules[!is.redundant(sym_only_rules)] 
 summary(Sym_only_nonr_rules)##119
 
 Sym_only_filtered_rules <- subset(Sym_only_nonr_rules, subset = lift > 1.5)
 
 summary(Sym_only_filtered_rules)### 85  rules


 ## Filter out statistically insignificant rules
 Sym_only_sig_rules <- Sym_only_filtered_rules[!is.significant(Sym_only_filtered_rules, 
                                        Sym_only_tData, 
                                         method = "fisher", 
                                         adjust = 'bonferroni')]
 
 ## Check
 summary(Sym_only_sig_rules)## 71 rules
 inspect(Sym_only_sig_rules)
 
 a <- (inspect(head(sort(Sym_only_sig_rules, by ="support"),20)))
 lhs <- a$lhs
 rhs <- a$rhs
 Support <- a$support
 Confidence <- a$confidence
 Lift <- a$lift
 Count <- a$count
 
Symptom_rule_table <- data.frame(lhs,rhs,Support,  Confidence, Lift, Count)

write.csv(Symptom_rule_table, 'p.csv', row.names=FALSE)
 
 

plot(Sym_only_sig_rules, method = "graph")

# plot(Sym_only_sig_rules, engine = "htmlwidget")
 
 #plot(Sym_only_sig_rules, method = "matrix", engine = "htmlwidget")
library(gridExtra)
subrules_symp <- head( Sym_only_sig_rules, n = 10, by = "support")

 
plot1 <- plot(subrules_symp, method = "graph", engine = "htmlwidget")

saveAsGraph(subrules_symp, file = "rules.graphml")




plot2 <- plot(subrules_symp, method = "graph", engine = "htmlwidget")

grid.arrange(plot1, plot2 ) 
 
 

#### syptoms and gender
 
 Sym_data <- read.csv("Sym_data_1560.csv", colClasses = "factor", header = T)
 str(Sym_data)
 Sym_sex_only <- select(Sym_data, -1,-2,-4,-5,-6, -7, -34)
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
 ### plot frequency of symptoms in the transaction 
 
 itemFrequencyPlot(Sym_sex_only_tData, topN= 27)
 ## check data 
 #inspect(head(Sym_sex_only_tData))
 
 
 ### apply rules function 
 Sym_sex_only_rules <- apriori(Sym_sex_only_tData, 
                           parameter = list(supp = 0.001,  ## Support (0.1%)
                                            conf=0.9,       ## Confidence (90%)
                                            minlen=3,      ## At least 3 items in rule
                                            maxlen=5))     ## At most 5 items in rule
 
 ## Check range of lift of generated rules
 summary(Sym_sex_only_rules)### 785 rules established
 
 ## Filter out redundant rules
 Sym_sex_only_nonr_rules <- Sym_sex_only_rules[!is.redundant(Sym_sex_only_rules)]   
 
 ## Check
 summary(Sym_sex_only_nonr_rules)### set of 256 rules
 
 
 Sym_sex_only_filtered_rules <- subset(Sym_sex_only_nonr_rules, subset = lift > 1.5)
 
 
 summary(Sym_sex_only_filtered_rules)### 203  rules
  
 ## Filter out statistically insignificant rules
 Sym_sex_only_sig_rules <- Sym_sex_only_filtered_rules[!is.significant(Sym_sex_only_filtered_rules, 
                                                          Sym_sex_only_tData, 
                                                           method = "fisher", 
                                                           adjust = 'bonferroni')]
 
 ## Check
 summary(Sym_sex_only_sig_rules)## 183 rules rules
 inspect(Sym_sex_only_sig_rules)
 
 ## For male only
 
 Male_rules <- subset(Sym_sex_only_sig_rules, subset=rhs %pin% 'Male')
 summary(Male_rules)
 b <- inspect(head(sort(Male_rules, by ="support"),20))
 
 lhs <- b$lhs
 rhs <- b$rhs
 Support <- b$support
 Confidence <- b$confidence
 Lift <- b$lift
 Count <- b$count
 
 male_rule_table <- data.frame(lhs,rhs,Support,  Confidence, Lift, Count)
 
 write.csv(male_rule_table, 'p.csv', row.names=FALSE)
## plot for male rules
 subrules_male <- head( Male_rules, n = 10, by = "support")
 plot( subrules_male, method = "graph", engine = "htmlwidget")
 
 ## for female only
Female_rules <- subset(Sym_sex_only_sig_rules, subset=rhs %pin% 'Female')
 summary(Female_rules)
 
 c <- inspect(head(sort(Female_rules, by ="support"),20))
 
 lhs <- c$lhs
 rhs <- c$rhs
 Support <- c$support
 Confidence <- c$confidence
 Lift <- c$lift
 Count <- c$count
 
 Female_rule_table <- data.frame(lhs,rhs,Support,  Confidence, Lift, Count)
 
 write.csv(Female_rule_table, 'p.csv', row.names=FALSE)
## plot female rules 
 subrules_Female <- head( Female_rules, n = 10, by = "support")
 plot(subrules_Female, method = "graph", engine = "htmlwidget")

 
 
 
 
