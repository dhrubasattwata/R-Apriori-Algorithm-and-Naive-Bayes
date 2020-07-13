# ---
# title: "Apriori Algorithm"
# author: "Dhrubasattwata Roy Choudhury"
# ---

## Apriori Algorithm


library('arules')
library('knitr')
library('arulesViz')


### Dataset
### Dataset: Importing

data(Groceries)
summary(Groceries)     
class(Groceries)     
Groceries@itemInfo[1:20,]    

#The following code displays the 1Oth to 20th transactions of the Groceries dataset.

apply(Groceries@data[,10:20],2,function(r)  paste(Groceries@itemInfo[r,"labels"],collapse=", "))

### BUILD the Model

### First, get itemsets of length 1
itemsets<-apriori(Groceries,parameter=list(minlen=1,maxlen=1,support=0.02,target="frequent itemsets"))
summary(itemsets)  
inspect(head(sort(itemsets,by="support"),10))   # lists top 10


### Second, get itemsets of length 2
itemsets<-apriori(Groceries,parameter=list(minlen=2,maxlen=2,support=0.02,target="frequent itemsets"))
summary(itemsets)  
inspect(head(sort(itemsets,by="support"),10))   # lists top 10

### Third, get itemsets of length 3
itemsets<-apriori(Groceries,parameter=list(minlen=3,maxlen=3,support=0.02,target="frequent itemsets"))
summary(itemsets)  
inspect(head(sort(itemsets,by="support"),10))   # lists top 10

### Fourth, get itemsets of length 4
itemsets<-apriori(Groceries,parameter=list(minlen=4,maxlen=4,support=0.02,target="frequent itemsets"))
summary(itemsets)  
inspect(head(sort(itemsets,by="support"),10))   # lists top 10

### The Apriori function  () is used to generate rules. A threshold is set lower than 0.001 and minimum confidence threshold is set to 0.6. Below code generates 2,918 rules.
rules <- apriori(Groceries,parameter=list(support=0.001,confidence=0.6,target="rules"))
summary(rules)  
plot(rules)               # displays scatterplot

### Compute the 1/Support(Y) ie slope
slope<-sort(round(rules@quality$lift/rules@quality$confidence,2))

### Display the number of times each slope appears in dataset
unlist(lapply(split(slope,f=slope),length))

### Inspect function is used to display the top 10 rules sorted by lift
inspect(head(sort(rules,by="lift"),10))

#Below code fetchces rules with confidence above 0.9
confidentRules<-rules[quality(rules)$confidence>0.9] 
confidentRules       # set of 127 rules

#Plot a matrix-based visualization of the LHS v RHS of rules. This produces a Matrix-based visualization of LHS and RHS, colored by lift and confidence, 
plot(confidentRules,method="matrix",measure=c("lift","confidence"),control=list(reorder="none"))

#Visualize the top 5 rules with the highest lift and plot them
highLiftRules<-head(sort(rules,by="lift"),5) 
plot(highLiftRules,method="graph",control=list(type="items"))



### ----------------------------------------------------------------------------
### R code to convert a CSV file to a BASKET FORMAT
### ----------------------------------------------------------------------------
#read transactions
# df_groceries <- read.csv("Groceries_dataset.csv")
# str(df_groceries)
# df_sorted <- df_groceries[order(df_groceries$Member_number),]

# convert member number to numeric
# df_sorted$Member_number <- as.numeric(df_sorted$Member_number)

#convert item description to categorical format
# df_sorted$itemDescription <- as.factor(df_sorted$itemDescription)
# str(df_sorted)

#convert dataframe to transaction format using ddply; 

# if(sessionInfo()['basePkgs']=="dplyr" | sessionInfo()['otherPkgs']=="dplyr"){
#  detach(package:dplyr, unload=TRUE)
# }

# group all the items that were bought together; by the same customer on the same date
# library(plyr)
# df_itemList <- ddply(df_groceries, c("Member_number","Date"), function(df1)paste(df1$itemDescription,collapse = ","))

#remove member number and date
# df_itemList$Member_number <- NULL
# df_itemList$Date <- NULL

# colnames(df_itemList) <- c("itemList")

