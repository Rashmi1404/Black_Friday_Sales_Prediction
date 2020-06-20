####Black friday Analysis Project####

###loading the packages

library(ggplot2) #for plotting
library(corrplot)
library(Amelia)
library(caTools)
library(rpart)
library(rpart.plot)
library(UsingR)
library(stringr)  # for STRING operations
library(tidyverse)  # to work with TIBBLE
library(stats)
library(prob)
library(DataExplorer) #for EDA
library(dtplyr)
library(data.table)
library(MASS)
library(tree)
library(party)
library(rattle)
library(RColorBrewer)
library(randomForest)
library(ROCR)
library(gridExtra)
library(scales)


#######Data Understanding####

##Loading the dataset
BlackFriday <-read.csv(file())
attach(BlackFriday)
summary(BlackFriday)
str(BlackFriday)
names(BlackFriday)

##Removing the NA values

BlackFriday<-na.omit(BlackFriday)
View(BlackFriday)
missmap(BlackFriday, col = c('yellow','black'), main = 'check') ##To check if their are any missing values and we had a full black plot so it indicate no missing values



####EDA(Exploratory Data Analysis)#####

#Exploring the basics in data
head(table(User_ID))
table(Gender)
###WE can see the gender have two variable: F and M . F=132197 and M=405380
table(Age)
##Age is divided into 7 ranges, Here Age is Categorical Variable
table(Occupation)
#There are 21 different occupation ranging from 0-21.
table(City_Category)
#Cities in which customers have lived is categorized into three categories: A B C
table(Stay_In_Current_City_Years)
#people lived in current city we can see 0,1,2,3,4+ which indicates the  years
table(Marital_Status)
#People have their marriage status marked as either 0 or 1


                          ###########FOR GENDER#########

dataset_gender = BlackFriday %>% select(User_ID, Gender) %>% group_by(User_ID) %>% distinct()  
head(dataset_gender)
summary.factor(dataset_gender$Gender)

options(scipen=10000)   # To remove scientific numbering

genderDist  = ggplot(data = dataset_gender) +
  geom_bar(mapping = aes(x = Gender, y = ..count.., fill = Gender)) +labs(title = 'Gender of Customers') + scale_fill_brewer(palette = 'Set2')
print(genderDist)
#We can conclude that on average their are Male(75%) buyers and (25%)Female buyers

#lets compute the average spending amount as it relates to Gender
total_purchase_user = BlackFriday %>% select(User_ID, Gender, Purchase) %>% group_by(User_ID) %>% arrange(User_ID) %>%
  summarise(Total_Purchase = sum(Purchase))

user_gender = BlackFriday %>% select(User_ID, Gender) %>% group_by(User_ID) %>% arrange(User_ID) %>%distinct()
head(user_gender)
head(total_purchase_user)

##Joining Both table
user_purchase_gender = full_join(total_purchase_user, user_gender, by = "User_ID")
head(user_purchase_gender)

average_spending_gender = user_purchase_gender %>% group_by(Gender) %>% summarize(Purchase = sum(as.numeric(Total_Purchase)), Count = n(), Average = Purchase/Count)
head(average_spending_gender)
#We can see that that the average transaction for Females was 247325
#and the average transaction for Males was 357568. Let visualize our results.


###Visual Representation#
genderAverage  = ggplot(data = average_spending_gender) +
  geom_bar(mapping = aes(x = Gender, y = Average, fill = Gender), stat = 'identity') +
  labs(title = 'Average Spending by Gender') +
  scale_fill_brewer(palette = 'Set2')
print(genderAverage)
#Even though female shoppers make less purchases than males at this specific store, 
#they seem to be purchasing almost as much on average as the male shoppers

                             #######Our top selling products#########


top_sellers = BlackFriday %>% count(Product_ID, sort = TRUE)

top_5 = head(top_sellers, 5)
top_5
#  Product_ID     n
#1 P00110742   1591
#2 P00025442   1586
#3 P00112142   1539
#4 P00057642   1430
#5 P00184942   1424
##Now that we have Identified our top 5 best selling products, lets examine the best selling product, P00110742.

best_seller = BlackFriday[BlackFriday$Product_ID == 'P00110742', ]
head(best_seller)

#Analysing if our best seller to see if any relationship to Gender exits
genderDist_bs  = ggplot(data = best_seller) +
  geom_bar(mapping = aes(x = Gender, y = ..count.., fill = Gender)) +
  labs(title = 'Gender of Customers (Best Seller)') +
  scale_fill_brewer(palette = 'Dark2')

###Visual representation
print(genderDist_bs)


##Comparing both the gender graph and this graph
genderDist_bs_prop = ggplot(data = best_seller) + 
  geom_bar(fill = 'lightblue4', mapping = aes(x = Gender, y = ..prop.., group = 1, fill = Gender)) +
  labs(title = 'Gender of Customers (Best Seller - Proportion)') +
  theme(plot.title = element_text(size=9.5))

genderDist_prop = ggplot(data = dataset_gender) + 
  geom_bar(fill = "red4", mapping = aes(x = Gender, y = ..prop.., group = 1)) +
  labs(title = 'Gender of Customers (Total Proportion)') +
  theme(plot.title = element_text(size=9.5)) 

grid.arrange(genderDist_prop, genderDist_bs_prop, ncol=2)

#We can see that between the overall observation set, both purchasers of the best seller 
#and purchasers of all products are roughly ~25% female and ~75% male. 
#A slight difference does exist but it seems like we can generally conclude 
#that our best seller does not serve to a specific gender.


                               ##########For AGE#############

customers_age = BlackFriday %>% select(User_ID, Age) %>% distinct() %>% count(Age)
customers_age
#we can see a dataset that shows the count of each Age category of customers

#Age of customer
customers_age_vis = ggplot(data = customers_age) + 
  geom_bar(color = 'black', stat = 'identity', mapping = aes(x = Age, y = n, fill = Age)) +
  labs(title = 'Age of Customers') +
  theme(axis.text.x = element_text(size = 10)) +
  scale_fill_brewer(palette = 'Paired') +
  theme(legend.position="none")

###Visual Representation##
print(customers_age_vis) 
#People withing range of 26-35 shopped most
#While people in age-range 0-17 or 55+ shopped least and almost none compared to 26-35.
#overall people within age range 18-45 are the group which is more active

by_age <- BlackFriday %>% group_by(Age) %>% summarise(Purchase = mean(Purchase))
ggplot(by_age, aes(Age, Purchase)) + geom_bar( stat = 'identity',fill = c('#c5fafa', '#002ba1', '#6287ec', '#dd0244', '#c90000', '#fb0202', '#ff9ecb')) 
#On average, each age group spends about the same. Younger age groups contribute more to the sales as a whole,
#however, the age group that spends the most on average are 51-55.

#####The Age Group Which bought the Best Selled Product
ageDist_bs  = ggplot(data = best_seller) +
  geom_bar(color = 'black', mapping = aes(x = Age, y = ..count.., fill = Age)) +
  labs(title = 'Age of Customers (Best Seller)') +
  theme(axis.text.x = element_text(size = 10)) +
  scale_fill_brewer(palette = 'Paired') + 
  theme(legend.position="none")

print(ageDist_bs)

grid.arrange(customers_age_vis, ageDist_bs, ncol=2)
#here we are see a slight difference only, we can conclude the the old age > 45 people are buying less of our best selled product


                                 #######For City Category######


customers_location = BlackFriday %>% select(User_ID, City_Category) %>% distinct()
head(customers_location)

#Location of customers by city category
customers_location_vis = ggplot(data = customers_location) +
  geom_bar(color = 'white', mapping = aes(x = City_Category, y = ..count.., fill = City_Category)) +
  labs(title = 'Location of Customers') + 
  scale_fill_brewer(palette = "Set2") + 
  theme(legend.position="none")

##Visual Representation
print(customers_location_vis)
#We can see that most of our customers live in City C. Now, 
#we can compute the total purchase amount by City to see the which city's customers spent the most

#Purchase in 1000s
purchases_city = BlackFriday %>%  group_by(City_Category) %>% summarise(Purchases = sum(Purchase))

purchases_city_1000s = purchases_city %>% mutate(purchasesThousands = purchases_city$Purchases / 1000)

purchases_city_1000s
#In order to work with larger numbers, we divided the Purchases column/1000

##Visual Representation
purchaseCity_vis = ggplot(data = purchases_city_1000s, aes(x = City_Category, y = purchasesThousands, fill = City_Category)) +
  geom_bar(color = 'white', stat = 'identity') +
  labs(title = 'Total Customer Purchase Amount (by City)', y = '($000s)', x = 'City Category') +
  scale_fill_brewer(palette = "Set2") + 
  theme(legend.position="none", plot.title = element_text(size = 9))
print(purchaseCity_vis)

#Comparing the Location of customer and purchaser
grid.arrange(customers_location_vis, purchaseCity_vis, ncol=2)

#Here we can see that customers from City C were the most frequent shoppers 
#on Black Friday but Customers from City B had the highest amount of total purchases.

#Lets see how many purchases were made by customers from each city. 
#First, we will get the total number of purchases for each corresponding User_ID.

customers = BlackFriday %>% group_by(User_ID) %>% count(User_ID)
head(customers)

customers_City =  BlackFriday %>% select(User_ID, City_Category) %>% group_by(User_ID) %>% distinct() %>% ungroup() %>% left_join(customers, customers_City, by = 'User_ID') 
head(customers_City)

city_purchases_count = customers_City %>% select(City_Category, n) %>% group_by(City_Category) %>% summarise(CountOfPurchases = sum(n))
city_purchases_count

#Visual Representation
city_count_purchases_vis = ggplot(data = city_purchases_count, aes(x = City_Category, y = CountOfPurchases, fill = City_Category)) +
  geom_bar(color = 'white', stat = 'identity') +
  labs(title = 'Total Purchase Count (by City)', y = 'Count', x = 'City Category') +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position="none", plot.title = element_text(size = 9))
print(city_count_purchases_vis)

#compare
grid.arrange(purchaseCity_vis, city_count_purchases_vis, ncol = 2)

#One inference we can make from these charts is that customers from City B are simply making more purchases than 
#residence of City A + City C, and not necessarily buying more expensive products. 
#We can make this assumption due to the fact that the "Total Count of Purchases" chart 
#has a very similar appearance to the "Total Customer Purchase Amount" chart. 
#If it were the other case, then customers from City B would most likely have a 
#lower count of total purchases corresponding to a higher total purchase amount.

                             #####Best Selled Product by city####

head(best_seller)

best_seller_city = best_seller %>% select(User_ID, City_Category) %>% distinct() %>% count(City_Category)
best_seller_city
#plot
best_seller_city_vis = ggplot(data = best_seller_city, aes(x = City_Category, y = n, fill = City_Category)) +
  geom_bar(color = 'white', stat = 'identity') +
  labs(title = 'Best Seller Purchase Count (by City)', y = 'Count', x = 'City Category') +
  scale_fill_brewer(palette = "Reds") +
  theme(legend.position="none", plot.title = element_text(size = 9))

#compare
grid.arrange(city_count_purchases_vis,best_seller_city_vis, ncol = 2)

##Although customers residing in City C purchase more of our "best seller" than City A + B, 
#residents of City C fall behind City B in overall number of purchases.


               #######For Stay_In_Current_city_Year#########

customers_stay = BlackFriday %>% select(User_ID, City_Category, Stay_In_Current_City_Years) %>% group_by(User_ID) %>% distinct()
head(customers_stay)
#Lets see where most of our customers are living.
residence = customers_stay %>% group_by(City_Category) %>% tally()
head(residence)

#Plot
customers_stay_vis = ggplot(data = customers_stay, aes(x = Stay_In_Current_City_Years, y = ..count.., fill = Stay_In_Current_City_Years)) +
  geom_bar(stat = 'count') +
  scale_fill_brewer(palette = 18) +
  labs(title = 'Customers Stay in Current City', y = 'Count', x = 'Stay in Current City', fill = 'Number of Years in Current City')
print(customers_stay_vis)
#It looks like most of our customers have only been living in their respective cities for 1 year. 
#In order to see a better distribution, lets make a stacked bar chart according to each City_Category.

stay_cities = customers_stay %>% group_by(City_Category, Stay_In_Current_City_Years) %>% tally() %>% mutate(Percentage = (n/sum(n))*100)
head(stay_cities)

ggplot(data = stay_cities, aes(x = City_Category, y = n, fill = Stay_In_Current_City_Years)) + 
  geom_bar(stat = "identity", color = 'white') + 
  scale_fill_brewer(palette = 17) + 
  labs(title = "City Category + Stay in Current City", 
       y = "Total Count (Years)", 
       x = "City", 
       fill = "Stay Years")

#we can notice that in every City_Category, the most common stay length seems to be 1 year.


#purchase of those people
by_stay <- BlackFriday %>% group_by(Stay_In_Current_City_Years) %>% summarise(Purchase = mean(Purchase))
ggplot(by_stay, aes(Stay_In_Current_City_Years, Purchase)) + geom_bar(stat = 'identity',fill = c('#c4d8ba','#d8ceba','#bac4d8','#6f2859','#abe6fa'))
#Same goes for the duration of stay. 
#Regardless of the duration the average spend was more or less the same eventhough new settlers were the group that was most likely to take advantage of Black Friday.



                             ###########For Purchase###########

# purchase range
range.purchase = range(Purchase)
cat("Range of amount shoppers spent = ", range.purchase)
#Range of amount shoppers spent =  185 23961

#Barplot
barplot(table(Purchase), border = c("darkgreen"), main="Purchase made in $ by shoppers", xlab = "Amount", ylab="Frequency of people")
#Hardly a shopper spend above $19000
#Shoppers mostly spent an amount of approximately 6800 or 8700 as they got highest peak in barplot

# breaks = 40
hist(Purchase,breaks=40, xlim=c(185,25000),col="orange3", main="Purchase made in $ by shoppers(Breaks=40)", xlab="Amount", ylab="Frequency of people")
#We can clearly see how much figures people spent
#If a shopper is coming to black friday sale there are maximum chances, he would be spending on an average at least $5000
#Maximum shoppers population lie across $5000 mark
#We may consider that people didn't spent in $9000 or $17000(avg of 15K & 20K) in sales.

#Boxplot
f = fivenum(Purchase)
oulier = c(f[2]-1.5*(f[4]-f[2]) , f[4]+1.5*(f[4]-f[2]))
boxplot(f,horizontal = TRUE, xaxt="n", xlab="Amount", col="yellow", main="Purchase made in $ by shoppers")
axis(side = 1, at = f, labels=TRUE)
text(f,srt=90, rep(1.2,5), adj=0,labels=c("Min", "Lower Hinge", "Mean","Upper Hinge", "Max"))
#We can consider an average shopper will spend $5866-$12073 in black friday sales

#Investigation regarding store customers and their purchases. 
#We will start by computing the total purchase amount by user ID

customers_total_purchase_amount = BlackFriday %>%  group_by(User_ID) %>% summarise(Purchase_Amount = sum(Purchase))
head(customers_total_purchase_amount)


###############TOP Spender
#Now that we have grouped our purchases and grouped by User ID, we will sort and find our top spenders.
customers_total_purchase_amount = arrange(customers_total_purchase_amount, desc((Purchase_Amount)))
head(customers_total_purchase_amount)

#Looks like User ID 1004277 is our top spender. 
#Lets use summary() to see other facets of our total customer spending data

summary(customers_total_purchase_amount)
##We can see an average total purchase amount of 326456, max total purchase amount of 2750504, 
#min total purchase amount of 1421 and a median purchase amount of 198671.



                                ######### FOR Marital status.#########
 

table(BlackFriday$Marital_Status)#there are 32126 more single customers than couples.

dataset_maritalStatus = BlackFriday %>% select(User_ID, Marital_Status) %>% group_by(User_ID) %>% distinct()
head(dataset_maritalStatus)

#we need to quickly change Marital_Status from a numeric variable to a categorical type.
dataset_maritalStatus$Marital_Status = as.character(dataset_maritalStatus$Marital_Status)
typeof(dataset_maritalStatus$Marital_Status) #Character

marital_vis = ggplot(data = dataset_maritalStatus) +
  geom_bar(mapping = aes(x = Marital_Status, y = ..count.., fill = Marital_Status)) +
  labs(title = 'Marital Status') +
  scale_fill_brewer(palette = 'Pastel1')

print(marital_vis)
#It looks like most of our shoppers happen to be single or unmarried.

dataset_maritalStatus = dataset_maritalStatus %>% full_join(customers_stay, by = 'User_ID') 
head(dataset_maritalStatus)

maritalStatus_cities = dataset_maritalStatus %>% group_by(City_Category, Marital_Status) %>% tally()
head(maritalStatus_cities)

#plot
ggplot(data = maritalStatus_cities, aes(x = City_Category, y = n, fill = Marital_Status)) + 
  geom_bar(stat = "identity", color = 'black') + 
  scale_fill_brewer(palette = 8) + 
  labs(title = "City + Marital Status", 
       y = "Total Count (Shoppers)", 
       x = "City", 
       fill = "Marital Status")
#we can see that out off all Cities, the highest proportion of single shoppers seems to be in City C. 
#Now, lets investigate the Stay_in_Current_City distribution within each City_Category.

Users_Age = BlackFriday %>% select(User_ID, Age) %>% distinct()
head(Users_Age)

dataset_maritalStatus = dataset_maritalStatus %>% full_join(Users_Age, by = 'User_ID')
head(dataset_maritalStatus)


City_A = dataset_maritalStatus %>% dplyr::filter(City_Category == 'A')
City_B = dataset_maritalStatus %>% dplyr::filter(City_Category == 'B')
City_C = dataset_maritalStatus %>% dplyr::filter(City_Category == 'C')
head(City_A)
head(City_B)
head(City_C)

###Plot
City_A_stay_vis = ggplot(data = City_A, aes(x = Age, y = ..count.., fill = Age)) + 
  geom_bar(stat = 'count') +
  scale_fill_brewer(palette = 8) +
  theme(legend.position="none", axis.text = element_text(size = 6)) +
  labs(title = 'City A', y = 'Count', x = 'Age', fill = 'Age')
City_B_stay_vis = ggplot(data = City_B, aes(x = Age, y = ..count.., fill = Age)) +
  geom_bar(stat = 'count') +
  scale_fill_brewer(palette = 9) +
  theme(legend.position="none", axis.text = element_text(size = 6)) +
  labs(title = 'City B', y = 'Count', x = 'Age', fill = 'Age')
City_C_stay_vis = ggplot(data = City_C, aes(x = Age, y = ..count.., fill = Age)) +
  geom_bar(stat = 'count') +
  scale_fill_brewer(palette = 11) +
  theme(legend.position="none", axis.text = element_text(size = 6)) +
  labs(title = 'City C', y = 'Count', x = 'Age', fill = 'Age')

grid.arrange(City_A_stay_vis, City_B_stay_vis, City_C_stay_vis, ncol = 3)

##It looks as though City A has less shoppers living there over the age of 45 compared to the other cities.
#This could be a factor in the resulting levels of Marital_Status within each individual city.


by_mar <- BlackFriday %>% group_by(Marital_Status) %>% summarise(Purchase = mean(Purchase))
ggplot(by_mar, aes(Marital_Status, Purchase)) + geom_bar(stat = 'identity', fill = c('pink', 'cyan'))
#Altough we had more single customers, couples spend as much as single customers on average.


                        
                          ############Top Shoppers###############


top_shoppers = BlackFriday %>% count(User_ID, sort = TRUE)
head(top_shoppers)

#Looks like User_ID 1001941 shows up the most on our master ledger of shopper data. 
#Since each individual row represents a different transaction/product, 
#it looks like this user made over 1000 total transactions!
#We can join together this top shoppers dataset with our total customer purchases dataset to see them combined.

top_shoppers =  top_shoppers %>% select(User_ID, n) %>%
  left_join(customers_total_purchase_amount, Purchase_Amount, by = 'User_ID')

head(top_shoppers)
#Now that we have joined the two tables together, we can see that although User_ID 1001941 has the highest number of total purchases,
#User_ID 1004277 has the highest Purchase_Amount as identified in our earlier charts as well. 
#From here, we can also compute the average Purchase_Amount for each user.

top_shoppers = mutate(top_shoppers, Average_Purchase_Amount = Purchase_Amount/n)
head(top_shoppers)

#we can sort according to Average_Purchase_Amount to see which customers, on average, are spending the most.
top_shoppers_averagePurchase = top_shoppers %>% arrange(desc(Average_Purchase_Amount))
head(top_shoppers_averagePurchase)

#Looks like User_ID 1001477 has the highest Average_Purchase_Amount and a total Purchase_Amount of 43585. 
#User_ID 1005069 is behind User_ID 1005069 in Average_Purchase_Amount, 
#but has a much higher total Purchase_Amount of 143436.




                           ######### FOR occupation#########

ggplot(BlackFriday, aes(Occupation)) + geom_bar(fill= 'pink3') + theme_classic()  #Occupation 0,4,7 are most noticeable among customers.

customers_Occupation =  BlackFriday %>% select(User_ID, Occupation) %>% group_by(User_ID) %>% distinct() %>%
  left_join(customers_total_purchase_amount, Occupation, by = 'User_ID')

head(customers_Occupation)
#Now that we have our dataset necessary, we can group together the total Purchase_Amount for each Occupation identifier. 
#We will then convert Occupation to a charater data type.

totalPurchases_Occupation = customers_Occupation %>% group_by(Occupation) %>% summarise(Purchase_Amount = sum(Purchase_Amount)) %>% arrange(desc(Purchase_Amount))

totalPurchases_Occupation$Occupation = as.character(totalPurchases_Occupation$Occupation)
typeof(totalPurchases_Occupation$Occupation) #Character

head(totalPurchases_Occupation)

#lets plot each occupation and their total Purchase_Amount
occupation = ggplot(data = totalPurchases_Occupation) +
  geom_bar(mapping = aes(x = reorder(Occupation, -Purchase_Amount), y = Purchase_Amount, fill = Occupation), stat = 'identity') +
  scale_x_discrete(name="Occupation", breaks = seq(0,20, by = 1), expand = c(0,0)) +
  scale_y_continuous(name="Purchase Amount ($)", expand = c(0,0), limits = c(0, 750000000)) +
  labs(title = 'Total Purchase Amount by Occupation') + 
  theme(legend.position="none")
print(occupation)

#Looks like customers labeled as Occupation 4 spent the most at our store on Black Friday, 
#with customers of Occupation 0 + 7 closely behind. 



                               #######Data Preprocessing######

#Spliting data into train(70%) and test(30%)

set.seed(222) # set seed to ensure you always have same random numbers generated
ind <- sample(2,nrow(BlackFriday),replace = TRUE,prob = c(0.7,0.3))
train <- BlackFriday [ind==1,]
test <- BlackFriday [ind==2,]



########Preparing the Multi Regression Model##
options(max.print = 999999)
str(train)
str(test)

#changing the variables to factors

train$Occupation <- factor(train$Occupation)
train$Marital_Status <- factor(train$Marital_Status)
train$Product_Category_1 <- factor(train$Product_Category_1)
train$Product_Category_2<- factor(train$Product_Category_2)
train$Product_Category_3<-factor(train$Product_Category_3)

test$Occupation <- factor(test$Occupation)
test$Marital_Status <- factor(test$Marital_Status)
test$Product_Category_1 <- factor(test$Product_Category_1)
test$Product_Category_2<- factor(test$Product_Category_2)
test$Product_Category_3<-factor(test$Product_Category_3)





########DATA MODELING#####

library(glmnet)
library(psych)
library(caret)

pairs.panels(train)


#Custom Control Parameter
custom <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       verboseIter = T)

####Repeatedcv= Cross validation, number=10(i.e. In 10 fold cross validation ,the train dataset is broken into 10 parts
#and then the model are made of 9 parts and 1 part is used for error estimation and this is repeated 10 times
#with a different part used for error estimation)

                            #######Linear Regression Model######

set.seed(1234)
lm <- train(Purchase ~ Gender+Age+Occupation+City_Category+Stay_In_Current_City_Years+
                 Marital_Status+Product_Category_1+Product_Category_2+Product_Category_3,train, method = 'lm' , trControl = custom)
summary(lm)
lm$results



###Prediction On Train dataset
pred_lm <- predict(lm,train)
pred_lm
results <- cbind(pred_lm,train$Purchase)
colnames(results) <- c('Predicted','Real')
head(results)

#predicting on the test dataset 
pred_test_lm <- predict(lm, test)
test_sub_lm<- data.frame(test$User_ID,test$Product_ID,pred_test_lm,test$Purchase)
head(test_sub_lm)

linearrmse <- sqrt(mean((test$Purchase-pred_test_lm)^2)) 
linearrmse  #3598.808

                           ###########RIDGE REGRESSION############

set.seed(1234)
ridge <- train(Purchase ~ Gender+Age+Occupation+City_Category+Stay_In_Current_City_Years+
                 Marital_Status+Product_Category_1+Product_Category_2+Product_Category_3,train, method = 'glmnet',tuneGrid = expand.grid(alpha = 0,lambda = seq(4,100,length = 8)),trControl = custom )

ridge
ridge$results
ridge$bestTune
##glmnet package is used for ridge regression, lasso, elastic net, alpha is 0 for ridge and 1 for lasso,for lambda we created a sequence
#and length indicate how much values between those sequence
#if we increase lamba we are increasing penalty and as it increases it makes coefficient to shrink

# Plot Results
plot(ridge)
#so We as the lambda value increases RMSE value increases but here their is not much difference..

plot(ridge$finalModel, xvar = "lambda", label = T)
plot(ridge$finalModel, xvar = 'dev', label=T)
plot(varImp(ridge, scale=T))


###Prediction On Train dataset
pred_rid <- predict(ridge,train)
pred_rid
results1 <- cbind(pred_rid,train$Purchase)
colnames(results1) <- c('Predicted','Real')
head(results1)

#predicting on the test dataset 
pred_test_rid <- predict(ridge, test)
test_sub_rid <- data.frame(test$User_ID,test$Product_ID,pred_test_rid,test$Purchase)
head(test_sub_rid)

ridrmse <- sqrt(mean((test$Purchase-pred_test_rid)^2)) 
ridrmse #3602.74

                            #########Lasso Regresssion########

set.seed(1234)
lasso <- train(Purchase ~ Gender+Age+Occupation+City_Category+Stay_In_Current_City_Years+
                 Marital_Status+Product_Category_1+Product_Category_2+Product_Category_3,train, method = 'glmnet',tuneGrid = expand.grid(alpha = 1,lambda = seq(0.0001,1,length = 5)),trControl = custom  )

lasso
lasso$results
lasso$bestTune

#Plot Results
plot(lasso)
#here when we decrease the value of lambda we decrease the RMSE value
plot(lasso$finalModel, xvar = "lambda", label = T)
plot(lasso$finalModel, xvar = 'dev', label=T)
plot(varImp(lasso, scale=T))

###Prediction On Train dataset
pred_lasso <- predict(lasso,train)
pred_lasso
results2 <- cbind(pred_lasso,train$Purchase)
colnames(results2) <- c('Predicted','Real')
head(results2)

#predicting on the test dataset 
pred_test_lasso <- predict(lasso, test)
test_sub_lasso <- data.frame(test$User_ID,test$Product_ID,pred_test_lasso,test$Purchase)
head(test_sub_lasso)


lassormse <- sqrt(mean((test$Purchase-pred_test_lasso)^2)) 
lassormse #3598.74


                                #######Elastic Net Regression######

set.seed(1234)
elastic <- train(Purchase ~ Gender+Age+Occupation+City_Category+Stay_In_Current_City_Years+
                   Marital_Status+Product_Category_1+Product_Category_2+Product_Category_3, train, method = 'glmnet',tuneGrid = expand.grid(alpha = seq(0,1, length = 10),lambda = seq(0.0001,3, length = 5)), trControl = custom)


elastic
elastic$results
elastic$bestTune
# Plot Results
plot(elastic)


plot(elastic$finalModel, xvar = "lambda", label = T)
plot(elastic$finalModel, xvar = 'dev', label=T)
plot(varImp(elastic, scale=T))

###Prediction On Train dataset
pred_elastic <- predict(elastic,train)
pred_elastic
results3 <- cbind(pred_elastic,train$Purchase)
colnames(results3) <- c('Predicted','Real')
head(results3)

#predicting on the test dataset 
pred_test_elastic <- predict(elastic, test)
test_sub_elastic <- data.frame(test$User_ID,test$Product_ID,pred_test_elastic,test$Purchase)
head(test_sub_elastic)

elasticrmse <- sqrt(mean((test$Purchase-pred_test_elastic)^2)) 
elasticrmse #3598.74

                            ###########Decision Tree#########

model_dt <- rpart(Purchase~Gender+Age+Occupation+City_Category+Stay_In_Current_City_Years+
                    Marital_Status+Product_Category_1+Product_Category_2+Product_Category_3, 
                  data = train)

model_dt
summary(model_dt)
printcp(model_dt)
plotcp(model_dt)

#plotting the tree
rpart.plot(model_dt,extra = 3)

#predicting on the test dataset 
pred_test_dt <- predict(model_dt, test)
test_sub_dt <- data.frame(test$User_ID,test$Product_ID,pred_test_dt,test$Purchase)
head(test_sub_dt)


DecisionT <- sqrt(mean((test$Purchase-pred_test_dt)^2)) 
DecisionT #3683.911
 

                            ########Random forest#############

set.seed(123)
model_rf <- randomForest(Purchase ~ Gender+Age+Occupation+City_Category+Stay_In_Current_City_Years+
                           Marital_Status+Product_Category_1+Product_Category_2+Product_Category_3, data = train, ntree = 15)
model_rf$ntree
model_rf$importance


print(model_rf)
plot(model_rf)


#predicting on the test dataset 
pred_test_rf <- predict(model_rf, test)
test_sub_rf <- data.frame(test$User_ID,test$Product_ID,pred_test_rf,test$Purchase)
head(test_sub_rf)



RandomF <- sqrt(mean((test$Purchase-pred_test_rf)^2)) 
RandomF  #3541.019

##################################

#Comparing the model
model_list <- data.frame("Model Name" = c( "LinearModel" , "RidgeModel", "LassoModel" , "Elasticnet" , "DecisionTree" , "RandomForest"), "RMSE Value" = c(linearrmse , ridrmse, lassormse,elasticrmse, DecisionT, RandomF))
df <-model_list[order(-model_list$RMSE.Value),]
print(df)

#####################################################################
###########ASSOCIATION RULE##############
library(arules)
library(arulesViz)
library(tidyverse)

# Data Preprocessing
# Getting the dataset into the correct format
customers_products = BlackFriday %>%
  select(User_ID, Product_ID) %>%   # Selecting the columns we will need
  group_by(User_ID) %>%             # Grouping by "User_ID"          
  arrange(User_ID) %>%              # Arranging by "User_ID" 
  mutate(id = row_number()) %>%     # Defining a key column for each "Product_ID" and its corresponding "User_ID" (Must do this for spread() to work properly)
  spread(User_ID, Product_ID) %>%   # Converting our dataset from tall to wide format, and grouping "Product_IDs" to their corresponding "User_ID"
  t()                               # Transposing the dataset from columns of "User_ID" to rows of "User_ID"

# Now we can remove the Id row we created earlier for spread() to work correctly.
customers_products = customers_products[-1,]

write.csv(customers_products, file = 'customers_products.csv')

customersProducts = read.transactions('customers_products.csv', sep = ',', rm.duplicates = TRUE) # remove duplicates with rm.duplicates

#Before we implement the Apriori algorithm to our problem, 
#lets take a look at our newly created sparse matrix.
summary(customersProducts)

#we can see that there are 5869 rows (elements/itemsets/transactions) and 6613 columns (items) in our sparse matrix. With this sumary function, we get a density of 0.0043 in our matrix. 
#The density tells us that we have 0.5% non-zero values (1) in our sparse matrix and 99.5% zero (0) values.

#as we discovered in our Exploratory Data Analysis, 
#the summary() function also gives us the most frequent items that customers purchased 
#and just to be sure, we can cross reference what we discovered earlier in the analysis. 
#Lets list out what our sparse matrix gave us.

#P00110742 P00025442 P00112142 P00057642 P00184942   (Other) 
#1591      1586      1539      1430      1424    162795 

#Now lets compare it to what we discovered earler.

#"Looks like our top 5 best sellers are (by product ID)"
#  Product_ID     n
#1 P00110742   1591
#2 P00025442   1586
#3 P00112142   1539
#4 P00057642   1430
#5 P00184942   1424

#Awesome! Looks like our sparce matrix is accurate to what we discovered earlier.


itemFrequencyPlot(customersProducts, topN = 25)    # topN is limiting to the top 50 products

##Rule
rules = apriori(data = customersProducts,
                parameter = list(support = 0.008, confidence = 0.80, maxtime = 0)) # maxtime = 0 will allow our algorithim to run until completion with no time limit

#It looks like apriori has created 4 rules in accordance to our specified parameters.
#"writing ... [4 rule(s)] done [0.13s]."
#Now, lets examine our results to get a better idea of how our algoritm worked.

inspect(sort(rules, by = 'lift'))
#PLOT
plot(rules, method = 'graph')


#Lets now try modifying some of the parameters for the Apriori algotrithm and see the results.
#This time, we will decrease our confidence value to 75% and keep our support value the same (0.008).

rules = apriori(data = customersProducts,parameter = list(support = 0.008, confidence = 0.75, maxtime = 0))
#Now that we have decreased the minimum confidence value to 75%, we have a total of 52 rules.

inspect(head(sort(rules, by = 'lift'))) # limiting to the top 6 rules
#We can now see that we now have a new set of rules and the rule with the highest lift value has also changed.
#Rule number 1 shows that Customers who bought items P00032042,P00057642,P00102642,P00145042 will also purchase item P00270942 ~ 88% of the time, given a support of 0.008.

plot(rules, method = 'graph', max = 25)
#Now that we have more that 7 rules, this visualization becomes alot more difficult to interpret.
#Instead, we can create a matrix and have a similar plot and clearer interpretation.

plot(rules, method = 'grouped', max = 25,col = "Pink")
#In this visualization, we can see that we have our LHS on top and on the right hand side, the corresponding RHS. 
#The size of the bubbles represents the support value of the rule and the fill/color represents the lift.
