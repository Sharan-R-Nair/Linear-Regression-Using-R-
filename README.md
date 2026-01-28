###############################################################
# INSTALLING PACKAGES 
###############################################################


install.packages("tidyverse")
install.packages("readxl")
install.packages("ggplot2")
install.packages("caret")
install.packages("broom")
install.packages("skimr")
install.packages("scales")
install.packages("Hmisc")

###############################################################
# TO CHECK AND ADD THE SETTING DIRECTORY
###############################################################


getwd()
setwd("F:/Languages/R Language")

###############################################################
# READING THE LIBRARY
###############################################################


library(tidyverse)
library(readxl)
library(ggplot2)
library(caret)
library(broom)
library(skimr)
library(scales)
library(Hmisc)

###############################################################
# READING, LOADING AND VIEWING THE EXCEL FILE 
###############################################################


read_excel("austinHousing.xlsx")
austin_housing <- read_excel("austinHousing.xlsx")    ## Changing read_excel name to austin_housing

cat('Number of data : rows =', nrow(austin_housing), ' cols =', ncol(austin_housing), '\n')

view(austin_housing)
glimpse(austin_housing)

###############################################################
# SELECTING  IMPORTANT VARIABLES FOR HYPOTHESIS
# H1: sqft_living   -> sale price 
# H2: bathrooms_num -> sale price 
# H3: school_rating -> sale price 
# H4: bedroom_num   -> sale price 
# H5: garage_spaces -> sale_price
# H6: stories_num     -> sale_price 
###############################################################

###############################################################
#EXPECTED PREDICTOR COLUMN NAMES 
###############################################################


predictors <- c ("sqft_living","bathrooms_num","bedrooms_num","school_rating","garage_spaces", "stories_num")


###############################################################
# CECKING AND CLEANAIG THE DATA
###############################################################

# Outliers check

boxplot(austin_housing $ sale_price, main="Sales_Price Outliers Check")  
austin_housing $ sale_price[austin_housing $ sale_price > 5000000] <- NA


# CHANGING DATA TYPE 

austin_housing <- austin_housing %>%
  mutate( 
    
    sqft_living   = as.numeric(sqft_living),
    bedrooms_num  = as.numeric(bedrooms_num),
    school_rating = as.numeric(school_rating),
    association   = as.factor(association),
    bathrooms_num = as.numeric(bathrooms_num)
  )

# CHECKING THE NUMBER OF THE BATHROOMS AND DOING REQUIRED MODIFICATION

austin_housing$bathrooms_num[austin_housing$bathrooms_num < 1] <- 1
austin_housing$bathrooms_num[austin_housing$bathrooms_num > 6] <- 6

summary(austin_housing $ bathrooms_num)

#CHECKING THE NA VALUES AND DROPPING IT 

colSums(is.na(austin_housing))
austin_housing <- austin_housing %>% drop_na(price_source)
austin_housing <- austin_housing %>% drop_na(sale_price)
colSums(is.na(austin_housing)) # To recheck if the values are removed or not 



###############################################################
# DESCRIPTIVE STATISTICS
###############################################################


desc_stats<- austin_housing %>%
  select(sale_price, all_of(predictors)) %>% 
  skim()
print(desc_stats)

##########################################################################################################
#  CHECKING THE DENSITY OF THE SALE_PRICE 
##########################################################################################################

ggplot(austin_housing, aes(x = sale_price)) +
  geom_density(fill = "lightblue", alpha = 0.8) +
  scale_x_continuous(labels = scales::label_dollar(scale = 1e-6, suffix = "M")) +
  labs(title = "Density of Sale Prices", x = "Sale Price ($)", y = "Density") +
  theme_minimal(base_size = 13)


###############################################################################################################
# H1 : THE SQURE FOOT OF LIVING AREA WILL BE POSTIVELY RELATED TO SALES PRICE (sqft_living -> sale price)
###############################################################################################################

# CHECKING THE CORRELATION 

cor.test(austin_housing $ sqft_living, austin_housing $ sale_price) 

ggplot(data = austin_housing, mapping = aes(x = sqft_living, y = sale_price))+
  geom_point(alpha = 0.5, color = "black") + 
  geom_smooth(method = lm, color = "skyblue")+
  coord_cartesian(xlim = c(0, 10000)) +
  scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M"))+
  labs(title = "\t SALES PRICE BY LIVING AREA", y = "PRICE($)", x = "LIVING AREA(sqft)")+
  theme_minimal()


###############################################################################################################
# H2 : THE BATHROOMS OF THE HOUSES ARE POSTIVELY RELATED TO SALES PRICE (bathroom_nums  -> sale price)
###############################################################################################################

cor.test(austin_housing $ bathrooms_num, austin_housing $ sale_price)

ggplot(data = austin_housing, mapping = aes(x = bathrooms_num, y = sale_price))+
  geom_jitter(width = 0.2, alpha = 0.3, color = "darkgreen")+
  geom_smooth(method = lm, color = "blue")+
  scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M"))+
  labs(title = "\t SALES PRICE BY BATHROOM NUMBERS", y = "PRICE($)", x = "BATHROOMS(NUMBERS)")+
  theme_minimal()


###############################################################################################################
# H3 : THE BEDROOMS OF THE HOUSES ARE POSTIVELY RELATED TO SALES PRICE (bedrooms_num -> sale price)
###############################################################################################################

cor.test(austin_housing $ bedrooms_num, austin_housing $ sale_price)


ggplot(austin_housing, aes(x = factor(round(bathrooms_num)), y = sale_price)) +
  geom_boxplot(fill = "green",color = "black",alpha = 0.7, ) +
  geom_jitter( width = 0.2, alpha = 0.3, color = "skyblue" ) +
  scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
  stat_smooth(aes(x= bathrooms_num,y = sale_price), method = "lm",se= TRUE, colour = "red")+
  labs(title = "Sales Price by Number of Bedrooms",x = "Bedroom (Number)",y = "Sale Price ($)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )


##########################################################################################################
# H4 : THE SCHOOL RATINGS ARE POSTIVELY RELATED TO SALES PRICE (school_rating -> sale price)
##########################################################################################################

cor.test(austin_housing $ school_rating,austin_housing $ sale_price)

austin_housing$school_rating[austin_housing$school_rating < 0] <- 0
austin_housing$school_rating[austin_housing$school_rating > 8] <- 8

ggplot(austin_housing, aes(x = factor(round(school_rating)), y = sale_price)) +
  geom_boxplot(fill = "blue",color = "black",alpha = 0.7, ) +
  scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
  stat_smooth(aes(x= school_rating,y = sale_price), method = "lm",se= TRUE, colour = "red")+
  labs(title = "Sales Price by School Rating",x = "Rating (0-10)",y = "Sale Price ($)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

##########################################################################################################
# H5 : THE NUMBER OF STORIES OF THE HOUSES ARE POSTIVELY RELATED TO SALES PRICE (stories_num -> sale price)
##########################################################################################################

cor.test(austin_housing $ stories_num, austin_housing $ sale_price)

ggplot(data = austin_housing, mapping = aes(x = stories_num, y = sale_price))+
  geom_point(alpha = 0.5, color = "black") + 
  geom_jitter(width = 0.2, alpha = 0.3, color = "darkgreen") +
  geom_smooth(method = lm, color = "blue")+
  scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M"))+
  labs(title = "\t SALES PRICE BY NUMBER OF STORIES", y = "PRICE($)", x = "NO OF STORIES")+
  theme_minimal()


##########################################################################################################
# H6 : THE GARAGE_SPACES OF THE HOUSES ARE POSTIVELY RELATED TO SALES PRICE (garage_space -> sale price)
##########################################################################################################

cor.test(austin_housing $ garage_spaces,austin_housing $ sale_price)

austin_housing$garage_spaces <- ifelse(                                     # Made with the help of ChatGpt     
  austin_housing$garage_spaces %in% c("y", "yes", "Y", "YES"), 1,
  ifelse(austin_housing$garage_spaces %in% c("n", "no", "N", "NO"), 0,
         as.numeric(austin_housing$garage_spaces))
)

df_clean <- na.omit(austin_housing[, c("garage_spaces", "sale_price")])

cor.test(df_clean$garage_spaces, df_clean$sale_price)

ggplot(austin_housing, aes(x = garage_spaces, y = sale_price)) +
  geom_jitter(width = 0.2, alpha = 0.3, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red") +
  coord_cartesian(xlim = c(0, 6)) +
  scale_y_continuous(labels = scales::label_dollar(scale = 1e-6, suffix = "M")) +
  labs(title = "Sale Price vs Number of Garage Spaces",
       x = "Garage Spaces", y = "Sale Price ($)") +
  theme_minimal(base_size = 14)



##########################################################################################################
# H7 : THE PARKING OF THE HOUSES ARE POSTIVELY RELATED TO SALES PRICE (parking -> sale price)
#######################################################################################################

cor.test(austin_housing $ parking, austin_housing $ sale_price)


ggplot(austin_housing, aes(x =factor(parking), y = sale_price)) +
  geom_violin(fill = "lightgreen", alpha = 0.6) +
  geom_boxplot(width = 0.1, color = "black") +
  stat_smooth(aes(x= as.numeric(parking),y = sale_price), method = "lm",se= TRUE, colour = "blue")+
  scale_x_discrete(limits = as.character(0:7))+
  coord_cartesian(xlim = c(0, 7)) +
  labs(title = "Sales Price by NO of Parking",x = "Number of Parking ",y = "Sale Price ($)")
theme_minimal()

##########################################################################################################
# H8 : THE VIEW OF THE HOUSES ARE POSTIVELY RELATED TO SALES PRICE (view -> sale price)
#########################################################################################################

cor.test(austin_housing $ view,austin_housing $ sale_price)

austin_housing$view <- ifelse(
  austin_housing$view %in% c("y","Y","yes","YES","Yes"), 1,
  ifelse(austin_housing$view %in% c("n","N","no","NO","No"), 0, NA))

ggplot(austin_housing, aes(x = factor(round(view)), y = sale_price)) +
  geom_boxplot(fill = "blue",color = "black",alpha = 0.7, ) +
  scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
  labs(title = "Sales Price by View",x = "VIEW YES/NO ",y = "Sale Price ($)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

###############################################################################################################
# H9 : THE WATER FRONT FEATURES POSTIVELY RELATED TO SALES PRICE (waterfront_features -> sale price)
###############################################################################################################


cor.test(austin_housing $ waterfront_features, austin_housing $ sale_price) 

ggplot(data = austin_housing, mapping = aes(x =round(waterfront_features), y = sale_price))+
  geom_point(alpha = 0.5, color = "black") + 
  geom_jitter(width = 0.2, alpha = 0.3, color = "darkgreen") +
  geom_smooth(method = lm, color = "skyblue")+
  scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M"))+
  labs(title = "\t SALES PRICE BY WATER FRONT FEATURES", y = "PRICE($)", x = "WATER FRONT FEATURES")+
  theme_minimal()

###############################################################################################################
# H10 : THE PARKING FEATURES POSTIVELY RELATED TO SALES PRICE (parking_features -> sale price)
###############################################################################################################  

cor.test(austin_housing $ parking_features, austin_housing $ sale_price)

ggplot(austin_housing, aes(x = parking_features, y = sale_price)) +
  geom_jitter(width = 0.2, alpha = 0.3) +   # spreads points to avoid overlap
  geom_smooth(method = "lm", color = "blue") +
  scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M"))+
  labs(x = "Number of Parking Spots", y = "Sale Price", 
       title = "Sale Price vs Parking Features")+
  
  theme_minimal()

###############################################################
#  TRAIN/TEST SPLIT FOR REGRESSION
###############################################################

austin_housing$log_price <- log(austin_housing$sale_price)

set.seed(123)

index <- createDataPartition(austin_housing$log_price, list=FALSE, p=0.8, times=1)

train <- austin_housing[index,]
test <- austin_housing[-index,]


###############################################################
#  SIMPLE LINEAR REGRESSION MODELS
###############################################################

Model01 <- lm(log_price  ~ sqft_living, data = train)
summary(Model01)

pred1 <- predict(Model01, newdata=test)

postResample(pred1, test$log_price )

sqrt(mean((pred1 - test$log_price )^2))


Model02 <- lm(log_price  ~ bathrooms_num, data = train)
summary(Model02)

pred2 <- predict(Model02, newdata=test)

postResample(pred2, test$log_price )

Model03<- lm(log_price  ~ bedrooms_num, data = train)
summary(Model03)

pred3 <- predict(Model03, newdata=test)

postResample(pred3, test$log_price )


Model04 <- lm(log_price  ~ school_rating, data = train)
summary(Model04)

pred4 <- predict(Model04, newdata=test)

postResample(pred4, test$log_price )

Model05 <- lm(log_price  ~ stories_num, data = train)
summary(Model05)

pred5 <- predict(Model05, newdata=test)

postResample(pred5, test$log_price )


###############################################################
#  MULTIPLE LINEAR REGRESSION MODELS
###############################################################

Multiregression <- lm(log_price  ~ sqft_living + 
                        bedrooms_num +
                        bathrooms_num +
                        school_rating +
                        stories_num , data = train)

summary(Multiregression)

multipredit <- predict(Multiregression, newdata = test)

postResample(multipredit, test$log_price )

###############################################################
#  IMPROVED 5-VARIABLE MODEL (POLYNOMIAL)
###############################################################

model_5_poly <- lm(
  log_price ~ 
    poly(sqft_living, 2, raw = TRUE) +
    poly(bathrooms_num, 2, raw = TRUE) +
    bedrooms_num +
    school_rating +
    stories_num,
  data = train
)

pred5_poly <- predict(model_5_poly, test)
postResample(pred5_poly, test$log_price)


###############################################################
#  EXTENDED MULTIPLE LINEAR REGRESSION MODELS
###############################################################

extendedregression <- lm(log_price  ~ sqft_living + 
                           bedrooms_num +
                           bathrooms_num +
                           school_rating +
                           stories_num +
                           garage_spaces+
                           view +
                           waterfront_features +
                           parking +
                           parking_features, data = train)

summary(extendedregression)

extendedpredit <- predict(extendedregression, newdata = test)

postResample(extendedpredit, test$log_price )

###############################################################
#  POLYNOMIAL REGRESSION MODEL 
###############################################################

poly_model <- lm(
  log_price ~ 
    poly(sqft_living, 2, raw = TRUE) +
    poly(bathrooms_num, 2, raw = TRUE) +
    bedrooms_num +
    school_rating +
    stories_num +
    garage_spaces +
    view +
    waterfront_features +
    parking_features,
  data = train
)

summary(poly_model)

poly_pred <- predict(poly_model, newdata = test)

postResample(poly_pred, test$log_price)



##############################################
# ASSUMPTION CHECKS 
##############################################

# VIF 
vif(Multiregression)
vif(extendedregression)

# Average VIF 
mean(vif(Multiregression))
mean(vif(extendedregression))

#For Multiple(5) Variable Regression

par(mfrow = c(2, 2))
# Residuals vs Fitted
plot(Multiregression, which = 1)

# Q-Q Residuals 
plot(Multiregression, which=2)

# Scale - Location
plot(Multiregression, which=3)


# Histogram of Residuals 
hist(Multiregression$residuals, breaks = 100,
     main="Histogram of Residuals", xlab="Residuals")

par(mfrow = c(1, 1))

# For Extended Regression

# Residuals vs Fitted
plot(extendedregression, which = 1)

# Q-Q Residuals 
plot(extendedregression, which=2)

# Scale - Location
plot(extendedregression, which=3)


# Histogram of Residuals 
hist(extendedregression$residuals, breaks = 100,
     main="Histogram of Residuals", xlab="Residuals")

par(mfrow = c(1, 1))


###############################################
# PUT ALL MODELS INTO A LIST
###############################################

models <- list(
  "Model 1: SqFt" = Model01,
  "Model 2: Bathrooms" = Model02,
  "Model 3: Bedrooms" = Model03,
  "Model 4: School Rating" = Model04,
  "Model 5: Stories" = Model05,
  "Multiple Regression" = Multiregression,
  "Extended Regression" = extendedregression
)



# PRINT CLEAN TABLE IN CONSOLE


modelsummary(models, fmt = 2)


###############################################
# EXPORT TO WORD FILE
###############################################

modelsummary(models, output = "Regression_Results.html")

