install.packages('prettydoc')
install.packages('psych')

library(readr)
library("dlookr")
library(dplyr)
library(psych)

#file.choose()

df = read.csv("C:/Users/Gaboelc/Desktop/predictive-analysis-of-customer-churn-in-banks/data/Churn_Modelling.csv",header = TRUE) #Cargo el dataSet

gender_dist = table(df$Gender)
barplot(gender_dist)

exited_dist = table(df$Exited)
exited_dist
barplot(exited_dist)

creditcard_dist = table(df$HasCrCard)
barplot(creditcard_dist)

numofprod_dist = table(df$NumOfProducts)
barplot(numofprod_dist)

geography_dist = table(df$Geography)
barplot(geography_dist)

#Varianza & desviacion estandar
creditscore_var = var(df$CreditScore)
creditscore_sd = sd(df$CreditScore)
creditscore_coevar = creditcard_sd/mean(df$CreditScore)*100

age_var = var(df$Age)
age_sd = sd(df$Age)
age_coevar = age_sd/mean(df$Age)*100

# Coeficiente de simetria y kurtosi
skew(df$CreditScore) # Coeficiente de simetria
kurtosi(df$CreditScore)

skew(df$Age)
kurtosi(df$Age)

# histogramas
hist(df$CreditScore)
hist(df$Age)

# descriptiva

summary(df$CreditScore)
summary(df$Age)

df$Gender = factor(x = df$Gender, levels = c('Female', 'Male'),labels = c(0,1)) #Un poco de limpieza de los valores GENDER

is.na(df) #Buscando valores na
sum(is.na(df)) #Me da la suma de los valores na

sum(complete.cases(df)) #revisar que la cantidad de observaciones sea la misma

df = subset(df, select=c("CreditScore", "Geography",	"Gender",	"Age",	"Tenure",	"Balance",
                         "NumOfProducts",	"HasCrCard",	"IsActiveMember",	"EstimatedSalary",
                         "Exited"))
df

df_grouped = group_by(df, Geography)

eda_web_report(df,output_format = "html")
