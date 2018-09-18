
#loading libraries
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(dplyr)
library(ROCR)
library(stringr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(robustbase)
library(magrittr)
library(InformationValue)
library(broom)
library(corrplot)

##########################################################################################
##########################################################################################
#
#01-Load & Prep Data
#
##########################################################################################

#1.1.loading data
employee_data<-read.csv(file='train_LZdllcl.csv',stringsAsFactors = F,check.names = F)

#test data
test_data<-read.csv(file='test_2umaH9m.csv',stringsAsFactors = F,check.names = F)
sapply(test_data, function(x) sum(is.na(x)))

#1.2.employee_survey_data - data prep
str(employee_data) #54808 obs. of  14 variables
nrow(employee_data)#54808
length(unique(employee_data$employee_id))#54808 All values are unique so no de-duplication investigation is required here
# check for na values
sum(is.na(employee_data))#4124
sapply(employee_data, function(x) sum(is.na(x)))
#previous_year_rating has 4124(all) of the na values  
#54808/4124 is 13.29% of data
#lets evaluated further for possible replacement
unique(employee_data[which(is.na(employee_data$previous_year_rating)==1),"length_of_service"])
#so all the folks who have 1year  of service are yet to be evaluated, 
#hence don't have a rating, lets verify this

unique(employee_data[which(is.na(employee_data$previous_year_rating)==0),"length_of_service"])
# even length_of_service = 1 seems to have previous_year_rating
nrow(employee_data[which(is.na(employee_data$previous_year_rating)==0 & employee_data$length_of_service==1 ),])#423
#423 of these have a rating this seems weird since we don't have resource to valdate we will leave this hear
unique(employee_data[which(is.na(employee_data$previous_year_rating)==0 & employee_data$length_of_service==1 ),"previous_year_rating"])#423
#3 5 1 4 2  they got all the rating
rating_for_1yr_exp_folks<-factor(employee_data[which(is.na(employee_data$previous_year_rating)==0 & employee_data$length_of_service==1 ),"previous_year_rating"])
str(rating_for_1yr_exp_folks)
summary(rating_for_1yr_exp_folks)
# 1   2   3   4   5 
# 44  43 158 114  64 
#3,4 seems to be teh prominant values for 1 years folsk who got the rating

#we will use the WOE to subsitute the values since 13.29% of data is significant and we don't wana losse out on the other attribute segregation
#we will validate if teh WOE value agrees with the one that we found in folks that have rating with 1 year of exp with teh firm
#creating a backup before making chnages
employee_data_backup<-employee_data

employee_data$is_promoted<-factor(employee_data$is_promoted)
str(employee_data$is_promoted)
#Factor w/ 2 levels "0","1"
levels(employee_data$is_promoted)<-c(F,T)
employee_data$is_promoted<-as.logical(employee_data$is_promoted)

unique(employee_data$previous_year_rating)
WOETable(X=factor(ifelse(is.na(employee_data$previous_year_rating),0,employee_data$previous_year_rating)), Y=employee_data$is_promoted)
# CAT GOODS  BADS TOTAL      PCT_G      PCT_B         WOE           IV
#  0   339  3785  4124 0.07262211 0.07548863 -0.03871268 0.0001109708
#  1    88  6135  6223 0.01885176 0.12235740 -1.87034016 0.1935907607
#  2   181  4044  4225 0.03877464 0.08065417 -0.73240417 0.0306727443
#  3  1355 17263 18618 0.29027421 0.34429597 -0.17067565 0.0092201997
#  4   784  9093  9877 0.16795201 0.18135221 -0.07676277 0.0010286364
#  5  1921  9820 11741 0.41152528 0.19585162  0.74251314 0.1601405287

#3, and 4 were common for folks who got the rating with 1 year of exp 
#In term of WOE also they are the closet numbers but 4 is much closer we will use taht fro replacemnet

#assigning 4 to na of previous_year_rating with length_of_service=1
employee_data[which(is.na(employee_data$previous_year_rating)==1 & employee_data$length_of_service==1),"previous_year_rating"]<-4

#applying same chages to test data
sapply(test_data, function(x) sum(is.na(x)))
#previous_year_rating 1812 na values
length(test_data[which(is.na(test_data$previous_year_rating)==1 & test_data$length_of_service==1),"previous_year_rating"])
#all 1812 are faliing under the same criteria
test_data[which(is.na(test_data$previous_year_rating)==1 & test_data$length_of_service==1),"previous_year_rating"]<-4

#Forming mergerd data for futher processing
employee_data$is_promoted<-as.numeric(employee_data$is_promoted)
unique(employee_data$is_promoted)
test_data$is_promoted<-2
nrow(employee_data)#54808
nrow(test_data)#23490

merged_data<-rbind(employee_data,test_data)

#checking corelation
correl_num_cols<-names(which(sapply(merged_data,is.numeric)))

cortable<-cor(merged_data[,correl_num_cols])
View(cortable)
#length_of_servic and age has a very high corealtion 0.6533393201
#KPIs_met >80% and previous_year_rating has a high corealtion 0.333669655
#KPIs_met >80%  and awards_won? has marginal corelation 0.1003648092
# remaining coefficient are two small to have any impact
corrplot(cortable, type = "upper", order = "hclust", tl.col = "black", tl.srt = 90)
col<- colorRampPalette(c("blue", "white", "pink"))(40)
heatmap(x = cortable, col = col, symm = TRUE)



######################################################
#lets work on continous variables, we will use binning to produce a stable model
QuartileNHist <- function(data) 
{
  print(quantile(unlist(data),seq(0, 1, 0.01)))
  print(ggplot(data.frame(data),aes(x=data))+geom_histogram() + scale_x_continuous(breaks = round(seq(min(data), max(data),
                                                                                                      by = (max(data)-min(data))/30 ),1)) + theme(axis.text=element_text(size=8)))
  
}

#The below function gives the binning statement clause based on the binning values passed in the partition variable
mutatecol<- function(colname,partition)
{
  clause<- paste(" ",colname ," = case_when( ")
  for (i in 1:length(partition))  
  { 
    if (i==1)
    { clause<-paste(clause , " ( ",colname ," <= ", partition[i]," )~ ",i," , ")
    
    } else if (i>1 
               # & i!=length(partition)
    )
    {
      clause<-paste(clause, " ( ", colname ," > ", partition[i-1]," & ",colname ," <= ",partition[i]," ) ~ ",i," , ")
    }
  }
  clause<-paste(clause , " ( ", colname ," > ", partition[length(partition)]," )~ ",length(partition)+1," ,T~",length(partition)+2," ) ")
  
  print(clause)  
}

#looking at the pvalue and distinguisins is difficul with naked eye
library(xtable)
pvalue.based.sort<-function(model){
  idx <- order(coef(summary(model))[,4],decreasing=T)  # sort out the p-values
  out <- coef(summary(model))[idx,]       # reorder coef, SE, etc. by increasing p
  print(out)
  
}


#employee_data
sapply(employee_data, function(x) length(unique(x)))
sapply(employee_data, function(x) is.numeric(x))

#list of cnotinous variable
# no_of_trainings,age,length_of_service & avg_training_score are the continuous variables

#create a backup of merged_data
merged_data_bkp<-merged_data
#data analysis will be done on training data alone
#change will be applied to merged set

#no_of_trainings
QuartileNHist(employee_data$no_of_trainings)
#based on the histogram and quartile values, let's bin the data
partition<-c(1,2,3,4)
mutatecol('no_of_trainings',partition)
merged_data %<>% mutate(no_of_trainings  = case_when(   (  no_of_trainings  <=  1  )~  1  ,   (  no_of_trainings  >  1  &  no_of_trainings  <=  2  ) ~  2  ,   (  no_of_trainings  >  2  &  no_of_trainings  <=  3  ) ~  3  ,   (  no_of_trainings  >  3  &  no_of_trainings  <=  4  ) ~  4  ,   (  no_of_trainings  >  4  )~  5  ,T~ 6  ))



#age
QuartileNHist(employee_data$age)
#based on the histogram and quartile values, let's bin the data
partition<-c(22,25,26,27,28,29,30,33,34,38,41,42,45,46,49,50,52,53,56,57,58)
mutatecol('age',partition)
merged_data %<>% mutate( age  = case_when(   (  age  <=  22  )~  1  ,   (  age  >  22  &  age  <=  25  ) ~  2  ,   (  age  >  25  &  age  <=  26  ) ~  3  ,   (  age  >  26  &  age  <=  27  ) ~  4  ,   (  age  >  27  &  age  <=  28  ) ~  5  ,   (  age  >  28  &  age  <=  29  ) ~  6  ,   (  age  >  29  &  age  <=  30  ) ~  7  ,   (  age  >  30  &  age  <=  33  ) ~  8  ,   (  age  >  33  &  age  <=  34  ) ~  9  ,   (  age  >  34  &  age  <=  38  ) ~  10  ,   (  age  >  38  &  age  <=  41  ) ~  11  ,   (  age  >  41  &  age  <=  42  ) ~  12  ,   (  age  >  42  &  age  <=  45  ) ~  13  ,   (  age  >  45  &  age  <=  46  ) ~  14  ,   (  age  >  46  &  age  <=  49  ) ~  15  ,   (  age  >  49  &  age  <=  50  ) ~  16  ,   (  age  >  50  &  age  <=  52  ) ~  17  ,   (  age  >  52  &  age  <=  53  ) ~  18  ,   (  age  >  53  &  age  <=  56  ) ~  19  ,   (  age  >  56  &  age  <=  57  ) ~  20  ,   (  age  >  57  &  age  <=  58  ) ~  21  ,   (  age  >  58  )~  22  ,T~ 23  ))

#length_of_service
QuartileNHist(employee_data$length_of_service)
#based on the histogram and quartile values, let's bin the data
partition<-c(2,3,4,5,6,7,9,10,11,13,16,17,19,23)
mutatecol('length_of_service',partition)
merged_data %<>% mutate(length_of_service  = case_when(   (  length_of_service  <=  2  )~  1  ,   (  length_of_service  >  2  &  length_of_service  <=  3  ) ~  2  ,   (  length_of_service  >  3  &  length_of_service  <=  4  ) ~  3  ,   (  length_of_service  >  4  &  length_of_service  <=  5  ) ~  4  ,   (  length_of_service  >  5  &  length_of_service  <=  6  ) ~  5  ,   (  length_of_service  >  6  &  length_of_service  <=  7  ) ~  6  ,   (  length_of_service  >  7  &  length_of_service  <=  9  ) ~  7  ,   (  length_of_service  >  9  &  length_of_service  <=  10  ) ~  8  ,   (  length_of_service  >  10  &  length_of_service  <=  11  ) ~  9  ,   (  length_of_service  >  11  &  length_of_service  <=  13  ) ~  10  ,   (  length_of_service  >  13  &  length_of_service  <=  16  ) ~  11  ,   (  length_of_service  >  16  &  length_of_service  <=  17  ) ~  12  ,   (  length_of_service  >  17  &  length_of_service  <=  19  ) ~  13  ,   (  length_of_service  >  19  &  length_of_service  <=  23  ) ~  14  ,   (  length_of_service  >  23  )~  15  ,T~ 16  ) )


#avg_training_score
QuartileNHist(employee_data$avg_training_score)
#based on the histogram and quartile values, let's bin the data
partition<-c(42,45,47,52,57,59,61,63,67,71,78,81,84,86,88,90,94,98)
mutatecol('avg_training_score',partition)
merged_data %<>% mutate( avg_training_score  = case_when(   (  avg_training_score  <=  42  )~  1  ,   (  avg_training_score  >  42  &  avg_training_score  <=  45  ) ~  2  ,   (  avg_training_score  >  45  &  avg_training_score  <=  47  ) ~  3  ,   (  avg_training_score  >  47  &  avg_training_score  <=  52  ) ~  4  ,   (  avg_training_score  >  52  &  avg_training_score  <=  57  ) ~  5  ,   (  avg_training_score  >  57  &  avg_training_score  <=  59  ) ~  6  ,   (  avg_training_score  >  59  &  avg_training_score  <=  61  ) ~  7  ,   (  avg_training_score  >  61  &  avg_training_score  <=  63  ) ~  8  ,   (  avg_training_score  >  63  &  avg_training_score  <=  67  ) ~  9  ,   (  avg_training_score  >  67  &  avg_training_score  <=  71  ) ~  10  ,   (  avg_training_score  >  71  &  avg_training_score  <=  78  ) ~  11  ,   (  avg_training_score  >  78  &  avg_training_score  <=  81  ) ~  12  ,   (  avg_training_score  >  81  &  avg_training_score  <=  84  ) ~  13  ,   (  avg_training_score  >  84  &  avg_training_score  <=  86  ) ~  14  ,   (  avg_training_score  >  86  &  avg_training_score  <=  88  ) ~  15  ,   (  avg_training_score  >  88  &  avg_training_score  <=  90  ) ~  16  ,   (  avg_training_score  >  90  &  avg_training_score  <=  94  ) ~  17  ,   (  avg_training_score  >  94  &  avg_training_score  <=  98  ) ~  18  ,   (  avg_training_score  >  98  )~  19  ,T~ 20  ))


 
# change to 2level categotical variables 
Cate.Var.Conv.2level<- function(colname)
{
  col.number<-which( colnames(merged_data)==colname )
  uni.values<-length(unique(merged_data[,col.number]))
  merged_data[,col.number]<-factor(merged_data[,col.number])
  print(str(merged_data[,col.number]))
  levels(merged_data[,col.number])<-c(0:(uni.values-1))
  print(str(merged_data[,col.number]))
  merged_data[,col.number]<- as.numeric(levels(merged_data[,col.number]))[merged_data[,col.number]]
  merged_data[,col.number]
  merged_data <<- merged_data  
}

View(merged_data)
sapply(employee_data, function(x) length(unique(x)))
sapply(employee_data, function(x) is.numeric(x))
#gender, KPIs_met >80% ,awards_won?

#gender
merged_data[,"gender"]
Cate.Var.Conv.2level("gender")
# Factor w/ 2 levels "f","m": 1 2 2 2 2 2 1 2 2 2 ...
# Factor w/ 2 levels "0","1": 1 2 2 2 2 2 1 2 2 2 ...

#KPIs_met >80%
merged_data[,"KPIs_met >80%"]
Cate.Var.Conv.2level("KPIs_met >80%")
# Factor w/ 2 levels "0","1": 2 1 1 1 1 1 1 1 1 2 ...
# Factor w/ 2 levels "0","1": 2 1 1 1 1 1 1 1 1 2 ...

#awards_won?
merged_data[,"awards_won?"]
Cate.Var.Conv.2level("awards_won?")
# Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
# Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...

#employee_data
sapply(employee_data, function(x) length(unique(x)))
sapply(employee_data, function(x) is.numeric(x))



#factor with more than 2 level
#numerical factors will remains as is
factor.var<-model.matrix(~department+region+education+recruitment_channel,data=merged_data)
View(factor.var)
factor.var<-factor.var[,-1]
str(factor.var)

#dropping the original categorical column
drops <- c('department','region','education','recruitment_channel')

merged_data_ordinal<-merged_data[ , !(names(merged_data) %in% drops)]
#combining the modified categorical columns set to the data
merged_data_ordinal<-cbind(merged_data_ordinal,factor.var)

#coverting all ordinal categotical as well 
merged_data_ordinal$no_of_trainings<-factor(merged_data_ordinal$no_of_trainings)
merged_data_ordinal$age<-factor(merged_data_ordinal$age)
merged_data_ordinal$length_of_service<-factor(merged_data_ordinal$length_of_service)
merged_data_ordinal$avg_training_score<-factor(merged_data_ordinal$avg_training_score)
merged_data_ordinal$previous_year_rating<-factor(merged_data_ordinal$previous_year_rating)


factor.var<-model.matrix(~no_of_trainings+age+length_of_service+
                           avg_training_score+ previous_year_rating,
                         data=merged_data_ordinal)
View(factor.var)
factor.var<-factor.var[,-1]
str(factor.var)

#dropping the original categorical column
drops <- c('no_of_trainings','age','length_of_service',
             'avg_training_score', 'previous_year_rating')


merged_data_allcat<-merged_data_ordinal[ , !(names(merged_data_ordinal) %in% drops)]
#combining the modified categorical columns set to the data
merged_data_allcat<-cbind(merged_data_allcat,factor.var)
#avoiding rename in teh code for now
merged_data_ordinal<-merged_data_allcat

########################################################
#part 1 pocessing is done we will build a model with this data 
#thsi set has teh cantinuos variable binned
#we can further treat them liek categorical variable but we will do it as a next step
#we will first build the model on this data

#seprating test and train data
Final_test_data<-merged_data_ordinal[which(merged_data_ordinal$is_promoted==2),]
nrow(Final_test_data)#23490
Final_test_data<-Final_test_data[, !(names(Final_test_data) %in% "is_promoted")]

train_data<-merged_data_ordinal[which(merged_data_ordinal$is_promoted!=2),]
nrow(train_data)#54808
#removing the employee_id column from the dataset,
#as its a unique identifier and provides no discriminating power
train_data<-train_data[, !(names(train_data) %in% "employee_id")]

########################################################################
# 02.Build model
########################################################################

# splitting the data between train and test
set.seed(100)

indices = sample.split(train_data$is_promoted, SplitRatio = 0.7)

train = train_data[indices,]

test = train_data[!(indices),]


#comapting spread of data in teh actaul data set and test data set
ggplot(train_data,aes(x=is_promoted))+geom_bar()+  geom_text(stat='count',aes(label=..count..),vjust=-1)
#50140/4668=10.74
ggplot(test,aes(x=is_promoted))+geom_bar()+  geom_text(stat='count',aes(label=..count..),vjust=-1)
#15042/1400=10.72

# our test data represt out total data w.r.t o/p varaible

# Logistic Regression: 

#Initial model
model_1 = glm(is_promoted ~ ., data = train, family = "binomial")
summary(model_1) 
# Null deviance: 22347  on 38365  degrees of freedom
# Residual deviance: 14972  on 38255  degrees of freedom
# AIC: 15194
# 
# Number of Fisher Scoring iterations: 7
nrow(summary(model_1)$coefficients)#111 coefficients


# Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")
summary(model_2) 
# Null deviance: 22347  on 38365  degrees of freedom
# Residual deviance: 15618  on 38326  degrees of freedom
# AIC: 15698
# 
# Number of Fisher Scoring iterations: 7
nrow(summary(model_2)$coefficients)#40 coefficients
#step aic was able to remove 15 varaibels for us

#Let's try to remove the varaibel taht are not contributing significantly
next.iteration<- function(imodel)
{
  cor.var<-as.matrix(vif(imodel))
  cor.var <- data.frame(row.names(cor.var), cor.var, row.names = NULL)
  n<-length(cor.var[,2])
  print(paste("var_name","var_vif","var_pvalue",sep="        "))
  for (i in 1:n-1){
    #print('VIF')
    #print(cor.var[which(cor.var$cor.var==sort(cor.var[,2],partial=n-i)[n-i]),])
    var_name<-cor.var[which(cor.var$cor.var==sort(cor.var[,2],partial=n-i)[n-i]),1]
    var_vif<-cor.var[which(cor.var$cor.var==sort(cor.var[,2],partial=n-i)[n-i]),2]
    
    test.var<-as.character(cor.var[which(cor.var$cor.var==sort(cor.var[,2],partial=n-i)[n-i]),1])
    
    p.var<-as.matrix(summary(imodel)$coefficients[,4])
    p.var <- data.frame(row.names(p.var), p.var, row.names = NULL)
    #print(paste('P-value for ',test.var,':-'))
    #print(p.var[which(p.var$row.names.p.var.==test.var),])
    var_pvalue<-p.var[which(p.var$row.names.p.var.==test.var),2]
    print(paste(var_name,var_vif,var_pvalue,sep="        "))
    
  }
}

#looking at the pvalue and distinguisins is difficul with naked eye
library(xtable)
pvalue.based.sort<-function(model){
  idx <- order(coef(summary(model))[,4],decreasing=T)  # sort out the p-values
  out <- coef(summary(model))[idx,]       # reorder coef, SE, etc. by increasing p
  print(out)
  
}


###################
View(train)
next.iteration(model_2)# none of teh high vif(>2) are less significant
# we will use teh p-values for deletion
pvalue.based.sort(model_2)
#removing
#regionregion_20                -0.28667909 0.200947180  -1.426639  1.536840e-01
model_2.1<-glm(is_promoted ~ `KPIs_met >80%` + `awards_won?` + departmentFinance + 
                 departmentHR + departmentLegal + departmentOperations + departmentProcurement + 
                 `departmentR&D` + `departmentSales & Marketing` + departmentTechnology + 
                 regionregion_11 + regionregion_12 + regionregion_15 + regionregion_17 + 
                 regionregion_2 + regionregion_20 + regionregion_21 + regionregion_22 + 
                 regionregion_23 + regionregion_24 + regionregion_25 + regionregion_28 + 
                 regionregion_29 + regionregion_3 + regionregion_31 + regionregion_32 + 
                 regionregion_34 + regionregion_4 + regionregion_5 + regionregion_6 + 
                 regionregion_7 + regionregion_9 + `educationBachelor's` + 
                 `educationBelow Secondary` + `educationMaster's & above` + 
                 no_of_trainings2 + no_of_trainings4 + no_of_trainings5 + 
                 age3 + age4 + age5 + age6 + age7 + age8 + age9 + age10 + 
                 age12 + age14 + age15 + age16 + age18 + age19 + age20 + length_of_service2 + 
                 length_of_service3 + length_of_service4 + length_of_service5 + 
                 length_of_service6 + length_of_service7 + length_of_service8 + 
                 length_of_service9 + length_of_service10 + length_of_service13 + 
                 length_of_service14 + avg_training_score4 + avg_training_score5 + 
                 avg_training_score6 + avg_training_score7 + avg_training_score8 + 
                 avg_training_score9 + avg_training_score10 + avg_training_score11 + 
                 avg_training_score12 + avg_training_score13 + avg_training_score14 + 
                 avg_training_score15 + avg_training_score16 + avg_training_score17 + 
                 avg_training_score18 + avg_training_score19 + previous_year_rating2 + 
                 previous_year_rating3 + previous_year_rating4 + previous_year_rating5, 
               family = "binomial", data = train)
summary(model_2.1)
#AIC: AIC: 15151
next.iteration(model_2.1)# none of teh high vif are less significant
# we will use teh p-values for deletion
pvalue.based.sort(model_2.1)
# "`educationBachelor's`        9.19396231943774        0.00226137647883658"
model_2.2<-glm(is_promoted ~ `KPIs_met >80%` + `awards_won?` + departmentFinance + 
                  departmentHR + departmentLegal + departmentOperations + departmentProcurement + 
                  `departmentR&D` + `departmentSales & Marketing` + departmentTechnology + 
                  regionregion_11 + regionregion_12 + regionregion_15 + regionregion_17 + 
                  regionregion_2 + regionregion_20 + regionregion_21 + regionregion_22 + 
                  regionregion_23 + regionregion_24 + regionregion_25 + regionregion_28 + 
                  regionregion_29 + regionregion_3 + regionregion_31 + regionregion_32 + 
                  regionregion_34 + regionregion_4 + regionregion_5 + regionregion_6 + 
                  regionregion_7 + regionregion_9 +  `educationBelow Secondary` + `educationMaster's & above` + 
                  no_of_trainings2 + no_of_trainings4 + no_of_trainings5 + 
                  age3 + age4 + age5 + age6 + age7 + age8 + age9 + age10 + 
                  age12 + age14 + age15 + age16 + age18 + age19 + age20 + length_of_service2 + 
                  length_of_service3 + length_of_service4 + length_of_service5 + 
                  length_of_service6 + length_of_service7 + length_of_service8 + 
                  length_of_service9 + length_of_service10 + length_of_service13 + 
                  length_of_service14 + avg_training_score4 + avg_training_score5 + 
                  avg_training_score6 + avg_training_score7 + avg_training_score8 + 
                  avg_training_score9 + avg_training_score10 + avg_training_score11 + 
                  avg_training_score12 + avg_training_score13 + avg_training_score14 + 
                  avg_training_score15 + avg_training_score16 + avg_training_score17 + 
                  avg_training_score18 + avg_training_score19 + previous_year_rating2 + 
                  previous_year_rating3 + previous_year_rating4 + previous_year_rating5, 
                family = "binomial", data = train)
summary(model_2.2)
#AIC: 15159
next.iteration(model_2.2)# none of teh high vif are less significant
# we will use teh p-values for deletion
pvalue.based.sort(model_2.2)
#[1] "avg_training_score4        3.61850562578235        0.168282284335778"
model_2.3<-glm(is_promoted ~ `KPIs_met >80%` + `awards_won?` + departmentFinance + 
                 departmentHR + departmentLegal + departmentOperations + departmentProcurement + 
                 `departmentR&D` + `departmentSales & Marketing` + departmentTechnology + 
                 regionregion_11 + regionregion_12 + regionregion_15 + regionregion_17 + 
                 regionregion_2 + regionregion_20 + regionregion_21 + regionregion_22 + 
                 regionregion_23 + regionregion_24 + regionregion_25 + regionregion_28 + 
                 regionregion_29 + regionregion_3 + regionregion_31 + regionregion_32 + 
                 regionregion_34 + regionregion_4 + regionregion_5 + regionregion_6 + 
                 regionregion_7 + regionregion_9 +  `educationBelow Secondary` + `educationMaster's & above` + 
                 no_of_trainings2 + no_of_trainings4 + no_of_trainings5 + 
                 age3 + age4 + age5 + age6 + age7 + age8 + age9 + age10 + 
                 age12 + age14 + age15 + age16 + age18 + age19 + age20 + length_of_service2 + 
                 length_of_service3 + length_of_service4 + length_of_service5 + 
                 length_of_service6 + length_of_service7 + length_of_service8 + 
                 length_of_service9 + length_of_service10 + length_of_service13 + 
                 length_of_service14 +  avg_training_score5 + 
                 avg_training_score6 + avg_training_score7 + avg_training_score8 + 
                 avg_training_score9 + avg_training_score10 + avg_training_score11 + 
                 avg_training_score12 + avg_training_score13 + avg_training_score14 + 
                 avg_training_score15 + avg_training_score16 + avg_training_score17 + 
                 avg_training_score18 + avg_training_score19 + previous_year_rating2 + 
                 previous_year_rating3 + previous_year_rating4 + previous_year_rating5, 
               family = "binomial", data = train)
summary(model_2.3)
#AIC: 15159
next.iteration(model_2.3)# none of teh high vif are less significant
# we will use teh p-values for deletion
pvalue.based.sort(model_2.3)
#"age10        1.84593823061312        0.00743432259459468"
model_2.4<-glm(is_promoted ~ `KPIs_met >80%` + `awards_won?` + departmentFinance + 
                 departmentHR + departmentLegal + departmentOperations + departmentProcurement + 
                 `departmentR&D` + `departmentSales & Marketing` + departmentTechnology + 
                 regionregion_11 + regionregion_12 + regionregion_15 + regionregion_17 + 
                 regionregion_2 + regionregion_20 + regionregion_21 + regionregion_22 + 
                 regionregion_23 + regionregion_24 + regionregion_25 + regionregion_28 + 
                 regionregion_29 + regionregion_3 + regionregion_31 + regionregion_32 + 
                 regionregion_34 + regionregion_4 + regionregion_5 + regionregion_6 + 
                 regionregion_7 + regionregion_9 +  `educationBelow Secondary` + `educationMaster's & above` + 
                 no_of_trainings2 + no_of_trainings4 + no_of_trainings5 + 
                 age3 + age4 + age5 + age6 + age7 + age8 + age9 +  
                 age12 + age14 + age15 + age16 + age18 + age19 + age20 + length_of_service2 + 
                 length_of_service3 + length_of_service4 + length_of_service5 + 
                 length_of_service6 + length_of_service7 + length_of_service8 + 
                 length_of_service9 + length_of_service10 + length_of_service13 + 
                 length_of_service14 +  avg_training_score5 + 
                 avg_training_score6 + avg_training_score7 + avg_training_score8 + 
                 avg_training_score9 + avg_training_score10 + avg_training_score11 + 
                 avg_training_score12 + avg_training_score13 + avg_training_score14 + 
                 avg_training_score15 + avg_training_score16 + avg_training_score17 + 
                 avg_training_score18 + avg_training_score19 + previous_year_rating2 + 
                 previous_year_rating3 + previous_year_rating4 + previous_year_rating5, 
               family = "binomial", data = train)#train_data
summary(model_2.4)
#AIC: 15164 , slight increase
next.iteration(model_2.4)# none of teh high vif are less significant
# we will use teh p-values for deletion
pvalue.based.sort(model_2.4)
#avg_training_score19           26.84451736 324.05420541   0.08283959
#"regionregion_2        1.69114160228895        0.0257521547351836"
model_2.5<-glm(is_promoted ~ `KPIs_met >80%` + `awards_won?` + departmentFinance + 
                 departmentHR + departmentLegal + departmentOperations + departmentProcurement + 
                 `departmentR&D` + `departmentSales & Marketing` + departmentTechnology + 
                 regionregion_11 + regionregion_12 + regionregion_15 + regionregion_17 + 
                 avg_training_score19 + regionregion_20 + regionregion_21 + regionregion_22 + 
                 regionregion_23 + regionregion_24 + regionregion_25 + regionregion_28 + 
                 regionregion_29 + regionregion_3 + regionregion_31 + regionregion_32 + 
                 regionregion_34 + regionregion_4 + regionregion_5 + regionregion_6 + 
                 regionregion_7 + regionregion_9 +  `educationBelow Secondary` + `educationMaster's & above` + 
                 no_of_trainings2 + no_of_trainings4 + no_of_trainings5 + 
                 age3 + age4 + age5 + age6 + age7 + age8 + age9 +  
                 age12 + age14 + age15 + age16 + age18 + age19 + age20 + length_of_service2 + 
                 length_of_service3 + length_of_service4 + length_of_service5 + 
                 length_of_service6 + length_of_service7 + length_of_service8 + 
                 length_of_service9 + length_of_service10 + length_of_service13 + 
                 length_of_service14 +  avg_training_score5 + 
                 avg_training_score6 + avg_training_score7 + avg_training_score8 + 
                 avg_training_score9 + avg_training_score10 + avg_training_score11 + 
                 avg_training_score12 + avg_training_score13 + avg_training_score14 + 
                 avg_training_score15 + avg_training_score16 + avg_training_score17 + 
                 avg_training_score18 +  previous_year_rating2 + 
                 previous_year_rating3 + previous_year_rating4 + previous_year_rating5, 
               family = "binomial", data = train)
summary(model_2.5)
#AIC: 15166 , slight increase

next.iteration(model_2.5)# none of teh high vif are less significant
# we will use teh p-values for deletion
pvalue.based.sort(model_2.5)
# [1] "length_of_service7        1.4832108826625        0.0576560525700123"
# [1] "length_of_service3        1.43901778729228        0.0440299324959047"
# [1] "length_of_service6        1.43763819859422        0.187848267746278"
# [1] "length_of_service2        1.435283797416        0.230187197603324"
# [1] "age8        1.39819944188276        0.00062240117581965"
# [1] "length_of_service4        1.39262172169076        0.007710506946931"
# [1] "length_of_service5        1.37164145232523        0.061241573220158"

model_2.6<-glm(is_promoted ~ `KPIs_met >80%` + `awards_won?` + departmentFinance + 
                 departmentHR + departmentLegal + departmentOperations + departmentProcurement + 
                 `departmentR&D` + `departmentSales & Marketing` + departmentTechnology + 
                 regionregion_11 + regionregion_12 + regionregion_15 + regionregion_17 + 
                 avg_training_score19 + regionregion_20 + regionregion_21 + regionregion_22 + 
                 regionregion_23 + regionregion_24 + regionregion_25 + regionregion_28 + 
                 regionregion_29 + regionregion_3 + regionregion_31 + regionregion_32 + 
                 regionregion_34 + regionregion_4 + regionregion_5 + regionregion_6 + 
                 regionregion_7 + regionregion_9 +  `educationBelow Secondary` + `educationMaster's & above` + 
                 no_of_trainings2 + no_of_trainings4 + no_of_trainings5 + 
                 age3 + age4 + age5 + age6 + age7 +  age9 +  
                 age12 + age14 + age15 + age16 + age18 + age19 + age20 +
                  length_of_service8 + 
                 length_of_service9 + length_of_service10 + length_of_service13 + 
                 length_of_service14 +  avg_training_score5 + 
                 avg_training_score6 + avg_training_score7 + avg_training_score8 + 
                 avg_training_score9 + avg_training_score10 + avg_training_score11 + 
                 avg_training_score12 + avg_training_score13 + avg_training_score14 + 
                 avg_training_score15 + avg_training_score16 + avg_training_score17 + 
                 avg_training_score18 +  previous_year_rating2 + 
                 previous_year_rating3 + previous_year_rating4 + previous_year_rating5, 
               family = "binomial", data = train)

summary(model_2.6)
#AIC: 15181 , slight increase

next.iteration(model_2.6)# none of teh high vif are less significant
# we will use teh p-values for deletion
pvalue.based.sort(model_2.6)
# [1] "`educationMaster's & above`        1.2477978052381        0.00163742959204151"
# [1] "`departmentR&D`        1.15688197428924        0.0109494815394144"
# [1] "age5        1.14466317106787        0.00185031279521068"
# [1] "age4        1.13145255110833        0.00214256526383013"
# [1] "regionregion_7        1.12568677088266        0.000172893350159935"
# [1] "age6        1.12021471982948        0.208141312987111"
# [1] "age7        1.10878718651408        0.100070224203958"
# [1] "age15        1.10672965827455        0.0926153319993892"
# [1] "length_of_service13        1.0941841696115        0.011416338170286"
model_2.7<-glm(is_promoted ~ `KPIs_met >80%` + `awards_won?` + departmentFinance + 
                 departmentHR + departmentLegal + departmentOperations + departmentProcurement + 
                `departmentSales & Marketing` + departmentTechnology + 
                 regionregion_11 + regionregion_12 + regionregion_15 + regionregion_17 + 
                 avg_training_score19 + regionregion_20 + regionregion_21 + regionregion_22 + 
                 regionregion_23 + regionregion_24 + regionregion_25 + regionregion_28 + 
                 regionregion_29 + regionregion_3 + regionregion_31 + regionregion_32 + 
                 regionregion_34 + regionregion_4 + regionregion_5 + regionregion_6 + 
                  regionregion_9 +  
                 `educationBelow Secondary` + 
                 no_of_trainings2 + no_of_trainings4 + no_of_trainings5 + 
                 age3 + age9 +  
                 age12 + age14 +  age16 + age18 + age19 + age20 +
                 length_of_service8 + 
                 length_of_service9 + length_of_service10 + length_of_service13 + 
                 length_of_service14 +  avg_training_score5 + 
                 avg_training_score6 + avg_training_score7 + avg_training_score8 + 
                 avg_training_score9 + avg_training_score10 + avg_training_score11 + 
                 avg_training_score12 + avg_training_score14 + 
                 avg_training_score15 + avg_training_score16 + avg_training_score17 + 
                 avg_training_score18 +  previous_year_rating2 + 
                 previous_year_rating3 + previous_year_rating4 + previous_year_rating5, 
               family = "binomial", data = train)

summary(model_2.7)
#AIC: 15181 , slight increase

next.iteration(model_2.6)# none of teh high vif are less significant
# we will use teh p-values for deletion
pvalue.based.sort(model_2.6)
###########################################################################


#######################################################
#3.0.Model Validation
#######################################################
#below are the model we will consider for validation

Model_for_testing <- list(model_2.4,model_2.5)


no_of_model<-length(Model_for_testing)

result_matrix<-c()
for  (i in 1:no_of_model){
  test_pred = predict(Model_for_testing[[i]], type = "response", 
                      newdata = test[,-4])#ignoring is_promoted
  
  test_pred_churn <- factor(ifelse(test_pred >= 0.5, "Yes", "No"))
  test_actual_churn <- factor(ifelse(test$is_promoted, "Yes", "No"))
  test_conf <- caret::confusionMatrix(data=test_pred_churn, reference=test_actual_churn, positive = "Yes")
  result_matrix<-rbind(result_matrix,c(test_conf$overall["Accuracy"],
                                       test_conf$byClass["Sensitivity"],
                                       test_conf$byClass["Specificity"]))
  
}


result_matrix
View(result_matrix)
#     Accuracy Sensitivity Specificity
#[1,] 0.935166   0.2900000   0.9952134
#model_2.4 is the one that is using least varaible and comparable Sensitivity & Specificity

# let's check for varaious value for conversion what are accuracy, specifity 
# and sesnstivity of out choosen model
result_matrix<-data.frame()
str(result_matrix)
for  (i in seq(0.5,.05,-.01)){
  test_pred = predict(model_2.4, type = "response", 
                      newdata = test[,-4])
  
  test_pred_churn <- factor(ifelse(test_pred >= i, "Yes", "No"))
  test_actual_churn <- factor(ifelse(test$is_promoted, "Yes", "No"))
  test_conf <- caret::confusionMatrix(data=test_pred_churn, reference=test_actual_churn, positive = "Yes")
  result_matrix<-rbind(result_matrix,c(i,test_conf$overall["Accuracy"],
                                       test_conf$byClass["Sensitivity"],
                                       test_conf$byClass["Specificity"],
                                       (2*(test_conf$byClass["Sensitivity"]*test_conf$byClass["Specificity"])/(test_conf$byClass["Sensitivity"]+test_conf$byClass["Specificity"]))
                                       
                                       ))
  
}
colnames(result_matrix)<-c("Check_value","Accuracy","Sensitivity","Specificity","F-score")
View(result_matrix)
result_matrix
#ideal cutoff lies between 0.07 and 0.09


# let's check for varaious value for conversion what are accuracy, specifity 
# and sesnstivity of out choosen model
result_matrix<-data.frame()
str(result_matrix)
for  (i in seq(0.09,.07,-.001)){
  test_pred = predict(model_2.4, type = "response", 
                      newdata = test[,-4])
  
  test_pred_churn <- factor(ifelse(test_pred >= i, "Yes", "No"))
  test_actual_churn <- factor(ifelse(test$is_promoted, "Yes", "No"))
  test_conf <- caret::confusionMatrix(data=test_pred_churn, reference=test_actual_churn, positive = "Yes")
  result_matrix<-rbind(result_matrix,c(i,test_conf$overall["Accuracy"],
                                       test_conf$byClass["Sensitivity"],
                                       test_conf$byClass["Specificity"],
                                       (2*(test_conf$byClass["Sensitivity"]*test_conf$byClass["Specificity"])/(test_conf$byClass["Sensitivity"]+test_conf$byClass["Specificity"]))
                                       
  ))
  
}
colnames(result_matrix)<-c("Check_value","Accuracy","Sensitivity","Specificity","F-score")
View(result_matrix)
result_matrix

# 0.085 0.7801362   0.8178571   0.7766254 0.7967082
# we will use 0.085 as our differntiator with highest f-score

##################################
#final model
test_pred = predict(model_2.4, type = "response", newdata = test[,-4])



test_pred_churn <- factor(ifelse(test_pred >= 0.085, "Yes", "No"))
test_actual_churn <- factor(ifelse(test$is_promoted, "Yes", "No"))
test_conf <- caret::confusionMatrix(data=test_pred_churn, reference=test_actual_churn, positive = "Yes")
test_conf
# Accuracy : 0.7801    
# Sensitivity : 0.81786         
# Specificity : 0.77663


##################################################################################################
### KS -statistic - Test Data ######
##################################################################################################

test_cutoff_churn <- ifelse(test_pred_churn=="Yes",1,0)
test_actual_churn <- ifelse(test_actual_churn=="Yes",1,0)

#on testing  data
pred_object_test<- prediction(test_cutoff_churn, test_actual_churn)
pred_object_test
performance_measures_test<- performance(pred_object_test, "tpr", "fpr")
performance_measures_test



ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)#1] 0.5944826



####################################################################
# Lift & Gain Chart 
####################################################################

# plotting the lift chart

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Churn_decile = lift(test_actual_churn, test_pred, groups = 10)

Churn_decile


####################################################################
#lets use boot straping to test the stability of the model
####################################################################

set.seed(100)

#choosing a sample size of 35 
ratio_row<-35/nrow(test)
result_matrix<-data.frame()
for (c in 1:500){
  indices = sample.split(test$is_promoted, SplitRatio = ratio_row)
  test_sample = test[!(indices),]
  
  
  test_pred = predict(model_2.4, type = "response", 
                      newdata = test_sample[,-4])
  
  test_pred_churn <- factor(ifelse(test_pred >= 0.08, "Yes", "No"))
  test_actual_churn <- factor(ifelse(test_sample$is_promoted, "Yes", "No"))
  test_conf <- caret::confusionMatrix(data=test_pred_churn, reference=test_actual_churn, positive = "Yes")
  
  result_matrix<-rbind(result_matrix,c(c,test_conf$overall["Accuracy"],
                                       test_conf$byClass["Sensitivity"],
                                       test_conf$byClass["Specificity"],
                                       (2*(test_conf$byClass["Sensitivity"]*test_conf$byClass["Specificity"])/(test_conf$byClass["Sensitivity"]+test_conf$byClass["Specificity"]))))
  
}


result_matrix
View(result_matrix)
colnames(result_matrix)<-c("Check_value","Accuracy","Sensitivity","Specificity","F_score")


#Accuracy
c(mean(result_matrix$Accuracy),max(result_matrix$Sensitivity),min(result_matrix$Specificity))
# 0.7702834 0.8052971 0.7666889
#Sensitivity
c(mean(result_matrix$Sensitivity),max(result_matrix$Sensitivity),min(result_matrix$Sensitivity))
# 0.8035820 0.8052971 0.8031496
#Specificity
c(mean(result_matrix$Specificity),max(result_matrix$Specificity),min(result_matrix$Specificity))
# 0.7671843 0.7678215 0.7666889
#F_score
c(mean(result_matrix$F_score),max(result_matrix$F_score),min(result_matrix$F_score))
# 0.7849613 0.7858684 0.7844958

#############################################################
#Actual_predcition

# we trained teh data on complete train data set before making actual prediction

test_pred = predict(model_2.4, type = "response", 
                    newdata = Final_test_data)

test_pred_churn <- ifelse(test_pred >= 0.085, 1, 0)
unique(test_pred_churn)

result_set<-data.frame(cbind(employee_id=Final_test_data$employee_id,
                             is_promoted=test_pred_churn))
summary(result_set)
unique(result_set$is_promoted)
write.csv(result_set,'Submission_ordinal.csv',quote=FALSE,row.names =FALSE)
#########################################################################
