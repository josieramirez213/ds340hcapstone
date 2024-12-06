#Josie Ramirez
#DS 340


## Downloading Data

#Focusing on getting the needed variables for one year, 2023. Once I am able to do one year, I can do the same for the other seven years


getwd()
#setwd("C:/Users/Ramirez/OneDrive/Desktop/Cla$$/Fall '24/ds340h/atusRQ")
#C:\Users\Ramirez\OneDrive\Desktop\ds340hcapstone\atusRQ

setwd("C:/Users/Ramirez/OneDrive/Desktop/ds340hcapstone/code_and_data_files")

# act<-read.csv("atusact_2023.dat",header=TRUE)
# cpus<- read.csv("atuscps_2023.dat", header = TRUE)
# 
# head(act)
# head(cpus)



## Working with Activity 2023

library(tidyverse) #trying to use tidyverse rather than r [] syntax

#install.packages("lubridate")
#install.packages("hms")
library(lubridate)
library(hms)
#library(ggplot2) #already loaded with tidyverse




#loading in all the data sets--------------------------------- 

act23<-read.csv("atusact_2023.dat",header=TRUE)
act22<-read.csv("atusact_2022.dat",header=TRUE)
act21<-read.csv("atusact_2021.dat",header=TRUE)
act20<-read.csv("atusact_2020.dat",header=TRUE)
act19<-read.csv("atusact_2019.dat",header=TRUE)
act18<-read.csv("atusact_2018.dat",header=TRUE)
act17<-read.csv("atusact_2017.dat",header=TRUE)
act16<-read.csv("atusact_2016.dat",header=TRUE)



#making them into a list
myList = list()
myList[["df1"]] <- act23
myList[["df2"]] <- act22
myList[["df3"]] <- act21
myList[["df4"]] <- act20
myList[["df5"]] <- act19
myList[["df6"]] <- act18
myList[["df7"]] <- act17
myList[["df8"]] <- act16

#print(names(myList))

#lapply(myList, head)

#loop through every element in that list
y = 2023

dfList = list()


#--------------------------------- 
# Creating Function to merge calculate total time for all 8 years
#---------------------------------- 
for (i in myList){
  actY<- i %>%
    mutate(
      #total_mins = ifelse(
        #as_hms(TUSTOPTIME) < as_hms(TUSTARTTIM),  # Stop time is earlier (spanning midnight)
        #as.numeric(difftime(as_hms(TUSTOPTIME) + hours(24), as_hms(TUSTARTTIM), units = "mins")),  # Add 24 hours to stop time
        #as.numeric(difftime(as_hms(TUSTOPTIME), as_hms(TUSTARTTIM), units = "mins"))  # Normal case (no midnight span)
      #)
    #) %>%
      TUSTOPTIME = as_hms(TUSTOPTIME),
      TUSTARTTIM = as_hms(TUSTARTTIM),
      
      # Convert to numeric seconds to avoid errors with hms arithmetic
      TUSTOPTIME_sec = as.numeric(TUSTOPTIME),
      TUSTARTTIM_sec = as.numeric(TUSTARTTIM),
      
      # Calculate total_mins, handling midnight crossing manually
      total_mins = ifelse(
        TUSTOPTIME_sec < TUSTARTTIM_sec,  # If stop time is earlier (spanning midnight)
        (TUSTOPTIME_sec + 86400 - TUSTARTTIM_sec) / 60,  # Add 86400 seconds (24 hours) to stop time
        (TUSTOPTIME_sec - TUSTARTTIM_sec) / 60  # Normal case (no midnight span)
      )
    ) %>%
    
    #mutate(total_mins = as.numeric(difftime(as_hms(TUSTOPTIME), as_hms(TUSTARTTIM), units = "mins")) ) %>%
    mutate(year = y)
  
  ##Taking only the columns I want
  
  newActY<- actY %>%
    filter((TUTIER1CODE == 3 | TUTIER1CODE == 4) &
             (TUTIER2CODE == 1 | TUTIER2CODE == 2 | TUTIER2CODE == 3)
    ) %>%
    select(TUCASEID, TUACTIVITY_N, TUSTARTTIM, TUSTOPTIME, TUTIER1CODE, TUTIER2CODE, TUTIER3CODE, total_mins, year)
  
  dfList[[paste0("year", y)]] <- newActY
  #myList[[as.character(y)]] <- newActY
  
  
  y<- y-1
}

print(names(dfList))

lapply(dfList, head)


#--------------------------------- 
# Merging all the datasets, rbind


full_df<- rbind(dfList[["year2023"]], dfList[["year2022"]], 
                dfList[["year2021"]], dfList[["year2020"]], 
                dfList[["year2019"]], dfList[["year2018"]], 
                dfList[["year2017"]], dfList[["year2016"]])

head(full_df)
tail(full_df)
dim(full_df)
range(full_df$TUTIER2CODE)
range(full_df$total_mins)
#from 1 min to almost 24 hours

#if tucaseid is the same and 1code is 3 and 2code is 1, 2, 3, i want to add total_mins_hh
# 1code is 4 and 2code is 1, 2, 3, i want to add total_mins_nhh
# after that i can do total for each row and add them together

# full_df2<- full_df %>%
#   group_by(TUCASEID, TUTIER1CODE) %>%
#   summarise(total_mins2 = sum(total_mins))
# 
# head(full_df2)
# tail(full_df2)
# dim(full_df2)
anyNA((full_df$TUTIER1CODE))
full_df2<- full_df %>%
  group_by(TUCASEID, year) %>%
  summarize(
    
    hh_totalmins = sum(total_mins[TUTIER1CODE ==3]),
    
    nhh_totalmins = sum(total_mins[TUTIER1CODE ==4]),
    .groups = "drop"
  )
head(full_df2)
full_df2<- as.data.frame(full_df2)
head(full_df2)
range(full_df2$nhh_totalmins)
range(full_df2$hh_totalmins)
range(full_df2$year)

dim(full_df2)
#18061, 4
length(unique((full_df2$TUCASEID))) #18061

#invalid_rows <- full_df[as_hms(full_df$TUSTARTTIM) > as_hms(full_df$TUSTOPTIME), ]
#print(invalid_rows)

#--------------------------------- will possibly return to later--------------------------------- 
## Total Time for Household and NonHousehold Children
# time<- full_df %>%
#   group_by(TUTIER1CODE) %>%
#   summarise(mean_time = mean(total_mins), sum_mins = sum(total_mins), sum_hr = sum(total_mins)/60)
# time
# #--------------------------------- 
# ## Household Children: Total Time Spent by Tier2Code
# timeHH_type<- full_df %>%
#   filter(TUTIER1CODE==3)%>%
#   group_by(TUTIER2CODE) %>%
#   summarise(mean_time = mean(total_mins), sum_mins = sum(total_mins), sum_hr = sum(total_mins)/60)
# 
# timeHH_type
# #--------------------------------- 
# ## NonHousehold Children: Total Time Spent by Tier2Code
# 
# timeNH_type<- full_df %>%
#   filter(TUTIER1CODE==4)%>%
#   group_by(TUTIER2CODE) %>%
#   summarise(mean_time = mean(total_mins), sum_mins = sum(total_mins), sum_hr = sum(total_mins)/60)
# 
# timeNH_type


#--------------------------------- 
## Visual showing difference between total time spent between household and nonhousehold children

#
head(full_df2)

# ggplot(full_df, aes(x = factor(TUTIER1CODE), y = total_mins)) +
#   geom_violin(trim = FALSE) +
#   labs(x = "Children", y = "Total Time Spent with Child", title = paste("In the past 8 years, total time spent caring for and helping","\nhousehold and nonhousehold children, capped at 250")) + theme_minimal() +
#   scale_x_discrete(labels = c("Household Children", "Nonhousehold Children")) +  # Custom labels +
#   
#   # Add mean and median
#   stat_summary(aes(color = "Mean"), fun = mean, geom = "point",size = 1) +  
#   stat_summary(aes(color = "Median"), fun = median, geom = "point", size = 1) +
#   
#   scale_color_manual(values = c("Mean" = "blue", "Median" = "yellow")) +  # Custom colors for legend
#   guides(color = guide_legend(title = NULL))  +
#   
#   #scale_y_continuous(limits = c(0, 250), breaks = seq(0, 650, by = 50)) +
#   theme(plot.title = element_text(hjust = 0.5))

anyNA(full_df$total_mins)
#says false, meaning there not
#-------------------------------------------------------------------------------------------------
# jpeg("time_w_hh_nhh.jpg", width = 8, height = 6, units = "in", res = 300)
# 
# ggplot(full_df, aes(x = factor(TUTIER1CODE), y = log(total_mins))) +
#   geom_violin(trim = FALSE) +
#   labs(x = "Children", y = "Total Time Spent with Child", title = paste("In the past 8 years, logged total time spent caring for and helping","\nhousehold and nonhousehold children")) + theme_minimal() +
#   scale_x_discrete(labels = c("Household Children", "Nonhousehold Children")) +  # Custom labels +
#   
#   # Add mean and median
#   stat_summary(aes(color = "Mean"), fun = mean, geom = "point",size = 1) +  
#   stat_summary(aes(color = "Median"), fun = median, geom = "point", size = 1) +
#   
#   scale_color_manual(values = c("Mean" = "blue", "Median" = "yellow")) +  # Custom colors for legend
#   guides(color = guide_legend(title = NULL))  +
#   
#   theme(plot.title = element_text(hjust = 0.5))
# 
# dev.off()






#Trying to Merge Activity with Income

head(full_df)
colnames(full_df)
dim(full_df)
#67039   

head(full_df2)
colnames(full_df2)
dim(full_df2) #18061 4

full_df2$total<- full_df2$hh_totalmins + full_df2$nhh_totalmins
head(full_df2)
tail(full_df2)
colnames(full_df2)
dim(full_df2) #18061 5


#reading in cpus
#loading in all the data sets--------------------------------- 

cpus23<-read.csv("atuscps_2023.dat",header=TRUE)
colnames(cpus23)
#PTDTRACE, PESEX
cpus22<-read.csv("atuscps_2022.dat",header=TRUE)
cpus21<-read.csv("atuscps_2021.dat",header=TRUE)
cpus20<-read.csv("atuscps_2020.dat",header=TRUE)
cpus19<-read.csv("atuscps_2019.dat",header=TRUE)
cpus18<-read.csv("atuscps_2018.dat",header=TRUE)
cpus17<-read.csv("atuscps_2017.dat",header=TRUE)
cpus16<-read.csv("atuscps_2016.dat",header=TRUE)



#making them into a list
myList2 = list()
myList2[["df1"]] <- cpus23
myList2[["df2"]] <- cpus22
myList2[["df3"]] <- cpus21
myList2[["df4"]] <- cpus20
myList2[["df5"]] <- cpus19
myList2[["df6"]] <- cpus18
myList2[["df7"]] <- cpus17
myList2[["df8"]] <- cpus16

print(names(myList2))

#lapply(myList, head)

#loop through every element in that list
y = 2023

dfList2 = list()


#--------------------------------- 
# Creating Function to select desired columns from each cps file 
#---------------------------------- 
for (i in myList2){
  
  cpsY<- i %>%
    mutate(year = y) %>% 
    filter(TULINENO==1) %>%
    #1 bcus that is the respondent whose activity info we have 
    select(TUCASEID, HEFAMINC, PRCITSHP, PEMARITL, PRTAGE, PTDTRACE, PESEX)
    ##Taking only the columns I want
  
  
  dfList2[[paste0("year", y)]] <- cpsY
  #myList[[as.character(y)]] <- newActY
  
  
  y<- y-1
}

print(names(dfList2))

lapply(dfList2, head)

#--------------------------------- 
# Merging all the cpus data, rbind


cps_df2<- rbind(dfList2[["year2023"]], dfList2[["year2022"]], 
                dfList2[["year2021"]], dfList2[["year2020"]], 
                dfList2[["year2019"]], dfList2[["year2018"]], 
                dfList2[["year2017"]], dfList2[["year2016"]])

head(cps_df2)
tail(cps_df2)
dim(cps_df2)

#--------------------------------- 
#Trying to Merge Activity with Income

head(full_df)
colnames(full_df)
dim(full_df)

head(full_df2)
colnames(full_df2)
dim(full_df2)

#for all those in full i want to add
merged_act_c<- full_df2 %>%
  left_join(select(cps_df2, TUCASEID, HEFAMINC, PRCITSHP, PEMARITL, PRTAGE, PTDTRACE, PESEX), by = "TUCASEID")

head(merged_act_c)
tail(merged_act_c)
dim(merged_act_c)

anyNA(merged_act_c)



#reading in resp
#loading in all the data sets--------------------------------- 

resp23<-read.csv("atusresp_2023.dat",header=TRUE)
colnames(resp23)
#TUDIARYDAY , TELFS , TRCHILDNUM, TRNUMHOU
#TUDIARYDAY
resp22<-read.csv("atusresp_2022.dat",header=TRUE)
resp21<-read.csv("atusresp_2021.dat",header=TRUE)
resp20<-read.csv("atusresp_2020.dat",header=TRUE)
resp19<-read.csv("atusresp_2019.dat",header=TRUE)
resp18<-read.csv("atusresp_2018.dat",header=TRUE)
resp17<-read.csv("atusresp_2017.dat",header=TRUE)
resp16<-read.csv("atusresp_2016.dat",header=TRUE)



#making them into a list
myList3 = list()
myList3[["df1"]] <- resp23
myList3[["df2"]] <- resp22
myList3[["df3"]] <- resp21
myList3[["df4"]] <- resp20
myList3[["df5"]] <- resp19
myList3[["df6"]] <- resp18
myList3[["df7"]] <- resp17
myList3[["df8"]] <- resp16

print(names(myList3))


#loop through every element in that list
y = 2023

dfList3 = list()


#--------------------------------- 
# Creating Function to select desired columns from each cps file 
#---------------------------------- 
for (i in myList3){
  
  respY<- i %>%
    mutate(year = y) %>% 
    filter(TULINENO==1) %>%
    #1 bcus that is the respondent whose activity info we have 
    select(TUCASEID, TUDIARYDAY, TELFS, TRCHILDNUM, TRNUMHOU)
    #TUDIARYDAY
  ##Taking only the columns I want
  
  
  dfList3[[paste0("year", y)]] <- respY
  #myList[[as.character(y)]] <- newActY
  
  
  y<- y-1
}

print(names(dfList3))

lapply(dfList3, head)


#--------------------------------- 
# Merging all the cpus data, rbind


resp_Df<- rbind(dfList3[["year2023"]], dfList3[["year2022"]], 
                dfList3[["year2021"]], dfList3[["year2020"]], 
                dfList3[["year2019"]], dfList3[["year2018"]], 
                dfList3[["year2017"]], dfList3[["year2016"]])

head(resp_Df)
tail(resp_Df)
#dim(resp_Df)

#--------------------------------- 
#Trying to Merge Act and CPS with Resp Variables 

head(merged_act_c)
colnames(merged_act_c)
dim(merged_act_c)


#for all those in full i want to add
fullMerge<- merged_act_c %>%
  left_join(select(resp_Df, TUCASEID, TUDIARYDAY, TELFS, TRCHILDNUM, TRNUMHOU), by = "TUCASEID")
  #TUDIARYDAY

head(fullMerge)
tail(fullMerge)
dim(fullMerge)
colnames(fullMerge)
#18061    





#mean of federal poverty guidelines 

#same for 48 states, plus Alaska and Hawaii
one_p<- c(12798, 15990, 14721)
two_p<- c(17290, 21609, 19888)
three_p<- c(21783, 27228, 25054)
four_p<- c(26275,32846, 30220)
five_p<- c(30768,98465,35386)
six_p<- c(35260,44084, 40553)
seven_p<- c(39754,49703,45719)
eight_p<- c(44249, 55324, 50888)
more_p<- c(4373, 5621, 5166)

meanOne<- mean(one_p)
meanTwo<- mean(two_p)
meanThree<- mean(three_p)
meanFour<- mean(four_p)
meanFive<- mean(five_p)
meanSix<- mean(six_p)
meanSeven<- mean(seven_p)
meanEight<- mean(eight_p)
meanMore<- mean(more_p)



head(fullMerge)
dim(fullMerge)
#predefined poverty guidelines
povertyG<- data.frame(
  houseSize = 1:9,
  guide = c(meanOne,meanTwo,meanThree, meanFour, meanFive, meanSix, meanSeven, meanEight, meanMore)
)
povertyG
povertyG[9,2]
#povertyG[9,2] +100

#income nums for coded income variable
income <- c(5000, 7499, 9999, 12499, 14999, 19999, 24999, 29999, 34999, 39999, 
            49999, 59999, 74999, 99999, 149999, 150000)
#giving col name basically 
names(income) <- 1:16
income

fullMerge$poverty<- NA
head(fullMerge)

for (i in 1:nrow(fullMerge)){
  #print(df[i, ])
  
  #getting income and household size for one row 
  earning<- income[fullMerge$HEFAMINC[i]] #variable goes to 
  #print(earning)
  houseSize<- fullMerge$TRNUMHOU[i]
  
  #if house size 1-8, then we have predefined threshold
  if (houseSize <= 8){
    povGuide<- povertyG$guide[povertyG$houseSize == houseSize]
  }
  #if greater than 8, then we need to calculate threshold
  else{
    last<- povertyG$guide[povertyG$houseSize == 8]
    povGuide<- last + (houseSize - 8) * povertyG[9,2]
    
  }
  
  if (earning < povGuide){
    fullMerge$poverty[i] <- 1 #below poverty line
  }
  else{
    fullMerge$poverty[i] <- 0 #above poverty line 
  }

}

head(fullMerge)
tail(fullMerge)
dim(fullMerge)
#18061

u <- unique(fullMerge$poverty)
print(u) #okay so 1 is there, just got nervous from head and tail


potentialNA <- anyNA(fullMerge)
potentialNA
#seems like there are no NA's 
#so know I can start analysis 
summary(fullMerge)


#------------------------------------------------#------------------------------------------------#------------------------------------------------
#running linear models 
head(fullMerge)
colnames(fullMerge)
dim(fullMerge)
length(unique(fullMerge$TUCASEID))
range(fullMerge$total) #30 hours and 5 minutes



#manipulating race col
#non-white is 0 and white is 1 
fullMerge$PTDTRACE[fullMerge$PTDTRACE != "1"] <- "0"
fullMerge$PTDTRACE[is.na(fullMerge$PTDTRACE)] <- "0"
unique(fullMerge$PTDTRACE)



#checking categorical and continuous vars
class(fullMerge$HEFAMINC)
fullMerge$HEFAMINC<- factor(fullMerge$HEFAMINC)
fullMerge$PRCITSHP <- factor(fullMerge$PRCITSHP )
fullMerge$PEMARITL <- factor(fullMerge$PEMARITL )
fullMerge$PTDTRACE <- factor(fullMerge$PTDTRACE )
fullMerge$PESEX<- factor(fullMerge$PESEX)
fullMerge$TUDIARYDAY <- factor(fullMerge$TUDIARYDAY )
fullMerge$TELFS <- factor(fullMerge$TELFS )
fullMerge$poverty<- factor(fullMerge$poverty)


#i want all these to be factors, not numerical 
class(fullMerge$HEFAMINC)
class(fullMerge$PRCITSHP)
class(fullMerge$PEMARITL)
class(fullMerge$PTDTRACE)
class(fullMerge$PESEX)
class(fullMerge$TUDIARYDAY)
class(fullMerge$TELFS)
class(fullMerge$poverty)

levels(fullMerge$PTDTRACE)

head(fullMerge)
summary(fullMerge)




df_predict1<- fullMerge %>%
  select(-c(TUCASEID, hh_totalmins, nhh_totalmins))

head(df_predict1)
class(df_predict1$TELFS)


set.seed(13)
train_rows<- sample(nrow(df_predict1), size = 0.8 *nrow(df_predict1))

test<- df_predict1[-train_rows, ]
head(test)
dim(test)
levels(test$PTDTRACE)

train<- df_predict1[train_rows, ]
head(train)
dim(train)
#okay now i have train and test data

#fitting linear regression model with all cols as predictors
m1<- lm(log(total) ~ . , data = train)
summary(m1)

predict1<- predict(m1, newdata = test)


rmseTrain<- sqrt(mean((m1$residuals)^2))
print(paste("Model pair RMSE train: ", rmseTrain))
rmse1<- sqrt(mean((log(test$total) - predict1)^2))
print(paste("Model 1 RMSE: ", rmse1))
bic1<- BIC(m1)
print(paste("Model 1 BIC: ", bic1))



#.^2
#poverty*.
#includes main effects and interactions, 
m2<- lm(log(total) ~ . + poverty:PEMARITL + poverty:PRTAGE + poverty:PESEX + poverty:TRCHILDNUM + poverty:TRNUMHOU , data = train)
summary(m2)

predict2<- predict(m2, newdata = test)


rmseTrain2<- sqrt(mean((m2$residuals)^2))
print(paste("Model pair RMSE train: ", rmseTrain2))
rmse2<- sqrt(mean((log(test$total) - predict2)^2))
print(paste("Model 2 RMSE: ", rmse2))
bic2<- BIC(m2)
print(paste("Model 2 BIC: ", bic2))
#------------------------------------------------#------------------------------------------------#------------------------------------------------
#this is the model i have decided is best, so I will now use 
#entire dataset to re train model and predict to get a fitted value
#and i will use this outcome for my anova analysis 

df_predict1
final_model_poverty<- lm(log(total) ~ . + poverty:PEMARITL + poverty:PRTAGE + poverty:PESEX + poverty:TRCHILDNUM + poverty:TRNUMHOU , data = df_predict1)
summary(final_model_poverty)





###
m_pair<- lm(log(total) ~ .^2 , data = train)
summary(m_pair)

predict_pair<- predict(m_pair, newdata = test)

rmseTrain_pair<- sqrt(mean((m_pair$residuals)^2))
print(paste("Model pair RMSE train: ", rmseTrain_pair))
rmse_pair<- sqrt(mean((log(test$total) - predict_pair)^2))
print(paste("Model pair RMSE test: ", rmse_pair))
bic_pair<- BIC(m_pair)
print(paste("Model pair BIC: ", bic_pair))







df_predict2<- fullMerge %>%
  select(-c(TUCASEID, hh_totalmins, nhh_totalmins, poverty))
head(df_predict2)

final_model_poverty<- lm(log(total) ~ . + poverty:PEMARITL + poverty:PRTAGE + poverty:PESEX + poverty:TRCHILDNUM + poverty:TRNUMHOU , data = df_predict1)
summary(final_model_poverty)

final_model_NOpoverty<- lm(log(total) ~. , data = df_predict2)
summary(final_model_NOpoverty)

bic_no<- BIC(final_model_NOpoverty)
print(paste("Model pair BIC: ", bic_no))



anova(final_model_poverty, final_model_NOpoverty)




#------------------------------------------------
#graphic of how many respondents

head(fullMerge)
boxplot(log(fullMerge$total)~fullMerge$poverty )
#main result, make it cute
jpeg("time_by_poverty.jpg", width =8, height = 6, units = "in", res = 300)

ggplot(fullMerge, aes(x = factor(poverty), y = log(total))) +
  geom_violin(trim = FALSE) +
  labs(x = "Poverty Status", y = "Logged Total Time Spent with Children", title = paste("In the past 8 years, logged total time spent with children \nby federal poverty guideline"))+
  scale_x_discrete(labels = c("Above Poverty Guideline", "Below Poverty Guideline")) +  # Custom labels +

  
  
  # Add mean and median
  stat_summary(fun = mean, geom = "point",size = 1, color = "darkgreen", shape = 16) +
  stat_summary(fun = mean, geom = "text", size = 4, 
               vjust = 1.5, aes(label = round(..y.., 2)), color = "darkgreen") +
  
  stat_summary(fun = median, geom = "point", size = 1,color = "deeppink4", shape = 17) +

  scale_color_manual(name = "Summary Statistics", values = c("Mean" = "darkgreen", "Median" = "deeppink4"), labels = c("Mean", "Median")) +
  scale_shape_manual(name = "Summary Statistics", values = c("Mean" = 16, "Median" = 17), labels = c("Mean", "Median"))  +
                     #breaks = c("Mean", "Median")) +  # Custom colors for legend
  
  guides(color = guide_legend(title = "Summary Statistics"),
         shape = guide_legend(title = "Summary Statistics"))  +

  theme(plot.title = element_text(hjust = 0.5))

dev.off()

######################################################
jpeg("fitted_povertyModel.jpg", width = 8, height = 6, units = "in", res = 300)



df_fitted_resid <- data.frame(Fitted = final_model_poverty$fitted.values, Residuals = final_model_poverty$residuals)

# Plot with ggplot2
ggplot(df_fitted_resid, aes(x = Fitted, y = Residuals)) +
  geom_point(shape = 1, alpha = 0.5, color = "palegreen") +
  geom_hline(yintercept = 0, color = "black") +
  labs(title = "Residuals vs. Fitted Values Plot for Model 2, including poverty",
       x = "Fitted Values", y = "Residuals") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

dev.off()

######################################################
jpeg("fitted_NOpovertyModel.jpg", width = 8, height = 6, units = "in", res = 300)



df_fitted_resid <- data.frame(Fitted = final_model_NOpoverty$fitted.values, Residuals = final_model_NOpoverty$residuals)

# Plot with ggplot2
ggplot(df_fitted_resid, aes(x = Fitted, y = Residuals)) +
  geom_point(shape = 1, alpha = 0.5, color = "sandybrown") +
  geom_hline(yintercept = 0, color = "black") +
  labs(title = "Residuals vs. Fitted Values Plot for Model 4, excluding poverty",
       x = "Fitted Values", y = "Residuals") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

dev.off()


######################################################

jpeg("respo_poverty_status.jpg", width = 10, height = 4.5, units = "in", res = 300)

ggplot(fullMerge, aes(x = factor(poverty))) +
  geom_bar(stat = "count") +
  labs(x = "Poverty Status", y = "Number of Respondents", title = paste("Number of Respondents Below and Above Federal Poverty Guidelines")) + theme_minimal() +
  scale_x_discrete(labels = c("Above Poverty Guideline", "Below Poverty Guideline")) +  # Custom labels +
  
  # Add mean and median
  #stat_summary(aes(color = "Mean"), fun = mean, geom = "point",size = 1) +  
  #stat_summary(aes(color = "Median"), fun = median, geom = "point", size = 1) +
  
  #scale_color_manual(values = c("Mean" = "blue", "Median" = "yellow")) +  # Custom colors for legend
  #guides(color = guide_legend(title = NULL))  +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.2, size = 5)

dev.off()

######################################################

jpeg("interactions.jpg", width = 10, height = 6, units = "in", res = 300)

fullMerge$poverty <- factor(fullMerge$poverty, levels = c(1, 0), labels = c("Below Poverty", "Above Poverty"))


ggplot(fullMerge, aes(x = interaction(poverty, PEMARITL), y = log(total), fill = PEMARITL)) +
  geom_boxplot() +
  labs(x = "Poverty and Marital Status", 
       y = "Logged Total Time Spent with Children", 
       title = "Interaction between Poverty and Marital Status on Time Spent with Children") +
  scale_x_discrete(labels = c("Below Poverty\nMarried", "Above Poverty\nMarried", 
                              "Below Poverty\nMarried,\nspouse absent", "Above Poverty\nMarried,\nspouse absent",
                              "Below Poverty\nWidowed", "Above Poverty\nWidowed",
                             "Below Poverty\nDivorced", "Above Poverty\nDivored",
                             "Below Poverty\nSeparated", "Above Poverty\nSeparated",
                             "Below Poverty\nNever Married", "Above Poverty\nNever Married")) +
  #scale_fill_manual(values = c("skyblue", "salmon", "orange")) +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))




dev.off()


#--------------------------------- 
#--------------------------------- 
#--------------------------------- 
#Left off here
dim(fullMerge)
colnames(fullMerge)
range(fullMerge$hh_totalmins)
sum(fullMerge$nhh_totalmins==0)
range(fullMerge$nhh_totalmins)


jpeg("poverty_childtype.jpg", width = 8, height = 6, units = "in", res = 300)


fullMerge_long <- fullMerge %>%
  pivot_longer(cols = c(hh_totalmins, nhh_totalmins), 
               names_to = "child_time_type", 
               values_to = "child_time")


ggplot(fullMerge_long, aes(x = factor(poverty), y = log(child_time + 1), fill = child_time_type)) +
    geom_violin(trim = FALSE, alpha = 0.6) +
  
  stat_summary(fun = "mean", geom = "text", aes(label = round(..y.., 1)), 
               position = position_dodge(width = 1.25), size = 4, vjust = 2.70, color = "black") +
  
  scale_fill_manual(values = c("mediumpurple1", "lightpink2"), 
                    labels = c("Household Children", "Nonhousehold Children")) +  # Customize legend labels
  
  labs(x = "Poverty Status", 
       y = "Logged Time Spent with Children", 
       title = "Time Spent with Children by Poverty Status and Type of Time") +
  
  scale_x_discrete(labels = c("0" = "Above Poverty Guideline", "1" = "Below Poverty Guideline")) + 
  
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  
        legend.title = element_blank(), 
        legend.position = "top")  

dev.off()











#############
# code I'm not using anymore 
#############

set.seed(333)
train_rows2<- sample(nrow(df_predict2), size = 0.8 *nrow(df_predict2))

test2<- df_predict2[-train_rows2, ]
head(test2)
dim(test2)
train2<- df_predict2[train_rows2, ]
head(train2)
dim(train2)

m3<- lm(log(total) ~ . , data = train2)
summary(m3)

predict3<- predict(m3, newdata = test2)

rmse3<- sqrt(mean((log(test2$total) - predict3)^2))
print(paste("Model 3 RMSE: ", rmse3))


#includes main effects and interactions, pairwise
m4<- lm(log(total) ~ .^2 , data = train2)
summary(m4)

predict4<- predict(m4, newdata = test2)

rmse4<- sqrt(mean((log(test2$total) - predict4)^2))
print(paste("Model 4 RMSE: ", rmse4))




levels(train$PTDTRACE)

#keep getting an error with race so trying to ensure factors are the same
# train$PTDTRACE<- factor(train$PTDTRACE)
# class(train$PTDTRACE)
# levels(train$PTDTRACE)

# test$PTDTRACE<- factor(test$PTDTRACE, levels = levels(train$PTDTRACE))
# class(test$PTDTRACE)
# levels(test$PTDTRACE)

# act<-read.csv("atusact_2023.dat",header=TRUE)
# cpus<- read.csv("atuscps_2023.dat", header = TRUE)
# 
# r<- read.csv("atusresp_2023.dat", header = TRUE)
# newR<- r %>%
#   select(TUCASEID, TRNUMHOU)
# head(newR)
# # 
# head(act)
# head(cpus)
# 
# 
# #variables wanted: TUCASEID, TESEX, TUACTIVITY_N, TUSTARTTIM, TUSTOPTIME, TUTIER1CODE, TUTIER2CODE, TUTIER3CODE
# 
# newAct<- act %>%
#   filter((TUTIER1CODE == 03 | TUTIER1CODE == 04) &
#            (TUTIER2CODE == 1 | TUTIER2CODE == 2 | TUTIER2CODE == 3)
#   ) %>%
#   select(TUCASEID, TUACTIVITY_N, TUSTARTTIM, TUSTOPTIME, TUTIER1CODE, TUTIER2CODE, TUTIER3CODE)
# 
# # #TESEX not in activity would need to see another file
# # 
# head(newAct)
# head(cpus)
# 
# 
# #i want to add the income variable for all tucaseid in newAct, other values drop
# 
# mergedDf<- newAct %>%
#   left_join(select(cpus, TUCASEID, HEFAMINC), by = "TUCASEID")
# 
# head(mergedDf)
# tail(mergedDf)
# 
# new_mergedDf<- mergedDf %>%
#   left_join(select(newR, TUCASEID, TRNUMHOU), by = "TUCASEID")
# 
# head(new_mergedDf)
# tail(new_mergedDf)


#realized i also need the total respondents to be able to calculate the federal measurement code


# grouped23<- newAct %>%
#   group_by(TUTIER1CODE) %>%
#   summarise(mean_time = mean(total_mins), sum_mins = sum(total_mins), sum_hr = sum(total_mins)/60)
# grouped23


# grouped23_type<- newAct %>%
#   filter(TUTIER1CODE==3)%>%
#   group_by(TUTIER2CODE) %>%
#   summarise(mean_time = mean(total_mins), sum_mins = sum(total_mins), sum_hr = sum(total_mins)/60)
# 
# grouped23_type


## Focusing on getting total time for Household Children

# class(newAct$TUSTARTTIM)
# #need to convert start and stop times 
# 
#
# 
# hh <- newAct[newAct$TUTIER1CODE == 3, ]
# dim(hh)
# #these indicate specifically household children care
# 
# hh<- hh %>%
#   mutate(total_mins = as.numeric(difftime(as_hms(TUSTOPTIME), as_hms(TUSTARTTIM), units = "mins")) )
# 
# dim(hh)
# head(hh)
# 
# 
# ## For household children, this is the total amount of time spent
# 
# minsBy2Code <- aggregate(total_mins ~ TUTIER2CODE, data = hh, FUN = sum)
# minsBy2Code
# 
# 
# 
# ## Focusing on getting total time for Non Household Children
# 
# nonhh <- newAct[newAct$TUTIER1CODE == 4, ]
# dim(hh)
# #these indicate specifically household children care
# 
# nonhh<- nonhh %>%
#   mutate(total_mins = as.numeric(difftime(as_hms(TUSTOPTIME), as_hms(TUSTARTTIM), units = "mins")) )
# 
# head(nonhh)
# dim(nonhh)
# 
# minsBy2Code <- aggregate(total_mins ~ TUTIER2CODE, data = nonhh, FUN = sum)
# minsBy2Code
# 
# ## Combing datasets so info is all in one
# act23 <- merge(hh, nonhh, by = "TUCASEID", all = FALSE)
# head(act23)
# tail(act23)
# #not really sure
# #-------------ask



ggplot(fullMerge, aes(x = factor(poverty), y = log(total))) +
  geom_violin(trim = FALSE) +
  labs(x = "Poverty Status", y = "Logged Total Time Spent with Children", title = paste("In the past 8 years, logged total time spent with children \nby federal poverty guideline"))+
  scale_x_discrete(labels = c("Above Poverty Guideline", "Below Poverty Guideline")) +  # Custom labels +
  
  
  
  # Add mean and median
  stat_summary(fun = mean, geom = "point",size = 1, aes(color = "Mean", shape = "Mean")) +
  stat_summary(fun = mean, geom = "text", size = 4, 
               vjust = 1.5, aes(label = round(..y.., 2), color = "Mean")) +
  
  stat_summary(fun = median, geom = "point", size = 1,  aes(color = "Median", shape = "Median")) +
  
  scale_color_manual(values = c("Mean" = "darkgreen", "Median" = "deeppink4")) +
  scale_shape_manual(values = c("Mean" = 16, "Median" = 16)) +
  #breaks = c("Mean", "Median")) +  # Custom colors for legend
  guides(color = guide_legend(title = "Summary Statistics"),
         shape = guide_legend(title = "Summary Statistics"))  +
  
  theme(plot.title = element_text(hjust = 0.5))

