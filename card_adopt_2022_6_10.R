### See revision notes on G Doc online.
#
# The following packages are used:
library(formattable)# has percent function
#library(plotrix)# weighted histograms
library(dplyr)
library(xtable)# for LaTeX tables
library(writexl)# export to Excel 
#library(ggplot2)
library(spatstat) # for weighted.median
library(mfx)
library(texreg)# exports regression result to LaTeX (just like stargazer) or HTML (can be read by Excel or Word)
#library(regclass) # for confusion_matrix
#library(nnet) # for multinomial logit
#library(AER) # for p-values of nnet multinom logit coeftest(regression)
#library(rpart)
#library(rpart.plot)
#library(partykit)# modifies rpart tree plot
#library("randomForest")

setwd("~/Papers/card_adopt/card_adopt_coding")
dir()

### Reading RDS datasets
#c1.df = readRDS("dcpc_2021_merged.rds")
i1.df = readRDS("indiv_2022_5_16.rds")# indiv data
t1.df = readRDS("trans_2022_5_16.rds")# trans data
d1.df = readRDS("day_2022_5_16.rds")# day data

# Remove 289 respondents who did not complete 4 logins
length(unique(subset(d1.df, is.na(login_date) )$id))# num of respondents who did not log in in ALL 4 days. To be removed from the sample. 
# now create a list of the id of these respondents (to be removed from m1.df) # this was added in the 2022_6_7 R-code
miss_login.vec = subset(d1.df, is.na(login_date) )$id
length(miss_login.vec)
# remove duplication
miss_login2.vec = unique(miss_login.vec)
length(miss_login2.vec)# num resp to be removed

# remove 289 respondents from all 3 datasets
i2.df = subset(i1.df, !(id %in% miss_login2.vec))
nrow(i1.df)
nrow(i2.df)# new num resp
#
t2.df = subset(t1.df, !(id %in% miss_login2.vec))
nrow(t1.df)
nrow(t2.df)# new num of trans
#
d2.df = subset(d1.df, !(id %in% miss_login2.vec))
nrow(d1.df)
nrow(d2.df)# new number of diary days 
#head(d2.df, 100)# each id appears 4 times (4 login days)
#

# remove transactions made in September 29 and 30 
nrow(t2.df)# num trans including September
table(t2.df$date)
nrow(subset(t2.df, date >= "2021-10-01"))
nrow(t2.df) - nrow(subset(t2.df, date >= "2021-10-01"))
#
t2_temp.df = subset(t2.df, date >= "2021-10-01")
nrow(t2_temp.df)
table(t2_temp.df$date)
t2.df = t2_temp.df
nrow(t2.df)

### Table 1: Individual data only
table(i2.df$dc_adopt)
sum(is.na(i2.df$dc_adopt))# num resp with NA
length(i2.df[is.na(i2.df$dc_adopt), ]$id)# same as above
# num resp who reported no DC but have a checking acc.
nrow(subset(i2.df, dc_adopt==0 & chk_acnt_adopt==1))
# num resp who did not report DC but have a checking acc and CC 
nrow(subset(i2.df, is.na(dc_adopt) & chk_acnt_adopt==1 & cc_adopt==1))
#
# num resp who did not report DC but have a checking acc.
nrow(subset(i2.df, is.na(dc_adopt) & chk_acnt_adopt==1))
# Assumption: the above do have a debit card (b/c they have a checking account)
i3.df = i2.df
i3.df[which(i3.df$chk_acnt_adopt==1), ]$dc_adopt=1
table(i2.df$dc_adopt)
table(i3.df$dc_adopt)# after making the dc_adopt==NA who are banked own a DC
sum(is.na(i3.df$dc_adopt))
# remove this resp
#i3.df = subset(i2.df, !is.na(dc_adopt))
nrow(i1.df)
nrow(i2.df)
nrow(i3.df) # => no loss of resp
# are there missing cc_adopt
sum(is.na(i3.df$cc_adopt))

## rescaling indiv weights
nrow(i3.df)
sum(i3.df$ind_weight)
i3.df$ind_weight2 = nrow(i3.df)*i3.df$ind_weight/sum(i3.df$ind_weight)
sum(i3.df$ind_weight2)

## dividing into 3 groups
i3.df$adopt=NA
i3.df$adopt[(i3.df$dc_adopt==1 & i3.df$cc_adopt==1) | (i3.df$dc_adopt==0 & i3.df$cc_adopt==1)] = "Group1"
i3.df$adopt[i3.df$dc_adopt==1 & i3.df$cc_adopt==0] = "Group2"
i3.df$adopt[i3.df$dc_adopt==0 & i3.df$cc_adopt==0] = "Group3"
#
table(i3.df$adopt)
sum(table(i3.df$adopt))
nrow(i3.df)

# group 1, 2, 3 trans
group1_indiv.df = subset(i3.df, adopt=="Group1")
group2_indiv.df = subset(i3.df, adopt=="Group2")
group3_indiv.df = subset(i3.df, adopt=="Group3")
nrow(i3.df)
nrow(group1_indiv.df) + nrow(group2_indiv.df) + nrow(group3_indiv.df)
#

## column All in Table 1
(all_cc_adopt_frac = sum(subset(i3.df, cc_adopt==1)$ind_weight2)/sum(i3.df$ind_weight2) )
#
(all_dc_adopt_frac = sum(subset(i3.df, dc_adopt==1)$ind_weight2)/sum(i3.df$ind_weight2) )
#
(all_num_resp = sum(i3.df$ind_weight2))
#
(all_resp_frac = sum(i3.df$ind_weight2)/sum(i3.df$ind_weight2))
#
(all_male_frac = sum(i3.df[which(i3.df$gender==1), ]$ind_weight2)/all_num_resp )
#
(all_age_med = weighted.median(i3.df$age, i3.df$ind_weight2))
#
(all_white_frac = sum(i3.df[which(i3.df$race==1), ]$ind_weight2)/all_num_resp)
#
(all_latinx_frac = sum(i3.df[which(i3.df$hispaniclatinx==1), ]$ind_weight2)/all_num_resp)
#
(all_edu_frac = sum(i3.df[which(i3.df$highest_education > 10), ]$ind_weight2)/all_num_resp)# assoc degree and higher
#
(all_income_med = weighted.median(i3.df$income_hh, i3.df$ind_weight2))
# 
# Finalizing the All column in Table 1
(all_indiv.vec = round(c(100*all_cc_adopt_frac, 100*all_dc_adopt_frac, all_num_resp, 100*all_resp_frac, all_male_frac, all_age_med, all_white_frac, all_latinx_frac, all_edu_frac, all_income_med), 2))

## column group 1 in Table 1
(group1_cc_adopt_frac = sum(subset(group1_indiv.df, cc_adopt==1)$ind_weight2)/sum(group1_indiv.df$ind_weight2) )
#
(group1_dc_adopt_frac = sum(subset(group1_indiv.df, dc_adopt==1)$ind_weight2)/sum(group1_indiv.df$ind_weight2) )
#
(group1_num_resp = sum(group1_indiv.df$ind_weight2))
#
(group1_resp_frac = sum(group1_indiv.df$ind_weight2)/sum(i3.df$ind_weight2))
#
(group1_male_frac = sum(group1_indiv.df[which(group1_indiv.df$gender==1), ]$ind_weight2)/group1_num_resp )
#
(group1_age_med = weighted.median(group1_indiv.df$age, group1_indiv.df$ind_weight2))
#
(group1_white_frac = sum(group1_indiv.df[which(group1_indiv.df$race==1), ]$ind_weight2)/group1_num_resp)
#
(group1_latinx_frac = sum(group1_indiv.df[which(group1_indiv.df$hispaniclatinx==1), ]$ind_weight2)/group1_num_resp)
#
(group1_edu_frac = sum(group1_indiv.df[which(group1_indiv.df$highest_education > 10), ]$ind_weight2)/group1_num_resp)# assoc degree and higher
#
(group1_income_med = weighted.median(group1_indiv.df$income_hh, group1_indiv.df$ind_weight2))
#
# Finalizing the group column in Table 1
(group1_indiv.vec = round(c(100*group1_cc_adopt_frac, 100*group1_dc_adopt_frac, group1_num_resp, 100*group1_resp_frac, group1_male_frac, group1_age_med, group1_white_frac, group1_latinx_frac, group1_edu_frac, group1_income_med), 2))

## column group 2 in Table 1
(group2_cc_adopt_frac = sum(subset(group2_indiv.df, cc_adopt==1)$ind_weight2)/sum(group2_indiv.df$ind_weight2) )
#
(group2_dc_adopt_frac = sum(subset(group2_indiv.df, dc_adopt==1)$ind_weight2)/sum(group2_indiv.df$ind_weight2) )
#
(group2_num_resp = sum(group2_indiv.df$ind_weight2))
#
#
(group2_resp_frac = sum(group2_indiv.df$ind_weight2)/sum(i3.df$ind_weight2))
#
(group2_male_frac = sum(group2_indiv.df[which(group2_indiv.df$gender==1), ]$ind_weight2)/group2_num_resp )
#
(group2_age_med = weighted.median(group2_indiv.df$age, group2_indiv.df$ind_weight2))
#
(group2_white_frac = sum(group2_indiv.df[which(group2_indiv.df$race==1), ]$ind_weight2)/group2_num_resp)
#
(group2_latinx_frac = sum(group2_indiv.df[which(group2_indiv.df$hispaniclatinx==1), ]$ind_weight2)/group2_num_resp)
#
(group2_edu_frac = sum(group2_indiv.df[which(group2_indiv.df$highest_education > 10), ]$ind_weight2)/group2_num_resp)# assoc degree and higher
#
(group2_income_med = weighted.median(group2_indiv.df$income_hh, group2_indiv.df$ind_weight2))
#
# Finalizing the group column in Table 1
(group2_indiv.vec = round(c(100*group2_cc_adopt_frac, 100*group2_dc_adopt_frac, group2_num_resp, 100*group2_resp_frac, group2_male_frac, group2_age_med, group2_white_frac, group2_latinx_frac, group2_edu_frac, group2_income_med), 2))


## column group 3 in Table 1
(group3_cc_adopt_frac = sum(subset(group3_indiv.df, cc_adopt==1)$ind_weight2)/sum(group3_indiv.df$ind_weight2) )
#
(group3_dc_adopt_frac = sum(subset(group3_indiv.df, dc_adopt==1)$ind_weight2)/sum(group3_indiv.df$ind_weight2) )
#
(group3_num_resp = sum(group3_indiv.df$ind_weight2))
#
(group3_resp_frac = sum(group3_indiv.df$ind_weight2)/sum(i3.df$ind_weight2))
#
(group3_male_frac = sum(group3_indiv.df[which(group3_indiv.df$gender==1), ]$ind_weight2)/group3_num_resp )
#
(group3_age_med = weighted.median(group3_indiv.df$age, group3_indiv.df$ind_weight2))
#
(group3_white_frac = sum(group3_indiv.df[which(group3_indiv.df$race==1), ]$ind_weight2)/group3_num_resp)
#
(group3_latinx_frac = sum(group3_indiv.df[which(group3_indiv.df$hispaniclatinx==1), ]$ind_weight2)/group3_num_resp)
#
(group3_edu_frac = sum(group3_indiv.df[which(group3_indiv.df$highest_education > 10), ]$ind_weight2)/group3_num_resp)# assoc degree and higher
#
(group3_income_med = weighted.median(group3_indiv.df$income_hh, group3_indiv.df$ind_weight2))
#
# Finalizing the group column in Table 1
(group3_indiv.vec = round(c(100*group3_cc_adopt_frac, 100*group3_dc_adopt_frac, group3_num_resp, 100*group3_resp_frac, group3_male_frac, group3_age_med, group3_white_frac, group3_latinx_frac, group3_edu_frac, group3_income_med), 2))
#
# Building a column of variable names
indiv_var.vec = c("Credit card adopter", "Debit card adopter", "Number of respondents", "Share of all respondents", "Share of male", "Median age", "Share of white", "Share of hispanic", "Share of associate degree and higher", "Median household income")

## Building Table 1 data frame
(indiv.df = data.frame("Variable" = indiv_var.vec, "All" = all_indiv.vec, "Group_1"=group1_indiv.vec, "Group_2" =  group2_indiv.vec, "Group_3" = group3_indiv.vec) )
#
#Export to CSV
write.csv(indiv.df, "table_1.csv")

### Table 3: Descriptive stats
## Table 3: All column
(all_num_resp = sum(i3.df$ind_weight2))
#
table(i3.df$gender)
#
(all_male_frac = sum(subset(i3.df, gender==1)$ind_weight2)/all_num_resp)
#
(all_female_frac = sum(subset(i3.df, gender==0)$ind_weight2)/all_num_resp)
sum(all_male_frac+all_female_frac)
#
(all_age_under_25_frac = sum(subset(i3.df, age < 25)$ind_weight2)/all_num_resp)
#
(all_age_25_34_frac = sum(subset(i3.df, age >= 25 & age <= 34)$ind_weight2)/all_num_resp)
#
(all_age_35_44_frac = sum(subset(i3.df, age >= 35 & age <= 44)$ind_weight2)/all_num_resp)
#
(all_age_45_54_frac = sum(subset(i3.df, age >= 45 & age <= 54)$ind_weight2)/all_num_resp)
#
(all_age_55_64_frac = sum(subset(i3.df, age >= 55 & age <= 64)$ind_weight2)/all_num_resp)
#
(all_age_over_65_frac = sum(subset(i3.df, age >= 65)$ind_weight2)/all_num_resp)
#
all_age_under_25_frac + all_age_25_34_frac + all_age_35_44_frac + all_age_45_54_frac + all_age_55_64_frac + all_age_over_65_frac
#
table(i3.df$race)
#
(all_race_white_frac = sum(subset(i3.df, race==1)$ind_weight2)/all_num_resp)
#
(all_race_black_frac = sum(subset(i3.df, race==2)$ind_weight2)/all_num_resp)
#
(all_race_asian_frac = sum(subset(i3.df, race==4)$ind_weight2)/all_num_resp)
#
(all_race_other_frac = sum(subset(i3.df, race==3| race==5 | race==6)$ind_weight2)/all_num_resp)
#
all_race_white_frac + all_race_black_frac + all_race_asian_frac + all_race_other_frac
#
(all_latinx_frac = sum(i3.df[which(i3.df$hispaniclatinx==1), ]$ind_weight2)/all_num_resp)
#
(all_nonlatinx_frac = sum(i3.df[which(i3.df$hispaniclatinx==0), ]$ind_weight2)/all_num_resp)
#
all_latinx_frac + all_nonlatinx_frac
#
(all_citizen_frac = sum(subset(i3.df, citizen==1)$ind_weight2)/all_num_resp)
#
(all_noncitizen_frac = sum(subset(i3.df, citizen==0)$ind_weight2)/all_num_resp)
#
table(i3.df$highest_education)
#
(all_edu_less_high_school_frac = sum(subset(i3.df, highest_education < 9)$ind_weight2)/all_num_resp)
#
(all_edu_high_school_frac = sum(subset(i3.df, highest_education == 9)$ind_weight2)/all_num_resp)
#
(all_edu_some_college_frac = sum(subset(i3.df, highest_education == 10)$ind_weight2)/all_num_resp)
#
(all_edu_associate_frac = sum(subset(i3.df, highest_education == 11 | highest_education == 12 )$ind_weight2)/all_num_resp)
#
(all_edu_college_frac = sum(subset(i3.df, highest_education == 13)$ind_weight2)/all_num_resp)
#
(all_edu_graduate_frac = sum(subset(i3.df, highest_education > 13)$ind_weight2)/all_num_resp)
#
all_edu_less_high_school_frac + all_edu_high_school_frac + all_edu_some_college_frac + all_edu_associate_frac + all_edu_college_frac + all_edu_graduate_frac
#
summary(i3.df$income_hh)
# computing the sum of weights of the 95 NA resp
(all_income_na = sum(subset(i3.df, is.na(income_hh))$ind_weight2))
#
(all_0k_30k_frac = sum(subset(i3.df, income_hh <= 30000)$ind_weight2)/(all_num_resp - all_income_na))
#
(all_30k_60k_frac = sum(subset(i3.df, income_hh > 30000 & income_hh <=60000 )$ind_weight2)/(all_num_resp - all_income_na))
#
(all_60k_90k_frac = sum(subset(i3.df, income_hh > 60000 & income_hh <=90000 )$ind_weight2)/(all_num_resp - all_income_na))
#
(all_over_90k_frac = sum(subset(i3.df, income_hh > 90000 )$ind_weight2)/(all_num_resp - all_income_na))
#
all_0k_30k_frac + all_30k_60k_frac + all_60k_90k_frac + all_over_90k_frac
#
(all_desc.vec = round(c(all_num_resp, all_male_frac, all_female_frac, all_age_under_25_frac, all_age_25_34_frac, all_age_35_44_frac, all_age_45_54_frac, all_age_55_64_frac, all_age_over_65_frac, all_race_white_frac, all_race_black_frac, all_race_asian_frac, all_race_other_frac, all_latinx_frac, all_nonlatinx_frac, all_citizen_frac, all_noncitizen_frac, all_edu_less_high_school_frac, all_edu_high_school_frac, all_edu_some_college_frac, all_edu_associate_frac, all_edu_college_frac, all_edu_graduate_frac, all_0k_30k_frac, all_30k_60k_frac, all_60k_90k_frac, all_over_90k_frac), 2))

## Table 3: Group 1 column
(group1_num_resp = sum(group1_indiv.df$ind_weight2))
#
table(group1_indiv.df$gender)
#
(group1_male_frac = sum(subset(group1_indiv.df, gender==1)$ind_weight2)/group1_num_resp)
#
(group1_female_frac = sum(subset(group1_indiv.df, gender==0)$ind_weight2)/group1_num_resp)
#
sum(group1_male_frac+group1_female_frac)
#
(group1_age_under_25_frac = sum(subset(group1_indiv.df, age < 25)$ind_weight2)/group1_num_resp)
#
(group1_age_25_34_frac = sum(subset(group1_indiv.df, age >= 25 & age <= 34)$ind_weight2)/group1_num_resp)
#
(group1_age_35_44_frac = sum(subset(group1_indiv.df, age >= 35 & age <= 44)$ind_weight2)/group1_num_resp)
#
(group1_age_45_54_frac = sum(subset(group1_indiv.df, age >= 45 & age <= 54)$ind_weight2)/group1_num_resp)
#
(group1_age_55_64_frac = sum(subset(group1_indiv.df, age >= 55 & age <= 64)$ind_weight2)/group1_num_resp)
#
(group1_age_over_65_frac = sum(subset(group1_indiv.df, age >= 65)$ind_weight2)/group1_num_resp)
#
group1_age_under_25_frac + group1_age_25_34_frac + group1_age_35_44_frac + group1_age_45_54_frac + group1_age_55_64_frac + group1_age_over_65_frac
#
table(group1_indiv.df$race)
#
(group1_race_white_frac = sum(subset(group1_indiv.df, race==1)$ind_weight2)/group1_num_resp)
#
(group1_race_black_frac = sum(subset(group1_indiv.df, race==2)$ind_weight2)/group1_num_resp)
#
(group1_race_asian_frac = sum(subset(group1_indiv.df, race==4)$ind_weight2)/group1_num_resp)
#
(group1_race_other_frac = sum(subset(group1_indiv.df, race==3| race==5 | race==6)$ind_weight2)/group1_num_resp)
#
group1_race_white_frac + group1_race_black_frac + group1_race_asian_frac + group1_race_other_frac
#
(group1_latinx_frac = sum(group1_indiv.df[which(group1_indiv.df$hispaniclatinx==1), ]$ind_weight2)/group1_num_resp)
#
(group1_nonlatinx_frac = sum(group1_indiv.df[which(group1_indiv.df$hispaniclatinx==0), ]$ind_weight2)/group1_num_resp)
#
group1_latinx_frac + group1_nonlatinx_frac
#
(group1_citizen_frac = sum(subset(group1_indiv.df, citizen==1)$ind_weight2)/group1_num_resp)
#
(group1_noncitizen_frac = sum(subset(group1_indiv.df, citizen==0)$ind_weight2)/group1_num_resp)
#
table(group1_indiv.df$highest_education)
#
(group1_edu_less_high_school_frac = sum(subset(group1_indiv.df, highest_education < 9)$ind_weight2)/group1_num_resp)
#
(group1_edu_high_school_frac = sum(subset(group1_indiv.df, highest_education == 9)$ind_weight2)/group1_num_resp)
#
(group1_edu_some_college_frac = sum(subset(group1_indiv.df, highest_education == 10)$ind_weight2)/group1_num_resp)
#
(group1_edu_associate_frac = sum(subset(group1_indiv.df, highest_education == 11 | highest_education == 12 )$ind_weight2)/group1_num_resp)
#
(group1_edu_college_frac = sum(subset(group1_indiv.df, highest_education == 13)$ind_weight2)/group1_num_resp)
#
(group1_edu_graduate_frac = sum(subset(group1_indiv.df, highest_education > 13)$ind_weight2)/group1_num_resp)
#
group1_edu_less_high_school_frac + group1_edu_high_school_frac + group1_edu_some_college_frac + group1_edu_associate_frac + group1_edu_college_frac + group1_edu_graduate_frac
#
summary(group1_indiv.df$income_hh)
# computing the sum of weights of the 66 NA resp
(group1_income_na = sum(subset(group1_indiv.df, is.na(income_hh))$ind_weight2))
#
(group1_0k_30k_frac = sum(subset(group1_indiv.df, income_hh <= 30000)$ind_weight2)/(group1_num_resp-group1_income_na))
#
(group1_30k_60k_frac = sum(subset(group1_indiv.df, income_hh > 30000 & income_hh <=60000 )$ind_weight2)/(group1_num_resp-group1_income_na))
#
(group1_60k_90k_frac = sum(subset(group1_indiv.df, income_hh > 60000 & income_hh <=90000 )$ind_weight2)/(group1_num_resp-group1_income_na))
#
(group1_over_90k_frac = sum(subset(group1_indiv.df, income_hh > 90000 )$ind_weight2)/(group1_num_resp-group1_income_na))
#
group1_0k_30k_frac + group1_30k_60k_frac + group1_60k_90k_frac + group1_over_90k_frac
#
(group1_desc.vec = round(c(group1_num_resp, group1_male_frac, group1_female_frac, group1_age_under_25_frac, group1_age_25_34_frac, group1_age_35_44_frac, group1_age_45_54_frac, group1_age_55_64_frac, group1_age_over_65_frac, group1_race_white_frac, group1_race_black_frac, group1_race_asian_frac, group1_race_other_frac, group1_latinx_frac, group1_nonlatinx_frac, group1_citizen_frac, group1_noncitizen_frac, group1_edu_less_high_school_frac, group1_edu_high_school_frac, group1_edu_some_college_frac, group1_edu_associate_frac, group1_edu_college_frac, group1_edu_graduate_frac, group1_0k_30k_frac, group1_30k_60k_frac, group1_60k_90k_frac, group1_over_90k_frac), 2))

## Table 3: Group 2 column
(group2_num_resp = sum(group2_indiv.df$ind_weight2))
#
table(group2_indiv.df$gender)
#
(group2_male_frac = sum(subset(group2_indiv.df, gender==1)$ind_weight2)/group2_num_resp)
#
(group2_female_frac = sum(subset(group2_indiv.df, gender==0)$ind_weight2)/group2_num_resp)
#
sum(group2_male_frac+group2_female_frac)
#
(group2_age_under_25_frac = sum(subset(group2_indiv.df, age < 25)$ind_weight2)/group2_num_resp)
#
(group2_age_25_34_frac = sum(subset(group2_indiv.df, age >= 25 & age <= 34)$ind_weight2)/group2_num_resp)
#
(group2_age_35_44_frac = sum(subset(group2_indiv.df, age >= 35 & age <= 44)$ind_weight2)/group2_num_resp)
#
(group2_age_45_54_frac = sum(subset(group2_indiv.df, age >= 45 & age <= 54)$ind_weight2)/group2_num_resp)
#
(group2_age_55_64_frac = sum(subset(group2_indiv.df, age >= 55 & age <= 64)$ind_weight2)/group2_num_resp)
#
(group2_age_over_65_frac = sum(subset(group2_indiv.df, age >= 65)$ind_weight2)/group2_num_resp)
#
group2_age_under_25_frac + group2_age_25_34_frac + group2_age_35_44_frac + group2_age_45_54_frac + group2_age_55_64_frac + group2_age_over_65_frac
#
table(group2_indiv.df$race)
#
(group2_race_white_frac = sum(subset(group2_indiv.df, race==1)$ind_weight2)/group2_num_resp)
#
(group2_race_black_frac = sum(subset(group2_indiv.df, race==2)$ind_weight2)/group2_num_resp)
#
(group2_race_asian_frac = sum(subset(group2_indiv.df, race==4)$ind_weight2)/group2_num_resp)
#
(group2_race_other_frac = sum(subset(group2_indiv.df, race==3| race==5 | race==6)$ind_weight2)/group2_num_resp)
#
group2_race_white_frac + group2_race_black_frac + group2_race_asian_frac + group2_race_other_frac
#
(group2_latinx_frac = sum(group2_indiv.df[which(group2_indiv.df$hispaniclatinx==1), ]$ind_weight2)/group2_num_resp)
#
(group2_nonlatinx_frac = sum(group2_indiv.df[which(group2_indiv.df$hispaniclatinx==0), ]$ind_weight2)/group2_num_resp)
#
group2_latinx_frac + group2_nonlatinx_frac
#
(group2_citizen_frac = sum(subset(group2_indiv.df, citizen==1)$ind_weight2)/group2_num_resp)
#
(group2_noncitizen_frac = sum(subset(group2_indiv.df, citizen==0)$ind_weight2)/group2_num_resp)
#
table(group2_indiv.df$highest_education)
#
(group2_edu_less_high_school_frac = sum(subset(group2_indiv.df, highest_education < 9)$ind_weight2)/group2_num_resp)
#
(group2_edu_high_school_frac = sum(subset(group2_indiv.df, highest_education == 9)$ind_weight2)/group2_num_resp)
#
(group2_edu_some_college_frac = sum(subset(group2_indiv.df, highest_education == 10)$ind_weight2)/group2_num_resp)
#
(group2_edu_associate_frac = sum(subset(group2_indiv.df, highest_education == 11 | highest_education == 12 )$ind_weight2)/group2_num_resp)
#
(group2_edu_college_frac = sum(subset(group2_indiv.df, highest_education == 13)$ind_weight2)/group2_num_resp)
#
(group2_edu_graduate_frac = sum(subset(group2_indiv.df, highest_education > 13)$ind_weight2)/group2_num_resp)
#
group2_edu_less_high_school_frac + group2_edu_high_school_frac + group2_edu_some_college_frac + group2_edu_associate_frac + group2_edu_college_frac + group2_edu_graduate_frac
#
summary(group2_indiv.df$income_hh)
# computing the sum of weights of the 17 NA resp
(group2_income_na = sum(subset(group2_indiv.df, is.na(income_hh))$ind_weight2))
#
(group2_0k_30k_frac = sum(subset(group2_indiv.df, income_hh <= 30000)$ind_weight2)/(group2_num_resp-group2_income_na))
#
(group2_30k_60k_frac = sum(subset(group2_indiv.df, income_hh > 30000 & income_hh <=60000 )$ind_weight2)/(group2_num_resp-group2_income_na))
#
(group2_60k_90k_frac = sum(subset(group2_indiv.df, income_hh > 60000 & income_hh <=90000 )$ind_weight2)/(group2_num_resp-group2_income_na))
#
(group2_over_90k_frac = sum(subset(group2_indiv.df, income_hh > 90000 )$ind_weight2)/(group2_num_resp-group2_income_na))
#
group2_0k_30k_frac + group2_30k_60k_frac + group2_60k_90k_frac + group2_over_90k_frac
#
(group2_desc.vec = round(c(group2_num_resp, group2_male_frac, group2_female_frac, group2_age_under_25_frac, group2_age_25_34_frac, group2_age_35_44_frac, group2_age_45_54_frac, group2_age_55_64_frac, group2_age_over_65_frac, group2_race_white_frac, group2_race_black_frac, group2_race_asian_frac, group2_race_other_frac, group2_latinx_frac, group2_nonlatinx_frac, group2_citizen_frac, group2_noncitizen_frac, group2_edu_less_high_school_frac, group2_edu_high_school_frac, group2_edu_some_college_frac, group2_edu_associate_frac, group2_edu_college_frac, group2_edu_graduate_frac, group2_0k_30k_frac, group2_30k_60k_frac, group2_60k_90k_frac, group2_over_90k_frac), 2))

## Table 3: Group 3 column
(group3_num_resp = sum(group3_indiv.df$ind_weight2))
#
table(group3_indiv.df$gender)
#
(group3_male_frac = sum(subset(group3_indiv.df, gender==1)$ind_weight2)/group3_num_resp)
#
(group3_female_frac = sum(subset(group3_indiv.df, gender==0)$ind_weight2)/group3_num_resp)
#
sum(group3_male_frac+group3_female_frac)
#
(group3_age_under_25_frac = sum(subset(group3_indiv.df, age < 25)$ind_weight2)/group3_num_resp)
#
(group3_age_25_34_frac = sum(subset(group3_indiv.df, age >= 25 & age <= 34)$ind_weight2)/group3_num_resp)
#
(group3_age_35_44_frac = sum(subset(group3_indiv.df, age >= 35 & age <= 44)$ind_weight2)/group3_num_resp)
#
(group3_age_45_54_frac = sum(subset(group3_indiv.df, age >= 45 & age <= 54)$ind_weight2)/group3_num_resp)
#
(group3_age_55_64_frac = sum(subset(group3_indiv.df, age >= 55 & age <= 64)$ind_weight2)/group3_num_resp)
#
(group3_age_over_65_frac = sum(subset(group3_indiv.df, age >= 65)$ind_weight2)/group3_num_resp)
#
group3_age_under_25_frac + group3_age_25_34_frac + group3_age_35_44_frac + group3_age_45_54_frac + group3_age_55_64_frac + group3_age_over_65_frac
#
table(group3_indiv.df$race)
#
(group3_race_white_frac = sum(subset(group3_indiv.df, race==1)$ind_weight2)/group3_num_resp)
#
(group3_race_black_frac = sum(subset(group3_indiv.df, race==2)$ind_weight2)/group3_num_resp)
#
(group3_race_asian_frac = sum(subset(group3_indiv.df, race==4)$ind_weight2)/group3_num_resp)
#
(group3_race_other_frac = sum(subset(group3_indiv.df, race==3| race==5 | race==6)$ind_weight2)/group3_num_resp)
#
group3_race_white_frac + group3_race_black_frac + group3_race_asian_frac + group3_race_other_frac
#
(group3_latinx_frac = sum(group3_indiv.df[which(group3_indiv.df$hispaniclatinx==1), ]$ind_weight2)/group3_num_resp)
#
(group3_nonlatinx_frac = sum(group3_indiv.df[which(group3_indiv.df$hispaniclatinx==0), ]$ind_weight2)/group3_num_resp)
#
group3_latinx_frac + group3_nonlatinx_frac
#
(group3_citizen_frac = sum(subset(group3_indiv.df, citizen==1)$ind_weight2)/group3_num_resp)
#
(group3_noncitizen_frac = sum(subset(group3_indiv.df, citizen==0)$ind_weight2)/group3_num_resp)
#
table(group3_indiv.df$highest_education)
#
(group3_edu_less_high_school_frac = sum(subset(group3_indiv.df, highest_education < 9)$ind_weight2)/group3_num_resp)
#
(group3_edu_high_school_frac = sum(subset(group3_indiv.df, highest_education == 9)$ind_weight2)/group3_num_resp)
#
(group3_edu_some_college_frac = sum(subset(group3_indiv.df, highest_education == 10)$ind_weight2)/group3_num_resp)
#
(group3_edu_associate_frac = sum(subset(group3_indiv.df, highest_education == 11 | highest_education == 12 )$ind_weight2)/group3_num_resp)
#
(group3_edu_college_frac = sum(subset(group3_indiv.df, highest_education == 13)$ind_weight2)/group3_num_resp)
#
(group3_edu_graduate_frac = sum(subset(group3_indiv.df, highest_education > 13)$ind_weight2)/group3_num_resp)
#
group3_edu_less_high_school_frac + group3_edu_high_school_frac + group3_edu_some_college_frac + group3_edu_associate_frac + group3_edu_college_frac + group3_edu_graduate_frac
#
summary(group3_indiv.df$income_hh)
# computing the sum of weights of the 12 NA resp
(group3_income_na = sum(subset(group3_indiv.df, is.na(income_hh))$ind_weight2))
#
(group3_0k_30k_frac = sum(subset(group3_indiv.df, income_hh <= 30000)$ind_weight2)/(group3_num_resp-group3_income_na))
#
(group3_30k_60k_frac = sum(subset(group3_indiv.df, income_hh > 30000 & income_hh <=60000 )$ind_weight2)/(group3_num_resp-group3_income_na))
#
(group3_60k_90k_frac = sum(subset(group3_indiv.df, income_hh > 60000 & income_hh <=90000 )$ind_weight2)/(group3_num_resp-group3_income_na))
#
(group3_over_90k_frac = sum(subset(group3_indiv.df, income_hh > 90000 )$ind_weight2)/(group3_num_resp-group3_income_na))
#
group3_0k_30k_frac + group3_30k_60k_frac + group3_60k_90k_frac + group3_over_90k_frac
#
(group3_desc.vec = round(c(group3_num_resp, group3_male_frac, group3_female_frac, group3_age_under_25_frac, group3_age_25_34_frac, group3_age_35_44_frac, group3_age_45_54_frac, group3_age_55_64_frac, group3_age_over_65_frac, group3_race_white_frac, group3_race_black_frac, group3_race_asian_frac, group3_race_other_frac, group3_latinx_frac, group3_nonlatinx_frac, group3_citizen_frac, group3_noncitizen_frac, group3_edu_less_high_school_frac, group3_edu_high_school_frac, group3_edu_some_college_frac, group3_edu_associate_frac, group3_edu_college_frac, group3_edu_graduate_frac, group3_0k_30k_frac, group3_30k_60k_frac, group3_60k_90k_frac, group3_over_90k_frac), 2))

## Building Table 3 data frame
desc_var.vec = c("Number of respondents", "Share male", "Share female", "Share under 25", "Share 25-34", "Share 35-44", "Share 45-54", "Share 55-64", "Share over 65", "Share white", "Share black", "Share Asian", "Share other", "Share hispanic", "Share non-hispanic", "Share citizen", "Share non-citizen", "Share less than high school", "Share high school", "Share some college", "Share associate degree", "Share college", "Share graduate", "Share 0k-30k", "Share 30k-60k", "Share 60k-90k", "Share over 90k" )
#
(desc.df = data.frame("Variable" = desc_var.vec, "All" = all_desc.vec, "Group_1"=group1_desc.vec, "Group_2" =  group2_desc.vec, "Group_3" = group3_desc.vec) )
#
#Export to CSV
write.csv(desc.df, "table_3.csv")

### Starting Table 2 (payments)
## Merge the 3 datasets 
m1.df = merge(i3.df, t2.df, by="id", all.x = T, all.y = F)
dim(m1.df)# num payments
length(unique(m1.df$id))# num respondents

nrow(subset(m1.df, payment==1))# num of payments
table(m1.df$payment==1)
nrow(subset(m1.df, payment==1 & bill==1)) # bill payments
# restrict to payments only
#m3.df = subset(m2.df, payment==1)# bad because it eliminates respondents who did not make a payment

table(m1.df$pi)# distribution of transactions by PI
100*(prop.table(table(m1.df$pi)))
# NOTE: I keep all PI althou I am suing only pi=1:7. This is to keep the number of respondents who did not make any payment 

# # Below, restricting to PI 1 to 7
# m4.df = subset(m3.df, pi %in% 1:7)
# length(unique(m4.df$id))
# nrow(m4.df) - nrow(m3.df)# num payments lost by restricting PI
# length(unique(m4.df$id)) - length(unique(m3.df$id)) # num respondents lost by restricting PI
# table(m4.df$pi)
# sum(table(m4.df$pi))# Number of trans 
# length(unique(m4.df$id))# Number of respondents 

# # Below, give names to 7 PI (removing other PI levels)
# table(m3.df$pi)
# str(m3.df$pi)
# m5.df = m4.df
# m5.df$pi[m5.df$pi==1] = "cash"
# m5.df$pi[m5.df$pi==2] = "check"
# m5.df$pi[m5.df$pi==3] = "credit"
# m5.df$pi[m5.df$pi==4] = "debit"
# m5.df$pi[m5.df$pi==5] = "prepaid"
# m5.df$pi[m5.df$pi==6] = "banp"
# m5.df$pi[m5.df$pi==7] = "obbp"
# table(m5.df$pi)

# dim(m5.df)
# levels(m5.df$pi)
# table(m5.df$pi)
# m6.df = m5.df
# str(m6.df$pi)
# m6.df$pi = factor(m6.df$pi, levels = c("cash", "check", "credit", "debit", "prepaid", "banp", "obbp"))
# table(m6.df$pi)
# dim(m6.df)

m7.df = m1.df

# rescaling weights to fit trans data [not used]
#names(select(m7.df, contains("weight")))# not clear why it does not work
sum(m7.df$ind_weight)
m8.df = m7.df
m8.df$t_weight = nrow(m8.df)* m8.df$ind_weight/sum(m8.df$ind_weight)
sum(m8.df$t_weight)
nrow(m8.df)

## Start All column Table 2
(all_all_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi%in% 1:7))/length(unique(m8.df$id)))
#
(all_all_avg_monthly_payments_frac = all_all_avg_monthly_payments/all_all_avg_monthly_payments)
#
(all_all_total_val_payments = sum(subset(m8.df, payment==1 & pi%in% 1:7)$amnt))
#
(all_all_val_payments_frac = all_all_total_val_payments/all_all_total_val_payments)
#
(all_all_avg_val_payments = mean(subset(m8.df, payment==1 & pi%in% 1:7)$amnt)) 
#
(all_all_med_val_payments = median(subset(m8.df, payment==1 & pi%in% 1:7)$amnt)) 
#
(all_all_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1))/length(unique(m8.df$id)))
#
(all_all_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1))/nrow(subset(m8.df, payment==1 & pi%in% 1:7  & bill==1)))
#
(all_all_total_val_bills = sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1)$amnt))
#
(all_all_val_bills_frac = sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1)$amnt)/sum(subset(m8.df, payment==1 & pi%in% 1:7  & bill==1)$amnt))

(pay_all_all.vec = c(all_all_avg_monthly_payments, 100*all_all_avg_monthly_payments_frac, all_all_total_val_payments, 100*all_all_val_payments_frac, all_all_avg_val_payments, all_all_med_val_payments, all_all_avg_monthly_bills, 100*all_all_monthly_bills_frac, all_all_total_val_bills, 100*all_all_val_bills_frac))

# continue All column with Group 1 Table 2
(all_group1_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group1"))/length(unique(subset(m8.df, adopt=="Group1")$id)))
#
(all_group1_avg_monthly_payments_frac = all_group1_avg_monthly_payments/all_group1_avg_monthly_payments)
#
(all_group1_total_val_payments = sum(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group1")$amnt))
#
(all_group1_val_payments_frac = all_group1_total_val_payments/all_group1_total_val_payments)
#
(all_group1_avg_val_payments = mean(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group1")$amnt)) 
#
(all_group1_med_val_payments = median(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group1")$amnt)) 
#
(all_group1_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group1"))/length(unique(subset(m8.df, adopt=="Group1" )$id)))
#
(all_group1_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group1"))/nrow(subset(m8.df, payment==1 & pi%in% 1:7  & bill==1 & adopt=="Group1")))
#
(all_group1_total_val_bills = sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group1")$amnt))
#
(all_group1_val_bills_frac = sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group1")$amnt)/sum(subset(m8.df, payment==1 & pi%in% 1:7  & bill==1 & adopt=="Group1")$amnt))

(pay_all_group1.vec = c(all_group1_avg_monthly_payments, 100*all_group1_avg_monthly_payments_frac, all_group1_total_val_payments, 100*all_group1_val_payments_frac, all_group1_avg_val_payments, all_group1_med_val_payments, all_group1_avg_monthly_bills, 100*all_group1_monthly_bills_frac, all_group1_total_val_bills, 100*all_group1_val_bills_frac))

# continue All column with Group 2 Table 2
(all_group2_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group2"))/length(unique(subset(m8.df, adopt=="Group2")$id)))
#
(all_group2_avg_monthly_payments_frac = all_group2_avg_monthly_payments/all_group2_avg_monthly_payments)
#
(all_group2_total_val_payments = sum(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group2")$amnt))
#
(all_group2_val_payments_frac = all_group2_total_val_payments/all_group2_total_val_payments)
#
(all_group2_avg_val_payments = mean(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group2")$amnt)) 
#
(all_group2_med_val_payments = median(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group2")$amnt)) 
#
(all_group2_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group2"))/length(unique(subset(m8.df, adopt=="Group2" )$id)))
#
(all_group2_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group2"))/nrow(subset(m8.df, payment==1 & pi%in% 1:7  & bill==1 & adopt=="Group2")))
#
(all_group2_total_val_bills = sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group2")$amnt))
#
(all_group2_val_bills_frac = sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group2")$amnt)/sum(subset(m8.df, payment==1 & pi%in% 1:7  & bill==1 & adopt=="Group2")$amnt))

(pay_all_group2.vec = c(all_group2_avg_monthly_payments, 100*all_group2_avg_monthly_payments_frac, all_group2_total_val_payments, 100*all_group2_val_payments_frac, all_group2_avg_val_payments, all_group2_med_val_payments, all_group2_avg_monthly_bills, 100*all_group2_monthly_bills_frac, all_group2_total_val_bills, 100*all_group2_val_bills_frac))

# continue All column with Group 3 Table 2
(all_group3_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group3"))/length(unique(subset(m8.df, adopt=="Group3")$id)))
#
(all_group3_avg_monthly_payments_frac = all_group3_avg_monthly_payments/all_group3_avg_monthly_payments)
#
(all_group3_total_val_payments = sum(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group3")$amnt))
#
(all_group3_val_payments_frac = all_group3_total_val_payments/all_group3_total_val_payments)
#
(all_group3_avg_val_payments = mean(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group3")$amnt)) 
#
(all_group3_med_val_payments = median(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group3")$amnt)) 
#
(all_group3_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group3"))/length(unique(subset(m8.df, adopt=="Group3" )$id)))
#
(all_group3_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group3"))/nrow(subset(m8.df, payment==1 & pi%in% 1:7  & bill==1 & adopt=="Group3")))
#
(all_group3_total_val_bills = sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group3")$amnt))
#
(all_group3_val_bills_frac = sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group3")$amnt)/sum(subset(m8.df, payment==1 & pi%in% 1:7  & bill==1 & adopt=="Group3")$amnt))

(pay_all_group3.vec = c(all_group3_avg_monthly_payments, 100*all_group3_avg_monthly_payments_frac, all_group3_total_val_payments, 100*all_group3_val_payments_frac, all_group3_avg_val_payments, all_group3_med_val_payments, all_group3_avg_monthly_bills, 100*all_group3_monthly_bills_frac, all_group3_total_val_bills, 100*all_group3_val_bills_frac))

# Start Cash column Table 2
(cash_all_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi== 1))/length(unique(m8.df$id)))
#
(cash_all_avg_monthly_payments_frac = cash_all_avg_monthly_payments/all_all_avg_monthly_payments)
#
(cash_all_total_val_payments = sum(subset(m8.df, payment==1 & pi == 1)$amnt))
#
(cash_all_val_payments_frac = cash_all_total_val_payments/all_all_total_val_payments)
#
(cash_all_avg_val_payments = mean(subset(m8.df, payment==1 & pi == 1)$amnt)) 
#
(cash_all_med_val_payments = median(subset(m8.df, payment==1 & pi == 1)$amnt)) 
#
(cash_all_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi == 1 & bill==1))/length(unique(m8.df$id)))
#
(cash_all_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi == 1 & bill==1))/nrow(subset(m8.df, payment==1 & bill==1 & pi%in% 1:7)))
#
(cash_all_total_val_bills = sum(subset(m8.df, payment==1 & pi == 1 & bill==1)$amnt))
#
(cash_all_val_bills_frac = sum(subset(m8.df, payment==1 & pi == 1 & bill==1)$amnt)/sum(subset(m8.df, payment==1 & bill==1 & pi%in% 1:7)$amnt))

(pay_cash_all.vec = c(cash_all_avg_monthly_payments, 100*cash_all_avg_monthly_payments_frac, cash_all_total_val_payments, 100*cash_all_val_payments_frac, cash_all_avg_val_payments, cash_all_med_val_payments, cash_all_avg_monthly_bills, 100*cash_all_monthly_bills_frac, cash_all_total_val_bills, 100*cash_all_val_bills_frac))

# continue Cash column with Group 1 Table 2
(cash_group1_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi == 1 & adopt=="Group1"))/length(unique(subset(m8.df, adopt=="Group1")$id)))
#
(cash_group1_avg_monthly_payments_frac = cash_group1_avg_monthly_payments/all_group1_avg_monthly_payments)
#
(cash_group1_total_val_payments = sum(subset(m8.df, payment==1 & pi == 1 & adopt=="Group1")$amnt))
#
(cash_group1_val_payments_frac = cash_group1_total_val_payments/all_group1_total_val_payments)
#
(cash_group1_avg_val_payments = mean(subset(m8.df, payment==1 & pi == 1 & adopt=="Group1")$amnt)) 
#
(cash_group1_med_val_payments = median(subset(m8.df, payment==1 & pi == 1 & adopt=="Group1")$amnt)) 
#
(cash_group1_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi == 1 & bill==1 & adopt=="Group1"))/length(unique(subset(m8.df, adopt=="Group1" )$id)))
#
(cash_group1_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi == 1 & bill==1 & adopt=="Group1"))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group1" & bill==1)))
#
(cash_group1_total_val_bills = sum(subset(m8.df, payment==1 & pi == 1 & bill==1 & adopt=="Group1")$amnt))
#
(cash_group1_val_bills_frac = sum(subset(m8.df, payment==1 & pi ==  1 & bill==1 & adopt=="Group1")$amnt)/sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group1")$amnt))

(pay_cash_group1.vec = c(cash_group1_avg_monthly_payments, 100*cash_group1_avg_monthly_payments_frac, cash_group1_total_val_payments, 100*cash_group1_val_payments_frac, cash_group1_avg_val_payments, cash_group1_med_val_payments, cash_group1_avg_monthly_bills, 100*cash_group1_monthly_bills_frac, cash_group1_total_val_bills, 100*cash_group1_val_bills_frac))

# continue Cash column with Group 2 Table 2
(cash_group2_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi == 1 & adopt=="Group2"))/length(unique(subset(m8.df, adopt=="Group2")$id)))
#
(cash_group2_avg_monthly_payments_frac = cash_group2_avg_monthly_payments/all_group2_avg_monthly_payments)
#
(cash_group2_total_val_payments = sum(subset(m8.df, payment==1 & pi == 1 & adopt=="Group2")$amnt))
#
(cash_group2_val_payments_frac = cash_group2_total_val_payments/all_group2_total_val_payments)
#
(cash_group2_avg_val_payments = mean(subset(m8.df, payment==1 & pi == 1 & adopt=="Group2")$amnt)) 
#
(cash_group2_med_val_payments = median(subset(m8.df, payment==1 & pi == 1 & adopt=="Group2")$amnt)) 
#
(cash_group2_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi == 1 & bill==1 & adopt=="Group2"))/length(unique(subset(m8.df, adopt=="Group2" )$id)))
#
(cash_group2_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi == 1 & bill==1 & adopt=="Group2"))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group2" & bill==1)))
#
(cash_group2_total_val_bills = sum(subset(m8.df, payment==1 & pi == 1 & bill==1 & adopt=="Group2")$amnt))
#
(cash_group2_val_bills_frac = sum(subset(m8.df, payment==1 & pi ==  1 & bill==1 & adopt=="Group2")$amnt)/sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group2")$amnt))

(pay_cash_group2.vec = c(cash_group2_avg_monthly_payments, 100*cash_group2_avg_monthly_payments_frac, cash_group2_total_val_payments, 100*cash_group2_val_payments_frac, cash_group2_avg_val_payments, cash_group2_med_val_payments, cash_group2_avg_monthly_bills, 100*cash_group2_monthly_bills_frac, cash_group2_total_val_bills, 100*cash_group2_val_bills_frac))

# continue Cash column with Group 2 Table 2
(cash_group3_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi == 1 & adopt=="Group3"))/length(unique(subset(m8.df, adopt=="Group3")$id)))
#
(cash_group3_avg_monthly_payments_frac = cash_group3_avg_monthly_payments/all_group3_avg_monthly_payments)
#
(cash_group3_total_val_payments = sum(subset(m8.df, payment==1 & pi == 1 & adopt=="Group3")$amnt))
#
(cash_group3_val_payments_frac = cash_group3_total_val_payments/all_group3_total_val_payments)
#
(cash_group3_avg_val_payments = mean(subset(m8.df, payment==1 & pi == 1 & adopt=="Group3")$amnt)) 
#
(cash_group3_med_val_payments = median(subset(m8.df, payment==1 & pi == 1 & adopt=="Group3")$amnt)) 
#
(cash_group3_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi == 1 & bill==1 & adopt=="Group3"))/length(unique(subset(m8.df, adopt=="Group3" )$id)))
#
(cash_group3_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi == 1 & bill==1 & adopt=="Group3"))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group3" & bill==1)))
#
(cash_group3_total_val_bills = sum(subset(m8.df, payment==1 & pi == 1 & bill==1 & adopt=="Group3")$amnt))
#
(cash_group3_val_bills_frac = sum(subset(m8.df, payment==1 & pi ==  1 & bill==1 & adopt=="Group3")$amnt)/sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group3")$amnt))

(pay_cash_group3.vec = c(cash_group3_avg_monthly_payments, 100*cash_group3_avg_monthly_payments_frac, cash_group3_total_val_payments, 100*cash_group3_val_payments_frac, cash_group3_avg_val_payments, cash_group3_med_val_payments, cash_group3_avg_monthly_bills, 100*cash_group3_monthly_bills_frac, cash_group3_total_val_bills, 100*cash_group3_val_bills_frac))

## Start Check column Table 2
(check_all_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi== 2))/length(unique(m8.df$id)))
#
(check_all_avg_monthly_payments_frac = check_all_avg_monthly_payments/all_all_avg_monthly_payments)
#
(check_all_total_val_payments = sum(subset(m8.df, payment==1 & pi == 2)$amnt))
#
(check_all_val_payments_frac = check_all_total_val_payments/all_all_total_val_payments)
#
(check_all_avg_val_payments = mean(subset(m8.df, payment==1 & pi == 2)$amnt)) 
#
(check_all_med_val_payments = median(subset(m8.df, payment==1 & pi == 2)$amnt)) 
#
(check_all_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi == 2 & bill==1))/length(unique(m8.df$id)))
#
(check_all_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi == 2 & bill==1))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1)))
#
(check_all_total_val_bills = sum(subset(m8.df, payment==1 & pi == 2 & bill==1)$amnt))
#
(check_all_val_bills_frac = sum(subset(m8.df, payment==1 & pi == 2 & bill==1)$amnt)/sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill == 1)$amnt))

(pay_check_all.vec = c(check_all_avg_monthly_payments, 100*check_all_avg_monthly_payments_frac, check_all_total_val_payments, 100*check_all_val_payments_frac, check_all_avg_val_payments, check_all_med_val_payments, check_all_avg_monthly_bills, 100*check_all_monthly_bills_frac, check_all_total_val_bills, 100*check_all_val_bills_frac))


# continue check column with Group 1 Table 2
(check_group1_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi == 2 & adopt=="Group1"))/length(unique(subset(m8.df, adopt=="Group1")$id)))
#
(check_group1_avg_monthly_payments_frac = check_group1_avg_monthly_payments/all_group1_avg_monthly_payments)
#
(check_group1_total_val_payments = sum(subset(m8.df, payment==1 & pi == 2 & adopt=="Group1")$amnt))
#
(check_group1_val_payments_frac = check_group1_total_val_payments/all_group1_total_val_payments)
#
(check_group1_avg_val_payments = mean(subset(m8.df, payment==1 & pi == 2 & adopt=="Group1")$amnt)) 
#
(check_group1_med_val_payments = median(subset(m8.df, payment==1 & pi == 2 & adopt=="Group1")$amnt)) 
#
(check_group1_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi == 2 & bill==1 & adopt=="Group1"))/length(unique(subset(m8.df, adopt=="Group1" )$id)))
#
(check_group1_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi == 2 & bill==1 & adopt=="Group1"))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group1" & bill==1)))
#
(check_group1_total_val_bills = sum(subset(m8.df, payment==1 & pi == 2 & bill==1 & adopt=="Group1")$amnt))
#
(check_group1_val_bills_frac = sum(subset(m8.df, payment==1 & pi ==  2 & bill==1 & adopt=="Group1")$amnt)/sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group1")$amnt))

(pay_check_group1.vec = c(check_group1_avg_monthly_payments, 100*check_group1_avg_monthly_payments_frac, check_group1_total_val_payments, 100*check_group1_val_payments_frac, check_group1_avg_val_payments, check_group1_med_val_payments, check_group1_avg_monthly_bills, 100*check_group1_monthly_bills_frac, check_group1_total_val_bills, 100*check_group1_val_bills_frac))

# continue check column with Group 2 Table 2
(check_group2_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi == 2 & adopt=="Group2"))/length(unique(subset(m8.df, adopt=="Group2")$id)))
#
(check_group2_avg_monthly_payments_frac = check_group2_avg_monthly_payments/all_group2_avg_monthly_payments)
#
(check_group2_total_val_payments = sum(subset(m8.df, payment==1 & pi == 2 & adopt=="Group2")$amnt))
#
(check_group2_val_payments_frac = check_group2_total_val_payments/all_group2_total_val_payments)
#
(check_group2_avg_val_payments = mean(subset(m8.df, payment==1 & pi == 2 & adopt=="Group2")$amnt)) 
#
(check_group2_med_val_payments = median(subset(m8.df, payment==1 & pi == 2 & adopt=="Group2")$amnt)) 
#
(check_group2_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi == 2 & bill==1 & adopt=="Group2"))/length(unique(subset(m8.df, adopt=="Group2" )$id)))
#
(check_group2_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi == 2 & bill==1 & adopt=="Group2"))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group2" & bill==1)))
#
(check_group2_total_val_bills = sum(subset(m8.df, payment==1 & pi == 2 & bill==1 & adopt=="Group2")$amnt))
#
(check_group2_val_bills_frac = sum(subset(m8.df, payment==1 & pi ==  2 & bill==1 & adopt=="Group2")$amnt)/sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group2")$amnt))

(pay_check_group2.vec = c(check_group2_avg_monthly_payments, 100*check_group2_avg_monthly_payments_frac, check_group2_total_val_payments, 100*check_group2_val_payments_frac, check_group2_avg_val_payments, check_group2_med_val_payments, check_group2_avg_monthly_bills, 100*check_group2_monthly_bills_frac, check_group2_total_val_bills, 100*check_group2_val_bills_frac))


# continue Check column with Group 3 Table 2
(check_group3_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi == 2 & adopt=="Group3"))/length(unique(subset(m8.df, adopt=="Group3")$id)))
#
(check_group3_avg_monthly_payments_frac = check_group3_avg_monthly_payments/all_group3_avg_monthly_payments)
#
(check_group3_total_val_payments = sum(subset(m8.df, payment==1 & pi == 2 & adopt=="Group3")$amnt))
#
(check_group3_val_payments_frac = check_group3_total_val_payments/all_group3_total_val_payments)
#
(check_group3_avg_val_payments = mean(subset(m8.df, payment==1 & pi == 2 & adopt=="Group3")$amnt)) 
#
(check_group3_med_val_payments = median(subset(m8.df, payment==1 & pi == 2 & adopt=="Group3")$amnt)) 
#
(check_group3_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi == 2 & bill==1 & adopt=="Group3"))/length(unique(subset(m8.df, adopt=="Group3" )$id)))
#
(check_group3_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi == 2 & bill==1 & adopt=="Group3"))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group3" & bill==1)))
#
(check_group3_total_val_bills = sum(subset(m8.df, payment==1 & pi == 2 & bill==1 & adopt=="Group3")$amnt))
#
(check_group3_val_bills_frac = sum(subset(m8.df, payment==1 & pi ==  2 & bill==1 & adopt=="Group3")$amnt)/sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group3")$amnt))

(pay_check_group3.vec = c(check_group3_avg_monthly_payments, 100*check_group3_avg_monthly_payments_frac, check_group3_total_val_payments, 100*check_group3_val_payments_frac, check_group3_avg_val_payments, check_group3_med_val_payments, check_group3_avg_monthly_bills, 100*check_group3_monthly_bills_frac, check_group3_total_val_bills, 100*check_group3_val_bills_frac))

## Start credit (pi==3) column Table 2
(credit_all_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi== 3))/length(unique(m8.df$id)))
#
(credit_all_avg_monthly_payments_frac = credit_all_avg_monthly_payments/all_all_avg_monthly_payments)
#
(credit_all_total_val_payments = sum(subset(m8.df, payment==1 & pi == 3)$amnt))
#
(credit_all_val_payments_frac = credit_all_total_val_payments/all_all_total_val_payments)
#
(credit_all_avg_val_payments = mean(subset(m8.df, payment==1 & pi == 3)$amnt)) 
#
(credit_all_med_val_payments = median(subset(m8.df, payment==1 & pi == 3)$amnt)) 
#
(credit_all_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi == 3 & bill==1))/length(unique(m8.df$id)))
#
(credit_all_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi == 3 & bill==1))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1)))
#
(credit_all_total_val_bills = sum(subset(m8.df, payment==1 & pi == 3 & bill==1)$amnt))
#
(credit_all_val_bills_frac = sum(subset(m8.df, payment==1 & pi == 3 & bill==1)$amnt)/sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill == 1)$amnt))

(pay_credit_all.vec = c(credit_all_avg_monthly_payments, 100*credit_all_avg_monthly_payments_frac, credit_all_total_val_payments, 100*credit_all_val_payments_frac, credit_all_avg_val_payments, credit_all_med_val_payments, credit_all_avg_monthly_bills, 100*credit_all_monthly_bills_frac, credit_all_total_val_bills, 100*credit_all_val_bills_frac))

# continue credit column with Group 1 Table 2
(credit_group1_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi == 3 & adopt=="Group1"))/length(unique(subset(m8.df, adopt=="Group1")$id)))
#
(credit_group1_avg_monthly_payments_frac = credit_group1_avg_monthly_payments/all_group1_avg_monthly_payments)
#
(credit_group1_total_val_payments = sum(subset(m8.df, payment==1 & pi == 3 & adopt=="Group1")$amnt))
#
(credit_group1_val_payments_frac = credit_group1_total_val_payments/all_group1_total_val_payments)
#
(credit_group1_avg_val_payments = mean(subset(m8.df, payment==1 & pi == 3 & adopt=="Group1")$amnt)) 
#
(credit_group1_med_val_payments = median(subset(m8.df, payment==1 & pi == 3 & adopt=="Group1")$amnt)) 
#
(credit_group1_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi == 3 & bill==1 & adopt=="Group1"))/length(unique(subset(m8.df, adopt=="Group1" )$id)))
#
(credit_group1_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi == 3 & bill==1 & adopt=="Group1"))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group1" & bill==1)))
#
(credit_group1_total_val_bills = sum(subset(m8.df, payment==1 & pi == 3 & bill==1 & adopt=="Group1")$amnt))
#
(credit_group1_val_bills_frac = sum(subset(m8.df, payment==1 & pi ==  3 & bill==1 & adopt=="Group1")$amnt)/sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group1")$amnt))

(pay_credit_group1.vec = c(credit_group1_avg_monthly_payments, 100*credit_group1_avg_monthly_payments_frac, credit_group1_total_val_payments, 100*credit_group1_val_payments_frac, credit_group1_avg_val_payments, credit_group1_med_val_payments, credit_group1_avg_monthly_bills, 100*credit_group1_monthly_bills_frac, credit_group1_total_val_bills, 100*credit_group1_val_bills_frac))


# continue credit column with Group 2 Table 2
(credit_group2_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi == 3 & adopt=="Group2"))/length(unique(subset(m8.df, adopt=="Group2")$id)))
#
(credit_group2_avg_monthly_payments_frac = credit_group2_avg_monthly_payments/all_group2_avg_monthly_payments)
#
(credit_group2_total_val_payments = sum(subset(m8.df, payment==1 & pi == 3 & adopt=="Group2")$amnt))
#
(credit_group2_val_payments_frac = credit_group2_total_val_payments/all_group2_total_val_payments)
#
(credit_group2_avg_val_payments = mean(subset(m8.df, payment==1 & pi == 3 & adopt=="Group2")$amnt)) 
#
(credit_group2_med_val_payments = median(subset(m8.df, payment==1 & pi == 3 & adopt=="Group2")$amnt)) 
#
(credit_group2_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi == 3 & bill==1 & adopt=="Group2"))/length(unique(subset(m8.df, adopt=="Group2" )$id)))
#
(credit_group2_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi == 3 & bill==1 & adopt=="Group2"))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group2" & bill==1)))
#
(credit_group2_total_val_bills = sum(subset(m8.df, payment==1 & pi == 3 & bill==1 & adopt=="Group2")$amnt))
#
(credit_group2_val_bills_frac = sum(subset(m8.df, payment==1 & pi ==  3 & bill==1 & adopt=="Group2")$amnt)/sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group2")$amnt))

(pay_credit_group2.vec = c(credit_group2_avg_monthly_payments, 100*credit_group2_avg_monthly_payments_frac, credit_group2_total_val_payments, 100*credit_group2_val_payments_frac, credit_group2_avg_val_payments, credit_group2_med_val_payments, credit_group2_avg_monthly_bills, 100*credit_group2_monthly_bills_frac, credit_group2_total_val_bills, 100*credit_group2_val_bills_frac))

# continue credit column with Group 3 Table 2
(credit_group3_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi == 3 & adopt=="Group3"))/length(unique(subset(m8.df, adopt=="Group3")$id)))
#
(credit_group3_avg_monthly_payments_frac = credit_group3_avg_monthly_payments/all_group3_avg_monthly_payments)
#
(credit_group3_total_val_payments = sum(subset(m8.df, payment==1 & pi == 3 & adopt=="Group3")$amnt))
#
(credit_group3_val_payments_frac = credit_group3_total_val_payments/all_group3_total_val_payments)
#
(credit_group3_avg_val_payments = mean(subset(m8.df, payment==1 & pi == 3 & adopt=="Group3")$amnt)) 
#
(credit_group3_med_val_payments = median(subset(m8.df, payment==1 & pi == 3 & adopt=="Group3")$amnt)) 
#
(credit_group3_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi == 3 & bill==1 & adopt=="Group3"))/length(unique(subset(m8.df, adopt=="Group3" )$id)))
#
(credit_group3_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi == 3 & bill==1 & adopt=="Group3"))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group3" & bill==1)))
#
(credit_group3_total_val_bills = sum(subset(m8.df, payment==1 & pi == 3 & bill==1 & adopt=="Group3")$amnt))
#
(credit_group3_val_bills_frac = sum(subset(m8.df, payment==1 & pi ==  3 & bill==1 & adopt=="Group3")$amnt)/sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group3")$amnt))

(pay_credit_group3.vec = c(credit_group3_avg_monthly_payments, 100*credit_group3_avg_monthly_payments_frac, credit_group3_total_val_payments, 100*credit_group3_val_payments_frac, credit_group3_avg_val_payments, credit_group3_med_val_payments, credit_group3_avg_monthly_bills, 100*credit_group3_monthly_bills_frac, credit_group3_total_val_bills, 100*credit_group3_val_bills_frac))

## Start debit (pi==4) column Table 2
(debit_all_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi== 4))/length(unique(m8.df$id)))
#
(debit_all_avg_monthly_payments_frac = debit_all_avg_monthly_payments/all_all_avg_monthly_payments)
#
(debit_all_total_val_payments = sum(subset(m8.df, payment==1 & pi == 4)$amnt))
#
(debit_all_val_payments_frac = debit_all_total_val_payments/all_all_total_val_payments)
#
(debit_all_avg_val_payments = mean(subset(m8.df, payment==1 & pi == 4)$amnt)) 
#
(debit_all_med_val_payments = median(subset(m8.df, payment==1 & pi == 4)$amnt)) 
#
(debit_all_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi == 4 & bill==1))/length(unique(m8.df$id)))
#
(debit_all_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi == 4 & bill==1))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1)))
#
(debit_all_total_val_bills = sum(subset(m8.df, payment==1 & pi == 4 & bill==1)$amnt))
#
(debit_all_val_bills_frac = sum(subset(m8.df, payment==1 & pi == 4 & bill==1)$amnt)/sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill == 1)$amnt))

(pay_debit_all.vec = c(debit_all_avg_monthly_payments, 100*debit_all_avg_monthly_payments_frac, debit_all_total_val_payments, 100*debit_all_val_payments_frac, debit_all_avg_val_payments, debit_all_med_val_payments, debit_all_avg_monthly_bills, 100*debit_all_monthly_bills_frac, debit_all_total_val_bills, 100*debit_all_val_bills_frac))

# continue debit column with Group 1 Table 2
(debit_group1_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi == 4 & adopt=="Group1"))/length(unique(subset(m8.df, adopt=="Group1")$id)))
#
(debit_group1_avg_monthly_payments_frac = debit_group1_avg_monthly_payments/all_group1_avg_monthly_payments)
#
(debit_group1_total_val_payments = sum(subset(m8.df, payment==1 & pi == 4 & adopt=="Group1")$amnt))
#
(debit_group1_val_payments_frac = debit_group1_total_val_payments/all_group1_total_val_payments)
#
(debit_group1_avg_val_payments = mean(subset(m8.df, payment==1 & pi == 4 & adopt=="Group1")$amnt)) 
#
(debit_group1_med_val_payments = median(subset(m8.df, payment==1 & pi == 4 & adopt=="Group1")$amnt)) 
#
(debit_group1_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi == 4 & bill==1 & adopt=="Group1"))/length(unique(subset(m8.df, adopt=="Group1" )$id)))
#
(debit_group1_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi == 4 & bill==1 & adopt=="Group1"))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group1" & bill==1)))
#
(debit_group1_total_val_bills = sum(subset(m8.df, payment==1 & pi == 4 & bill==1 & adopt=="Group1")$amnt))
#
(debit_group1_val_bills_frac = sum(subset(m8.df, payment==1 & pi ==  4 & bill==1 & adopt=="Group1")$amnt)/sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group1")$amnt))

(pay_debit_group1.vec = c(debit_group1_avg_monthly_payments, 100*debit_group1_avg_monthly_payments_frac, debit_group1_total_val_payments, 100*debit_group1_val_payments_frac, debit_group1_avg_val_payments, debit_group1_med_val_payments, debit_group1_avg_monthly_bills, 100*debit_group1_monthly_bills_frac, debit_group1_total_val_bills, 100*debit_group1_val_bills_frac))

# continue debit column with Group 2 Table 2
(debit_group2_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi == 4 & adopt=="Group2"))/length(unique(subset(m8.df, adopt=="Group2")$id)))
#
(debit_group2_avg_monthly_payments_frac = debit_group2_avg_monthly_payments/all_group2_avg_monthly_payments)
#
(debit_group2_total_val_payments = sum(subset(m8.df, payment==1 & pi == 4 & adopt=="Group2")$amnt))
#
(debit_group2_val_payments_frac = debit_group2_total_val_payments/all_group2_total_val_payments)
#
(debit_group2_avg_val_payments = mean(subset(m8.df, payment==1 & pi == 4 & adopt=="Group2")$amnt)) 
#
(debit_group2_med_val_payments = median(subset(m8.df, payment==1 & pi == 4 & adopt=="Group2")$amnt)) 
#
(debit_group2_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi == 4 & bill==1 & adopt=="Group2"))/length(unique(subset(m8.df, adopt=="Group2" )$id)))
#
(debit_group2_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi == 4 & bill==1 & adopt=="Group2"))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group2" & bill==1)))
#
(debit_group2_total_val_bills = sum(subset(m8.df, payment==1 & pi == 4 & bill==1 & adopt=="Group2")$amnt))
#
(debit_group2_val_bills_frac = sum(subset(m8.df, payment==1 & pi ==  4 & bill==1 & adopt=="Group2")$amnt)/sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group2")$amnt))

(pay_debit_group2.vec = c(debit_group2_avg_monthly_payments, 100*debit_group2_avg_monthly_payments_frac, debit_group2_total_val_payments, 100*debit_group2_val_payments_frac, debit_group2_avg_val_payments, debit_group2_med_val_payments, debit_group2_avg_monthly_bills, 100*debit_group2_monthly_bills_frac, debit_group2_total_val_bills, 100*debit_group2_val_bills_frac))

# continue debit column with Group 3 Table 2
(debit_group3_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi == 4 & adopt=="Group3"))/length(unique(subset(m8.df, adopt=="Group3")$id)))
#
(debit_group3_avg_monthly_payments_frac = debit_group3_avg_monthly_payments/all_group3_avg_monthly_payments)
#
(debit_group3_total_val_payments = sum(subset(m8.df, payment==1 & pi == 4 & adopt=="Group3")$amnt))
#
(debit_group3_val_payments_frac = debit_group3_total_val_payments/all_group3_total_val_payments)
#
(debit_group3_avg_val_payments = mean(subset(m8.df, payment==1 & pi == 4 & adopt=="Group3")$amnt)) 
#
(debit_group3_med_val_payments = median(subset(m8.df, payment==1 & pi == 4 & adopt=="Group3")$amnt)) 
#
(debit_group3_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi == 4 & bill==1 & adopt=="Group3"))/length(unique(subset(m8.df, adopt=="Group3" )$id)))
#
(debit_group3_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi == 4 & bill==1 & adopt=="Group3"))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group3" & bill==1)))
#
(debit_group3_total_val_bills = sum(subset(m8.df, payment==1 & pi == 4 & bill==1 & adopt=="Group3")$amnt))
#
(debit_group3_val_bills_frac = sum(subset(m8.df, payment==1 & pi ==  4 & bill==1 & adopt=="Group3")$amnt)/sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group3")$amnt))

(pay_debit_group3.vec = c(debit_group3_avg_monthly_payments, 100*debit_group3_avg_monthly_payments_frac, debit_group3_total_val_payments, 100*debit_group3_val_payments_frac, debit_group3_avg_val_payments, debit_group3_med_val_payments, debit_group3_avg_monthly_bills, 100*debit_group3_monthly_bills_frac, debit_group3_total_val_bills, 100*debit_group3_val_bills_frac))

## Start prepaid (pi==5) column Table 2
(prepaid_all_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi== 5))/length(unique(m8.df$id)))
#
(prepaid_all_avg_monthly_payments_frac = prepaid_all_avg_monthly_payments/all_all_avg_monthly_payments)
#
(prepaid_all_total_val_payments = sum(subset(m8.df, payment==1 & pi == 5)$amnt))
#
(prepaid_all_val_payments_frac = prepaid_all_total_val_payments/all_all_total_val_payments)
#
(prepaid_all_avg_val_payments = mean(subset(m8.df, payment==1 & pi == 5)$amnt)) 
#
(prepaid_all_med_val_payments = median(subset(m8.df, payment==1 & pi == 5)$amnt)) 
#
(prepaid_all_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi == 5 & bill==1))/length(unique(m8.df$id)))
#
(prepaid_all_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi == 5 & bill==1))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1)))
#
(prepaid_all_total_val_bills = sum(subset(m8.df, payment==1 & pi == 5 & bill==1)$amnt))
#
(prepaid_all_val_bills_frac = sum(subset(m8.df, payment==1 & pi == 5 & bill==1)$amnt)/sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill == 1)$amnt))

(pay_prepaid_all.vec = c(prepaid_all_avg_monthly_payments, 100*prepaid_all_avg_monthly_payments_frac, prepaid_all_total_val_payments, 100*prepaid_all_val_payments_frac, prepaid_all_avg_val_payments, prepaid_all_med_val_payments, prepaid_all_avg_monthly_bills, 100*prepaid_all_monthly_bills_frac, prepaid_all_total_val_bills, 100*prepaid_all_val_bills_frac))

# continue prepaid column with Group 1 Table 2
(prepaid_group1_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi == 5 & adopt=="Group1"))/length(unique(subset(m8.df, adopt=="Group1")$id)))
#
(prepaid_group1_avg_monthly_payments_frac = prepaid_group1_avg_monthly_payments/all_group1_avg_monthly_payments)
#
(prepaid_group1_total_val_payments = sum(subset(m8.df, payment==1 & pi == 5 & adopt=="Group1")$amnt))
#
(prepaid_group1_val_payments_frac = prepaid_group1_total_val_payments/all_group1_total_val_payments)
#
(prepaid_group1_avg_val_payments = mean(subset(m8.df, payment==1 & pi == 5 & adopt=="Group1")$amnt)) 
#
(prepaid_group1_med_val_payments = median(subset(m8.df, payment==1 & pi == 5 & adopt=="Group1")$amnt)) 
#
(prepaid_group1_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi == 5 & bill==1 & adopt=="Group1"))/length(unique(subset(m8.df, adopt=="Group1" )$id)))
#
(prepaid_group1_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi == 5 & bill==1 & adopt=="Group1"))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group1" & bill==1)))
#
(prepaid_group1_total_val_bills = sum(subset(m8.df, payment==1 & pi == 5 & bill==1 & adopt=="Group1")$amnt))
#
(prepaid_group1_val_bills_frac = sum(subset(m8.df, payment==1 & pi ==  5 & bill==1 & adopt=="Group1")$amnt)/sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group1")$amnt))

(pay_prepaid_group1.vec = c(prepaid_group1_avg_monthly_payments, 100*prepaid_group1_avg_monthly_payments_frac, prepaid_group1_total_val_payments, 100*prepaid_group1_val_payments_frac, prepaid_group1_avg_val_payments, prepaid_group1_med_val_payments, prepaid_group1_avg_monthly_bills, 100*prepaid_group1_monthly_bills_frac, prepaid_group1_total_val_bills, 100*prepaid_group1_val_bills_frac))

# continue prepaid column with Group 2 Table 2
(prepaid_group2_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi == 5 & adopt=="Group2"))/length(unique(subset(m8.df, adopt=="Group2")$id)))
#
(prepaid_group2_avg_monthly_payments_frac = prepaid_group2_avg_monthly_payments/all_group2_avg_monthly_payments)
#
(prepaid_group2_total_val_payments = sum(subset(m8.df, payment==1 & pi == 5 & adopt=="Group2")$amnt))
#
(prepaid_group2_val_payments_frac = prepaid_group2_total_val_payments/all_group2_total_val_payments)
#
(prepaid_group2_avg_val_payments = mean(subset(m8.df, payment==1 & pi == 5 & adopt=="Group2")$amnt)) 
#
(prepaid_group2_med_val_payments = median(subset(m8.df, payment==1 & pi == 5 & adopt=="Group2")$amnt)) 
#
(prepaid_group2_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi == 5 & bill==1 & adopt=="Group2"))/length(unique(subset(m8.df, adopt=="Group2" )$id)))
#
(prepaid_group2_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi == 5 & bill==1 & adopt=="Group2"))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group2" & bill==1)))
#
(prepaid_group2_total_val_bills = sum(subset(m8.df, payment==1 & pi == 5 & bill==1 & adopt=="Group2")$amnt))
#
(prepaid_group2_val_bills_frac = sum(subset(m8.df, payment==1 & pi == 5 & bill==1 & adopt=="Group2")$amnt)/sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group2")$amnt))

(pay_prepaid_group2.vec = c(prepaid_group2_avg_monthly_payments, 100*prepaid_group2_avg_monthly_payments_frac, prepaid_group2_total_val_payments, 100*prepaid_group2_val_payments_frac, prepaid_group2_avg_val_payments, prepaid_group2_med_val_payments, prepaid_group2_avg_monthly_bills, 100*prepaid_group2_monthly_bills_frac, prepaid_group2_total_val_bills, 100*prepaid_group2_val_bills_frac))

# continue prepaid column with Group 3 Table 2
(prepaid_group3_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi == 5 & adopt=="Group3"))/length(unique(subset(m8.df, adopt=="Group3")$id)))
#
(prepaid_group3_avg_monthly_payments_frac = prepaid_group3_avg_monthly_payments/all_group3_avg_monthly_payments)
#
(prepaid_group3_total_val_payments = sum(subset(m8.df, payment==1 & pi == 5 & adopt=="Group3")$amnt))
#
(prepaid_group3_val_payments_frac = prepaid_group3_total_val_payments/all_group3_total_val_payments)
#
(prepaid_group3_avg_val_payments = mean(subset(m8.df, payment==1 & pi == 5 & adopt=="Group3")$amnt)) 
#
(prepaid_group3_med_val_payments = median(subset(m8.df, payment==1 & pi == 5 & adopt=="Group3")$amnt)) 
#
(prepaid_group3_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi == 5 & bill==1 & adopt=="Group3"))/length(unique(subset(m8.df, adopt=="Group3" )$id)))
#
(prepaid_group3_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi == 5 & bill==1 & adopt=="Group3"))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group3" & bill==1)))
#
(prepaid_group3_total_val_bills = sum(subset(m8.df, payment==1 & pi == 5 & bill==1 & adopt=="Group3")$amnt))
#
(prepaid_group3_val_bills_frac = sum(subset(m8.df, payment==1 & pi ==  5 & bill==1 & adopt=="Group3")$amnt)/sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group3")$amnt))

(pay_prepaid_group3.vec = c(prepaid_group3_avg_monthly_payments, 100*prepaid_group3_avg_monthly_payments_frac, prepaid_group3_total_val_payments, 100*prepaid_group3_val_payments_frac, prepaid_group3_avg_val_payments, prepaid_group3_med_val_payments, prepaid_group3_avg_monthly_bills, 100*prepaid_group3_monthly_bills_frac, prepaid_group3_total_val_bills, 100*prepaid_group3_val_bills_frac))

## Start banp (pi==6) column Table 2
(banp_all_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi== 6))/length(unique(m8.df$id)))
#
(banp_all_avg_monthly_payments_frac = banp_all_avg_monthly_payments/all_all_avg_monthly_payments)
#
(banp_all_total_val_payments = sum(subset(m8.df, payment==1 & pi == 6)$amnt))
#
(banp_all_val_payments_frac = banp_all_total_val_payments/all_all_total_val_payments)
#
(banp_all_avg_val_payments = mean(subset(m8.df, payment==1 & pi == 6)$amnt)) 
#
(banp_all_med_val_payments = median(subset(m8.df, payment==1 & pi == 6)$amnt)) 
#
(banp_all_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi == 6 & bill==1))/length(unique(m8.df$id)))
#
(banp_all_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi == 6 & bill==1))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1)))
#
(banp_all_total_val_bills = sum(subset(m8.df, payment==1 & pi == 6 & bill==1)$amnt))
#
(banp_all_val_bills_frac = sum(subset(m8.df, payment==1 & pi == 6 & bill==1)$amnt)/sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill == 1)$amnt))

(pay_banp_all.vec = c(banp_all_avg_monthly_payments, 100*banp_all_avg_monthly_payments_frac, banp_all_total_val_payments, 100*banp_all_val_payments_frac, banp_all_avg_val_payments, banp_all_med_val_payments, banp_all_avg_monthly_bills, 100*banp_all_monthly_bills_frac, banp_all_total_val_bills, 100*banp_all_val_bills_frac))

# continue banp column with Group 1 Table 2
(banp_group1_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi == 6 & adopt=="Group1"))/length(unique(subset(m8.df, adopt=="Group1")$id)))
#
(banp_group1_avg_monthly_payments_frac = banp_group1_avg_monthly_payments/all_group1_avg_monthly_payments)
#
(banp_group1_total_val_payments = sum(subset(m8.df, payment==1 & pi == 6 & adopt=="Group1")$amnt))
#
(banp_group1_val_payments_frac = banp_group1_total_val_payments/all_group1_total_val_payments)
#
(banp_group1_avg_val_payments = mean(subset(m8.df, payment==1 & pi == 6 & adopt=="Group1")$amnt)) 
#
(banp_group1_med_val_payments = median(subset(m8.df, payment==1 & pi == 6 & adopt=="Group1")$amnt)) 
#
(banp_group1_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi == 6 & bill==1 & adopt=="Group1"))/length(unique(subset(m8.df, adopt=="Group1" )$id)))
#
(banp_group1_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi == 6 & bill==1 & adopt=="Group1"))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group1" & bill==1)))
#
(banp_group1_total_val_bills = sum(subset(m8.df, payment==1 & pi == 6 & bill==1 & adopt=="Group1")$amnt))
#
(banp_group1_val_bills_frac = sum(subset(m8.df, payment==1 & pi ==  6 & bill==1 & adopt=="Group1")$amnt)/sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group1")$amnt))

(pay_banp_group1.vec = c(banp_group1_avg_monthly_payments, 100*banp_group1_avg_monthly_payments_frac, banp_group1_total_val_payments, 100*banp_group1_val_payments_frac, banp_group1_avg_val_payments, banp_group1_med_val_payments, banp_group1_avg_monthly_bills, 100*banp_group1_monthly_bills_frac, banp_group1_total_val_bills, 100*banp_group1_val_bills_frac))

# continue banp column with Group 2 Table 2
(banp_group2_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi == 6 & adopt=="Group2"))/length(unique(subset(m8.df, adopt=="Group2")$id)))
#
(banp_group2_avg_monthly_payments_frac = banp_group2_avg_monthly_payments/all_group2_avg_monthly_payments)
#
(banp_group2_total_val_payments = sum(subset(m8.df, payment==1 & pi == 6 & adopt=="Group2")$amnt))
#
(banp_group2_val_payments_frac = banp_group2_total_val_payments/all_group2_total_val_payments)
#
(banp_group2_avg_val_payments = mean(subset(m8.df, payment==1 & pi == 6 & adopt=="Group2")$amnt)) 
#
(banp_group2_med_val_payments = median(subset(m8.df, payment==1 & pi == 6 & adopt=="Group2")$amnt)) 
#
(banp_group2_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi == 6 & bill==1 & adopt=="Group2"))/length(unique(subset(m8.df, adopt=="Group2" )$id)))
#
(banp_group2_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi == 6 & bill==1 & adopt=="Group2"))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group2" & bill==1)))
#
(banp_group2_total_val_bills = sum(subset(m8.df, payment==1 & pi == 6 & bill==1 & adopt=="Group2")$amnt))
#
(banp_group2_val_bills_frac = sum(subset(m8.df, payment==1 & pi == 6 & bill==1 & adopt=="Group2")$amnt)/sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group2")$amnt))

(pay_banp_group2.vec = c(banp_group2_avg_monthly_payments, 100*banp_group2_avg_monthly_payments_frac, banp_group2_total_val_payments, 100*banp_group2_val_payments_frac, banp_group2_avg_val_payments, banp_group2_med_val_payments, banp_group2_avg_monthly_bills, 100*banp_group2_monthly_bills_frac, banp_group2_total_val_bills, 100*banp_group2_val_bills_frac))

# continue banp column with Group 3 Table 2
(banp_group3_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi == 6 & adopt=="Group3"))/length(unique(subset(m8.df, adopt=="Group3")$id)))
#
(banp_group3_avg_monthly_payments_frac = banp_group3_avg_monthly_payments/all_group3_avg_monthly_payments)
#
(banp_group3_total_val_payments = sum(subset(m8.df, payment==1 & pi == 6 & adopt=="Group3")$amnt))
#
(banp_group3_val_payments_frac = banp_group3_total_val_payments/all_group3_total_val_payments)
#
(banp_group3_avg_val_payments = mean(subset(m8.df, payment==1 & pi == 6 & adopt=="Group3")$amnt)) 
#
(banp_group3_med_val_payments = median(subset(m8.df, payment==1 & pi == 6 & adopt=="Group3")$amnt)) 
#
(banp_group3_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi == 6 & bill==1 & adopt=="Group3"))/length(unique(subset(m8.df, adopt=="Group3" )$id)))
#
(banp_group3_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi == 6 & bill==1 & adopt=="Group3"))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group3" & bill==1)))
#
(banp_group3_total_val_bills = sum(subset(m8.df, payment==1 & pi == 6 & bill==1 & adopt=="Group3")$amnt))
#
(banp_group3_val_bills_frac = sum(subset(m8.df, payment==1 & pi ==  6 & bill==1 & adopt=="Group3")$amnt)/sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group3")$amnt))

(pay_banp_group3.vec = c(banp_group3_avg_monthly_payments, 100*banp_group3_avg_monthly_payments_frac, banp_group3_total_val_payments, 100*banp_group3_val_payments_frac, banp_group3_avg_val_payments, banp_group3_med_val_payments, banp_group3_avg_monthly_bills, 100*banp_group3_monthly_bills_frac, banp_group3_total_val_bills, 100*banp_group3_val_bills_frac))

## Start obbp (pi==7) column Table 2
(obbp_all_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi== 7))/length(unique(m8.df$id)))
#
(obbp_all_avg_monthly_payments_frac = obbp_all_avg_monthly_payments/all_all_avg_monthly_payments)
#
(obbp_all_total_val_payments = sum(subset(m8.df, payment==1 & pi == 7)$amnt))
#
(obbp_all_val_payments_frac = obbp_all_total_val_payments/all_all_total_val_payments)
#
(obbp_all_avg_val_payments = mean(subset(m8.df, payment==1 & pi == 7)$amnt)) 
#
(obbp_all_med_val_payments = median(subset(m8.df, payment==1 & pi == 7)$amnt)) 
#
(obbp_all_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi == 7 & bill==1))/length(unique(m8.df$id)))
#
(obbp_all_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi == 7 & bill==1))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1)))
#
(obbp_all_total_val_bills = sum(subset(m8.df, payment==1 & pi == 7 & bill==1)$amnt))
#
(obbp_all_val_bills_frac = sum(subset(m8.df, payment==1 & pi == 7 & bill==1)$amnt)/sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill == 1)$amnt))

(pay_obbp_all.vec = c(obbp_all_avg_monthly_payments, 100*obbp_all_avg_monthly_payments_frac, obbp_all_total_val_payments, 100*obbp_all_val_payments_frac, obbp_all_avg_val_payments, obbp_all_med_val_payments, obbp_all_avg_monthly_bills, 100*obbp_all_monthly_bills_frac, obbp_all_total_val_bills, 100*obbp_all_val_bills_frac))

# continue obbp column with Group 1 Table 2
(obbp_group1_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi == 7 & adopt=="Group1"))/length(unique(subset(m8.df, adopt=="Group1")$id)))
#
(obbp_group1_avg_monthly_payments_frac = obbp_group1_avg_monthly_payments/all_group1_avg_monthly_payments)
#
(obbp_group1_total_val_payments = sum(subset(m8.df, payment==1 & pi == 7 & adopt=="Group1")$amnt))
#
(obbp_group1_val_payments_frac = obbp_group1_total_val_payments/all_group1_total_val_payments)
#
(obbp_group1_avg_val_payments = mean(subset(m8.df, payment==1 & pi == 7 & adopt=="Group1")$amnt)) 
#
(obbp_group1_med_val_payments = median(subset(m8.df, payment==1 & pi == 7 & adopt=="Group1")$amnt)) 
#
(obbp_group1_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi == 7 & bill==1 & adopt=="Group1"))/length(unique(subset(m8.df, adopt=="Group1" )$id)))
#
(obbp_group1_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi == 7 & bill==1 & adopt=="Group1"))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group1" & bill==1)))
#
(obbp_group1_total_val_bills = sum(subset(m8.df, payment==1 & pi == 7 & bill==1 & adopt=="Group1")$amnt))
#
(obbp_group1_val_bills_frac = sum(subset(m8.df, payment==1 & pi ==  7 & bill==1 & adopt=="Group1")$amnt)/sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group1")$amnt))

(pay_obbp_group1.vec = c(obbp_group1_avg_monthly_payments, 100*obbp_group1_avg_monthly_payments_frac, obbp_group1_total_val_payments, 100*obbp_group1_val_payments_frac, obbp_group1_avg_val_payments, obbp_group1_med_val_payments, obbp_group1_avg_monthly_bills, 100*obbp_group1_monthly_bills_frac, obbp_group1_total_val_bills, 100*obbp_group1_val_bills_frac))

# continue obbp column with Group 2 Table 2
(obbp_group2_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi == 7 & adopt=="Group2"))/length(unique(subset(m8.df, adopt=="Group2")$id)))
#
(obbp_group2_avg_monthly_payments_frac = obbp_group2_avg_monthly_payments/all_group2_avg_monthly_payments)
#
(obbp_group2_total_val_payments = sum(subset(m8.df, payment==1 & pi == 7 & adopt=="Group2")$amnt))
#
(obbp_group2_val_payments_frac = obbp_group2_total_val_payments/all_group2_total_val_payments)
#
(obbp_group2_avg_val_payments = mean(subset(m8.df, payment==1 & pi == 7 & adopt=="Group2")$amnt)) 
#
(obbp_group2_med_val_payments = median(subset(m8.df, payment==1 & pi == 7 & adopt=="Group2")$amnt)) 
#
(obbp_group2_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi == 7 & bill==1 & adopt=="Group2"))/length(unique(subset(m8.df, adopt=="Group2" )$id)))
#
(obbp_group2_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi == 7 & bill==1 & adopt=="Group2"))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group2" & bill==1)))
#
(obbp_group2_total_val_bills = sum(subset(m8.df, payment==1 & pi == 7 & bill==1 & adopt=="Group2")$amnt))
#
(obbp_group2_val_bills_frac = sum(subset(m8.df, payment==1 & pi == 7 & bill==1 & adopt=="Group2")$amnt)/sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group2")$amnt))

(pay_obbp_group2.vec = c(obbp_group2_avg_monthly_payments, 100*obbp_group2_avg_monthly_payments_frac, obbp_group2_total_val_payments, 100*obbp_group2_val_payments_frac, obbp_group2_avg_val_payments, obbp_group2_med_val_payments, obbp_group2_avg_monthly_bills, 100*obbp_group2_monthly_bills_frac, obbp_group2_total_val_bills, 100*obbp_group2_val_bills_frac))

# continue obbp column with Group 3 Table 2
(obbp_group3_avg_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi == 7 & adopt=="Group3"))/length(unique(subset(m8.df, adopt=="Group3")$id)))
#
(obbp_group3_avg_monthly_payments_frac = obbp_group3_avg_monthly_payments/all_group3_avg_monthly_payments)
#
(obbp_group3_total_val_payments = sum(subset(m8.df, payment==1 & pi == 7 & adopt=="Group3")$amnt))
#
(obbp_group3_val_payments_frac = obbp_group3_total_val_payments/all_group3_total_val_payments)
#
(obbp_group3_avg_val_payments = mean(subset(m8.df, payment==1 & pi == 7 & adopt=="Group3")$amnt)) 
#
(obbp_group3_med_val_payments = median(subset(m8.df, payment==1 & pi == 7 & adopt=="Group3")$amnt)) 
#
(obbp_group3_avg_monthly_bills = (31/3)*nrow(subset(m8.df, payment==1 & pi == 7 & bill==1 & adopt=="Group3"))/length(unique(subset(m8.df, adopt=="Group3" )$id)))
#
(obbp_group3_monthly_bills_frac =  nrow(subset(m8.df, payment==1 & pi == 7 & bill==1 & adopt=="Group3"))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group3" & bill==1)))
#
(obbp_group3_total_val_bills = sum(subset(m8.df, payment==1 & pi == 7 & bill==1 & adopt=="Group3")$amnt))
#
(obbp_group3_val_bills_frac = sum(subset(m8.df, payment==1 & pi ==  7 & bill==1 & adopt=="Group3")$amnt)/sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1 & adopt=="Group3")$amnt))

(pay_obbp_group3.vec = c(obbp_group3_avg_monthly_payments, 100*obbp_group3_avg_monthly_payments_frac, obbp_group3_total_val_payments, 100*obbp_group3_val_payments_frac, obbp_group3_avg_val_payments, obbp_group3_med_val_payments, obbp_group3_avg_monthly_bills, 100*obbp_group3_monthly_bills_frac, obbp_group3_total_val_bills, 100*obbp_group3_val_bills_frac))

## Finalizing Table 2 (how people pay)
# All column
(pay_all.vec = c(pay_all_all.vec, pay_all_group1.vec, pay_all_group2.vec, pay_all_group3.vec))
#
(pay_cash.vec = c(pay_cash_all.vec, pay_cash_group1.vec, pay_cash_group2.vec, pay_cash_group3.vec))
#
(pay_check.vec = c(pay_check_all.vec, pay_check_group1.vec, pay_check_group2.vec, pay_check_group3.vec))
#
(pay_credit.vec = c(pay_credit_all.vec, pay_credit_group1.vec, pay_credit_group2.vec, pay_credit_group3.vec))
#
(pay_debit.vec = c(pay_debit_all.vec, pay_debit_group1.vec, pay_debit_group2.vec, pay_debit_group3.vec))
#
(pay_prepaid.vec = c(pay_prepaid_all.vec, pay_prepaid_group1.vec, pay_prepaid_group2.vec, pay_prepaid_group3.vec))
#
(pay_banp.vec = c(pay_banp_all.vec, pay_banp_group1.vec, pay_banp_group2.vec, pay_banp_group3.vec))
#
(pay_obbp.vec = c(pay_obbp_all.vec, pay_obbp_group1.vec, pay_obbp_group2.vec, pay_obbp_group3.vec))

# Make a data frame
# Add row names column
variable.vec = c("Average number of payments", "Share of all payments (%)", "Total value of payments", "Share of all value (%)", "Average value of payments", "Median value of payments", "Average number of bill payments", "Share of all bill payments (%)", "Total value of bill payments", "Share of value of bill payments (%)")
length(variable.vec)
#
# Add list of 4 groups
(group_list.vec = c(rep("All", 10), rep("Group 1", 10), rep("Group 2", 10), rep("Group 3", 10)))

(pay1.df = data.frame("Group" = group_list.vec, "Variable" = variable.vec, "All" = round(pay_all.vec,2), "Cash" = round(pay_cash.vec,2), "Check" = round(pay_check.vec, 2), "Credit" = round(pay_credit.vec, 2), "Debit" = round(pay_debit.vec, 2), "Prepaid" = round(pay_prepaid.vec, 2), "BANP" = round(pay_banp.vec, 2), "OBBP" = round(pay_obbp.vec, 2)) )
#
dim(pay1.df)

#
# Export to csv
write.csv(pay1.df, "table_2.csv")
 
### Start Table 5: Regression of groups 1,2,3 (adopt var in i3.df) on demographic variables. Data uses indiv.df and variable to follow the order Table 3: 
desc.df

names(i3.df)
table(i3.df$adopt)
sum(table(i3.df$adopt))
i4.df = i3.df

# Three new binary variables
i4.df$Group_1 = NA
i4.df$Group_1 = if_else(i4.df$adopt=="Group1", 1, 0)
table(i4.df$Group_1)
i4.df$Group_2 = NA
i4.df$Group_2 = if_else(i4.df$adopt=="Group2", 1, 0)
table(i4.df$Group_2)
i4.df$Group_3 = NA
i4.df$Group_3 = if_else(i4.df$adopt=="Group3", 1, 0)
table(i4.df$Group_3)

# Need to modify some variables in i4.df to factors
i5.df = i4.df
#
table(i5.df$gender)
i5.df$gender_is_ = NA
i5.df$gender_is_ = factor(ifelse(i5.df$gender==0, "female", "male"))
table(i5.df$gender_is_)
str(i5.df$gender_is_)
#
str(i5.df$age)# leave as numeric
#
table(i5.df$race)
i5.df$race_is_ = NA
i5.df[which(i5.df$race==1), ]$race_is_="white"
i5.df[which(i5.df$race==2), ]$race_is_="black"
i5.df[which(i5.df$race==4), ]$race_is_="asian"
i5.df[which(i5.df$race %in% c(3, 5, 6, NA)), ]$race_is_="other"
str(i5.df$race_is_)
i5.df$race_is_ = factor(i5.df$race_is_, levels = c("white", "black", "asian", "other"))
table(i5.df$race_is_)

str(i5.df$hispaniclatinx)
table(i5.df$hispaniclatinx)
i5.df$hispanic_ = NA
table(i5.df$hispaniclatinx)
i5.df[which(i5.df$hispaniclatinx==1), ]$hispanic_="yes"
i5.df[which(i5.df$hispaniclatinx==0), ]$hispanic_="no"
table(i5.df$hispanic_)
i5.df$hispanic_ = factor(i5.df$hispanic_, levels = c( "no", "yes"))
table(i5.df$hispanic_)

table(i5.df$citizen)
i5.df$citizen_ = NA
i5.df[which(i5.df$citizen==1), ]$citizen_="yes"
i5.df[which(i5.df$citizen==0), ]$citizen_="no"
i5.df$citizen_ = factor(i5.df$citizen_, levels = c("yes", "no"))
table(i5.df$citizen_)

table(i5.df$highest_education)
i5.df$edu_=NA
i5.df[which(i5.df$highest_education < 9), ]$edu_ = "less_than_high_school"
#
i5.df[which(i5.df$highest_education == 9), ]$edu_ = "high_school"
#
i5.df[which(i5.df$highest_education ==10), ]$edu_ = "some_college"
#
i5.df[which(i5.df$highest_education == 11 | i5.df$highest_education == 12), ]$edu_ = "associate"
#
i5.df[which(i5.df$highest_education == 13), ]$edu_ = "college"
#
i5.df[which(i5.df$highest_education > 13), ]$edu_ = "graduate"
#
table(i5.df$edu_)
str(table(i5.df$edu_))
#
i5.df$edu_ = factor(i5.df$edu_, levels = c("college",  "less_than_high_school", "high_school", "some_college", "associate", "graduate"))
table(i5.df$edu_)

str(i5.df$income_hh)
summary(i5.df$income_hh)
i5.df$log10_income_hh = log(i5.df$income_hh + 1, 10)
summary(i5.df$log10_income_hh)

i5.df$income_hh_1000 = i5.df$income_hh/1000# income in k for the regression.

# removing a person with 40m income and 95 NAs
max(i5.df$income_hh, na.rm = T)
nrow(i5.df)
i5_reg.df = subset(i5.df, income_hh < max(i5.df$income_hh, na.rm = T))
nrow(i5_reg.df)
summary(i5_reg.df$income_hh)

# # regressions using log 10 income
# (g1.reg = logitmfx(Group_1 ~ gender_is_ + age + race_is_ +  hispanic_ + citizen_ + edu_ + log10_income_hh , data = i5.df))
# (g2.reg = logitmfx(Group_2 ~ gender_is_ + age + race_is_ +  hispanic_ + citizen_ + edu_ + log10_income_hh , data = i5.df))
# (g3.reg = logitmfx(Group_3 ~ gender_is_ + age + race_is_ +  hispanic_ + citizen_ + edu_ + log10_income_hh , data = i5.df))

# # regressions using income (not log)
# (g1.reg = logitmfx(Group_1 ~ gender_is_ + age + race_is_ +  hispanic_ + citizen_ + edu_ + income_hh , data = i5.df))
# (g2.reg = logitmfx(Group_2 ~ gender_is_ + age + race_is_ +  hispanic_ + citizen_ + edu_ + income_hh , data = i5.df))
# (g3.reg = logitmfx(Group_3 ~ gender_is_ + age + race_is_ +  hispanic_ + citizen_ + edu_ + income_hh , data = i5.df))

# regressions using income in 1000s
(g1.reg = logitmfx(Group_1 ~ gender_is_ + age + race_is_ +  hispanic_ + citizen_ + edu_ + income_hh_1000 , data = i5_reg.df, atmean = F))
(g2.reg = logitmfx(Group_2 ~ gender_is_ + age + race_is_ +  hispanic_ + citizen_ + edu_ + income_hh_1000 , data = i5_reg.df, atmean = F))
(g3.reg = logitmfx(Group_3 ~ gender_is_ + age + race_is_ +  hispanic_ + citizen_ + edu_ + income_hh_1000 , data = i5_reg.df, atmean = F))

# below conversion to LaTeX
#texreg(list(g1.reg, g2.reg, g3.reg), custom.model.names = c("Group 1", "Group 2", "Group 3"), digits = 4)

# converting to HTML (can be read by Excel)
htmlreg(list(g1.reg, g2.reg, g3.reg), custom.model.names = c("Group 1", "Group 2", "Group 3"), digits = 4)

### Start table 6: Average num and value of payments by group. Date are taken from Table 2. Recall:

#all_all_avg_monthly_payments
all_group1_avg_monthly_payments
all_group2_avg_monthly_payments
all_group3_avg_monthly_payments
#
all_group1_avg_monthly_bills
all_group2_avg_monthly_bills
all_group3_avg_monthly_bills

# Share of bills in all payments
(all_group1_share_bill_in_payments = all_group1_avg_monthly_bills/all_group1_avg_monthly_payments)
#
(all_group2_share_bill_in_payments = all_group2_avg_monthly_bills/all_group2_avg_monthly_payments)
#
(all_group3_share_bill_in_payments = all_group3_avg_monthly_bills/all_group3_avg_monthly_payments)


# getting share of remote to in-person num payments
table(m8.df$in_person)# showing 1 and 2 instead of 0 and 1.

#Total payments group 1
(all_group1_total_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group1")))
# out of that, in-person
(all_group1_total_monthly_payments_in_person = (31/3)*nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group1" & in_person==1)))
# share remote to in-person group 1
(all_group1_remote_to_in_person_frac = nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group1" & in_person==2))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group1" & in_person==1)))
#
#Total payments group 2
(all_group2_total_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group2")))
# out of that, in-person
(all_group2_total_monthly_payments_in_person = (31/3)*nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group2" & in_person==1)))
# share remote to in-person group 2
(all_group2_remote_to_in_person_frac = nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group2" & in_person==2))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group2" & in_person==1)))
#
#Total payments group 3
(all_group3_total_monthly_payments = (31/3)*nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group3")))
# out of that, in-person
(all_group3_total_monthly_payments_in_person = (31/3)*nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group3" & in_person==1)))
# share remote to in-person group 1
(all_group3_remote_to_in_person_frac = nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group3" & in_person==2))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group3" & in_person==1)))
                                              
# avg num p2p: merch == 16
(all_all_avg_monthly_p2p = (31/3)*nrow(subset(m8.df, payment==1 & merch==16 & pi%in% 1:7 ))/length((unique(m8.df$id))))
#
(all_group1_avg_monthly_p2p = (31/3)*nrow(subset(m8.df, payment==1 & merch==16 & pi%in% 1:7 & adopt=="Group1"))/length(unique(subset(m8.df, adopt=="Group1")$id)))
#
(all_group2_avg_monthly_p2p = (31/3)*nrow(subset(m8.df, payment==1 & merch==16 & pi%in% 1:7 & adopt=="Group2"))/length(unique(subset(m8.df, adopt=="Group2")$id)))
#
(all_group3_avg_monthly_p2p = (31/3)*nrow(subset(m8.df, payment==1 & merch==16 & pi%in% 1:7 & adopt=="Group3"))/length(unique(subset(m8.df, adopt=="Group3")$id)))

#All Share of p2p in all payments
(all_group1_share_p2p_in_payments = nrow(subset(m8.df, payment==1 & merch==16 & pi%in% 1:7 & adopt=="Group1"))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group1")))
#
(all_group2_share_p2p_in_payments = nrow(subset(m8.df, payment==1 & merch==16 & pi%in% 1:7 & adopt=="Group2"))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group2")))
#
(all_group3_share_p2p_in_payments = nrow(subset(m8.df, payment==1 & merch==16 & pi%in% 1:7 & adopt=="Group3"))/nrow(subset(m8.df, payment==1 & pi%in% 1:7 & adopt=="Group3")))

#
# put it into group vectors
(group1_paymements.vec = c(all_group1_avg_monthly_payments, all_group1_avg_monthly_bills, 100*all_group1_share_bill_in_payments, 100*all_group1_remote_to_in_person_frac, all_group1_avg_monthly_p2p, 100*all_group1_share_p2p_in_payments))
#
(group2_paymements.vec = c(all_group2_avg_monthly_payments, all_group2_avg_monthly_bills, 100*all_group2_share_bill_in_payments, 100*all_group2_remote_to_in_person_frac, all_group2_avg_monthly_p2p, 100*all_group2_share_p2p_in_payments))
#
(group3_paymements.vec = c(all_group3_avg_monthly_payments, all_group3_avg_monthly_bills, 100*all_group3_share_bill_in_payments, 100*all_group3_remote_to_in_person_frac, all_group3_avg_monthly_p2p, 100*all_group3_share_p2p_in_payments))

#
(avg_num_var.vec = c("Average monthly number of payments", "Average monthly number of bill payments", "Share of bill payments (%)" , "Share remote to in_person (%)", "Average monthly number of p2p payments", "Share of p2p payments (%)"))

# make it a data frame
(avg_num.df = data.frame("Variable" = avg_num_var.vec, "Group_1" = group1_paymements.vec, "Group_2" = group2_paymements.vec, "Group_3" = group3_paymements.vec ))

# export to Excel
write.csv(avg_num.df, "table_6.csv")

### Not for this paper: Here I use weights to compare our results to Kevin's who uses the nopayment variable in the day-level dataset.

dim(m8.df)
length(unique(m8.df$id))
names(m8.df)
sum(m8.df$t_weight)
dim(m8.df)

(all_all_avg_monthly_payments_weighted = (31/3)*sum(subset(m8.df, payment==1 & pi%in% 1:7)$t_weight)/length(unique(m8.df$id)))
#
(all_all_avg_monthly_bills = (31/3)*sum(subset(m8.df, payment==1 & pi%in% 1:7 & bill==1)$t_weight)/length(unique(m8.df$id)))


# ### Start classification tree.  [save but not used]
# # setting up the tree model
# group_model1 = adopt ~ gender_is_ + age + race_is_ +  hispanic_ + citizen_ + edu_ + income_hh
# #+ log10_income_hh
# #
# set.seed(1955)# to be able to reproduce the rpart CV below
# group_tree1 = rpart(group_model1, data = i5.df, method = "class", control = rpart.control(cp = 0.001))# Extremely-long tree first, then prune it
# #Below, plot a tree (Note: Longer than optimal, but needed for later prunning and redrawing). 
# prp(group_tree1, type = 3, box.palette = "auto", extra = 100, under = T, tweak = 1.0, varlen = 0, faclen = 0)#faclet=0 avoids abbreviations, tweak for char size
# #now search for optimal cp, rpart has cp table built in
# plotcp(group_tree1)# plot cp: Not used for this demo plot. See training data below
# names(group_tree1)
# group_tree1$cptable # List cp, number of splits and errors
# # Below, I choose cp to use for pruning (highest rel error below the dashed line)
# (cp.choice = group_tree1$cptable[4, "CP"]) # Corresponds to 6 splits (just for demonstration)
# group_prune1 = prune.rpart(group_tree1, cp=cp.choice)
# #
# #rpart.plot(pi_prune2, cex = 1)
# 
# rpart.plot(group_prune1, type = 5, extra = 100, legend.x=NA, legend.y=NA, tweak = 1.9, fallen.leaves = FALSE, gap = 0, space = 1, digits = 4, compress = T, ycompress = F)
# 
# # Saving the above plot on your hard drive 
# #png(filename='Figure_5.png', type='cairo-png');
# #rpart.plot(group_prune1, type = 5, extra = 100, legend.x=NA, legend.y=NA, tweak = 1.9, fallen.leaves = FALSE, gap = 0, space = 1, digits = 4, compress = T, ycompress = F)
# #dev.off();
# 
# ### Random Forest VIP graph [saved, but not used]
# # making adopt a factor (should have been done earlier)
# i6.df = i5.df
# i6.df$adopt = factor(i6.df$adopt)
# levels(i6.df$adopt)
# table(i6.df$adopt)
# 
# group_rf=randomForest(group_model1, data=i6.df, mtry=3, importance=T, na.action=na.roughfix)
# importance(group_rf) # Table of variable importance
# # Below, Plot of variable importance (displayed in paper)
# varImpPlot(group_rf, type = 1, main ='' )#default type 1&2, 

