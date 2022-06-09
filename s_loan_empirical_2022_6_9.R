# s_loan plot tuition versus loan rates
# Packages
library(ggplot2); theme_set(theme_bw())
#library(latex2exp)# LaTeX in ggplot
library(xtable)

setwd("~/Papers/s_loan/s_loan_R_Derive")
dir()

t1.df = read.csv("tuition_2022_5_30.csv")# read tuition
#
f1.df = read.csv("loans_fixed_2022_5_30.csv")# read fixed loan rates 2006-2019 (academic year start date)
#
v1.df = read.csv("loans_var_2022_5_30.csv")# read var rates 1992-2005 (academic year start date)
#
g1.df = read.csv("loans_gsl_2022_6_1.csv")# read GSL rates 1969-1991  (academic year start date)

str(t1.df)
t1.df
dim(t1.df)
t2.df = t1.df
names(t2.df)
names(t2.df) = c("year_start", "year_end", "tuition_public", "tuition_private")
t2.df$tuition_public = as.numeric(t2.df$tuition_public)
t2.df$tuition_private = as.numeric(t2.df$tuition_private)
t2.df
# delete academic year 1963-64 and 1968 to maintain continuity to start with 1969-70
(t3.df = t2.df[-c(1,2), ])

str(f1.df)
f2.df = f1.df
names(f2.df) = c("year_start", "year_end", "subsidized", "unsubsidized")
f2.df

str(v1.df)
v2.df = v1.df
names(v2.df) = c("year_start", "year_end", "in_school", "repayment")
v2.df

str(g1.df)
g2.df = g1.df
names(g2.df) = c("year_start", "year_end", "type", "gsl_rate", "t_bill_rate")
g2.df

# Merging the 3 datasets
(m1.df = merge(x=t3.df, y=f2.df, all.x = T))# merge fixed loan rate starting 2006-7
#
(m2.df = merge(x=m1.df, y=v2.df, all.x = T))# merge variable loan rate starting 1992-3 , ending 2005-2006
#
(m3.df = merge(x=m2.df, y=g2.df, all.x = T))# merge gls rate
#
names(m3.df)

#remove 2 columns (not used for plotting)
m4.df = subset(m3.df, select = c("year_start", "year_end",  "tuition_public", "tuition_private", "subsidized", "unsubsidized",  "in_school", "repayment", "gsl_rate" ))
names(m4.df)


# plot tuition and loan rates
#y_axis_2_scale = 100*100*50 #scaling right y axis for interest rate
#ggplot(m4.df, aes(x=year_end, y=tuition_public)) + geom_line(size=1.2, color="black") + geom_line(aes(x=year_end, y=tuition_private), size=1.2, color="black") + xlab("Academic year end") + ylab("Total tuition, fees, room, and board (constant 2019-2020 prices)") + geom_line(aes(x=year_end, y=y_axis_2_scale*subsidized), size=1.2, color="blue") + geom_line(aes(x=year_end, y=y_axis_2_scale*unsubsidized), size=1.2, linetype="twodash", color="blue") + geom_line(aes(x=year_end, y=y_axis_2_scale*repayment), size=1.2, linetype = "twodash", color="blue") +geom_line(aes(x=year_end, y=y_axis_2_scale*in_school), size=1.2, color="blue")  +geom_line(aes(x=year_end, y=y_axis_2_scale*gsl_rate), size=1.2, color="blue") + scale_y_continuous(breaks = seq(0, 50000, 5000), name = "Total tuition, fees, room, and board (2019-2020 prices)", sec.axis = sec_axis(trans = ~./y_axis_2_scale , name = "Student loan interest rate")) + theme(axis.text.y.right = element_text(color="blue")) + theme(axis.title.y.right = element_text(color="blue"))+ theme(axis.text.x = element_text(size = 18, color = "black"), axis.text.y = element_text(size = 18, color = "black"), text = element_text(size = 20)) +annotate("text", x = 1999.5, y = 7000, label = "Stafford Variable", size = 7, color="blue") +annotate("text", x = 1990.5, y = 7000, label = "GSL Var", size = 7, color="blue") +annotate("text", x = 1977, y = 7000, label = "GSL fixed", size = 7, color="blue") +annotate("text", x = 2015, y = 7000, label = "Stafford Fixed", size = 7, color="blue") +annotate("text", x = 1975, y = 20000, label = "Private college", size = 7, color="black") +annotate("text", x = 1975, y = 11000, label = "Public college", size = 7, color="black") + geom_vline(xintercept = 2006.5, size=1.5, linetype="dotted")+ geom_vline(xintercept = 1992.5, size=1.5, linetype="dotted") + geom_vline(xintercept = 1988.5, size=1.5, linetype="dotted") 

# Reversing the y-left y-right axes
y_axis_2_scale = 1/(100*10*5) #scaling right y axis for interest rate
ggplot(m4.df, aes(x=year_end, y=100*gsl_rate)) + geom_line(size=1.2, color="black") + geom_line(aes(x=year_end, y=100*subsidized), size=1.2, color="black") + geom_line(aes(x=year_end, y=100*unsubsidized), size=1.2, linetype="twodash", color="black") + geom_line(aes(x=year_end, y=100*repayment), size=1.2, linetype = "twodash", color="black") +geom_line(aes(x=year_end, y=100*in_school), size=1.2, color="black") + geom_line(aes(x=year_end, y=y_axis_2_scale*tuition_private), size=1.2, color="blue") + geom_line(aes(x=year_end, y=y_axis_2_scale*tuition_public), size=1.2, color="blue") +annotate("text", x = 1999.5, y = 1, label = "Stafford variable rate", size = 7, color="black") +annotate("text", x = 1990.5, y = 1, label = "GSL V", size = 7, color="black") +annotate("text", x = 1977, y = 1, label = "GSL fixed rate", size = 7, color="black") +annotate("text", x = 2015, y = 1, label = "Stafford fixed rate", size = 7, color="black") + geom_vline(xintercept = 2006.5, size=1.5, linetype="dotted")+ geom_vline(xintercept = 1992.5, size=1.5, linetype="dotted") + geom_vline(xintercept = 1988.5, size=1.5, linetype="dotted") + scale_x_continuous(breaks = seq(1970, 2020, 5)) + scale_y_continuous(breaks = seq(0, 10, 0.5), name = "Student loan interest rate (%)" , sec.axis = sec_axis(trans = ~./y_axis_2_scale , name = "Total tuition, fees, room, and board (2019-2020 dollar prices)")) + theme(axis.text.y.right = element_text(color="blue"))  +annotate("text", x = 1975, y = 4, label = "Private college", size = 7, color="blue") +annotate("text", x = 1975, y = 2.1, label = "Public college", size = 7, color="blue") + theme(axis.text.x = element_text(size = 18, color = "black"), axis.text.y = element_text(size = 18, color = "black"), text = element_text(size = 20)) +ylab("Equilibrium tuition levels") + xlab("Academic year end") + theme(axis.title.y.right = element_text(vjust = 3)) + theme(plot.margin = margin(0.5,1,0.5,0.5, "cm"))

# computing correlation coefficients for the variable and fixed rate periods. 
(m4_gsl_fixed.df = subset(m4.df, year_end <=1987))
(m4_gsl_variable.df = subset(m4.df, year_end > 1987 & year_end <=1992))
(m4_variable.df = subset(m4.df, year_end >= 1993 & year_end <=2006))
(m4_fixed.df = subset(m4.df, year_end > 2006))

(gsl_fixed_private = cor(m4_gsl_fixed.df$tuition_private, m4_gsl_fixed.df$gsl_rate))
(gsl_variable_private = cor(m4_gsl_variable.df$tuition_private, m4_gsl_variable.df$gsl_rate))
#
(gsl_fixed_public = cor(m4_gsl_fixed.df$tuition_public, m4_gsl_fixed.df$gsl_rate))
(gsl_variable_public = cor(m4_gsl_variable.df$tuition_public, m4_gsl_variable.df$gsl_rate))
#
(var_private_in_school = cor(m4_variable.df$tuition_private, m4_variable.df$in_school))
(var_private_repayment = cor(m4_variable.df$tuition_private,   m4_variable.df$repayment))
#
(var_public_in_school = cor(m4_variable.df$tuition_public, m4_variable.df$in_school))
(var_public_repayment = cor(m4_variable.df$tuition_public, m4_variable.df$repayment))
#
(fixed_private_subsidized = cor(m4_fixed.df$tuition_private, m4_fixed.df$subsidized))
(fixed_private_unsubsidized = cor(m4_fixed.df$tuition_private, m4_fixed.df$unsubsidized))
(fixed_public_subsidized = cor(m4_fixed.df$tuition_public, m4_fixed.df$subsidized))
(fixed_public_unsubsidized = cor(m4_fixed.df$tuition_public, m4_fixed.df$unsubsidized))

# constructing correlation data frame
(gsl_fixed.vec = c(gsl_fixed_private, gsl_fixed_public))
(gsl_variable.vec = c(gsl_variable_private, gsl_variable_public))
(in_school.vec = c(var_private_in_school, var_public_in_school))
(repayment.vec = c(var_private_repayment, var_public_repayment))
(subsidized.vec = c(fixed_private_subsidized, fixed_public_subsidized))
(unsubsidized.vec = c(fixed_private_unsubsidized, fixed_public_unsubsidized))
#
college_type.vec = c("Private", "Public")

(correlation.df = data.frame("College_type" = college_type.vec, "GSL_fixed" = gsl_fixed.vec, "GSL_variable" = gsl_variable.vec, "In_school"= in_school.vec, "Repayment" = repayment.vec, "Subsidized" = subsidized.vec, "Unsubsidized" = unsubsidized.vec))

#export to LaTeX
print(xtable(correlation.df, digits = 2), include.rownames=FALSE)

### Replacing the above chart (which compares tuition with interest rate) with a comparison of tuition and origination of Federal student loans (in dollars)

# To match the College Board's data on student loans, use year_start instead of year_end. Start with 1970 (year start)
(year_start.vec = m4.df$year_start)
(year_start.vec = year_start.vec[-1])
#
dir()
(fed_loans.df = read.csv("fed_loans_2022_6_8.csv" ))
names(fed_loans.df)
# merge it with grand data frame
dim(m4.df)
range(m4.df$year_start)
range(fed_loans.df$year_start)
#
m5.df = merge(m4.df, fed_loans.df, by="year_start", all.x = F, all.y = T)
range(m5.df$year_start)
names(m5.df)

# plot revised Figure 1: Fed loans versus private and public college tuition. 
y_axis_2_scale = 1/250 #scaling right y axis for interest rate
ggplot(m5.df, aes(x=year_start, y=total_fed_loans_dollar_millions/1000)) + geom_line(size=1.2, color="black") + geom_line(aes(x=year_start, y=y_axis_2_scale*tuition_public), size=1.2, color="blue") + geom_line(aes(x=year_start, y=y_axis_2_scale*tuition_private), size=1.2, color="blue")  + scale_x_continuous(breaks = seq(1970, 2020, 5)) + scale_y_continuous(breaks = seq(0, 180, 10), name = "Total Federal loans in 2020 Dollars (billions)" , sec.axis = sec_axis(trans = ~./y_axis_2_scale , name = "Total tuition, fees, room, and board (2019-2020 dollar prices)")) + theme(axis.text.y.right = element_text(color="blue"))  +annotate("text", x = 1976, y = 83, label = "Private college tuition", size = 7, color="blue") +annotate("text", x = 1976, y = 43, label = "Public college tuition", size = 7, color="blue") +annotate("text", x = 1973, y = 15, label = "Federal loans", size = 7, color="black") + theme(axis.text.x = element_text(size = 18, color = "black"), axis.text.y = element_text(size = 18, color = "black"), text = element_text(size = 20)) +ylab("Equilibrium tuition levels") + xlab("Academic year start") + theme(axis.title.y.right = element_text(vjust = 3)) + theme(plot.margin = margin(0.5,1,0.5,0.5, "cm")) + theme(axis.title.y.right = element_text(color="blue"))

# computing correlations for discussion of Figure 1
names(m5.df)
#
with(m5.df, cor(total_fed_loans_dollar_millions, tuition_private) )
cor(m5$total_fed_loans_dollar_millions, m5$tuition_private)
#
with(m5.df, cor(total_fed_loans_dollar_millions, tuition_public) )
cor(m5$total_fed_loans_dollar_millions, m5$tuition_private)
