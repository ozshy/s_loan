# s_loan plot tuition versus loan rates
# Packages
library(ggplot2); theme_set(theme_bw())
#library(latex2exp)# LaTeX in ggplot
library(xtable)

setwd("~/Papers/s_loan/s_loan_R_Derive")
dir()

#t1.df = read.csv("tuition_2022_5_30.csv")# read tuition
#saveRDS(t1.df, "tuition_2022_5_30.rds")
dir()
t2.df = readRDS("tuition_2022_5_30.rds")
names(t2.df)
names(t2.df) = c("year_start", "year_end", "tuition_public", "tuition_private")
t2.df$tuition_public = as.numeric(t2.df$tuition_public)
t2.df$tuition_private = as.numeric(t2.df$tuition_private)
t2.df
# delete academic year 1963-64 and 1968 to maintain continuity to start with 1969-70
(t3.df = t2.df[-c(1,2), ])
dim(t3.df)

# CPI is NOT used in Figure 1 because Figure 1 is already in constant prices
#cpi1.df = read.csv("cpi_1970_2020_baseyear_1982-84.csv")# read CPI
#saveRDS(cpi1.df, "cpi_1970_2020_baseyear_1982-84.rds")
cpi2.df = readRDS("cpi_1970_2020_baseyear_1982-84.rds")
dim(cpi2.df)
head(cpi2.df)

# merge CPI into tuition data frame [CPI is not used in Figure 1] because Figure 1 is already in constant prices
(m1.df = c(t3.df, cpi2.df))
str(m1.df)
m2.df = as.data.frame(m1.df)
str(m2.df)
names(m2.df)
(names(m2.df) = c("year_start", "year_end", "tuition_public", "tuition_private", "year", "CPI"))

# plot tuition and CPI
#y_axis_2_scale = 100*100*50 #scaling right y axis for interest rate
ggplot(m2.df, aes(x=year_end, y=tuition_public)) + geom_line(size=1.2, color="black") + geom_line(aes(x=year_end, y=tuition_private), size=1.2, color="black") + xlab("Academic year end") + ylab("Total tuition, fees, room, and board (constant 2019-2020 prices)") + scale_y_continuous(breaks = seq(0, 50000, 5000), name = "Total tuition, fees, room, and board (2019-2020 prices)")+ theme(axis.text.x = element_text(size = 18, color = "black"), axis.text.y = element_text(size = 18, color = "black"), text = element_text(size = 20)) +annotate("text", x = 1975, y = 20000, label = "Private college", size = 7, color="black") +annotate("text", x = 1975, y = 11000, label = "Public college", size = 7, color="black")  + scale_x_continuous(breaks = seq(1970, 2020, 5), name = "Academic year end")

# compute CAGR
range(m2.df$year)
length(m2.df$year)
# CAGR for private tuition 1970-2020
(cagr_private = 100*((m2.df$tuition_private[51]/m2.df$tuition_private[1])^(1/51) -1))
# CAGR for public tuition 1970-2020
(cagr_private = 100*((m2.df$tuition_public[51]/m2.df$tuition_public[1])^(1/51) -1))
#
# CAGR for private tuition 1980-2020
(cagr_private = 100*((m2.df$tuition_private[51]/m2.df$tuition_private[11])^(1/41) -1))
# CAGR for public tuition 1980-2020
(cagr_private = 100*((m2.df$tuition_public[51]/m2.df$tuition_public[11])^(1/41) -1))
