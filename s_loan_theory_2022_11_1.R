# s_loan simulations 
# Packages
library(ggplot2); theme_set(theme_bw())
library(latex2exp)# LaTeX in ggplot

### Section 4: Equilibrium with capacity constraints, equation (5)
# Model parameters
(delta = 0.8)# discount factor
(r1 = 0.2) #interest on loans (over 4 years)
(r2 = 0.15) # reduced r (loans are more accessible)
(r.vec = seq(0, 1, 0.01 ))
#(r_thresh = 1/delta-1)# threshold between low and high r (Definition 1)
#
(mua = 20)# ability adjustment parameter college A
(mub = 18)# ability adjustment parameter college B
(mun = 1)# ability nondegree job
(rho = 0.8)# prob college degree job
#
(ka = 0.3) # enrollment capacity of college A
(kb = 0.5) # enrollment capacity of college B
# hence verify
ka + kb < 1
#
# loan cap
c=1
(c.vec = seq(0, 1.5, 0.01))

# Verify Assumption 1(c)
delta*rho*mub/(delta*rho+1)
delta*rho*(4*mua-mub)/(3*(delta*rho+1))
# hence,
mun < min(delta*rho*mub/(delta*rho+1), delta*rho*(4*mua-mub)/(3*(delta*rho+1)))
#
# verify Assumption 2(a)
(1-delta)/delta# Hence,
r1 < (1-delta)/delta
r2 < (1-delta)/delta

# equilibrium tuition: eq (5)
(ta = delta*rho*(mua-mun) - mun - ka*(delta*rho*(mua-mun)-mun) - kb*(delta*rho*(mub-mun)-mun) + c*(1-delta-r1*delta))
# verify loan cap is binding
ta > c
#
(tb = (1-ka-kb)*(delta*rho*(mub-mun)-mun + c*(1-delta-r1*delta)))
# verify loan cap is binding
tb > c
# Now in terms of vectors for drawing (interest r1)
(ta1.vec = delta*rho*(mua-mun) - mun - ka*(delta*rho*(mua-mun)-mun) - kb*(delta*rho*(mub-mun)-mun) + c.vec*(1-delta-r1*delta))
# verify loan cap is binding
ta1.vec > c.vec
#
(ta2.vec = delta*rho*(mua-mun) - mun - ka*(delta*rho*(mua-mun)-mun) - kb*(delta*rho*(mub-mun)-mun) + c.vec*(1-delta-r2*delta))
# verify loan cap is binding
ta2.vec > c.vec
#
(tb1.vec = (1-ka-kb)*(delta*rho*(mub-mun)-mun) + c.vec*(1-delta-r1*delta))
# verify loan cap is binding
tb1.vec > c.vec
#
(tb2.vec = (1-ka-kb)*(delta*rho*(mub-mun)-mun) + c.vec*(1-delta-r2*delta))
# verify loan cap is binding
tb2.vec > c.vec

# abar from equation (3)
(abar1.vec = (c.vec*(r1*delta+delta-1)+tb1.vec)/(delta*rho*(mub-mun)-mun))
# 
# abar from equation (3)
(abar2.vec = (c.vec*(r2*delta+delta-1)+tb2.vec)/(delta*rho*(mub-mun)-mun))

# compute ahat from (3)
(ahat1.vec = (ta1.vec-tb1.vec)/(delta*rho*(mua-mub)))
#
(ahat2.vec = (ta2.vec-tb2.vec)/(delta*rho*(mua-mub)))

# place them into data frame
(eql_sec4.df = data.frame(c.vec, ta1.vec, ta2.vec, tb1.vec, tb2.vec))

# Figure 3: plot teql [ta, tb]
ggplot(eql_sec4.df, aes(x=c.vec, y=ta1.vec)) + geom_line(size=1.2) + geom_line(aes(x=c.vec, y=tb1.vec), size=1.2) + geom_line(aes(x=c.vec, y=ta2.vec), size=1.2, color="red", linetype="longdash")  + geom_line(aes(x=c.vec, y=tb2.vec), size=1.2, color="red", linetype="longdash") + geom_segment(aes(x=0, y=2.5, xend=0.3, yend=2.5), color="magenta", size=1.2, arrow = arrow(length = unit(0.5, "cm"))) +annotate("text", x = 0.75, y = 2.5, label = "Loans become more accessible", size = 7, color="magenta")+annotate("text", x = 0.75, y = 2.45, label = "(Higher cap on loans)", size = 7, color="magenta")  + geom_segment(aes(x=1.2, y=2.5, xend=1.5, yend=2.5), color="magenta", size=1.2, arrow = arrow(length = unit(0.5, "cm"))) + theme(axis.text.x = element_text(size = 18, color = "black"), axis.text.y = element_text(size = 18, color = "black"), text = element_text(size = 20))+ylab("Equilibrium tuition levels with enrollment capacity constraints") + xlab("Cap on student loans (c)") +annotate("text", x = 1.0, y = 2.86, label =TeX("$t_A (r=0.2)$"), size = 7) +annotate("text", x = 1.0, y = 2.99, label =TeX("$t_A (r=0.15)$"), size = 7, color="red") +annotate("text", x = 1.0, y = 1.97, label =TeX("$t_B (r=0.2)$"), size = 7) +annotate("text", x = 1.0, y = 2.1, label =TeX("$t_B (r=0.15)$"), size = 7, color="red") 

#+geom_line(aes(x=r.vec, y=ta.vec - tb.vec), size=1.2, color="red", linetype="longdash") + scale_x_continuous(breaks = seq(0,1,0.1))  + scale_y_continuous(breaks = seq(0,45,0.05)) + theme(axis.text.x = element_text(size = 18, color = "black"), axis.text.y = element_text(size = 18, color = "black"), text = element_text(size = 20)) +ylab("Equilibrium tuition levels") + xlab("Interest rate (r)") +annotate("text", x = 0.6, y = 0.28, label =TeX("$t_A$"), size = 7) +annotate("text", x = 0.6, y = 0.12, label =TeX("$t_B$"), size = 7) +annotate("text", x = 0.6, y = 0.18, label =TeX("$t_A-t_B$"), size = 7, color="red") + geom_segment(aes(x=0.3, y=0.08, xend=0.0, yend=0.08), color="magenta", size=1.2, arrow = arrow(length = unit(0.5, "cm"))) +annotate("text", x = 0.55, y = 0.08, label = "Loans become more affordable", size = 7, color="magenta")  + geom_segment(aes(x=1, y=0.08, xend=0.8, yend=0.08), color="magenta", size=1.2, arrow = arrow(length = unit(0.5, "cm")))

### Section 6: Equilibrium with maximizing colleges: equations (12)-(13) => The figures are not used in paper (these are simulations for the sake of verification)
# Model parameters
(alpha = 0.5)# Weights in colleges objective functions
(delta = 0.8)# discount factor
(r1 = 0.2) #interest on loans (over 4 years)
(r2 = 0.15) # reduced r (loans are more accessible)
(r.vec = seq(0, 1, 0.01 ))
#(r_thresh = 1/delta-1)# threshold between low and high r (Definition 1)
#
(mua = 40)# ability adjustment parameter college A
(mub = 36)# ability adjustment parameter college B
(mun = 10)# ability nondegree job
(rho = 0.8)# prob college degree job
#
#
# loan cap
(c.vec = seq(0, 0.1, 0.001))

# Verify Assumption 1(c)
delta*rho*mub/(delta*rho+1)
delta*rho*(4*mua-mub)/(3*(delta*rho+1))
# hence,
mun < min(delta*rho*mub/(delta*rho+1), delta*rho*(4*mua-mub)/(3*(delta*rho+1)))
#
# verify Assumption 2(a)
(1-delta)/delta# Hence,
r1 < (1-delta)/delta
r2 < (1-delta)/delta

# equilibrium tuition: eq (12)(13)
(lam = 3*mun - delta*rho*(4*mua-mub-3*mun))
#
(ta1_numerator.vec = 2*c.vec*alpha*delta*rho*(mua-mub)*(r1*delta+delta-1) -alpha*(4*delta^2*rho^2*(mua-mub)*(mua-mun) - delta*rho*(mua*(4*mun+3) -2*mub*(2*mun+1)-mun)+mun) -delta*rho*(3*mua - 2*mub - mun)+mun)
(ta1.vec = ta1_numerator.vec/(2*alpha*lam))
# verify loan cap is binding
ta1.vec > c.vec
#
(tb1_numerator.vec = 4*c.vec*alpha*delta*rho*(mua-mub)*(r1*delta+delta-1) -alpha*(2*delta^2*rho^2*(mua-mub)*(mub-mun) -delta*rho*(2*mua*(mun+1) -mub*(2*mun+3) +mun) -mun) -delta*rho*(2*mua-3*mub+mun) -mun)
(tb1.vec = tb1_numerator.vec/(2*alpha*lam))
# verify loan cap is binding
tb1.vec > c.vec
#
# recall
r1; r2
# Now compute ta2 and tb2 under r2
(ta2_numerator.vec = 2*c.vec*alpha*delta*rho*(mua-mub)*(r2*delta+delta-1) -alpha*(4*delta^2*rho^2*(mua-mub)*(mua-mun) - delta*rho*(mua*(4*mun+3) -2*mub*(2*mun+1)-mun)+mun) -delta*rho*(3*mua - 2*mub - mun)+mun)
(ta2.vec = ta2_numerator.vec/(2*alpha*lam))
# verify loan cap is binding
ta2.vec > c.vec
#
(tb2_numerator.vec = 4*c.vec*alpha*delta*rho*(mua-mub)*(r2*delta+delta-1) -alpha*(2*delta^2*rho^2*(mua-mub)*(mub-mun) -delta*rho*(2*mua*(mun+1) -mub*(2*mun+3) +mun) -mun) -delta*rho*(2*mua-3*mub+mun) -mun)
(tb2.vec = tb2_numerator.vec/(2*alpha*lam))
# verify loan cap is binding
tb2.vec > c.vec

# place them into data frame
(eql_sec6.df = data.frame(c.vec, ta1.vec, ta2.vec, tb1.vec, tb2.vec))

# Figure 4: plot teql [ta, tb] (maximizing colleges) => NOT used in paper, just asked as a verification of Result 3
# plot ta1, ta2, tb1, and tb2 together
ggplot(eql_sec6.df, aes(x=c.vec, y=ta1.vec)) + geom_line(size=1.2) + geom_line(aes(x=c.vec, y=tb1.vec), size=1.2) + geom_line(aes(x=c.vec, y=ta2.vec), size=1.2, color="red", linetype="longdash")  + geom_line(aes(x=c.vec, y=tb2.vec), size=1.2, color="red", linetype="longdash") 

# plot ta separately from tb (black for r1 and red for r2 < r1)
ggplot(eql_sec6.df, aes(x=c.vec, y=ta1.vec)) + geom_line(size=1.2)  + geom_line(aes(x=c.vec, y=ta2.vec), size=1.2, color="red", linetype="longdash")  

# plot tb separately from ta
ggplot(eql_sec6.df, aes(x=c.vec, y=tb1.vec)) + geom_line(size=1.2)  + geom_line(aes(x=c.vec, y=tb2.vec), size=1.2, color="red", linetype="longdash")  

# verify ta > tb
ta1.vec > tb1.vec
ta2.vec > tb2.vec

# verify ahat > abar
# abar from equation (3)
(abar1.vec = (c.vec*(r1*delta+delta-1)+tb1.vec)/(delta*rho*(mub-mun)-mun))
# 
(abar2.vec = (c.vec*(r2*delta+delta-1)+tb2.vec)/(delta*rho*(mub-mun)-mun))

# compute ahat from (3)
(ahat1.vec = (ta1.vec-tb1.vec)/(delta*rho*(mua-mub)))
#
(ahat2.vec = (ta2.vec-tb2.vec)/(delta*rho*(mua-mub)))
#
ahat1.vec > abar1.vec
ahat2.vec > abar2.vec

###### End of s_loan_theory R simulation file
