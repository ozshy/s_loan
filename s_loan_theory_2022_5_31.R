# s_loan simulations and calibrations?
# Packages
library(ggplot2); theme_set(theme_bw())
library(latex2exp)# LaTeX in ggplot

# Model parameters
(delta = 0.8)# discount factor
(r = 0.2) #interest on loans (over 4 years)
(r.vec = seq(0, 1, 0.01 ))
(r_thresh = 1/delta-1)# threshold between low and high r (Definition 1)
#
(mu = 3)# ability adjustment parameter
(Nl = 1)# num low-income
(Nh = 1)# num high-income
(rhob = 0.7)# prob employment of college 1 grads
(rhoa = 0.9)# prob employment of college 2 grads

# equilibrium tuition: eq (9)
(tb.vec = delta*mu*rhob*(Nl+Nh)*(rhoa-rhob)/ ((4*rhoa-rhob)* (Nl*delta*(1+r.vec)+Nh)))
(ta.vec = 2*delta*mu*rhoa*(Nl+Nh)*(rhoa-rhob)/ ((4*rhoa-rhob)* (Nl*delta*(1+r.vec)+Nh)))

# equilibrium abar from eq (5)
(abarl.vec = tb.vec*(1+r.vec)/(rhob*mu))
(abarh.vec = tb.vec/(rhob*delta*mu))

# equilibrium ahat from eq (6)
(ahatl.vec = (ta.vec-tb.vec)*(1+r.vec)/(mu*(rhoa-rhob)))
(ahath.vec = (ta.vec-tb.vec)/(delta*mu*(rhoa-rhob)))

# equilibrium abar from eq (11)
#(abarl.vec = (Nl+Nh)*delta*(1+r.vec)*(rhoa-rhob)/ ((4*rhoa-rhob)* (Nl*delta*(1+r.vec)+Nh)))
#(abarh.vec = (Nl+Nh)*(rhoa-rhob)/ ((4*rhoa-rhob)* (Nl*delta*(1+r.vec)+Nh)))

# equilibrium ahat from eq (12)
(ahatl.vec = (Nl+Nh)*delta*(1+r.vec)*(2*rhoa-rhob)/ ((4*rhoa-rhob)* (Nl*delta*(1+r.vec)+Nh)))
(ahath.vec = (Nl+Nh)*(2*rhoa-rhob)/ ((4*rhoa-rhob)* (Nl*delta*(1+r.vec)+Nh)))

# place them into data frame
(teql.df = data.frame(r.vec, tb.vec, ta.vec, abarl.vec, abarh.vec, ahatl.vec, ahath.vec))

# Figure 3: plot teql [ta, tb, deleting abarl and abarh, adding ta-tb]
ggplot(teql.df, aes(x=r.vec, y=tb.vec)) + geom_line(size=1.2) +geom_line(aes(x=r.vec, y=ta.vec), size=1.2) +geom_line(aes(x=r.vec, y=ta.vec - tb.vec), size=1.2, color="red", linetype="longdash") + scale_x_continuous(breaks = seq(0,1,0.1))  + scale_y_continuous(breaks = seq(0,45,0.05)) + theme(axis.text.x = element_text(size = 18, color = "black"), axis.text.y = element_text(size = 18, color = "black"), text = element_text(size = 20)) +ylab("Equilibrium tuition levels") + xlab("Interest rate (r)") +annotate("text", x = 0.6, y = 0.28, label =TeX("$t_A$"), size = 7) +annotate("text", x = 0.6, y = 0.12, label =TeX("$t_B$"), size = 7) +annotate("text", x = 0.6, y = 0.18, label =TeX("$t_A-t_B$"), size = 7, color="red") + geom_segment(aes(x=0.3, y=0.08, xend=0.0, yend=0.08), color="magenta", size=1.2, arrow = arrow(length = unit(0.5, "cm"))) +annotate("text", x = 0.55, y = 0.08, label = "Loans become more affordable", size = 7, color="magenta")  + geom_segment(aes(x=1, y=0.08, xend=0.8, yend=0.08), color="magenta", size=1.2, arrow = arrow(length = unit(0.5, "cm")))

### Figure 4 neql (enrollment by income type and college)
n0l.vec = Nl*abarl.vec # num low-income nonenrolled
n0h.vec = Nh*abarh.vec # num high-income nonenrolled
nal.vec = Nl*(1-ahatl.vec)# num low-income enrolled in A
nah.vec = Nh*(1-ahath.vec)# num high-income enrolled in A
nbl.vec = Nl*(ahatl.vec-abarl.vec)# num low-income enrolled in B
nbh.vec = Nh*(ahath.vec-abarh.vec)# num high-income enrolled in B

# place them into data frame
(neql.df = data.frame(r.vec, n0l.vec, n0h.vec, nal.vec, nah.vec, nbl.vec, nbh.vec))

# plot neql (Nl=Nh=1) Figure 4
ggplot(neql.df, aes(x=r.vec, y=nal.vec)) + geom_line(size=1.2) +geom_line(aes(x=r.vec, y=nah.vec), size=1.2)+geom_line(aes(x=r.vec, y=nbl.vec), linetype="longdash", size=1.2, color="blue") +geom_line(aes(x=r.vec, y=nbh.vec), linetype="longdash", size=1.2, color="blue") +geom_vline(xintercept =r_thresh, size = 1.2, color="red") +geom_line(aes(x=r.vec, y=n0l.vec), linetype="longdash", size=1.2, color="magenta") +geom_line(aes(x=r.vec, y=n0h.vec), linetype="longdash", size=1.2, color="magenta") + scale_x_continuous(breaks = seq(0,1,0.1))  + scale_y_continuous(breaks = seq(0,45,0.05)) + theme(axis.text.x = element_text(size = 18, color = "black"), axis.text.y = element_text(size = 18, color = "black"), text = element_text(size = 20)) +ylab("Equilibrium enrollment and nonenrollment levels") + xlab("Interest rate (r)") +annotate("text", x = 0.5, y = 0.11, label =TeX("$n_0^L$"), size = 7) +annotate("text", x = 0.5, y = 0.03, label =TeX("$n_0^H$"), size = 7) +annotate("text", x = 0.22, y = 0.45, label =TeX("$r =$"), size = 7) +annotate("text", x = 0.29, y = 0.45, label =TeX("$\\frac{1}{\\delta}\\, - 1$"), size = 7) +annotate("text", x = 0.5, y = 0.55, label =TeX("$n_A^L$"), size = 7) +annotate("text", x = 0.5, y = 0.69, label =TeX("$n_A^H$"), size = 7) +annotate("text", x = 0.5, y = 0.25, label =TeX("$n_B^H$"), size = 7) +annotate("text", x = 0.5, y = 0.37, label =TeX("$n_B^L$"), size = 7)

# plot neql (Nl=150 whereas Nh=100) alternative Figure 4
(Nl = 150)# num low-income
(Nh = 100)# num high-income

# equilibrium tuition: eq (9)
(tb.vec = delta*mu*rhob*(Nl+Nh)*(rhoa-rhob)/ ((4*rhoa-rhob)* (Nl*delta*(1+r.vec)+Nh)))
(ta.vec = 2*delta*mu*rhoa*(Nl+Nh)*(rhoa-rhob)/ ((4*rhoa-rhob)* (Nl*delta*(1+r.vec)+Nh)))

# equilibrium abar from eq (5)
(abarl.vec = tb.vec*(1+r.vec)/(rhob*mu))
(abarh.vec = tb.vec/(rhob*delta*mu))

# equilibrium ahat from eq (6)
(ahatl.vec = (ta.vec-tb.vec)*(1+r.vec)/(mu*(rhoa-rhob)))
(ahath.vec = (ta.vec-tb.vec)/(delta*mu*(rhoa-rhob)))

# equilibrium abar from eq (11)
#(abarl.vec = (Nl+Nh)*delta*(1+r.vec)*(rhoa-rhob)/ ((4*rhoa-rhob)* (Nl*delta*(1+r.vec)+Nh)))
#(abarh.vec = (Nl+Nh)*(rhoa-rhob)/ ((4*rhoa-rhob)* (Nl*delta*(1+r.vec)+Nh)))

# equilibrium ahat from eq (12)
(ahatl.vec = (Nl+Nh)*delta*(1+r.vec)*(2*rhoa-rhob)/ ((4*rhoa-rhob)* (Nl*delta*(1+r.vec)+Nh)))
(ahath.vec = (Nl+Nh)*(2*rhoa-rhob)/ ((4*rhoa-rhob)* (Nl*delta*(1+r.vec)+Nh)))

n0l.vec = Nl*abarl.vec # num low-income nonenrolled
n0h.vec = Nh*abarh.vec # num high-income nonenrolled
nal.vec = Nl*(1-ahatl.vec)# num low-income enrolled in A
nah.vec = Nh*(1-ahath.vec)# num high-income enrolled in A
nbl.vec = Nl*(ahatl.vec-abarl.vec)# num low-income enrolled in B
nbh.vec = Nh*(ahath.vec-abarh.vec)# num high-income enrolled in B

# place them into data frame
(neql.df = data.frame(r.vec, n0l.vec, n0h.vec, nal.vec, nah.vec, nbl.vec, nbh.vec))

ggplot(neql.df, aes(x=r.vec, y=nal.vec)) + geom_line(size=1.2) +geom_line(aes(x=r.vec, y=nah.vec), size=1.2)+geom_line(aes(x=r.vec, y=nbl.vec), linetype="longdash", size=1.2, color="blue") +geom_line(aes(x=r.vec, y=nbh.vec), linetype="longdash", size=1.2, color="blue") +geom_line(aes(x=r.vec, y=n0l.vec), linetype="longdash", size=1.2, color="red") +geom_line(aes(x=r.vec, y=n0h.vec), linetype="longdash", size=1.2, color="red") + scale_x_continuous(breaks = seq(0,1,0.1))  + scale_y_continuous(breaks = seq(0,150,5)) + theme(axis.text.x = element_text(size = 18, color = "black"), axis.text.y = element_text(size = 18, color = "black"), text = element_text(size = 20)) +ylab("Equilibrium enrollment and nonenrollment levels") + xlab("Interest rate (r)") +annotate("text", x = 0.5, y = 15, label =TeX("$n_0^L$"), size = 7, color="red") +annotate("text", x = 0.5, y = 3, label =TeX("$n_0^H$"), size = 7, color="red") +annotate("text", x = 0.5, y = 93, label =TeX("$n_A^L$"), size = 7) +annotate("text", x = 0.5, y = 70, label =TeX("$n_A^H$"), size = 7) +annotate("text", x = 0.5, y = 32, label =TeX("$n_B^H$"), size = 7, color="blue") +annotate("text", x = 0.5, y = 55, label =TeX("$n_B^L$"), size = 7, color="blue") + geom_segment(aes(x=0.3, y=100.0, xend=0.0, yend=100.0), color="magenta", size=1.2, arrow = arrow(length = unit(0.5, "cm"))) +annotate("text", x = 0.55, y = 100.0, label = "Loans become more affordable", size = 7, color="magenta") + geom_segment(aes(x=1, y=100, xend=0.8, yend=100), color="magenta", size=1.2, arrow = arrow(length = unit(0.5, "cm")))

### plotting number of defaulting students and value of defaulted debt) [not used] 
## number of defaulting students d
(nd.vec = nal.vec*(1-rhoa) + nbl.vec*(1-rhob))

## dollar value of defaulted loans
(vd =  nal.vec*(1-rhoa)*ta.vec*(1+r.vec) + nbl.vec*(1-rhob) * tb.vec*(1+r.vec) )

## dollar value of all debt taken
(v =  nal.vec*ta.vec*(1+r.vec) + nbl.vec*tb.vec*(1+r.vec) )


### plotting ATB avg (per-student) tuition burden: Figure 5
# case 1: 
(Nl = 100)# num low-income
(Nh = 100)# num high-income

# equilibrium tuition: eq (9)
(tb.vec = delta*mu*rhob*(Nl+Nh)*(rhoa-rhob)/ ((4*rhoa-rhob)* (Nl*delta*(1+r.vec)+Nh)))
(ta.vec = 2*delta*mu*rhoa*(Nl+Nh)*(rhoa-rhob)/ ((4*rhoa-rhob)* (Nl*delta*(1+r.vec)+Nh)))

# equilibrium abar from eq (5)
(abarl.vec = tb.vec*(1+r.vec)/(rhob*mu))
(abarh.vec = tb.vec/(rhob*delta*mu))

# equilibrium ahat from eq (6)
(ahatl.vec = (ta.vec-tb.vec)*(1+r.vec)/(mu*(rhoa-rhob)))
(ahath.vec = (ta.vec-tb.vec)/(delta*mu*(rhoa-rhob)))

# equilibrium abar from eq (11)
#(abarl.vec = (Nl+Nh)*delta*(1+r.vec)*(rhoa-rhob)/ ((4*rhoa-rhob)* (Nl*delta*(1+r.vec)+Nh)))
#(abarh.vec = (Nl+Nh)*(rhoa-rhob)/ ((4*rhoa-rhob)* (Nl*delta*(1+r.vec)+Nh)))

# equilibrium ahat from eq (12)
(ahatl.vec = (Nl+Nh)*delta*(1+r.vec)*(2*rhoa-rhob)/ ((4*rhoa-rhob)* (Nl*delta*(1+r.vec)+Nh)))
(ahath.vec = (Nl+Nh)*(2*rhoa-rhob)/ ((4*rhoa-rhob)* (Nl*delta*(1+r.vec)+Nh)))

n0l.vec = Nl*abarl.vec # num low-income nonenrolled
n0h.vec = Nh*abarh.vec # num high-income nonenrolled
nal.vec = Nl*(1-ahatl.vec)# num low-income enrolled in A
nah.vec = Nh*(1-ahath.vec)# num high-income enrolled in A
nbl.vec = Nl*(ahatl.vec-abarl.vec)# num low-income enrolled in B
nbh.vec = Nh*(ahath.vec-abarh.vec)# num high-income enrolled in B

#storing the ATB for Nh=Nl = 100
(burden1.vec = delta*(nal.vec*ta.vec*(1+r.vec) + nbl.vec*tb.vec*(1+r.vec)) + nah.vec*ta.vec + nbh.vec*tb.vec)

(burden_per_student1.vec = burden1.vec/(nal.vec + nbl.vec + nah.vec + nbh.vec))
r.vec[which.max(burden_per_student1.vec)] #r where avg burden is maximized
delta
(1-delta)/delta

# case 2: 
(Nl = 150)# num low-income
(Nh = 50)# num high-income

# equilibrium tuition: eq (9)
(tb.vec = delta*mu*rhob*(Nl+Nh)*(rhoa-rhob)/ ((4*rhoa-rhob)* (Nl*delta*(1+r.vec)+Nh)))
(ta.vec = 2*delta*mu*rhoa*(Nl+Nh)*(rhoa-rhob)/ ((4*rhoa-rhob)* (Nl*delta*(1+r.vec)+Nh)))

# equilibrium abar from eq (5)
(abarl.vec = tb.vec*(1+r.vec)/(rhob*mu))
(abarh.vec = tb.vec/(rhob*delta*mu))

# equilibrium ahat from eq (6)
(ahatl.vec = (ta.vec-tb.vec)*(1+r.vec)/(mu*(rhoa-rhob)))
(ahath.vec = (ta.vec-tb.vec)/(delta*mu*(rhoa-rhob)))

# equilibrium abar from eq (11)
#(abarl.vec = (Nl+Nh)*delta*(1+r.vec)*(rhoa-rhob)/ ((4*rhoa-rhob)* (Nl*delta*(1+r.vec)+Nh)))
#(abarh.vec = (Nl+Nh)*(rhoa-rhob)/ ((4*rhoa-rhob)* (Nl*delta*(1+r.vec)+Nh)))

# equilibrium ahat from eq (12)
(ahatl.vec = (Nl+Nh)*delta*(1+r.vec)*(2*rhoa-rhob)/ ((4*rhoa-rhob)* (Nl*delta*(1+r.vec)+Nh)))
(ahath.vec = (Nl+Nh)*(2*rhoa-rhob)/ ((4*rhoa-rhob)* (Nl*delta*(1+r.vec)+Nh)))

n0l.vec = Nl*abarl.vec # num low-income nonenrolled
n0h.vec = Nh*abarh.vec # num high-income nonenrolled
nal.vec = Nl*(1-ahatl.vec)# num low-income enrolled in A
nah.vec = Nh*(1-ahath.vec)# num high-income enrolled in A
nbl.vec = Nl*(ahatl.vec-abarl.vec)# num low-income enrolled in B
nbh.vec = Nh*(ahath.vec-abarh.vec)# num high-income enrolled in B

#storing the ATB for Nh 100, =Nl = 150 (case 2)
(burden2.vec = delta*(nal.vec*ta.vec*(1+r.vec) + nbl.vec*tb.vec*(1+r.vec)) + nah.vec*ta.vec + nbh.vec*tb.vec)

(burden_per_student2.vec = burden2.vec/(nal.vec + nbl.vec + nah.vec + nbh.vec))
r.vec[which.max(burden_per_student2.vec)] #r where avg burden is maximized
delta
r_thresh = (1-delta)/delta

# putting case 1 and case 2 burden into a data frame
(burden.df = data.frame(r.vec, burden_per_student1.vec, burden_per_student2.vec))

ggplot(burden.df, aes(x=r.vec, y=burden_per_student1.vec)) + geom_line(size=1.2) +geom_line(aes(x=r.vec, y=burden_per_student2.vec), size=1.2, linetype="longdash", color="red")+geom_vline(xintercept =r_thresh, size = 1.2, linetype = "dotted")+annotate("text", x = 0.22, y = 0.234, label =TeX("$\\hat{r} =$"), size = 7) +annotate("text", x = 0.29, y = 0.234, label =TeX("$\\frac{1}{\\delta}\\, - 1$"), size = 7) + theme(axis.text.x = element_text(size = 18, color = "black"), axis.text.y = element_text(size = 18, color = "black"), text = element_text(size = 20)) +ylab("Average (per-student) tuition burden (ATB)") + xlab("Interest rate (r)") +annotate("text", x = 0.55, y = 0.2325, label = "Loans become more affordable", size = 7, color="magenta")+ geom_segment(aes(x=1, y=0.2325, xend=0.8, yend=0.2325), color="magenta", size=1.2, arrow = arrow(length = unit(0.5, "cm")))+ geom_segment(aes(x=0.3, y=0.2325, xend=0.0, yend=0.2325), color="magenta", size=1.2, arrow = arrow(length = unit(0.5, "cm")))

### Simulating section 7 (capacity constraints) in order to verify ta, tb and the statements about ATB
(Nl = 100)# num low-income
(Nh = 100)# num high-income
(ka = 60)# college A enrollment capacity
(kb = 80)# college B enrollment capacity

# eq (22): eql tuition under ka kb
(ta_k.vec = (delta*mu*(rhoa*(Nl+Nh) - ka*rhoa - kb*rhob))/ (Nh + Nl*delta*(1+r.vec)))
#
(tb_k.vec = (delta*mu*rhob*(Nl+Nh - ka - kb))/ (Nh + Nl*delta*(1+r.vec)))
#
(ta_k.vec - tb_k.vec)

# equilibrium abar from eq (5)
(abarl_k.vec = tb_k.vec*(1+r.vec)/(rhob*mu))
(abarh_k.vec = tb_k.vec/(rhob*delta*mu))

# equilibrium ahat from eq (6)
(ahatl_k.vec = (ta_k.vec-tb_k.vec)*(1+r.vec)/(mu*(rhoa-rhob)))
(ahath_k.vec = (ta_k.vec-tb_k.vec)/(delta*mu*(rhoa-rhob)))

n0l_k.vec = Nl*abarl_k.vec # num low-income nonenrolled
n0h_k.vec = Nh*abarh_k.vec # num high-income nonenrolled
nal_k.vec = Nl*(1-ahatl_k.vec)# num low-income enrolled in A
nah_k.vec = Nh*(1-ahath_k.vec)# num high-income enrolled in A
nbl_k.vec = Nl*(ahatl_k.vec-abarl_k.vec)# num low-income enrolled in B
nbh_k.vec = Nh*(ahath_k.vec-abarh_k.vec)# num high-income enrolled in B


# ATB from eq (19)
(burden_k.vec = delta*(nal_k.vec*ta_k.vec*(1+r.vec) + nbl_k.vec*tb_k.vec*(1+r.vec)) + nah_k.vec*ta_k.vec + nbh_k.vec*tb_k.vec)
max(burden_k.vec)# => verify interior max of ATB under capacity constraints. 
r.vec[which.max(burden_k.vec)]# which r max burden
1/delta -1 # verifying Result 4 and Figure 5 under capacity constraints. 

 
# ### Plotting welfare of low- and high-income [Not used, I provide a logical verbal proof in the paper]
# (wl.vec = -0.5*Nl*delta*(abarl.vec^2*mu*rhob + ahatl.vec^2* mu* (rhoa-rhob) +2*r.vec*(ta.vec+tb.vec) + 2*ta.vec +2*tb.vec - mu*rhoa))
# 
# (wh.vec = -0.5*Nh*(abarh.vec^2*delta*mu*rhob + ahath.vec^2*delta*mu*(rhoa-rhob) + 2*ta.vec + 2*tb.vec -delta*mu*rhoa))
# 
# # place them into data frame
# (welfare.df = data.frame(r.vec, wl.vec, wh.vec))
# 
# # plot welfare 
# ggplot(welfare.df, aes(x=r.vec, y=wl.vec)) + geom_line(size=1.2) +geom_line(aes(x=r.vec, y=wh.vec), size=1.2) + scale_x_continuous(breaks = seq(0,1,0.1))  + scale_y_continuous(breaks = seq(0,45,0.05)) + theme(axis.text.x = element_text(size = 18, color = "black"), axis.text.y = element_text(size = 18, color = "black"), text = element_text(size = 20)) +ylab("Equilibrium enrollment and nonenrollment levels") + xlab("Interest rate (r)") +annotate("text", x = 0.5, y = 0.11, label =TeX("$n_0^L$"), size = 7) +annotate("text", x = 0.5, y = 0.03, label =TeX("$n_0^H$"), size = 7) +annotate("text", x = 0.22, y = 0.45, label =TeX("$r =$"), size = 7) +annotate("text", x = 0.29, y = 0.45, label =TeX("$\\frac{1}{\\delta}\\, - 1$"), size = 7) +annotate("text", x = 0.5, y = 0.55, label =TeX("$n_A^L$"), size = 7) +annotate("text", x = 0.5, y = 0.69, label =TeX("$n_A^H$"), size = 7) +annotate("text", x = 0.5, y = 0.25, label =TeX("$n_B^H$"), size = 7) +annotate("text", x = 0.5, y = 0.37, label =TeX("$n_B^L$"), size = 7)
