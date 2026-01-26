# cc_compete_2026_mm_dd.R 
# Simulations (verification) of computations in a paper titled: "A Reformulation of the Credit Card Interchange Fee Problem."

# packages used
library(ggplot2); theme_set(theme_bw())
library(latex2exp)# LaTeX in ggplot
#library(xtable)# export data frames to LaTeX tables

# Model parameters ####

# Model parameters
(K.vec = seq(1,10,0.1))# number of card-issuing banks

(alpha = 0.25)# inverse demand sensitivity to number of merchants
(beta = 0.2)# inverse demand sensitivity to own number of cards
(gamma =0.1)# inverse demand sensitivity to competing number of cards
(mu = 2)# slope of merchant acceptance function
(M = 6)# potential number of merchants

# Old Assumption 1 (works throughout, but may not be strong enough to endure ra, rb > 0)
#(3*beta-gamma)/(4*mu*(beta^2 - gamma^2))
#alpha < (3*beta-gamma)/(4*mu*(beta^2 - gamma^2))

# New Assumption 1 to have ra >0 & rb >0
#(beta)/(3*mu*(beta^2 - gamma^2))
#alpha < (beta)/(3*mu*(beta^2 - gamma^2))

# New-New Assumption 1 to have ra_opt >0 & rb_opt >0
1/(6*mu*(beta+gamma))
alpha < 1/(6*mu*(beta + gamma))

# Equilibrium interchange fees ia=ib=i
(i = (M/mu)*((beta-2*alpha*mu*(beta^2-gamma^2)))/(3*beta -4*mu*(beta^2-gamma^2) -gamma))

# equilibrium number of merchants accepting cards
(m = M*(beta-gamma)/(3*beta -4*alpha*mu*(beta^2-gamma^2)-gamma))

# equilibrium number of A and B card users na = nb = n
(n.vec = M*K.vec*(beta-alpha*mu*(beta^2-gamma^2))/(mu*(K.vec+1)*(beta+gamma)*(3*beta -4*alpha*mu*(beta^2-gamma^2) -gamma)))
# total cards (A and B combined
(n_total.vec = 2*n.vec)

#equilibrium rewards ra = rb = r
(r.vec = M*((K.vec*(2*alpha*mu*(beta^2-gamma^2)-beta) +alpha*mu*(beta^2-gamma^2)))/(mu*(K.vec+1)*(4*alpha*mu*(beta^2-gamma^2) -3*beta +gamma)))

# verify the above from the inverse demand for cards
#(r.vec = beta*n.vec +gamma*n.vec -alpha*(beta+gamma)*m)

# number of transactions per card
(t_per_card.vec = m * n.vec)

# Total equilibrium number of transactions (A & B combined)
(t_total.vec = m * n_total.vec)

# banks profit margin
(margin.vec = i - r.vec)
# Verify directly from paper ga and gb
#(margin.vec = (M/(mu*(K.vec+1)))*(beta -alpha*mu*(beta^2-gamma^2))/(3*beta -4*alpha*mu*(beta^2-gamma^2) -gamma))

#Start Figure 1 in paper: Profit margin w.r.t number of banks####
(margin.df = data.frame(K.vec, i, r.vec, margin.vec))

ggplot(margin.df, aes(x=K.vec)) +geom_line(aes(y=i), linetype="dotdash", linewidth=1.2, color="blue") +geom_line(aes(y=r.vec), linetype="longdash", linewidth=1.2, color="red") +geom_line(aes(y=margin.vec), linetype="solid", linewidth=1.2, color="black") + scale_x_continuous(breaks = seq(1,10,1)) + scale_y_continuous(breaks = seq(0,2,0.1)) +labs(x=TeX("Number of card-issuing banks: $K$"), y=TeX("Interchange fee ($i_A=i_B$); reward ($r_A=r_B$); profit margin ($g_A=g_B$)")) +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +annotate("text", x = 2, y = 1.9, label = TeX("$i_A=i_B$"), size = 8, color="blue") +annotate("text", x = 2, y = 0.85, label = TeX("$r_A=r_B$"), size = 8, color="red") +annotate("text", x = 2, y = 1.35, label = TeX("$g_A=g_B$"), size = 8, color="black")
#End Figure 1 in paper: Profit margin w.r.t number of banks####

#Start Figure 2 in paper: Comparing equilibrium vs optimal ia, ib, ra, rb####
(i_opt = (M/(4*mu)) *((1 -4*alpha*mu*(beta+gamma))/(1 -2*alpha*mu*(beta+gamma))))

(m_opt = M/(2*(1 -2*alpha*mu*(beta+gamma))))

(n_opt.vec = M*K.vec / (4*mu*(K.vec+1)*(beta+gamma)))
# combined A & B cards
(n_total_opt.vec = 2*n_opt.vec)

(r_opt.vec = M*((K.vec*(1-4*alpha*mu*(beta+gamma)) -2*alpha*mu*(beta+gamma))/(4*mu*(K.vec+1) *(1-2*alpha*mu*(beta+gamma)))))
#Verify r_opt from demand
#(r.opt.vec = beta*n_opt.vec + gamma*n_opt.vec -alpha*(beta+gamma)*m_opt)

# number of transaction on each card separately
(t_per_card_opt.vec = m_opt * n_opt.vec)

(t_total_opt.vec = m_opt*n_total_opt.vec)# Optimal total number of payments (both cards combined)

(opt.df = data.frame(K.vec, i, r.vec, i_opt, r_opt.vec))

ggplot(opt.df, aes(x=K.vec)) +geom_line(aes(y=i), linetype="solid", linewidth=1.2, color="black") +geom_line(aes(y=r.vec), linetype="solid", linewidth=1.2, color="black") +geom_line(aes(y=i_opt), linetype="longdash", linewidth=1.2, color="red") +geom_line(aes(y=r_opt.vec), linetype="longdash", linewidth=1.2, color="red") + scale_x_continuous(breaks = seq(1,10,1)) + scale_y_continuous(breaks = seq(0,2,0.1)) +labs(x=TeX("Number of card-issuing banks: $K$"), y=TeX("Equilibrium interchange fees and rewards versus optimum")) +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +annotate("text", x = 6, y = 1.87, label = TeX("$i_A=i_B$"), size = 8, color="black") +annotate("text", x = 6, y = 0.52, label = TeX("$i_A^*=i_B^*$"), size = 8, color="red") +annotate("text", x = 6, y = 1.1, label = TeX("$r_A=r_B$"), size = 8, color="black") +annotate("text", x = 6, y = 0.22, label = TeX("$r_A^*=r_B^*$"), size = 8, color="red") 

#End Figure 2 in paper: Comparing equilibrium vs optimal ia, ib, ra, rb####

#Start Figure 3 in paper: Comparing equilibrium versus optimal#### 
#for the chart: I use n per-card (not n_total) and also t per-card (not t_total)
(cards.df = data.frame(K.vec, m, m_opt, n.vec, n_opt.vec, t_per_card.vec, t_per_card_opt.vec))

# added to Figure: Consider no interchange fees and no rewards (none)
(m_none = M)# number of merchants (= all merchants)
#
(na_none = alpha*M)# number of A cards
(nb_none = alpha*M)# number of B cards
#
(ta_none = m_none * na_none)# number transactions with card A
(tb_none = m_none * nb_none)# number transactions with card B

#for the chart: I use n per-card (not n_total) and also t per-card (not t_total)
ggplot(cards.df, aes(x=K.vec)) +geom_line(aes(y=m), linetype="solid", linewidth=1.2, color="black") +geom_line(aes(y=n.vec), linetype="solid", linewidth=1.2, color="black") +geom_line(aes(y=m_opt), linetype="longdash", linewidth=1.2, color="red") +geom_line(aes(y=n_opt.vec), linetype="longdash", linewidth=1.2, color="red") +geom_line(aes(y=t_per_card.vec), linetype="solid", linewidth=1.2, color="black") +geom_line(aes(y=t_per_card_opt.vec), linetype="solid", linewidth=1.2, color="red") + scale_x_continuous(breaks = seq(0,10,1)) + scale_y_continuous(breaks = seq(0,10,0.5)) +labs(x=TeX("Number of card-issuing banks: $K$"), y=TeX("Equilibrium versus optimum")) +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20))  +annotate("text", x = 7, y = 5.4, label = TeX("$t_A=t_B$"), size = 8, color="black")  +annotate("text", x = 7, y = 9.8, label = TeX("$t_A^*=t_B^*$"), size = 8, color="red") +annotate("text", x = 9, y = 1.6, label = TeX("$m$"), size = 8, color="black") +annotate("text", x = 9.1, y = 4.6, label = TeX("$m^*$"), size = 8, color="red") +annotate("text", x = 7, y = 3.35, label = TeX("$N_A=N_B$"), size = 8, color="black")  +annotate("text", x = 7.1, y = 2.55, label = TeX("$N_A^*=N_B^*$"), size = 8, color="red") + geom_segment(aes(x = 8, y = 3.6, xend = 8, yend = 2.3), arrow = arrow(ends = "last", type = "open"), linewidth = 1.2, color="blue") + geom_segment(aes(x = 9.75, y = 1.4, xend = 9.75, yend = 4.2), arrow = arrow(ends = "last", type = "open"), linewidth = 1.2, color="blue") + geom_segment(aes(x = 9.75, y = 5.3, xend = 9.75, yend = 9.6), arrow = arrow(ends = "last", type = "open"), linewidth = 1.2, color="blue") +geom_hline(yintercept = ta_none, color = "darkgreen", linetype = "twodash", linewidth = 1.2) +annotate("text", x = 2, y = 9.4, label = TeX("$t_A^{no} = t_B^{no}$"), size = 8, color="darkgreen")

#End Figure 3 in paper: Comparing equilibrium versus optimal#### 

#Start Figure 4: Extension: comparing equilibrium i and r with optimum####

# Equilibrium i
(split_i.vec = (M/mu) *((K.vec*(2*alpha*mu*(beta^2-gamma^2) -beta) +alpha*mu*(2*beta^2 +beta*gamma -gamma^2) -beta) / (K.vec*(2*alpha*mu*(beta^2-gamma^2) -2*beta +gamma) +alpha*mu*(2*beta^2 +beta*gamma -gamma^2) -2*beta)))

# Optimal i*
(split_i_opt.vec = (M/(2*mu)) *(1-2*alpha*mu*(beta+gamma))/(1-alpha*mu*(beta+gamma)))

# Equilibrium m
(split_m.vec = M -mu*split_i.vec)

# Optimal m
(split_m_opt.vec = M-mu**split_i_opt.vec )

# Equilibrium N_A and N_B
(split_nk_numerator1.vec = split_i.vec*split_m.vec*beta*(K.vec+1))
#
(split_nk_numerator2.vec = -split_i.vec*gamma *(K.vec*split_m.vec +split_m.vec))
#
(split_nk_numerator3.vec =  alpha*(K.vec*split_m.vec^2*beta^2 *(beta^2-gamma^2) +split_m.vec^2*beta^2 +split_m.vec*split_m.vec*gamma *(beta-gamma) -split_m.vec^2*beta*gamma))
#
(split_nk_numerator.vec = split_nk_numerator1.vec+split_nk_numerator2.vec+split_nk_numerator3.vec )

(split_nk_den1.vec = K.vec^2*split_m.vec*split_m.vec*(beta^2-gamma^2))
#
(split_nk_den2.vec = -K.vec*(split_m.vec^2*gamma^2 -2*split_m.vec*split_m.vec*beta^2 +split_m.vec^2*gamma^2))
#
(split_nk_den3.vec = split_m.vec*split_m.vec*(beta^2-gamma^2))
#
(split_nk_den.vec = split_nk_den1.vec +split_nk_den2.vec +split_nk_den3.vec)

(split_n.vec = K.vec*split_m.vec *split_nk_numerator.vec / split_nk_den.vec)

# Optimal N_A and N_B
(split_nk_opt_numerator1.vec = split_i_opt.vec*split_m_opt.vec*beta*(K.vec+1))
#
(split_nk_opt_numerator2.vec = -split_i_opt.vec*gamma *(K.vec*split_m_opt.vec +split_m_opt.vec))
#
(split_nk_opt_numerator3.vec =  alpha*(K.vec*split_m_opt.vec^2*beta^2 *(beta^2-gamma^2) +split_m_opt.vec^2*beta^2 +split_m_opt.vec*split_m_opt.vec*gamma *(beta-gamma) -split_m_opt.vec^2*beta*gamma))
#
(split_nk_opt_numerator.vec = split_nk_opt_numerator1.vec+split_nk_opt_numerator2.vec+split_nk_opt_numerator3.vec )

(split_nk_opt_den1.vec = K.vec^2*split_m_opt.vec*split_m_opt.vec*(beta^2-gamma^2))
#
(split_nk_opt_den2.vec = -K.vec*(split_m_opt.vec^2*gamma^2 -2*split_m_opt.vec*split_m_opt.vec*beta^2 +split_m_opt.vec^2*gamma^2))
#
(split_nk_opt_den3.vec = split_m_opt.vec*split_m_opt.vec*(beta^2-gamma^2))
#
(split_nk_opt_den.vec = split_nk_opt_den1.vec +split_nk_opt_den2.vec +split_nk_opt_den3.vec)

(split_n_opt.vec = K.vec*split_m_opt.vec *split_nk_opt_numerator.vec / split_nk_opt_den.vec)

# Equilibrium ra rb
(split_r.vec = beta*split_n.vec +gamma*split_n.vec -alpha*(beta*split_m.vec +gamma*split_m.vec))
# Optimal ra rb
(split_r_opt.vec = beta*split_n_opt.vec +gamma*split_n_opt.vec -alpha*(beta*split_m_opt.vec +gamma*split_m_opt.vec))

# Equilibrium profit margin
split_i.vec - split_r.vec
# Optimal profit margin
split_i_opt.vec - split_r_opt.vec

# Equilibrium number of transactions by card brand: t_A=t_B
(split_t_per_card.vec = split_m.vec*split_n.vec )
# Optimal number of transactions by card brand: t_A=t_B
(split_t_per_card_opt.vec = split_m_opt.vec*split_n_opt.vec )

(split.df = data.frame(K.vec, split_i.vec, split_r.vec, split_i_opt.vec, split_r_opt.vec))

ggplot(split.df, aes(x=K.vec)) +geom_line(aes(y=split_i.vec), linetype="solid", linewidth=1.2, color="black") +geom_line(aes(y=split_r.vec), linetype="solid", linewidth=1.2, color="black") +geom_line(aes(y=split_i_opt.vec), linetype="longdash", linewidth=1.2, color="red") +geom_line(aes(y=split_r_opt.vec), linetype="longdash", linewidth=1.2, color="red") + scale_x_continuous(breaks = seq(1,10,1)) + scale_y_continuous(breaks = seq(0,2,0.1)) +labs(x=TeX("Number of card-issuing banks: $K$"), y=TeX("Equilibrium interchange fees and rewards versus optimum")) +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +annotate("text", x = 3, y = 1.78, label = TeX("$i_A=i_B$"), size = 8, color="black") +annotate("text", x = 3, y = 1.30, label = TeX("$i_A^*=i_B^*$"), size = 8, color="red") +annotate("text", x = 8, y = 1.5, label = TeX("$r_A=r_B$"), size = 8, color="black") +annotate("text", x = 8, y = 0.94, label = TeX("$r_A^*=r_B^*$"), size = 8, color="red") 



#End Figure 4: Extension: comparing equilibrium i and r with optimum####

#Unused code ####
#
#ggplot(pxpy.df, aes(x=lambda.vec)) +geom_line(aes(y=pxh.vec), linetype="dotdash", linewidth=1.2, color="blue") +geom_line(aes(y=pxf.vec), linetype="longdash", linewidth=1.2, color="red") +geom_line(aes(y=sxf.vec), linetype="solid", linewidth=1.2, color="black") + scale_x_continuous(breaks = seq(0,1,0.1)) + scale_y_continuous(breaks = seq(60,110,5)) +labs(x=TeX("Probability of tariff war: $\\lambda$"), y=TeX("Domestic and import consumer price and expected seller price")) +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +geom_vline(xintercept = lambda_hat, color = "black") +annotate("text", x = lambda_hat+0.013, y = 70, label = TeX("$\\hat{\\lambda}$"), size = 8, color="black") +annotate("text", x = 0.25, y = 96, label = TeX("$p^X_F=p^Y_H$ (consumer import price)"), size = 8, color="red") +annotate("text", x = 0.3, y = 68, label = TeX("$Es^X_F=Es^Y_H$ (expected seller export price)"), size = 8, color="black") +annotate("text", x = 0.35, y = 83, label = TeX("$p^X_H=p^Y_F$ (domestic price)"), size = 8, color="blue") +geom_hline(yintercept =c, linetype = "F1", size=1.2, color="darkgreen") +geom_hline(yintercept =c+T, linetype = "F1", size=1.2, color="darkgreen") +annotate("text", x = 0.3, y = c+1.8, label = TeX("$c$ (unit cost)"), size = 8, color="darkgreen") +annotate("text", x = 0.3, y = c+T-1.8, label = TeX("$c + T$ (unit cost + specific tariff)"), size = 8, color="darkgreen") +geom_segment(mapping = aes(x=lambda_hat-0.01, y=pxf.vec[which.min(profitx.vec)], xend = lambda_hat-0.01, yend = c+T-0.5), arrow = arrow(ends = "both"), size=1.0, color="darkgreen") +geom_segment(mapping = aes(x=lambda_hat-0.01, y=c+0.5, xend = lambda_hat-0.01, yend = pxf.vec[which.min(profitx.vec)]-0.5), arrow = arrow(ends = "both"), size=1.0, color="darkgreen") +annotate("text", x = lambda_hat-0.04, y = 101, label = TeX("(loss)"), size = 7, color="darkgreen", angle=90) +annotate("text", x = lambda_hat-0.04, y = 78, label = TeX("markup: (gain)"), size = 8, color="darkgreen", angle=90)

####################

# #Start Figure 1 in paper: card payments w.r.t number of banks
# (n.df = data.frame(K.vec, n.vec, M*alpha))
# 
# ggplot(n.df, aes(x=K.vec)) +geom_line(aes(y=n.vec), linetype="solid", linewidth=1.2, color="black") +geom_line(aes(y=M*alpha), linetype="longdash", linewidth=1.2, color="blue") + scale_x_continuous(breaks = seq(1,10,1)) + scale_y_continuous(breaks = seq(0,12,1)) +labs(x=TeX("Number of card-issuing banks: $K$"), y=TeX("Number of card users ($N_A=N_B$)")) +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) 
######################

#Start Figure 2 in paper: number of transactions w.r.t number of banks
# (t.df = data.frame(K.vec, t.vec, M*alpha))
# 
# ggplot(t.df, aes(x=K.vec)) +geom_line(aes(y=t.vec), linetype="solid", linewidth=1.2, color="black") +geom_line(aes(y=M*M*alpha), linetype="longdash", linewidth=1.2, color="blue") +geom_line(aes(y=n.vec), linetype="solid", linewidth=1.2, color="black")
# 
# + scale_x_continuous(breaks = seq(1,10,1)) + scale_y_continuous(breaks = seq(0,12,1)) +labs(x=TeX("Number of card-issuing banks: $K$"), y=TeX("Number of card users ($N_A=N_B$)")) +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) 
# 
# 
# 
# +annotate("text", x = 6, y = 1.8, label = TeX("$i_A=i_B$"), size = 8, color="blue") +annotate("text", x = 6, y = 1.3, label = TeX("$r_A=r_B$"), size = 8, color="red") +annotate("text", x = 6, y = 0.55, label = TeX("$g_A=g_B$"), size = 8, color="black")

#######################

# #Start Figure ?? in paper: Best response functions
# (ia_br.vec = seq(0,3,0.1))
# (ib_br.vec = seq(0,3,0.1))
# 
# # A's best response
# (bra.vec = (M*(2*alpha*mu*(beta^2-gamma^2)-beta) +ib_br.vec*mu*(gamma-beta) *(2*alpha*mu*(beta+gamma)-1))/(2*mu*(alpha*mu*(beta^2-gamma^2)-beta)))
# #
# (brb.vec = (M*(2*alpha*mu*(beta^2-gamma^2)-beta) +ia_br.vec*mu*(gamma-beta)*(2*alpha*mu*(beta+gamma)-1))/ (2*mu*(alpha*mu*(beta^2-gamma^2) -beta)))

#######################

#Start Figure ?? in paper: number of transactions w.r.t ia & ib
# (K_if = 5)# holding K (number of banks) constant
# #
# (i_if.vec = seq(0,2,0.1))# vector of IF to identify max t
# #
# (m_if.vec = M - mu*(i_if.vec+i_if.vec))# eq (1) in paper
# #
# (n_if.vec = K_if*(beta*i_if.vec -gamma*i_if.vec + m_if.vec*alpha*(beta^2-gamma^2))/((K_if +1) *(beta^2-gamma^2)))# number of card users: K times eq (6) in paper (also eq (7))
# #
# (r_if.vec = beta*n_if.vec +gamma*n_if.vec -alpha*(beta+gamma)*m_if.vec)
# #
# (t_if.vec = m_if.vec*n_if.vec)
# 
# (t.df = data.frame(K.vec, t.vec, M*alpha))
# 
# ggplot(t.df, aes(x=K.vec)) +geom_line(aes(y=t.vec), linetype="solid", linewidth=1.2, color="black") +geom_line(aes(y=M*M*alpha), linetype="longdash", linewidth=1.2, color="blue") +geom_line(aes(y=n.vec), linetype="solid", linewidth=1.2, color="black")

#+ scale_x_continuous(breaks = seq(1,10,1)) + scale_y_continuous(breaks = seq(0,12,1)) +labs(x=TeX("Number of card-issuing banks: $K$"), y=TeX("Number of card users ($N_A=N_B$)")) +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) 

#+annotate("text", x = 6, y = 1.8, label = TeX("$i_A=i_B$"), size = 8, color="blue") +annotate("text", x = 6, y = 1.3, label = TeX("$r_A=r_B$"), size = 8, color="red") +annotate("text", x = 6, y = 0.55, label = TeX("$g_A=g_B$"), size = 8, color="black")
#+
#End Figure ?? in paper: number of transactions w.r.t ia & ib

#######################
  