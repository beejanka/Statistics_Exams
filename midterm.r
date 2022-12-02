library(glmmTMB)
library(tidyverse)

dat = read.csv("dormancy.csv")
dat = na.omit(dat)

invlogit = function(x) 1/(1+exp(-x))

# plot all populations together

allpop_plot <- ggplot(data = dat,aes(x=timetosowing, y=germ2, color = pop)) +
geom_jitter( width = 0, height = .025) +
geom_smooth(aes(timetosowing, germ2), method = "glm", method.args = list(family = "quasibinomial")) +
labs(x = "Time to sowing (days)", y = "Germination sucess", color = "Population") +
theme_classic(base_size = 14) 
ggsave("allpop.png")


# model with 1 fixed effect 
m1 = glmmTMB(germ2 ~ pop + (1|blocktray), weights = nseed, data=dat, family ="binomial"(link="logit"))

# model with 2 fixed effects
m2 = glmmTMB(germ2 ~ timetosowing + pop + (1|blocktray), weights = nseed, data = dat, family ="binomial"(link="logit"))

# model with 3 fixed effects
m3 = glmmTMB(germ2 ~ timetosowing + MCseed + pop + (1|blocktray), weights = nseed, data = dat, family ="binomial"(link="logit"))
print(summary(m3))


# compare the models with AIC

mlist = list(m1, m2, m3)
AICTab = AIC(m1, m2, m3)
AICTab$logLik = unlist(lapply(mlist, logLik))
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
print(AICTab)


# how much variance is explained by fixed vs random effects?

VarAmongGroups = attr(VarCorr(m3)$cond$blocktray, "stddev")^2
VarWithinGroups = attr(VarCorr(m3)$cond, "sc")^2
print(VarAmongGroups/(VarAmongGroups+VarWithinGroups)*100)
print(VarWithinGroups/(VarAmongGroups+VarWithinGroups)*100)


# calculate 50 % germination time for each model

m3_co = summary(m3)$coefficients$cond
days_CC = -m3_co[1]/m3_co[2]
days_LM = -(m3_co[1]+m3_co[4])/m3_co[2]
days_PM = -(m3_co[1]+m3_co[5])/m3_co[2]
days_T = -(m3_co[1]+m3_co[6])/m3_co[2]


# plot the seed size effect for each population

all_pop = c("CC", "LM", "PM", "T")
all_colors = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")


for (i in 1:length(all_pop)){
    subdat = dat[dat$pop==all_pop[i],]
    m = glmmTMB(germ2 ~ timetosowing + MCseed  + (1|blocktray), 
        weights = nseed, data = subdat, family ="binomial"(link="logit"))
    xvals = seq(min(subdat$timetosowing, na.rm = T), max(subdat$timetosowing, na.rm = T), length.out = length(subdat$timetosowing))
    coefs = summary(m)$coefficients
    y_hat = coefs$cond[1] + coefs$cond[2] * xvals 
    y_hat_plus = coefs$cond[1] + coefs$cond[2] * xvals + coefs$cond[3]*sd(subdat$MCseed)
    y_hat_minus = coefs$cond[1] + coefs$cond[2] * xvals - coefs$cond[3]*sd(subdat$MCseed)
    
    subdat$yhat = invlogit(y_hat)
    subdat$yhatp = invlogit(y_hat_plus)
    subdat$yhatm = invlogit(y_hat_minus)
    subdat$xvals = xvals

    temp_plot <- ggplot(data = subdat, aes(x = timetosowing, y = germ2)) +
    geom_jitter( width = 0, height = .025,col = all_colors[i]) +
    geom_line(aes(x= subdat$xvals,y = subdat$yhat),col = all_colors[i], size = 1.5)+
    geom_line(aes(x= subdat$xvals,y = subdat$yhatp),col = all_colors[i])+
    geom_line(aes(x= subdat$xvals,y = subdat$yhatm),col = all_colors[i])+
    labs(x = "Time to sowing (days)", y = "Germination sucess") +
    ggtitle(all_pop[i]) +
    theme_classic(base_size = 14)
    
    ggsave(temp_plot, file=paste0("plot_", i,".png"))

    # calculate how much longer it takes for a bigger seed
    
    plus1sddays = (-(coefs$cond[1] + coefs$cond[3]*sd(subdat$MCseed))/coefs$cond[2])
    mean_days = (-(coefs$cond[1])/coefs$cond[2])
    print(plus1sddays - mean_days)

}