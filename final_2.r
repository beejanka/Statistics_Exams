
library(tidyverse)

dat = read.table("exam2022_part2.txt", header=T)
dat = na.omit(dat)


# show that left and right horn are strongly linearly correlated
m_horn = lm(dat$hornL ~ dat$hornR)
print(summary(m_horn))


# calculate the mean horn length from left and right horn
dat$horn = (dat$hornL+ dat$hornR)/2


#only include young goats
dat_young = dat[dat$age < 4,]


#plot mass over age
ggplot(data = dat, aes(x = age, y = mass, color = density))+
geom_point()+
geom_smooth(data=dat[dat$age <4,], method='lm')+
geom_smooth(data=dat[dat$age >=3,], method='lm')+
labs(x = "Age(years)", y = "Mass (kg)", color = "Population density") +
theme_classic(base_size = 14)
ggsave("mass_age.png")


#average mass and growth rate in first years
print(mean(subset(dat, density == "high")$mass))
print(mean(subset(dat, density == "low")$mass))

m_am = lm(mass ~ age * density, data = dat_young)
summary(m_am)


# plot the horn length over the mass for different densities
ggplot(data = dat, aes(x = mass, y = horn, color = density))+
geom_point()+
geom_smooth(method = "lm")+
labs(x = "Mass (kg)", y = "Horn length (mm)", color = "Population density") +
theme_classic(base_size = 14)
ggsave("mass_horn.png")


#compare the models
m1 = lm(horn ~ mass, data = dat)
m2 = lm(horn ~ density, data = dat)
m3 = lm(horn ~ mass + density, data = dat)

mlist = list(m1, m2, m3)
AICTab = AIC(m1, m2, m3)
AICTab$logLik = unlist(lapply(mlist, logLik))
AICTab = AICTab[order(AICTab$AIC, decreasing=F),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
print(AICTab)

# see results of best model
print(summary(m3))
print(anova(m3))
