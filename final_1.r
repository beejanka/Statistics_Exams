library(ggbiplot)
library(tidyverse)

dat = read.csv("exam2022_part1.csv")
dat = na.omit(dat)
dat[dat == "D"] <- "Dry"
dat[dat == "W"] <- "Wet"


# calculate the variance inflation factor for all traits
for (i in 6:13){
    for (j in 6:13){
        m1 = lm(unlist(dat[i])~unlist(dat[j]))
        r2 = summary(m1)$r.squared
        print(c(colnames(dat[i]), colnames(dat[j]), "vif:", 1/(1-r2)))
}
}

# PCA on response variables
dat1 <- dat[ , unlist(lapply(dat,is.numeric))]  #only use continuous variables
dat_pca = prcomp(dat1[,c(2:6)], center = TRUE, scale. = TRUE)
print(summary(dat_pca))
dat$PC = dat_pca$x[,1]


# plot PCA result
ggbiplot(dat_pca, groups = dat$treat, varname.size = 7, labels.size = 30) +
labs( colour = "Treatment/Species")+
stat_ellipse(aes( group=dat$sp, color = dat$sp),type = "norm", size = 1.3) +
scale_color_manual(values = c("#8d0af1", "#F8766D", "#00BFC4", "#f5cd05")) +
theme_classic(base_size = 20)
ggsave("pca.png")


#analysis and plots for selected traits and first principle component
mytraits = c( "ASD", "GSD", "GA", "UBW", "PC")
mylabels = c( "ASD (mm)", "GSD (mm)", "GA (sqrt of width * height in mm)",
"UBW (mm)", "First principle component" )

for (i in 1:length(mytraits)){

    # interaction plots for traits and for first principle component
    temp_plot = ggplot(data = dat, 
    aes_string(x = "treat", y = mytraits[i], group = "sp", color ="sp")) +
    labs(x = "Treatment", y = mylabels[i], colour = "Species")+
    stat_summary(fun = mean, geom = "line", size = 1.3)+
    stat_summary(fun.data = mean_se, geom = "linerange", size = 1.3)+
    theme_classic(base_size = 23)
    ggsave(temp_plot, file=paste0("interactionplot_", i,".png"))

    # anova analysis for each trait
    fm <- as.formula(paste(mytraits[i], "~ sp * treat", sep=""))
    m = lm(fm, data = dat)
    print(summary(m))
    print(anova(m))
}


#mean for each population for the anther-stigma distance
subdat1 = dat[dat$sp == "S",]
subdat2 = dat[dat$sp == "L",]

print(mean(subdat1$ASD))
print(mean(subdat2$ASD))

