load("~/Desktop/plastic.Rda")
head(plastic)

library(ggplot2)
library(skimr)
skim(plastic)

#Q1
library(ggpubr)
p1 <- ggplot(plastic, aes(x = PWaste)) +
  geom_histogram(aes(y=..density..))+
  geom_density(color='red', adjust = 1.5)+
  ggtitle('Distribution of PWaste')+
  labs(x = "Amount of plastic waste per capita (kg/day)", y = "Density")+
  theme_minimal()
p2 <- ggplot(plastic, aes(x = MPWaste)) +
  geom_histogram(aes(y=..density..))+
  geom_density(color='blue', adjust = 1.5)+
  ggtitle('Distribution of MPWaste')+
  labs(x = "Amount of Mismanaged plastic waste per capita (kg/day)", y = "Density")+
  theme_minimal()
ggarrange(p1, p2, ncol = 2, nrow = 1)

par(mfrow=c(1,2))
qqnorm(plastic$PWaste)
qqline(plastic$PWaste) 
#distribution does not match the shape of the normal distribution
qqnorm(plastic$MPWaste)
qqline(plastic$MPWaste) #Mismatch with the shape of the normal distribution

par(mfrow=c(1,2))
options(repr.plot.width = 8, repr.plot.height = 4) # Adjust plot size
box1<-boxplot(plastic$PWaste,
        main="Boxplot of PWaste",
        col='lightblue',
        ylab="Amount of plastic waste per capita (kg/day)",
        xlab="PWaste")
box2<-boxplot(plastic$MPWaste, 
        main="Boxplot of MPWaste",
        col='lightyellow',
        ylab="Amount of mismanaged plastic waste per capita (kg/day)",
        xlab="MPWaste")

summary(plastic$PWaste) 
summary(plastic$MPWaste)

#missing values
image(is.na(plastic))
colSums(is.na(plastic))
library(naniar)
gg_miss_var(plastic,show_pct = TRUE) #PWaste:2,MPWaste:41
library(visdat)
vis_dat(plastic)
vis_miss(plastic)
dev.off()
library(mice)
options(width = 100) 
md.pattern(plastic)

#dealing with missing values
missingdata<-data.frame(plastic$PWaste,plastic$MPWaste)
imp <- mice(missingdata,m=5,maxit=50,seed=1, method = "pmm")
missingdata <- complete(imp)
complete_data <- as.data.frame(missingdata)
plastic$PWaste <- complete_data$plastic.PWaste
plastic$MPWaste <- complete_data$plastic.MPWaste

#Q2
library(gridExtra)
library(RColorBrewer)
inc_colors <- brewer.pal(4, "Set2")
reg_colors <- brewer.pal(7, "Set2")

#table(plastic$incomestatus)
plot_inc<-ggplot(data=plastic, aes(x=IncomeStatus, fill=IncomeStatus)) + 
  geom_bar() + 
  scale_fill_manual(values=inc_colors) +
  labs(title="Barplot of Income Status") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) +
  coord_cartesian(ylim=c(0, 80))

#table(plastic$region)
plot_reg <- ggplot(data=plastic, aes(x=Region, fill=Region)) + 
  geom_bar() + 
  scale_fill_manual(values=reg_colors) +
  labs(title="Barplot of Region") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) +
  coord_cartesian(ylim=c(0, 60))

grobs <- list(ggplotGrob(plot_inc), ggplotGrob(plot_reg))
grid.arrange(grobs = grobs, ncol = 2)

# Violin plots by region
pwasteregion<-ggplot(plastic, aes(x = Region, y = PWaste, fill = Region)) +
  geom_violin() +
  ggtitle("Plastic waste by Region") +
  ylab("Plastic waste per capita (kg/day)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_boxplot(width=0.1)
mpwasteregion<-ggplot(plastic, aes(x = Region, y = MPWaste, fill = Region)) +
  geom_violin() + 
  ggtitle("Mismanaged plastic waste by Region") + 
  ylab("Mismanaged plastic waste per capita (kg/day)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_boxplot(width=0.1)
grobs <- list(ggplotGrob(pwasteregion), ggplotGrob(mpwasteregion))
grid.arrange(grobs = grobs, ncol = 2)

# Violin plots by income status
pwasteincome<-ggplot(plastic, aes(x = IncomeStatus, y = PWaste, fill = IncomeStatus)) + 
  geom_violin() + 
  ggtitle("Plastic waste by income status") + 
  ylab("Plastic waste per capita (kg/day)")+
  geom_boxplot(width=0.1)
mpwasteincome<-ggplot(plastic, aes(x = IncomeStatus, y = MPWaste, fill = IncomeStatus)) + 
  geom_violin() + 
  ggtitle("Mismanaged plastic waste by income status") + 
  ylab("Mismanaged plastic waste per capita (kg/day)")+
  geom_boxplot(width=0.1)
grobs <- list(ggplotGrob(pwasteincome), ggplotGrob(mpwasteincome))
grid.arrange(grobs = grobs, ncol = 2)

#ridges plots
library(ggridges)
ridges_region1<-ggplot(plastic, aes(PWaste, y=factor(Region), fill=Region)) + 
  geom_density_ridges(scale = 1.5)
ridges_region2<-ggplot(plastic, aes(MPWaste, y=factor(Region), fill=Region)) + 
  geom_density_ridges(scale = 1.5)
grobs <- list(ggplotGrob(ridges_region1), ggplotGrob(ridges_region2))
grid.arrange(grobs = grobs, ncol = 2)

ridges_income1<-ggplot(plastic, aes(PWaste, y=factor(IncomeStatus), fill=IncomeStatus)) + 
  geom_density_ridges(scale = 1.5)
ridges_income2<-ggplot(plastic, aes(MPWaste, y=factor(IncomeStatus), fill=IncomeStatus)) + 
  geom_density_ridges(scale = 1.5)
grobs <- list(ggplotGrob(ridges_income1), ggplotGrob(ridges_income2))
grid.arrange(grobs = grobs, ncol = 2)

#t-test
high_income_M <- plastic[plastic$IncomeStatus == "HIC", "MPWaste"]
low_income_M <- plastic[plastic$IncomeStatus == "LIC", "MPWaste"]
high_income <- plastic[plastic$IncomeStatus == "HIC", "PWaste"]
low_income <- plastic[plastic$IncomeStatus == "LIC", "PWaste"]
t.test(high_income, low_income) #Significantly different
t.test(high_income_M, low_income_M) #Significantly identical

#kruskal-test
kruskal.test(plastic$PWaste ~ plastic$Region) #Significantly different
kruskal.test(plastic$MPWaste ~ plastic$Region) #Significantly different

kruskal.test(plastic$PWaste ~ plastic$IncomeStatus) #Significantly different
kruskal.test(plastic$MPWaste ~ plastic$IncomeStatus) #Significantly different

#dun.test
library(dunn.test)
dunn.test(plastic$PWaste, plastic$Region, method = "bonferroni")
dunn.test(plastic$MPWaste, plastic$Region, method = "bonferroni")

dunn.test(plastic$PWaste, plastic$IncomeStatus, method = "bonferroni")
dunn.test(plastic$MPWaste, plastic$IncomeStatus, method = "bonferroni")

# Boxplots 
dev.off()
par(mfrow=c(1,1))
boxplot(PWaste ~ Region, data = plastic, main = "Boxplot of Plastic waste by region",
        col = 2:8, xlab='Region',theme(axis.text.x = element_text(angle = 45)),cex.axis=0.6,ylab = "Amount of plastic waste")
legend("topright", legend = levels(plastic$Region),
       fill = 2:8, cex = 0.6, title = "Region") 

boxplot(MPWaste ~ Region, data = plastic, main = "Boxplot of Mismanaged plastic waste by region",
        col = 2:8,xlab='Region',theme(axis.text.x = element_text(angle = 45)),cex.axis=0.6,ylab = "Amount of mismanaged plastic waste")
legend("topright", legend = levels(plastic$Region),
       fill = 2:8, cex = 0.8, title = "Region") 

#PWaste-income
par(mfrow=c(1,1))
boxplot(PWaste ~ IncomeStatus, data = plastic, main = "Boxplot of Plastic waste by income status", 
        col=2:5, xlab='Income Status', ylab = "Plastic waste per capita (kg/day)")
legend("topright", legend = levels(plastic$IncomeStatus),
       fill = 2:8, cex = 0.8, title = "Income Status") 
par(mfrow=c(4,1))
hist(plastic$PWaste[plastic$IncomeStatus=="LIC"], main='LIC',xlab='PWaste', col=2)
hist(plastic$PWaste[plastic$IncomeStatus=="LMC"], main='LMC',xlab='PWaste', col=3)
hist(plastic$PWaste[plastic$IncomeStatus=="UMC"], main='UMC',xlab='PWaste', col=4)
hist(plastic$PWaste[plastic$IncomeStatus=="HIC"], main='HIC',xlab='PWaste', col=5)

#MPWaste-income
par(mfrow=c(1,1))
boxplot(MPWaste ~ IncomeStatus, data = plastic, main = "Mismanaged plastic waste by income status",
        col=2:5, xlab='Income Status', ylab = "Mismanaged plastic waste per capita (kg/day)")
legend("topleft", legend = levels(plastic$IncomeStatus),
       fill = 2:8, cex = 0.8, title = "Income Status") 
par(mfrow=c(4,1))
hist(plastic$MPWaste[plastic$IncomeStatus=="LIC"], main='LIC',xlab='MPWaste', col=2)
hist(plastic$MPWaste[plastic$IncomeStatus=="LMC"], main='LMC',xlab='MPWaste', col=3)
hist(plastic$MPWaste[plastic$IncomeStatus=="UMC"], main='UMC',xlab='MPWaste', col=4)
hist(plastic$MPWaste[plastic$IncomeStatus=="HIC"], main='HIC',xlab='MPWaste', col=5)


#Q3
library(scales)
par(mfrow=c(1,1))
plot(plastic$PWaste, plastic$MPWaste, 
     pch=16,col=alpha('black',0.5),
     xlab = "Plastic waste", 
     ylab = "Mismanaged plastic waste",
     main="Scatter plot of PWaste vs. MPWaste")
exception_points <- plastic[plastic$PWaste > 200 | plastic$MPWaste > 100, ]
points(exception_points$PWaste, exception_points$MPWaste, col="red", pch=19)
abline(a = 0, b = 1, col = "blue", lwd = 2)
legend("topright", 
       legend = c("Data points", "Exception points", "45-degree line"), 
       pch = c(16, 19, NA), 
       col = c(alpha("black", 0.5), "red", "blue"), 
       lty = c(NA, NA, 1))
#High density areas become darker and more unusual values fade away.

plot(x=plastic$PWaste, y=plastic$MPWaste,col=plastic$Region,pch=16,
     xlab = "Plastic waste", 
     ylab = "Mismanaged plastic waste",
     main="Scatter plot of PWaste vs. MPWaste by Region")
legend(x='topright', cex = 0.6, legend=levels(plastic$Region), pch=16,col=1:nlevels(plastic$Region))

ggplot(plastic, aes(x = PWaste, y = MPWaste, color = Region)) + 
  geom_point() + 
  ggtitle("Plastic waste vs. Mismanaged plastic waste by region") + 
  ylab("Mismanaged plastic waste") + 
  xlab("Plastic waste") + 
  theme(legend.position = "bottom")

plot(x=plastic$PWaste, y=plastic$MPWaste,col=plastic$IncomeStatus,pch=16,
     xlab = "Plastic waste", 
     ylab = "Mismanaged plastic waste",
     main="Scatter plot of PWaste vs. MPWaste by Income Status")
legend(x='topright', cex = 0.8, legend=levels(plastic$IncomeStatus), pch=16,col=1:nlevels(plastic$IncomeStatus))

ggplot(plastic, aes(x = PWaste, y = MPWaste, color = IncomeStatus)) + 
  geom_point() + 
  ggtitle("Plastic waste vs. Mismanaged plastic waste by income status") + 
  ylab("Mismanaged plastic waste") + 
  xlab("Plastic waste") +
  theme(legend.position = "bottom")

# Summary tables by region and income status
library(dplyr)
plastic %>% group_by(Region) %>% summarise(mean_PWaste = mean(PWaste), mean_MPWaste = mean(MPWaste))
plastic %>% group_by(IncomeStatus) %>% summarise(mean_PWaste = mean(PWaste), mean_MPWaste = mean(MPWaste))

region_summary <- aggregate(cbind(PWaste, MPWaste) ~ Region, plastic, FUN = summary)
region_summary
income_summary <- aggregate(cbind(PWaste, MPWaste) ~ IncomeStatus, plastic, FUN = summary)
income_summary

# Calculate correlation coefficient and p-value
cor.test(plastic$PWaste, plastic$MPWaste)
# Correlation analysis grouped by region and income status
regions <- unique(plastic$Region)
statuses <- unique(plastic$IncomeStatus)

for (region in regions) {
  subset <- plastic[plastic$Region == region, ]
  result <- cor.test(subset$PWaste, subset$MPWaste)
  print(paste("Region:", region, "Correlation:", result$estimate, "P-value:", result$p.value))
}

for (status in statuses) {
  subset <- plastic[plastic$IncomeStatus == status, ]
  result<- cor.test(subset$PWaste, subset$MPWaste)
  print(paste("Income Status:", status, "Correlation:", result$estimate, "P-value:", result$p.value))
}


library(lattice)
xyplot(MPWaste ~ PWaste | IncomeStatus, data = plastic,pch=16,
       xlab = "Plastic Waste",
       ylab = "Mismanaged Plastic Waste",
       main = "Relationship between PWaste and MPWaste by Income Status")

xyplot(MPWaste ~ PWaste | Region, data = plastic,pch=16,strip.names = TRUE,
       xlab = "Plastic Waste",
       ylab = "Mismanaged Plastic Waste",
       main = "Relationship between PWaste and MPWaste by Region")

#Consider a parallel coordinate plot
library(GGally)
region_s<-ggparcoord(plastic[,c("PWaste", "MPWaste", "Region")], 
           columns = c(1, 2), groupColumn = 3)

income_s<-ggparcoord(plastic[,c("PWaste", "MPWaste", "IncomeStatus")], 
           columns = c(1, 2), groupColumn = 3)
grobs <- list(ggplotGrob(region_s), ggplotGrob(income_s))
grid.arrange(grobs = grobs, ncol = 2)


#Q4
which.max(d2[,'PWaste'])
plastic[125,]
d2=data.frame(plastic[,c("PWaste", "GDP", "Population", "CoastalPopPC", "UrbanPopPC")])
cor(d2) 
names(d2)

par(mfrow=c(2,2))
for(i in names(d2[,2:5])){
  hist(d2[,i], col='#72abe3', xlab='', ylab='', main=paste("Hist of",i))
}

par(mfrow=c(1,1))
library(reshape2)
plastic_numeric <- d2[, sapply(d2, is.numeric)]
corr_matrix <- cor(plastic_numeric)
corr_melted <- melt(corr_matrix)
ggplot(corr_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  labs(title = "Correlation Matrix for Plastic Waste Variables") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(corrplot)
corrplot(cor(d2), diag=FALSE)

library(car)
scatterplotMatrix(d2[,1:5], col=d2[,1]+1,
                  pch=16, smooth=FALSE, regLine=FALSE,
                  diagonal=list(method ="histogram"),
                  main="Scatterplot Matrix of Other Variables",
                  cex.main=0.8) #PWaste

library(GGally)
ggpairs(d2)

library(gplots)
heatmap.2(as.matrix(d2),scale='column',trace='none', cexCol=0.75) 

parallelplot(plastic[,c('PWaste',"GDP", "Population", "CoastalPopPC", "UrbanPopPC")], 
             col=c('#f8766d','#00ba38','#619cff', '#e76bf3')[plastic$IncomeStatus],
             horizontal=FALSE,
             main = "Plastic waste variables by income status",
             xlab = "Variable",
             ylab = "Value",
             scales = list(alternating = 1),
             auto.key=list(text=c('LIC','LMC','UMC', 'HIC'),cex=0.8,
                           title="Income Status",space="top", columns=4, lines=TRUE))

parallelplot(plastic[,c('PWaste',"GDP", "Population", "CoastalPopPC", "UrbanPopPC")], 
             col=c('#F8766D', '#00BA38', '#619CFF', '#E76BF3', '#00CFCF', '#FFC107', '#A9A9A9')[plastic$Region],
             horizontal=FALSE,
             main = "Plastic waste variables by region",
             xlab = "Variable",
             ylab = "Value",
             scales = list(alternating = 1),
             auto.key=list(text=c('East Asia & Pacific','Europe & Central Asia','Latin America & Caribbean', 'Middle East & North Africa','North America','South Asia','Sub-Saharan Africa'),
                           title="Region",cex=0.8,space='top', columns=3, lines=TRUE))


#Q5
#check
any(plastic$CoastalPopPC < 0 | plastic$CoastalPopPC > 100)
plastic[plastic$CoastalPopPC < 0 | plastic$CoastalPopPC > 100, ]
sum(is.na(plastic$CoastalPopPC))

#the size of coastal population
plastic$SizeofCoastal<-(((plastic$CoastalPopPC)/100)*(plastic$Population))
plastic$SizeofCoastal<-as.numeric(plastic$SizeofCoastal)
plastic$SizeofCoastal <- round(plastic$SizeofCoastal, 2)

#sizeofcoastal
fitN <- ksmooth(plastic$SizeofCoastal, plastic$PWaste, 
                kernel = "normal")
lfit <- loess(PWaste~SizeofCoastal, data=plastic,span=0.25)
lpred <- predict(lfit, data.frame(SizeofCoastal = fitN$x), se = TRUE)
plot(x=plastic$SizeofCoastal, y=plastic$PWaste,
     axes = FALSE, 
     pch=16,xlab='Size of Coastal Population',
     ylab='Plastic Waste',
     main='Relationship between plastic waste and the size of coastal population')
axis(side = 2, las = 1, ylab = "Plastic Waste") #y-axis
at <- pretty(plastic$SizeofCoastal)
labels <- format(at, scientific = FALSE, big.mark = ",")
axis(side = 1, at = at, labels = labels) #x-axis
lines(x=fitN$x, y=lpred$fit,col='red',lwd=4)
polygon(x=c(fitN$x,rev(fitN$x)), y=c(lpred$fit- qt(0.975,lpred$df)*lpred$se,
                                     rev(lpred$fit+ qt(0.975,lpred$df)*lpred$se)),
        col=alpha('red',0.2),border=NA)
legend('topright', legend = c('LOESS smoothing', '95% Confidence Interval'),
       col = c('red', alpha('red', 0.2)), lty = c(1, 1), lwd = 4)

#GDP
fitN <- ksmooth(plastic$GDP, plastic$PWaste, 
                kernel = "normal")
lfit <- loess(PWaste~GDP, data=plastic,span=0.25)
lpred <- predict(lfit, data.frame(GDP = fitN$x), se = TRUE)
plot(x=plastic$GDP, y=plastic$PWaste,
     pch=16,xlab='GDP',
     ylab='Plastic Waste',
     main='Relationship between Plastic Waste and GDP')
lines(x=fitN$x, y=lpred$fit,col='red',lwd=4)
polygon(x=c(fitN$x,rev(fitN$x)), y=c(lpred$fit- qt(0.975,lpred$df)*lpred$se,
                                     rev(lpred$fit+ qt(0.975,lpred$df)*lpred$se)),
        col=alpha('red',0.2),border=NA)
legend('topright', legend = c('LOESS smoothing', '95% Confidence Interval'),
       col = c('red', alpha('red', 0.2)), lty = c(1, 1), lwd = 4)
