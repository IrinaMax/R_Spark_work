# https://www.youtube.com/watch?v=RlhnNbPZC0A
# two-sample t-test with confidence interval for the difference in means of two populations
# independent groups. There are parametric method appropriet for examining in means for 2 poulations

getwd()
LungCapData <- read.table(file.choose(), header =T, sep='\t')
attach(LungCapData)
LungCapData %>% head
# the way of examining of the relationship between a numeric outcome variable(Y) and cathegorical explanatory variable(X, with 2 levels)
names(LungCapData)
class(LungCap)
class(Smoke)
levels((Smoke))
#help(t.test)
boxplot(LungCapData)
boxplot(LungCap~ Smoke)

# where mean = 0(or ZeroHypothesis), alt hypot 2 tail test, conf int 95%, assumed where Variance not equal and two group are independent = not paired
t.test(LungCap ~ Smoke, mu =0, alt="two.sided", conf.level = 0.95, var.eq=F, paired = F)

t.test(LungCap[Smoke=="no"], LungCap[Smoke =="yes"])

# let try to do equal variences
t.test(LungCap ~ Smoke, mu =0, alt="two.sided", conf.level = 0.95, var.eq=T, paired = F)
# let compare the capasity of variance
var(LungCap[Smoke=="yes"])
# [1] 3.545292
var(LungCap[Smoke =="no"])
# [1] 7.431694
# and we can see th smoking group is almost twice bigger

# Levene'e test  Ho: population variance are equal
library(car)
leveneTest(LungCap~Smoke)
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value    Pr(>F)    
# group   1  12.955 0.0003408 ***
# 723                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# We can see here with very smoll P should regect Ho and variance are not equal, and use in this case not equal assumption

#-------------------------------
# 
# Wilcoxon Rank-Sum (aka Mann-Whitney U) test is the non-parametric alternative to the independent t-test
# Ho : Median Lung Capacity of not smokers = tat of not smokers
boxplot(LungCap~ Smoke)

# two sided test

wilcox.test(LungCap ~Smoke, mu = 0, alt = "two.sided", conf.int = T, conf.Level = 0.95, paired =F, exact=T, correct = T)

#______________________
#Paired  t test - parametric method  appropriate for examining the difference in Means for 2 populations that are pared or dependent on one another

# the data is sistolic blood pressure before and after treatment
bPressure = read.table(file.choose(), header = T, sep= "\t")
attach(bPressure)
names(bPressure)
dim (bPressure)
bPressure[1:5,]

boxplot(Before, After)
plot(Before, After)
abline(a=1,b=1, col = "red")

#Ho: Mean difference in SBP is o
# two side t test alternative
t.test(Before, After, mu =0, alt="two.sided", conf.level = 0.99,  paired = T)
t.test(After, Before, mu =0, alt="two.sided", conf.level = 0.99,  paired = T)
# __________________________

# Wilcoxon Signed Rank test in R. This test is the non-parametric alternative to the paired t-test.
# for examiming Median difference in observations for 2 populations, that are pared or dependent on one another.

boxplot(Before, After)
 # the blood pressure are lower after treatment
# Ho: Madian change in SBP is 0
# 2 sided test
wilcox.test(Before, After, mu = 0, alt = "two.sided", paired = T, conf.int = T, conf.level = 0.99, exact = F ) 
