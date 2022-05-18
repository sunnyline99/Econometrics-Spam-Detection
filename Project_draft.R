library("sandwich")
library("lmtest")
library("MASS")
library("mfx")
library("BaylorEdPsych")
library("htmltools")
library("LogisticDx")
library("aod")
library("logistf")

Sys.setenv(LANG = "en")
options(scipen = 5)


spam <- read.table("C:\\Users\\justy\\Desktop\\Info\\Inne\\DSC\\UW\\Semestr II\\Econometrics\\Project\\spambase.data", sep = ",")
names <- read.csv2("C:\\Users\\justy\\Desktop\\Info\\Inne\\DSC\\UW\\Semestr II\\Econometrics\\Project\\names.csv",header = FALSE)

names[49,1]<-"char_freq_semicolon"
names[50,1]<-"char_freq_bracket"
names[51,1]<-"char_freq_square_bracket"
names[52,1]<-"char_freq_exclamation"
names[53,1]<-"char_freq_dollar"
names[54,1]<-"char_freq_hashtag"


for(i in 1:57){
  colnames(spam)[i]<-names[i,1]
}

colnames(spam)[58] <-"spam"

spam = na.omit(spam)

plot(spam$word_freq_3d,col=ifelse(spam$spam==0,"green","red"))


# probit model estimation
myprobit <- glm(spam~char_freq_semicolon+
                char_freq_bracket+
                  char_freq_square_bracket+
                    char_freq_exclamation+
                      char_freq_dollar+
                        char_freq_hashtag
                    , data=spam, 
                family=binomial(link="probit"))
summary(myprobit)
myprobit$coefficients
summary(myprobit)$coefficients[2,3]


## summary - we cannot interpret coefs but can interpret the sign -> one additional nomination will increase probability of the success (winning oscar), same with golden globes/ in probit we can say that one var has stronger effect on probability and in logit we can divide and say how many times more

# ------
# Ad. a)

# Joint insignificance of all variables test - likelihood ratio test - restriceted and unrestricted model (notes)
null_probit = glm(spam~1, data=spam, family=binomial(link="probit")) # restricted model and above was unrestricted
lrtest(myprobit, null_probit)

# Chi^2 is equal 54.58 and p-value is almost 0 -> reject H0 -> all variables are jointly significant (short test next week) -> if we reject H0 then we go for unrestricted model if not then we stay with restricted model
# if we have one more variable and not restrict beta3 then we are not checking joint all significance but the restricted model

# marginal effects for the average observation
(meff = probitmfx(formula=x, data = spam, atmean=TRUE))


mean(oscar$nominations)
mean(oscar$gglobes)


(meff = probitmfx(formula=winner~nominations+gglobes, data = oscar, atmean=FALSE))

## Now I calcualte all marginal effect for single movie then we get average out of them
## Almost same interpretation -> but among all films

# marginal effect for user-defined observation
source("marginaleffects.R")
user.def.obs = c(1,7.43,1.31) #convention: (intercept, x1, x2, ...)
marginaleffects(myprobit, user.def.obs)

user.def.obs = c(1,3,5) #convention: (intercept, x1, x2, ...)
marginaleffects(myprobit, user.def.obs)

user.def.obs = c(1,0,0) #convention: (intercept, x1, x2, ...)
marginaleffects(myprobit, user.def.obs)

## Marginal effect of any single option is different 


## Test

for(i in 1:10){
  for(j in 1:10){
    x<user.def.obs = c(1,i,j)
  }
}

## END

# ------
# Ad. c)

# R-squared statistics
PseudoR2(myprobit)

## McKelvey.Zavoina -> if the latent (hidden) variable was observed (y) our model would explain around 70% of varaince (almost same as R^2 interpretation but we need to add latent)
## latent variable is a binary variable -> we state p star level to set threashold

## count -> 90% observations are well predicted -> 1=1 and 0=0 and only 10% when 1=0 and 0=1

## adjusted count -> 50% correct obseravations because of variation and without effect of p star (threashold level)

## McFadden r^2 -> it does not have interpreattion -> can compare models but not interpret

# ------
# Ad. d) - now we go for diagnostics

# Linktest - making sure we have hood specification H0: we have good specification (similar to ramsey reset test)

source("linktest.R")
linktest_result = linktest(myprobit)
summary(linktest_result)
# The specification is correct. y hat is significant and y hat squared is not significant -> this is appropriate specification if hat^2 is significant then we need to add sth as it is not good

# -----
# Ad. e) and f) - people who are data scientists they prefer Hosmer test and even more Osius-Rojek test
gof.results = gof(myprobit)
gof.results$gof

#we need first and third row -> two tests, p-values are 99 and 94 percent so we do not rejest H0 -> it means we have good specification of the model
# HL test is similar to chi square independence test -> this test is highly changable and depends on number of subgroups -> many researchers wanted more advanced versions of the test -> O-R test

# -----
# Ad. g)
myprobit <- glm(winner~nominations+gglobes, data=oscar, 
                family=binomial(link="probit"))
summary(myprobit)

myprobit_restricted <- glm(winner~nominations, data=oscar, 
                           family=binomial(link="probit"))
summary(myprobit_restricted)

lrtest(myprobit, myprobit_restricted)
# Result?



# linear probability model
lpm = lm(x, data=spam)
summary(lpm)

# -----
# Ad. b

# specification test
resettest(lpm, power=2:3, type="fitted")

# -----
# Ad. c

# heteroscedasticity
lpm.residuals = lpm$residuals
plot(lpm.residuals~log.pop, data=olympics)
plot(lpm.residuals~log.gdp, data=olympics)

bptest(lpm.residuals~log.pop, data=olympics)
bptest(lpm.residuals~x,data=spam)

View(lpm$fitted.values)

# -----
# Ad. d

# White's estimator of the variance-covariane matrix
robust_vcov = vcovHC(lpm, data = spam, type = "HC")
coeftest(lpm, vcov.=robust_vcov)

# to compare the simple lpm and the one with a robust vcov matrix
library("stargazer")
robust.lpm = coeftest(lpm, vcov.=robust_vcov)
stargazer(lpm, robust.lpm, type="text")






########### ALL VARS AS DEP

x=NA

formula="spam ~"

for(i in 1:57){
  formula=paste(formula,"+",colnames(spam)[i])
}

print(formula)

myprobit <- glm(x, data=spam, family=binomial(link="probit"))
summary(myprobit)
myprobit$coefficients
summary(myprobit)$coefficients[2,3]

p <- summary(myprobit)$coefficients[,"Pr(>|z|)"]

length(summary(myprobit)$coefficients)[max(p)]

z<-summary(myprobit)$coefficients[,4]==max(p)

spam_temp <- spam


while (any(p>0.05)){
  count = 1
  worstp <- summary(myprobit)$coefficients[,4]==max(p)
  for (i in worstp){
    ifelse(i==TRUE,numvar <- count-1,count<-count+1)
  }
  
  print(colnames(spam_temp[numvar]))
  
  spam_temp[,numvar] <- NULL
  
  
  formula="spam ~"
  
  
  for(i in 1:ncol(spam_temp)){
    formula<-paste(formula,"*",colnames(spam_temp)[i])
  }
  
  
  myprobit <- glm(formula, data=spam_temp, family=binomial(link="probit"))
  p <- summary(myprobit)$coefficients[,"Pr(>|z|)"]
  print(myprobit$aic)
  
}

print(formula)

summary(myprobit)$coefficients
stargazer(lpm, myprobit, type="text")



