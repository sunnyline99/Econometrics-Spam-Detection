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
