setwd("~/Desktop/attrPreference")
#install.packages(c("dplyr", "lme4"))
library(dplyr)
library(lme4)
library(dummies)
library(caret)
library(glmnet)
df = read.csv("Speed Dating Data.csv")

#Drop non-rating variables pertaining to partner, variables collected after the event, variables not specific to the person, and redundant
df = select(df, -starts_with("pf"),-ends_with("_s"),-ends_with("_2"), -ends_with("_3"), -ends_with("5_1"))

df = select(df, -position, -positin1, -id, -partner, -idg, -samerace,
            -from, -field, -field_cd,  -zipcode, -undergra, -career, -condtn, -round, -order, -int_corr,
            -(match_es:them_cal), -race_o, -match, -met_o, -age_o)


#Convert from character to numeric: average SAT score and average tuition of undergrad institution, average income for zipcode of origin
df$mn_sat = as.numeric(sapply(as.character(df$mn_sat), function(string){
  if(nchar(string) == 0){
    return(NA)
  }else if(nchar(string) == 6){
    return(as.numeric(substr(string, 1, 3)))
  }else if(nchar(string == 9)){
    return(1000 + as.numeric(substr(string, 3, 5)))
  }
}))
dollars = function(string){
  1000*as.numeric(strsplit(string, split =  ",")[[1]][1])
}
for(name in c("income", "tuition")){
  levels(df[[name]]) = as.numeric(sapply(levels(df[[name]]), dollars))
  df[[name]] = as.numeric(as.character(df[[name]]))
}

#Get Bayesian adjusted partner's average attractiveness rating
m = lmer(attr ~ (1|gender) +  (1|pid),df)
df$attrPAvg = predict(m, df, allow.new.levels = TRUE)

#Get Bayesian adjusted ratings by participiant and of participant, adjusting for event averages
sels = select(df, wave, iid, pid, attr_o:prob_o, attr:prob)
for(gen in 0:1){
  for(name in names(sels)[-2:0]){
    form0 = as.formula(paste(name, "~ (1|iid)"))
    m0 = lmer(form0, df[df$gender == gen,])
    form1 = as.formula(paste(name, "~ (1|wave)"))
    m1 = lmer(form1, df)
    sels[sels$gender == gen,name]  = predict(m0, df, allow.new.levels = TRUE) - predict(m1, df, allow.new.levels = TRUE)
  }  
}
df[names(sels)] = sels


#Aggregate by participant
agged = aggregate(df, df["iid"], FUN = mean, na.rm = TRUE)

#Rename categorical variables
cats = c("gender", "race", "career_c", "goal")
agged[cats] = lapply(agged[cats], as.factor)
levels(agged$gender) = c("woman", "man")
levels(agged$race) = c("black", "white", "latino", "asian", "other", NA)
levels(agged$career_c) = c("law", "academia", "psychology","medicine", "engineer", "arts", "business", "realEstate", "internationalAffairs", NA, "socialWork", "speech", "politics", "athletics", NA, "journalism", "architecture", NA)
levels(agged$goal) = c("funNight", "meetNew", "getDate", "seriousRelationship", "sayDid", NA, NA)


df$gender = factor(df$gender, labels = c("woman", "man"))

#Create dummy variables
dums = dummy.data.frame(agged[cats[-1]])
dums = dums[,colSums(dums) >= 10]
agged = cbind(agged, dums)

#Generate revealed preference for attractiveness variable
agged$attrRevPref = 0
agged = agged[-1]
for(gen in c("woman", "man")){
  m = glmer(dec ~ attrPAvg + (1 + attrPAvg|iid),df[df$gender == gen,], family = "binomial")
   agged[agged$gender == gen, "attrRevPref"] =  coef(m)$iid[,"attrPAvg"]
  for(name in names(agged[sapply(agged, is.numeric)])){
    arr = agged[agged$gender == gen,name]
    agged[agged$gender == gen,name]  = ifelse(is.na(arr) | is.nan(arr), mean(arr, na.rm = TRUE), arr)
  }
}


#Train regularized linear regression models for revealed preference for attractiveness
l = lapply(list(women = "woman", men = "man", both = c("woman", "man")), function(gen){
  gens = agged[agged$gender %in% gen,sapply(agged, is.numeric)]
  gens  = select(gens, -iid,-wave, -pid, -attrPAvg, -raceother, -goalNA)
  tar = scale(gens$attrRevPref)[,1]
  features = scale(gens[names(gens) != "attrRevPref"])
  features = features[,!is.nan(colSums(features)) & !is.na(colSums(features))]
  colSums(features)
  
  grid = expand.grid(lambda = (1.5)^(seq(-10, 20)), alpha = c(0, 1))
  Control <- trainControl(method = "repeatedcv",repeats = 3, verboseIter = T,search = "grid")
  set.seed(1)
  netFit <- train(x =features, y = tar,
                  method = "glmnet",
                  tuneGrid = grid,
                  metric = "RMSE",
                  trControl = Control)
  netFit 
  m = glmnet(features, tar, lambda = netFit$bestTune$lambda, alpha = netFit$bestTune$alpha)
  adf = as.data.frame(as.matrix(round(coef(m), 3)))
  adf = data.frame(feature = rownames(adf), coefficient =  adf$s0)
  adf = adf[adf$coefficient != 0, ]
  list(caretObject = netFit, coefficients = adf)
})
l
#Get coefficients for women
l$women
coefficients = l[[1]]$coefficients
coefficients$feature = as.character(coefficients$feature)

bdf <- data.frame(Seller=c("Ad","Rt","Ra","Mo","Ao","Do"), 
                 Avg_Cost=c(5.30,3.72,2.91,2.64,1.17,1.10), Num=c(6:1))
bdf

names(coefficients)

coefficients = coefficients[order(-abs(coefficients["coefficient"])),]
coefficients$X = nrow(coefficients):1

coefficients$feature[1:5] = c("Selectivity", "Fun (Self-Perceived)",  "Attractiveness (group consensus)",
                         "Fun (group consensus)", "Sports (involvement")
coefficients[1,"coefficient"] = -coefficients[1,"coefficient"]

ggplot(coefficients[1:5,], aes(x=reorder(feature, X), y=coefficient)) +
  geom_bar(stat='identity') +
  coord_flip()

