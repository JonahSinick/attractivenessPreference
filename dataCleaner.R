setwd("~/Desktop/attrPreference")
#install.packages(c("dplyr", "lme4"))
library(dplyr)
library(lme4)
library(dummies)
df = read.csv("Speed Dating Data.csv")
df = select(df, -starts_with("pf"),-ends_with("_s"),-ends_with("_2"), -ends_with("_3"))

df = select(df, -position, -positin1, -id, -partner, -idg, -samerace,
            -from, -field, -field_cd,  -zipcode, -undergra, -career, -condtn, -round, -order, -int_corr,
            -(match_es:them_cal), -race_o, -match, -met_o, -age_o)
names(df)

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
names(df)
m = lmer(attr ~ (1|gender) +  (1|pid),df)
df$attrPAvg = predict(m, df, allow.new.levels = TRUE)

sels = select(df, wave, iid, pid, attr_o:prob_o, attr:prob)
for(name in names(sels)[-2:0]){
  form0 = as.formula(paste(name, "~ (1|iid)"))
  m0 = lmer(form0, df)
  form1 = as.formula(paste(name, "~ (1|wave)"))
  m1 = lmer(form1, df)
  sels[[name]]  = predict(m0, df, allow.new.levels = TRUE) - predict(m1, df, allow.new.levels = TRUE)
}
df[names(sels)] = sels

agged = aggregate(df, df["iid"], FUN = mean, na.rm = TRUE)
cats = c("race", "career_c", "goal")
agged[cats] = lapply(agged[cats], as.factor)
levels(agged$race) = c("black", "white", "latino", "asian", "other", NA)
levels(agged$career_c) = c("law", "academia", "psychology","medicine", "engineer", "arts", "business", "realEstate", "internationalAffairs", NA, "socialWork", "speech", "politics", "athletics", NA, "journalism", "architecture", NA)
levels(agged$goal) = c("funNight", "meetNew", "getDate", "seriousRelationship", "sayDid", NA, NA)
dums = dummy.data.frame(agged[cats])
dums = dums[,colSums(dums) >= 10]
agged = cbind(agged, dums)
agged$attrRevPref = 0
agged = agged[-1]
for(gen in 0:1){
  m = glmer(dec ~ attrPAvg + (1 + attrPAvg|iid),df[df$gender == gen,], family = "binomial")
   agged[agged$gender == gen, "attrRevPref"] =  coef(m)$iid[,"attrPAvg"]
  for(name in names(agged[sapply(agged, is.numeric)])){
    arr = agged[agged$gender == gen,name]
    agged[agged$gender == gen,name]  = ifelse(is.na(arr) | is.nan(arr), mean(arr, na.rm = TRUE), arr)
  }
}

gens = agged[agged$gender == 0,sapply(agged, is.numeric)]
adf0 = as.data.frame(round(100*cor(gens,gens$attrRevPref)))
gens = agged[agged$gender == 1,sapply(agged, is.numeric)]
adf1 = as.data.frame(round(100*cor(gens,gens$attrRevPref)))
cbind(adf0, adf1)
library(caret)
library(glmnet)


l = lapply(0:1, function(gen){
  gens = agged[agged$gender == gen,sapply(agged, is.numeric)]
  gens  = select(gens, -iid,-wave, -pid, -attrPAvg, -raceother, -goalNA)
  tar = scale(gens$attrRevPref)[,1]
  features = scale(gens[names(gens) != "attrRevPref"])
  features = features[,!is.nan(colSums(features)) & !is.na(colSums(features))]
  colSums(features)
  
  grid = expand.grid(lambda = (1.5)^(seq(-10, 20)), alpha = seq(0, 1, 0.1))
  Control <- trainControl(method = "repeatedcv",repeats = 3, verboseIter = T,search = "grid")
  set.seed(1)
  tar
  netFit <- train(x =features, y = tar,
                  method = "glmnet",
                  tuneGrid = grid,
                  metric = "RMSE",
                  trControl = Control)
  netFit 
  m = glmnet(features, tar, lambda = netFit$bestTune$lambda, alpha = netFit$bestTune$alpha)
  adf = as.data.frame(as.matrix(round(coef(m), 3)))
  adf$X = 0
  adf = adf[adf$s0 != 0, ]
  adf  
})
l
