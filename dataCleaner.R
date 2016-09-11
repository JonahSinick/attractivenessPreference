setwd("~/Desktop")
#install.packages(c("dplyr", "lme4"))
library(dplyr)
library(lme4)
df = read.csv("Speed Dating Data.csv")
df = select(df, -starts_with("pf"),-ends_with("_s"),-ends_with("_2"), -ends_with("_3"))

df = select(df, -position, -positin1, -id, -partner, -idg, -samerace,
            -from, -field, -zipcode, -undergra, -career, -condtn, -round, -order, -int_corr,
            -(match_es:them_cal))
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
