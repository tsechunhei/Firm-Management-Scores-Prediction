#read
set.seed(21)
df = read.csv("AMP_graph_manufacturing.csv")
str(df)
df$year = as.factor(df$year)
df$mne_country[df$mne_country=='Australian']= 'Australia'
df$mne_country=sapply(df$mne_country, toupper)
df$country=sapply(df$country, toupper)
df$country[df$country=="UNITED STATES"]= "US"
df$country[df$country=="REPUBLIC OF IRELAND"]= "IRELAND"

#check NA
cbind(
  lapply(
    lapply(df[complete.cases(df[ , 5:6]),], is.na)
    , sum)
)


#drop whole coloumns
df$competition2004 = NULL
df$storage..display.....value = NULL
df$X = NULL
df$export = NULL
df$target = NULL
df$monitor = NULL
df = df[complete.cases(df[ , 14]),] #drop na year

###mice
install.packages("mice")
library(mice)
md.pattern(df)

###visualize missing values
install.packages("VIM")
library(VIM)
mice_plot = aggr(df, col=c('navyblue','yellow'),
                 numbers=TRUE, sortVars=TRUE,
                 labels=names(df), cex.axis=.7,
                 gap=3, ylab=c("Missing data","Pattern"))

###impute missing values
imputed_Data = mice(df, m=5, maxit = 30, method = 'pmm', seed = 100)
summary(imputed_Data)
imputed_Data$imp$review_scores

df = complete(imputed_Data,2)
str(df)



#drop outliers for dependent variable
Q1 = quantile(df$management, .25)
Q3 = quantile(df$management, .75)
IQR = IQR(df$management)
df = subset(df, df$management > (Q1 - 1.5*IQR) & df$management< (Q3 + 1.5*IQR))

#drop outliers for factors
table(df$ownership)
table(df$mne_country)
table(df$country)
table(df$year)

df = df[df$mne_country %in% names(table(df$mne_country)[table(df$mne_country) >= 100]), ] #drop mne_country with less than 100 observation
df = df[df$year %in% names(table(df$year)[table(df$year) >= 50]), ] #drop  with less than 50 observation

#replace '' with NA
df$year = as.factor(df$year)
df$ownership[df$ownership==''] = NA
df$mne_country[df$mne_country==''] = NA
df = df[complete.cases(df[ , 5:6]),]

write.csv(df, 'cleaned_dataset.csv', row.names = FALSE)
