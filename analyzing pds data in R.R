## install survey package (only have to do this once)
utils:::menuInstallPkgs()

## load survey package (need to do this each time you start R)
local({pkg<-select.list(sort(.packages(all.available=TRUE)),graphics=TRUE) 
if(nchar(pkg)) library(pkg, character.only=TRUE)})

## import raw data from PDS
teacher <- read.csv("c:/mydata/TNTeacherWave3.csv")

## save R workspace
save.image("C:\\mydata\\TNTeacherWave3.RData")

## load R workspace
teacher <- load("C:\\mydata\\TNTeacherWave3.RData")



## unweighted counts
xtabs(~factor(teacher$OutstandingTeachers_T))

## create data frame called repweights
repweights <- teacher[43:102]

## specify the design and give it a name "dmli"
dmli <-svrepdesign(data=teacher, type="other", weights=~FNLWGT0, repweights=repweights, mse=TRUE, dof=60, scale=1, rscales=rep(1, ncol(repweights)))


## weighted counts and standard errors including all values
svytotal(~factor(OutstandingTeachers_T), dmli)

## confidence intervals for weighted counts
## these don't match DS
confint(svytotal(~factor(OutstandingTeachers_T), dmli))



## weighted percentages and standard errors
## for weighted percentages we have to get rid of the negative values
## don't delete them altogether
#####
## DON'T delete records!!
## t.recode <- na.omit(teacher)
#####


## set negative values to NA
## this might be causing problems
teacher$OutstandingTeachers_T[teacher$OutstandingTeachers_T < 0] <- NA


## weighted percentages and SEs
svymean(~factor(teacher$OutstandingTeachers_T), dmli, na.rm=TRUE)

## confidence intervals for weighted percentages 
## these CIs don't match DS 
confint(svymean(~factor(teacher$OutstandingTeachers_T), dmli, na.rm=TRUE))


## weighted counts and standard errors including only non-missing values
svytotal(~factor(teacher$OutstandingTeachers_T), dmli, na.rm=TRUE)
confint(svytotal(~factor(teacher$OutstandingTeachers_T), dmli, na.rm=TRUE))


## means and medians
svymean(~OutstandingTeachers_T, subset(dmli, OutstandingTeachers_T > 0))
svymean(~teacher$OutstandingTeachers_T, dmli, na.rm=TRUE)

svyquantile(~teacher$OutstandingTeachers_T, dmli, na.rm=TRUE, 0.5)




## subpopulations
svyby(~OutstandingTeachers_T, ~YearsTeach2, dmli, svymean)
svyby(~factor(OutstandingTeachers_T), ~factor(YearsTeach2), dmli, svymean)

## weighted counts and se by subpopulations
svyby(~factor(OutstandingTeachers_T), ~factor(YearsTeach2), dmli, svytotal)

svyby(~factor(OutstandingTeachers_T), ~factor(YearsTeach2), subset(dmli, OutstandingTeachers_T > 0 & YearsTeach2 > 0), svytotal)
svyby(~factor(YearsTeach2), ~factor(OutstandingTeachers_T), subset(dmli, OutstandingTeachers_T > 0 & YearsTeach2 > 0), svytotal)

## weighted percentages and se by subpops
svyby(~factor(OutstandingTeachers_T), ~factor(YearsTeach2), subset(dmli, OutstandingTeachers_T > 0 & YearsTeach2 > 0), svymean)

## weighted mean of subpops
svyby(~OutstandingTeachers_T, ~factor(YearsTeach2), subset(dmli, OutstandingTeachers_T > 0 & YearsTeach2 > 0), svymean)


## statistical inferences
svyttest(OutstandingTeachers_T~YearsTeach2, subset(dmli, OutstandingTeachers_T > 0 & YearsTeach2 > 0))





####################################

## collapse responses using a new variables
teacher$FairResults_T2[teacher$FairResults_T < 3] <- 0
teacher$FairResults_T2[teacher$FairResults_T >= 3] <- 1
teacher$FairResults_T2[teacher$FairResults_T < 0] <- NA

## weighted percentages for new variable
svymean(~factor(teacher$FairResults_T2), design=dmli, na.rm=TRUE)



