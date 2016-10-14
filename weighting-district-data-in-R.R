########################################
#                                      #
# District Survey Results with Weights #
#                                      #
########################################

### --- Packages --- ###
if(!require(survey)){
  install.packages("survey")
  library(survey)
}

### --- Data --- ###
teacher <- read.csv("TNTeacherWave3.csv") #2016 TN Teacher Survey
districts <- read.csv("TNDistrictWave3.csv") #2016 TN District Names

### --- Districts of Interest --- ###
d <- c("4701080", "4701470", "4702190", "4702220", "4703480", "4700148", "4703990", "4704170", "4702400", "4703090", "4703240", "4704290", "4701230", "4704500")

### --- Variables of Interest --- ###
# FairResults_T
# Help_CurriculumCSS_P
# Help_DigitalTools_P

repweights <- teacher[43:102]
options( "survey.replicates.mse" = TRUE)

surveyfreq <- svrepdesign(repweights="FNLWGT[1-9]+",
                          weights = ~FNLWGT0, 
                          combined.weights = TRUE, 
                          type = "other", 
                          dof = 60,
                          scale = 1,
                          rscales = rep(1, ncol(repweights)),
                          data = teacher)

svymean(~factor(teacher$FairResults_T), design=surveyfreq, na.rm=TRUE)

#teacher.recode <- na.omit(teacher)
teacher$FairResults_T2[teacher$FairResults_T < 3] <- 0
teacher$FairResults_T2[teacher$FairResults_T >= 3] <- 1
teacher$FairResults_T2[teacher$FairResults_T < 0] <- NA

svymean(~factor(teacher$FairResults_T2), design=surveyfreq, na.rm=TRUE)
svytotal(~factor(teacher$FairResults_T2), design=surveyfreq, na.rm=TRUE)

confint(svymean(~factor(FairResults_T),design=surveyfreq))
confint(svytotal(~factor(FairResults_T),design=surveyfreq))

fair <- as.data.frame(svyby(formula = ~FairResults_T, by = ~LEAID, design = surveyfreq, FUN = svymean, na.rm=TRUE))
fair <- merge(fair, districts, by = "LEAID")
fair.counties <- fair[fair$LEAID %in% d, ]