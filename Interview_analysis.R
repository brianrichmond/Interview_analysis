# Import data
comp_int <- read.csv("some_interviews.csv", header=T)
pract_int <- read.csv("some_practice_interviews.csv", header=T)

# combine datasets, using rbind
pract_int$company_id <- NA
int <- rbind(comp_int, pract_int)
View(int)  # visualize dataframe in spreadsheet format in RStudio

# create count of practices by interviewees
require(plyr)
num_practices <- count(pract_int, 'interviewee')
# merge (left join) practice count with combined dataset
int <- merge(x=int, y=num_practices, by= 'interviewee', all.x = TRUE)
colnames(int)[colnames(int)=="freq"] <- "num_practices"  # rename variable
int$num_practices[is.na(int$num_practices)] <- 0  # replace practice count NAs with 0s

# basic eda
dim(int)
str(int)
summary(int)
dup <- int[duplicated(int),]  # check finds there are duplicate entries
int_dup <- unique(int)  # remove duplicates

# create "success" variable to mark successful interviews as True, unsuccessful as False, and NA for those with no company id
int$success <- ifelse(is.na(int$company_id), NA,
  ifelse(!is.na(int$company_id) & 
           int$interviewee_review_workWithThem==TRUE &
           int$interviewer_review_wouldYouHire == TRUE, TRUE,FALSE))

# create subset of data to use in logistic regression (omitting interviewer & interviewee ids, start times)
success_data <- subset(int, select = c(2,4:11,13:18,20:21))
str(success_data)  # verify that the correct data is included

## Classic Logistic Regression (glm)
set.seed(234)
success_Logit <- glm(success~., family = binomial(link="logit"),data = success_data)
summary(success_Logit)
anova(success_Logit,test="Chisq")
require(pscl)
pR2(success_Logit)  # measures of how much the logistic model explains

# remove a couple of variables that are too highly dependent on one another and cause regression model to fail to converge
success_data2 <- subset(int, select = c(2,5:11,14:18,20:21))
str(success_data2)  # verify that the correct data is included
set.seed(333)
success_Logit2 <- glm(success~., family = binomial(link="logit"),data = success_data2)
summary(success_Logit2)
anova(success_Logit2,test="Chisq")
pR2(success_Logit2)  # measures of how much the logistic model explains

# focus on interviewee qualities: rerun analysis
success_data_int <- subset(int, select = c(2,14:16,20:21))
str(success_data_int)  # verify that the correct data is included
set.seed(321)
success_Logit_int <- glm(success~., family = binomial(link="logit"),data = success_data_int)
summary(success_Logit_int)
anova(success_Logit_int,test="Chisq")

pR2(success_Logit_int)  # measures of how much the logistic model explains

## Density (smoothed histogram) plot of successful interviews by other variables
require(ggpubr)
# Success by number of practices
ggdensity(int[!is.na(int$success),], x = "num_practices",
          add = "mean", rug = TRUE,
          color = "success", fill = "success")
# Success by code skill
ggdensity(int[!is.na(int$success),], x = "interviewer_review_codeSkill",
          add = "mean", rug = TRUE,
          color = "success", fill = "success")
# Success by problem-solving ability
ggdensity(int[!is.na(int$success),], x = "interviewer_review_problemSolving",
          add = "mean", rug = TRUE,
          color = "success", fill = "success")
# Success by problem-solving howDidYouDo?
ggdensity(int[!is.na(int$success),], x = "interviewee_review_howDidYouDo",
          add = "mean", rug = TRUE,
          color = "success", fill = "success")

## Do the attributes that companies care about vary across the dataset?
skills_by_company <- subset(comp_int, select = c(1,10,12:16))
str(skills_by_company)
set.seed(123)
success_Logit3 <- glm(interviewer_review_wouldYouHire ~ language +
                        skill1 + skill2 + skill3 +
                        company_id:skill1 +
                        company_id:skill2 +
                        company_id:skill3 +
                        company_id:language,
                      family = binomial(link="logit"),data = skills_by_company)
summary(success_Logit3)
anova(success_Logit3,test="Chisq")
pR2(success_Logit3)  # measures of how much the logistic model explains

# Time & duration?
boxplot(duration~success,comp_int, ylab = "duration (min)", xlab = "Successful interview")

# Do interviewer ratings increase with interviewee num_practices?
set.seed(222)
# create practice dataset w num_practices
int_practice_data <- merge(x=pract_int, y=num_practices, by= 'interviewee', all.x = TRUE)
colnames(int_practice_data)[colnames(int_practice_data)=="freq"] <- "num_practices"  # rename variable
int_practice2 <- glm(success ~ num_practices,
                    family = binomial(link="logit"),
                    data = int_practice_data)
summary(int_practice2)
anova(int_practice2,test="Chisq")
pR2(int_practice2)  # measures of how much the logistic model explains

#### END ####