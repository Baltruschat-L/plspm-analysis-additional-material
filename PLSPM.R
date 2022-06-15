# Resource https://www.gastonsanchez.com/PLS_Path_Modeling_with_R.pdf
# install "devtools"
install.packages("devtools") 
library(devtools)

# install "plspm"
install_github("gastonstat/plspm")

install.packages("xlsx")
library('xlsx')

# load plspm
load(plspm)
require(plspm)

# load data
data <- read.csv("/Users/lilly/Desktop/Studium/6th Semester/CAP3000 Capstone/Dataset/data.csv", header=TRUE, row.names =1)


Performance_Expectancy = c(0,0,0,0,0,0,0,0,0,0)
Social_Influence = c(0,0,0,0,0,0,0,0,0,0)
Self_Eficacy = c(0,0,0,0,0,0,0,0,0,0)
Incentives = c(0,0,0,0,0,0,0,0,0,0)
Transparency = c(0,0,0,0,0,0,0,0,0,0)
Perceived_Security = c(0,0,0,0,0,0,0,0,0,0)
Effort_Expectancy = c(0,0,1,0,0,0,0,0,0,0)
Facilitating_Conditions = c(0,0,0,1,0,0,0,0,0,0)
Perceived_Trust = c(0,0,0,1,1,1,0,0,0,0)
Behaviour_Intention = c(1,1,0,0,0,0,1,1,1,0)

plsmodel = rbind(Performance_Expectancy, Social_Influence,Self_Eficacy,Incentives,Transparency,Perceived_Security,
                Facilitating_Conditions,Effort_Expectancy,Perceived_Trust, Behaviour_Intention)

colnames(plsmodel) = rownames(plsmodel)

plsmodel

innerplot(plsmodel)

#BuildingOutermodel
plsmodel_blocks = list(c("Q10.Performance_1","Q10.Performance_2","Q10.Performance_3","Q10.Performance_4","Q10.Performance_5"),
                       c("Q12.Social_1","Q12.Social_2"),
                       c("Q6.Self.Efficacy_1","Q6.Self.Efficacy_2","Q6.Self.Efficacy_4","Q6.Self.Efficacy_5"),
                       c("Q4.Incentives_1","Q4.Incentives_2","Q4.Incentives_4"),
                       c("Q5.Transparency_1","Q5.Transparency_2","Q5.Transparency_4"),
                       c("Q7.Security_1","Q7.Security_2","Q7.Security_4"),
                       c("Q11.Effort_1","Q11.Effort_2","Q11.Effort_3"),
                       c("Q12.Facilitating_1","Q12.Facilitating_2"),
                       c("Q8.Turst_1","Q8.Turst_2","Q8.Turst_3"),
                       c("Q13.Behaviour.intent_1","Q13.Behaviour.intent_2","Q13.Behaviour.intent_3"))

pls_modes=rep("A",10)

pls_results=plspm(data, plsmodel,plsmodel_blocks,pls_modes, scheme ="centroid")

summary(pls_results)

pls_results$outer_model

pls_results$unidim

pls_results$crossloadings

pls_results$inner_model

pls_results$path_coefs

pls_results$inner_summary

# moderating effect of Gender
dataFM<-data[!(data$Gender=="4"),]
pls_Gender=plspm(dataFM, plsmodel,plsmodel_blocks,pls_modes, boot.val = TRUE)
str(dataFM$Gender)

dataFM$Gender[dataFM$Gender == "1"]<- "Male"
dataFM$Gender[dataFM$Gender == "2"]<- "Female"

Male = dataFM[dataFM$Gender == "Male", ]
pls_Male_results = plspm(Male, plsmodel, plsmodel_blocks, pls_modes)
Female = dataFM[dataFM$Gender == "Female", ]
pls_Female_reuslts = plspm(Female, plsmodel, plsmodel_blocks, pls_modes)

dataFM$Gender <- as.factor(dataFM$Gender)
group_comparison_results1 = plspm.groups(pls_results, dataFM$Gender, method = "bootstrap")

group_comparison_results1

# moderating effect of Age
data <- read.csv("/Users/lilly/Desktop/Studium/6th Semester/CAP3000 Capstone/Dataset/data.csv", header=TRUE, row.names =1)
str(data$Age)
data$Age[data$Age == "4"]<- "Young"
data$Age[data$Age == "10"|data$Age == "5"|data$Age == "6"|data$Age == "7"|data$Age == "8"|data$Age == "9"]<- "Old"
Young = data[data$Age == "Young",]
pls_Young_results = plspm(Young, plsmodel, plsmodel_blocks, pls_modes)
Old = data[data$Age == "Old",]
pls_Old_reuslts = plspm(Old, plsmodel, plsmodel_blocks, pls_modes)

data$Age <- as.factor(data$Age)
group_comparison_results2 = plspm.groups(pls_results, data$Age, method = "bootstrap")

group_comparison_results2

# Experience and Voluntariness moderating effect
# get the latent variable scores in data frame format
data <- read.csv("/Users/lilly/Desktop/Studium/6th Semester/CAP3000 Capstone/Dataset/data.csv", header=TRUE, row.names =1)

Experience= c(0,0,0,0,0,0,0,0,0,0,0,0)
Voluntariness = c(0,0,0,0,0,0,0,0,0,0,0,0)
Performance_Expectancy = c(0,0,0,0,0,0,0,0,0,0,0,0)
Social_Influence = c(0,0,0,0,0,0,0,0,0,0,0,0)
Self_Eficacy = c(0,0,0,0,0,0,0,0,0,0,0,0)
Incentives = c(0,0,0,0,0,0,0,0,0,0,0,0)
Transparency = c(0,0,0,0,0,0,0,0,0,0,0,0)
Perceived_Security = c(0,0,0,0,0,0,0,0,0,0,0,0)
Effort_Expectancy = c(0,0,0,0,1,0,0,0,0,0,0,0)
Facilitating_Conditions = c(0,0,0,0,0,1,0,0,0,0,0,0)
Perceived_Trust = c(0,0,0,0,0,1,1,1,0,0,0,0)
Behaviour_Intention = c(1,1,1,1,0,0,0,0,1,1,1,0)

plsmodel = rbind(Experience, Voluntariness, Performance_Expectancy, Social_Influence,Self_Eficacy,Incentives,Transparency,Perceived_Security,
                Effort_Expectancy,Facilitating_Conditions,Perceived_Trust, Behaviour_Intention)

colnames(plsmodel) = rownames(plsmodel)

plsmodel

innerplot(plsmodel)

#BuildingOutermodel
plsmodel_blocks = list(c("Q2.Technology_1","Q2.Technology_2","Q2.Technology_4","Q2.Technology_5"),
                       c("Q3.Voluntairness_1", "Q3.Voluntairness_3"),
                       c("Q10.Performance_1","Q10.Performance_2","Q10.Performance_3","Q10.Performance_4","Q10.Performance_5"),
                       c("Q12.Social_1","Q12.Social_2"),
                       c("Q6.Self.Efficacy_1","Q6.Self.Efficacy_2","Q6.Self.Efficacy_4","Q6.Self.Efficacy_5"),
                       c("Q4.Incentives_1","Q4.Incentives_2","Q4.Incentives_4"),
                       c("Q5.Transparency_1","Q5.Transparency_2","Q5.Transparency_4"),
                       c("Q7.Security_1","Q7.Security_2","Q7.Security_4"),
                       c("Q12.Facilitating_1","Q12.Facilitating_2"),
                       c("Q11.Effort_1","Q11.Effort_2","Q11.Effort_3"),
                       c("Q8.Turst_1","Q8.Turst_2","Q8.Turst_3"),
                       c("Q13.Behaviour.intent_1","Q13.Behaviour.intent_2","Q13.Behaviour.intent_3"))

pls_modes=rep("A",12)

pls_results=plspm(data, plsmodel,plsmodel_blocks,pls_modes)
Scores = as.data.frame(pls_results$scores)
# create the interaction term
Scores$InterExp_Per = Scores$Performance_Expectancy * Scores$Experience
Scores$InterExp_Soc = Scores$Social_Influence * Scores$Experience
Scores$InterExp_Fac = Scores$Facilitating_Conditions * Scores$Experience
Scores$InterExp_Eff = Scores$Effort_Expectancy * Scores$Experience
Scores$InterExp_Tru = Scores$Perceived_Trust * Scores$Experience

Scores$InterVol_Per = Scores$Performance_Expectancy * Scores$Voluntariness
Scores$InterVol_Soc = Scores$Social_Influence * Scores$Voluntariness
Scores$InterVol_Fac = Scores$Facilitating_Conditions * Scores$Voluntariness
Scores$InterVol_Eff = Scores$Effort_Expectancy * Scores$Voluntariness
Scores$InterVol_Tru = Scores$Perceived_Trust * Scores$Voluntariness

# regression analysis
reg = lm(Behaviour_Intention ~ Experience + Voluntariness + Performance_Expectancy +
           Social_Influence + Facilitating_Conditions +Effort_Expectancy + Perceived_Trust + 
           InterVol_Per + InterVol_Tru + InterVol_Eff + InterVol_Soc + InterVol_Fac +
           InterExp_Eff + InterExp_Per + InterExp_Fac + InterExp_Soc + InterExp_Tru, data = Scores)

reg$coefficients
summary(reg)
