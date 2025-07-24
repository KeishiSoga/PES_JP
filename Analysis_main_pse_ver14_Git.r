# Remove all variables
rm(list = ls())

# Set working directory
setwd("/Users/sogakeishi/Desktop/TEMPA/webtest_main/main_data")


library(ggplot2)
library(QuantPsyc)
library(readr)
library(tidyr)
library(forcats)
library(readxl)
library(pwr)
library(psych)
library(interactions)
library(plyr)
library(tidyverse)
library(Hmisc)
library(factoextra)
library(Hmisc)
library(xtable)
library(psy)
library(car)
library(plyr)
library(FactoMineR)
library(lattice)
library(rmarkdown)
library(questionr)
library(lm.beta)
library(knitr)
library(ggplot2)
library(gghalves)
library(htmltools)
library(margins)
library(lme4)
library(lmerTest)
library(lmtest)
library(openxlsx)
library(dplyr)
library(lavaan)
library(semPlot)
library(lavaanPlot)
library(lavaangui)
library(pscl)  # Package for zeroinfl function
library(MASS)  # For Box-Cox transformation
library(olsrr)
library(Superpower)
library(devtools)
library(grmsem)
library(ltm)
library(hrbrthemes)


# Read CSV file
pes <- read.csv("ver2_main_data_5_21.csv", header = TRUE, stringsAsFactors = FALSE)

# Check the first few rows of data
head(pes)


# Convert each column to factor
pes$sc0 <- factor(pes$sc0)
pes$sc1 <- factor(pes$sc1)
pes$sc3 <- factor(pes$sc3)

# Rename variables
names(pes)[names(pes) == "sc1"] <- "sex"
names(pes)[names(pes) == "sc2_1"] <- "Age_num"

# Extract participants with problematic data (non-8 values with 0 entered)
problematic_rows <- pes %>%
  filter(
    (q2_a1 %in% 1:7 & q2_1b_1 == 0 & q2_1b_2 == 0) |
      (q2_a2 %in% 1:7 & q2_2b_1 == 0 & q2_2b_2 == 0) |
      (q2_a3 %in% 1:7 & q2_3b_1 == 0 & q2_3b_2 == 0)
  ) %>%
  dplyr::select(no)

print(problematic_rows)



#################################################################
# IPAQ
### FOR LEISURE TIME ###  

# Calculate vigorous physical activity time and replace NA with 0 (calculated as days times METs)
pes$q2_a1[pes$q2_a1 == 8] <- 0
pes <- mutate(pes, TL_Vig_AP = coalesce(q2_1b_1, 0) * 60 + coalesce(q2_1b_2, 0))
pes$vigorous_PA <- 8 * pes$TL_Vig_AP * pes$q2_a1
nrow(pes[pes$q2_a1== 0, ])
mean(pes$vigorous_PA )
# Calculate moderate physical activity time and replace NA with 0
pes$q2_a2[pes$q2_a2 == 8] <- 0
pes <- mutate(pes, TL_Mod_AP = coalesce(q2_2b_1, 0) * 60 + coalesce(q2_2b_2, 0))
pes$moderate_PA <- 4 * pes$TL_Mod_AP * pes$q2_a2
nrow(pes[pes$q2_a2== 0, ])
mean(pes$moderate_PA)
# Calculate walking time and replace NA with 0 (Note: original variable name TL_Marche)
pes$q2_a3[pes$q2_a3 == 8] <- 0
pes <- mutate(pes, TL_Walking = coalesce(q2_3b_1, 0) * 60 + coalesce(q2_3b_2, 0))
pes$walking <- 3.3 * pes$TL_Walking * pes$q2_a3

# Calculate sitting time and replace NA with 0
pes <- mutate(pes, sitting = coalesce(q2_4_1, 0) * 60 + coalesce(q2_4_2, 0))


# Calculate total moderate-to-vigorous physical activity time (including walking)
pes <- mutate(pes, mod_vig_PA = moderate_PA + vigorous_PA + walking)
# Mean
mean(pes$mod_vig_PA)

# Standard deviation
sd(pes$mod_vig_PA)


# Histogram of physical activity time (using ggplot2)
# Calculate number of people with 0 values
n_zero_vig <- sum(pes$vigorous_PA == 0)
n_zero_mod <- sum(pes$moderate_PA == 0)
n_zero_walking <- sum(pes$walking == 0)
n_zero_mvpa <- sum(pes$mod_vig_PA == 0)
n_zero_mvpa
# Histogram of vigorous physical activity time
p1 <- ggplot(pes, aes(x = vigorous_PA)) +
  geom_histogram(bins = 20, fill = "lightblue", color = "black") +
  labs(title = paste("Vigorous Physical Activity Time\n(Zero values:", n_zero_vig, "people)"),
       x = "Min-METs",
       y = "Frequency") +
  theme_minimal()

# Histogram of moderate physical activity time
p2 <- ggplot(pes, aes(x = moderate_PA)) +
  geom_histogram(bins = 20, fill = "lightgreen", color = "black") +
  labs(title = paste("Moderate Physical Activity Time\n(Zero values:", n_zero_mod, "people)"),
       x = "Min-METs",
       y = "Frequency") +
  theme_minimal()

# Histogram of walking time
p3 <- ggplot(pes, aes(x = walking)) +
  geom_histogram(bins = 20, fill = "lightpink", color = "black") +
  labs(title = paste("Walking Time\n(Zero values:", n_zero_walking, "people)"),
       x = "Min-METs",
       y = "Frequency") +
  theme_minimal()

# Histogram of total MVPA time
p4 <- ggplot(pes, aes(x = mod_vig_PA)) +
  geom_histogram(bins = 20, fill = "lightyellow", color = "black") +
  labs(title = paste("Total MVPA Time\n(Zero values:", n_zero_mvpa, "people)"),
       x = "Min-METs",
       y = "Frequency") +
  theme_minimal()

# Display graphs in 2x2 layout
library(gridExtra)
library(showtext)
library(sysfonts)
library(showtextdb)
# Add Google font (Noto Sans JP)
font_add_google("Noto Sans JP", family = "jp")

# Enable showtext
showtext_auto()
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

# Display descriptive statistics
cat("\nDescriptive statistics for vigorous physical activity time:\n")
summary(pes$vigorous_PA)
cat("\nDescriptive statistics for moderate physical activity time:\n")
summary(pes$moderate_PA)
cat("\nDescriptive statistics for walking time:\n")
summary(pes$walking)
cat("\nDescriptive statistics for total MVPA time:\n")
summary(pes$mod_vig_PA)
nrow(pes[pes$vigorous_PA == 0, ])
nrow(pes[pes$moderate_PA == 0, ])
nrow(pes[pes$mod_vig_PA == 0, ])


pes <- pes %>% 
  rowwise() %>% 
  mutate(
    # Weekly total MET
    met_total = mod_vig_PA,
    
    # Classification flags
    high_flag = (q2_a1 >= 3 & met_total >= 1500) |
      ((q2_a1 + q2_a2 + q2_a3) >= 7 & met_total >= 3000),
    
    moderate_flag = (q2_a1 >= 3 & TL_Vig_AP  >= 20) |
      (q2_a2 >= 5 & TL_Mod_AP >= 30) |
      (q2_a3 >= 5 & TL_Walking>= 30) |
      ((q2_a1 + q2_a2 + q2_a3) >= 5 & met_total >= 600),
    
    PA_category = case_when(
      high_flag ~ "High",
      moderate_flag ~ "Moderate",
      TRUE ~ "Low"
    )
  ) %>% 
  ungroup()

table(pes$PA_category)


##################################################################
# IntentionPA
pes$IntentionPA <- rowMeans(pes[, c("q3_1", "q3_2")], na.rm=T)

intention <- pes %>% dplyr::select("q3_1", "q3_2")

cronbach(intention)
################################################################
## INSTRUMENTAL ATTITUDES

pes$att_instru <- rowMeans(pes[, c("q4_1_1", "q4_1_2", "q4_1_3")], na.rm=T)
hist(pes$att_instru)

attitudes_instrumental <- pes %>% dplyr::select("q4_1_1", "q4_1_2", "q4_1_3")
cronbach(attitudes_instrumental)
# Affective attitudes

pes$att_aff <- rowMeans(pes[, c("q4_2_1", "q4_2_2", "q4_2_3")], na.rm=T)
hist(pes$att_aff)

attitudes_affective <- pes %>% dplyr::select("q4_2_1", "q4_2_2", "q4_2_3")
cronbach(attitudes_affective)
##################################################################
# Motivations toward physical activity

# Item 1: q5_1 - Intrinsic motivation
# Item 3: q5_2 - Integrated regulation
# Item 5: q5_3 - Identified regulation
# Item 7: q5_4 - Introjected regulation
# Item 9: q5_5 - External regulation
# Item 11: q5_6 - Amotivation
# Item 2: q5_7 - Intrinsic motivation
# Item 4: q5_8 - Integrated regulation
# Item 6: q5_9 - Identified regulation
# Item 8: q5_10 - Introjected regulation
# Item 10: q5_11 - External regulation
# Item 12: q5_12 - Amotivation

# AUTONOMOUS MOTIVATION (3-dimension)
# pre_tempa$Autonomous_Motivation <- rowMeans(pre_tempa[, c("q5_1", "q5_7","q5_3","q5_9")], na.rm=T)
pes$Autonomous_Motivation <- rowMeans(pes[, c("q5_1", "q5_7","q5_2","q5_8","q5_3","q5_9")], na.rm=T)
hist(pes$Autonomous_Motivation)
# autonomous_2<-pre_tempa %>% dplyr::select("q5_1", "q5_7","q5_3","q5_9")
# cronbach(autonomous_2)
autonomous_3<-pes %>% dplyr::select("q5_1", "q5_7","q5_2","q5_8","q5_3","q5_9")
cronbach(autonomous_3)

# CONTROLLED MOTIVATION 
pes$Controlled_Motivation <- rowMeans(pes[, c("q5_4", "q5_10","q5_5","q5_11")], na.rm=T)
controlled <- pes %>% dplyr::select("q5_4", "q5_10","q5_5","q5_11")
cronbach(controlled)

# AMOTIVATION
amotivation <- pes %>% dplyr::select("q5_6","q5_12")
cronbach(amotivation)
##################################################################
# Automaticity
pes$habits <- rowMeans(pes[, c("q6_1", "q6_2", "q6_3", "q6_4")], na.rm=T)
hist(pes$habits)
automaticity_c <- pes %>% dplyr::select("q6_1", "q6_2", "q6_3", "q6_4")
cronbach(automaticity_c)
##################################################################


# Variable name conversion
names(pes)[names(pes) == "q1_1"] <- "effort_approach_1"
names(pes)[names(pes) == "q1_3"] <- "effort_approach_2"
names(pes)[names(pes) == "q1_6"] <- "effort_approach_3"
names(pes)[names(pes) == "q1_8"] <- "effort_approach_4"
names(pes)[names(pes) == "q1_2"] <- "effort_avoid_1"
names(pes)[names(pes) == "q1_4"] <- "effort_avoid_2"
names(pes)[names(pes) == "q1_5"] <- "effort_avoid_3"
names(pes)[names(pes) == "q1_7"] <- "effort_avoid_4"
head(pes)



pes_efa <- pes %>% dplyr::select(contains("effort_"))
pes_efa <- pes_efa %>% mutate_if(is.character, as.numeric)
dial.pca <- princomp(as.matrix(pes_efa))
summary(dial.pca)

length(dial.pca$sdev) 
# Individual contribution rate of the first 2 components
(dial.pca$sdev^2 / sum(dial.pca$sdev^2) * 100)[1:2]

# Cumulative contribution rate of the first 2 components
cumsum(dial.pca$sdev^2 / sum(dial.pca$sdev^2) * 100)[2]

screeplot(dial.pca,npcs=15) # npcs is the number of components to plot, should be more than n dimensions expected

dial.efa_2F <- factanal(as.matrix(pes_efa),factors=2,
                     cor=T,rotation="promax")
print(dial.efa_2F)

model_cfa1 <-'
approach =~ effort_approach_1 + effort_approach_2 + effort_approach_3 + effort_approach_4
avoidance =~ effort_avoid_1 + effort_avoid_2 + effort_avoid_3 + effort_avoid_4

approach ~~ avoidance
'

fit<- lavaan::sem(model_cfa1,data=pes_efa,orthogonal = TRUE)

summary(fit,fit.measures=TRUE,standardized=TRUE,rsquare=TRUE, ci = .95)

# plot_lavaan(fit)
         
modindices(fit, sort = TRUE, maximum.number = 5)
# CRONBACH'S ALPHA
pes_approach <- pes_efa %>% dplyr::select("effort_approach_1", "effort_approach_2", "effort_approach_3", "effort_approach_4")
cronbach(pes_approach )
pes_avoidance <- pes_efa %>% dplyr::select("effort_avoid_1", "effort_avoid_2", "effort_avoid_3", "effort_avoid_4")
cronbach(pes_avoidance)


## CORRELATIONS BETWEEN APPROACH AND AVOIDANCE DIMENSIONS
library(gghalves)



pes <- pes %>% mutate_at(c("effort_approach_1", "effort_approach_2", "effort_approach_3", "effort_approach_4", 
                           "effort_avoid_1", "effort_avoid_2", "effort_avoid_3", "effort_avoid_4"), as.numeric)


pes <- mutate(pes, pes_approach = (effort_approach_1 + effort_approach_2 + effort_approach_3 + effort_approach_4 )/4)

pes <- mutate(pes, pes_avoid = (effort_avoid_1 + effort_avoid_2 + effort_avoid_3 + effort_avoid_4)/4)



# Mean and standard deviation of PES-JP approach score
mean(pes$pes_approach)
sd(pes$pes_approach)

# Mean and standard deviation of PES-JP avoidance score
mean(pes$pes_avoid)
sd(pes$pes_avoid)

## APPROACH 
mean(pes$pes_approach)
sd(pes$pes_approach)
min(pes$pes_approach)
max(pes$pes_approach)
hist(pes$pes_approach)


## AVOIDANCE
mean(pes$pes_avoid)
sd(pes$pes_avoid)
min(pes$pes_avoid)
max(pes$pes_avoid)
hist(pes$pes_avoid)



cor.test(pes$pes_approach, pes$pes_avoid)


# Calculate global score
pes <- mutate(pes, PES_Global = pes_approach - pes_avoid)
hist(pes$PES_Global)

# Calculate descriptive statistics
mean(pes$PES_Global)
sd(pes$PES_Global)
min(pes$PES_Global)
max(pes$PES_Global)


##########################################################
## CORRELATIONS
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  # Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coefficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  ## Truncate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## Build a new matrix that includes the correlations with their appropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## Remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## Remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## Remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
}

##########################################################

correlation <- pes[, c("pes_approach","pes_avoid", "PES_Global",
                       "IntentionPA",
                       "Autonomous_Motivation",
                       "att_aff",
                       "habits",
                       "Controlled_Motivation", 
                       "att_instru")
]

names(pes)
correlation <- rename.variable(correlation, "pes_approach","PES-Approach")
correlation <- rename.variable(correlation, "pes_avoid","PES-Avoidance")
correlation <- rename.variable(correlation, "IntentionPA","Intention")
correlation <- rename.variable(correlation, "Autonomous_Motivation","Autonomous motivation")
correlation <- rename.variable(correlation, "att_aff","Affective attitudes")
correlation <- rename.variable(correlation, "habits","Automaticity")
correlation <- rename.variable(correlation, "Controlled_Motivation","Controlled motivation")
correlation <- rename.variable(correlation, "att_instru","Instrumental attitudes")




# PLOT
library(corrplot)

testRes = cor.mtest(correlation, conf.level = 0.95)
M = cor(correlation)



## PLOT WITH COEFFICIENTS

corrplot(M, p.mat = testRes$p, method = 'circle', type = 'upper', 
         addCoef.col ='black', number.cex = 0.6, diag=FALSE)



corrplot(M, p.mat = testRes$p, method = 'circle', type = 'lower', insig='blank',
         diag = FALSE)$corrPos -> p1
text(p1$x, p1$y, round(p1$corr, 2))




correlation <- pes[, c("PES_Global",
                       "IntentionPA",
                       "Autonomous_Motivation",
                       "att_aff",
                       "habits", 
                       "Controlled_Motivation", 
                       "att_instru")
]



## WITH THE GLOBAL SCORE


## SELECT THE VARIABLES OF INTEREST and compute the table ##
a <- corstars(correlation[,1:7])
a

# Correlation coefficients and significance tests
cor.test(pes$pes_approach, pes$pes_avoid)
cor.test(pes$pes_approach, pes$IntentionPA)
cor.test(pes$pes_approach, pes$Autonomous_Motivation)
cor.test(pes$pes_approach, pes$att_aff)
cor.test(pes$pes_approach, pes$habits)

cor.test(pes$pes_avoid, pes$IntentionPA)
cor.test(pes$pes_avoid, pes$Autonomous_Motivation)
cor.test(pes$pes_avoid, pes$att_aff)
cor.test(pes$pes_avoid, pes$habits)

cor.test(pes$pes_approach, pes$Controlled_Motivation)
cor.test(pes$pes_approach, pes$att_instru)
cor.test(pes$pes_avoid, pes$Controlled_Motivation)
cor.test(pes$pes_avoid, pes$att_instru)


#############################################################
# pes <- mutate(pes, mod_vig_PA = moderate_PA + vigorous_PA + walking)
# Concurrent validity with physical activity
# FOR THE APPROACH DIMENSION 

mod_vig_PA_app <- lm(formula = scale(mod_vig_PA) ~ scale(pes_approach), data = pes)
summary(mod_vig_PA_app)
confint(mod_vig_PA_app)

mod_PA_app <- lm(formula = scale(moderate_PA) ~ scale(pes_approach), data = pes)
summary(mod_PA_app)
confint(mod_PA_app)

vig_PA_app <- lm(formula = scale(vigorous_PA) ~ scale(pes_approach), data = pes)
summary(vig_PA_app)
confint(vig_PA_app)


walking_PA_app <- lm(formula = scale(walking) ~ scale(pes_approach), data = pes)
summary(walking_PA_app)
confint(walking_PA_app)


sitting_PA_app <- lm(formula = scale(sitting) ~ scale(pes_approach), data = pes)
summary(sitting_PA_app)
confint(sitting_PA_app)

library(sjPlot)

# 全てのモデルを一つの表にまとめて表示
tab_model(
  mod_vig_PA_app,
  mod_PA_app,
  vig_PA_app,
  walking_PA_app,
  sitting_PA_app
)



# Convergent validity
autonomous_app <- lm(formula = scale(Autonomous_Motivation) ~ scale(pes_approach), data = pes)
summary(autonomous_app)

affect_att_app <- lm(formula = scale(att_aff) ~ scale(pes_approach), data = pes)
summary(affect_att_app)

intention_app <- lm(formula = scale(IntentionPA) ~ scale(pes_approach), data = pes)
summary(intention_app)


# Display all models in a single table
tab_model(
  autonomous_app,
  affect_att_app,
  intention_app
)



# Automaticity
automaticity_app <- lm(formula = scale(habits) ~ scale(pes_approach), data = pes)
summary(automaticity_app)


# Discriminant validity
controlled_app <- lm(formula = scale(Controlled_Motivation) ~ scale(pes_approach), data = pes)
summary(controlled_app)

instrumental_attitudes_app <- lm(formula = scale(att_instru) ~ scale(pes_approach), data = pes)
summary(instrumental_attitudes_app)

tab_model(
  controlled_app,
  instrumental_attitudes_app
)




#############################################################
# Concurrent validity with physical activity
# FOR THE AVOIDANCE DIMENSION 
mod_vig_PA_avoid <- lm(formula = scale(mod_vig_PA) ~ scale(pes_avoid), data = pes)
summary(mod_vig_PA_avoid)
confint(mod_vig_PA_avoid)


mod_PA_avoid <- lm(formula = scale(moderate_PA) ~ scale(pes_avoid), data = pes)
summary(mod_PA_avoid)
confint(mod_PA_avoid)



vig_PA_avoid <- lm(formula = scale(vigorous_PA) ~ scale(pes_avoid), data = pes)
summary(vig_PA_avoid)
confint(vig_PA_avoid)


walking_PA_avoid <- lm(formula = scale(walking) ~ scale(pes_avoid), data = pes)
summary(walking_PA_avoid)
confint(walking_PA_avoid)


sitting_PA_avoid <- lm(formula = scale(sitting) ~ scale(pes_avoid), data = pes)
summary(sitting_PA_avoid)
confint(sitting_PA_avoid)



# Display all models in a single table
tab_model(
    mod_vig_PA_avoid,
    mod_PA_avoid,
    vig_PA_avoid,
    walking_PA_avoid,
    sitting_PA_avoid
)


# Convergent validity
autonomous_avoid <- lm(formula = scale(Autonomous_Motivation) ~ scale(pes_avoid), data = pes)
summary(autonomous_avoid)

affect_att_avoid <- lm(formula = scale(att_aff) ~ scale(pes_avoid), data = pes)
summary(affect_att_avoid)

intention_avoid <- lm(formula = scale(IntentionPA) ~ scale(pes_avoid), data = pes)
summary(intention_avoid)


# Display all models in a single table
tab_model(
  autonomous_avoid,
  affect_att_avoid,
  intention_avoid
)





# Automaticity
automaticity_avoid <- lm(formula = scale(habits) ~ scale(pes_avoid), data = pes)
summary(automaticity_avoid)


# Discriminant validity
controlled_avoid <- lm(formula = scale(Controlled_Motivation) ~ scale(pes_avoid), data = pes)
summary(controlled_avoid)

instrumental_attitudes_avoid <- lm(formula = scale(att_instru) ~ scale(pes_avoid), data = pes)
summary(instrumental_attitudes_avoid)


# Display all models in a single table
tab_model(
  controlled_avoid,
  instrumental_attitudes_avoid
)




# Display all models in a single table (Automaticity)
tab_model(
  automaticity_app,
  automaticity_avoid
)



## FOR THE GLOBAL SCORE
mod_vig_PA_glob <- lm(formula = scale(mod_vig_PA) ~ scale(PES_Global), data = pes)
summary(mod_vig_PA_glob)
confint(mod_vig_PA_glob)

mod_PA_glob <- lm(formula = scale(moderate_PA) ~ scale(PES_Global), data = pes)
summary(mod_PA_glob)
confint(mod_PA_glob)

vig_PA_glob <- lm(formula = scale(vigorous_PA) ~ scale(PES_Global), data = pes)
summary(vig_PA_glob)
confint(vig_PA_glob)


walking_PA_glob <- lm(formula = scale(walking) ~ scale(PES_Global), data = pes)
summary(walking_PA_glob)
confint(walking_PA_glob)


sitting_PA_glob <- lm(formula = scale(sitting) ~ scale(PES_Global), data = pes)
summary(sitting_PA_glob)
confint(sitting_PA_glob)

# Display all models in a single table
tab_model(
  mod_vig_PA_glob,
  mod_PA_glob,
  vig_PA_glob,
  walking_PA_glob,
  sitting_PA_glob
)

# Convergent validity
autonomous_glob <- lm(formula = scale(Autonomous_Motivation) ~ scale(PES_Global), data = pes)
summary(autonomous_glob)

affect_att_glob <- lm(formula = scale(att_aff) ~ scale(PES_Global), data = pes)
summary(affect_att_glob)

intention_glob <- lm(formula = scale(IntentionPA) ~ scale(PES_Global), data = pes)
summary(intention_avoid)


# Display all models in a single table
tab_model(
  autonomous_glob,
  affect_att_glob,
  intention_glob
)


# Discriminant validity
controlled_glob <- lm(formula = scale(Controlled_Motivation) ~ scale(PES_Global), data = pes)
summary(controlled_glob)

instrumental_attitudes_glob <- lm(formula = scale(att_instru) ~ scale(PES_Global), data = pes)
summary(instrumental_attitudes_glob)


# Display all models in a single table
tab_model(
  controlled_glob,
  instrumental_attitudes_glob
)


# Automaticity
automaticity_glob <- lm(formula = scale(habits) ~ scale(PES_Global), data = pes)
summary(automaticity_glob)


# Display all models in a single table (Automaticity)
tab_model(
  automaticity_glob
)

########################################################################
# Considering age and sex

# Models considering age and sex
# Set female (2) as reference
pes$sex <- factor(pes$sex, levels = c(2, 1), labels = c("Female", "Male"))
# PES Approach analysis (age and sex adjusted models)
# Convergent validity
autonomous_app_2 <- lm(formula = scale(Autonomous_Motivation) ~ scale(Age_num) + sex + scale(pes_approach), data = pes)
summary(autonomous_app_2)

affect_att_app_2 <- lm(formula = scale(att_aff) ~ scale(Age_num) + sex + scale(pes_approach), data = pes)
summary(affect_att_app_2)

intention_app_2 <- lm(formula = scale(IntentionPA) ~ scale(Age_num) + sex + scale(pes_approach), data = pes)
summary(intention_app_2)

# Display all models in a single table
tab_model(
  autonomous_app_2,
  affect_att_app_2,
  intention_app_2
)



# Automaticity
automaticity_app_2 <- lm(formula = scale(habits) ~ scale(Age_num) + sex + scale(pes_approach), data = pes)
summary(automaticity_app_2)

# Discriminant validity
controlled_app_2 <- lm(formula = scale(Controlled_Motivation) ~ scale(Age_num) + sex + scale(pes_approach), data = pes)
summary(controlled_app_2)

instrumental_attitudes_app_2 <- lm(formula = scale(att_instru) ~ scale(Age_num) + sex + scale(pes_approach), data = pes)
summary(instrumental_attitudes_app_2)

tab_model(
  controlled_app_2,
  instrumental_attitudes_app_2
)


# PES Avoidance analysis (age and sex adjusted models)
# Convergent validity
autonomous_avoid_2 <- lm(formula = scale(Autonomous_Motivation) ~ scale(Age_num) + sex + scale(pes_avoid), data = pes)
summary(autonomous_avoid_2)

affect_att_avoid_2 <- lm(formula = scale(att_aff) ~ scale(Age_num) + sex + scale(pes_avoid), data = pes)
summary(affect_att_avoid_2)

intention_avoid_2 <- lm(formula = scale(IntentionPA) ~ scale(Age_num) + sex + scale(pes_avoid), data = pes)
summary(intention_avoid_2)

# Display all models in a single table
tab_model(
  autonomous_avoid_2,
  affect_att_avoid_2,
  intention_avoid_2
)



# Automaticity
automaticity_avoid_2 <- lm(formula = scale(habits) ~ scale(Age_num) + sex + scale(pes_avoid), data = pes)
summary(automaticity_avoid_2)

# Discriminant validity
controlled_avoid_2 <- lm(formula = scale(Controlled_Motivation) ~ scale(Age_num) + sex + scale(pes_avoid), data = pes)
summary(controlled_avoid_2)

instrumental_attitudes_avoid_2 <- lm(formula = scale(att_instru) ~ scale(Age_num) + sex + scale(pes_avoid), data = pes)
summary(instrumental_attitudes_avoid_2)

# Display all models in a single table
tab_model(
  controlled_avoid_2,
  instrumental_attitudes_avoid_2
)



# Display all models in a single table (Automaticity)
tab_model(
  automaticity_app_2,
  automaticity_avoid_2
)



# Supplementary: Overall comparison table (optional)
# For comparing main results of PES Approach and Avoid
tab_model(
  autonomous_app_2, autonomous_avoid_2,
  affect_att_app_2, affect_att_avoid_2,
  intention_app_2, intention_avoid_2,
  dv.labels = c("Autonomous (App)", "Autonomous (Avoid)", 
                "Affective Att (App)", "Affective Att (Avoid)",
                "Intention (App)", "Intention (Avoid)")
)
### MULTIPLE LINEAR REGRESSIONS


# Sex
table(pes$sex)
# Age
summary(pes$Age_num)
hist(pes$Age_num)
# Mean
mean(pes$Age_num)

# Standard deviation
sd(pes$Age_num)


# MVPA
# Baseline
hlm_mvpa_baseline <- lm(formula = scale(mod_vig_PA) ~ scale(Age_num) + 
                              sex,
#                             scale(IntentionPA)
#                            + scale(att_instru),
                            data = pes)
summary(hlm_mvpa_baseline)
confint(hlm_mvpa_baseline)

# Approach physical effort only
hlm_obtain_sample <- lm(formula = scale(mod_vig_PA) ~ scale(Age_num) + 
                                       sex+
#                                      scale(IntentionPA) +
#                                       scale(att_instru)+
                                       scale(pes_approach), data = pes)
summary(hlm_obtain_sample)
confint(hlm_obtain_sample)

# Avoid physical effort only
hlm_mvpa_avoid_only <- lm(formula = scale(mod_vig_PA) ~ scale(Age_num) + 
                             sex+  
#                            scale(IntentionPA) +
#                            scale(att_instru)+
                            scale(pes_avoid), data = pes)
summary(hlm_mvpa_avoid_only)
confint(hlm_mvpa_avoid_only)

tab_model(
  hlm_mvpa_baseline,
  hlm_obtain_sample,
  hlm_mvpa_avoid_only
)

# PES global
hlm_STEP2_mvpa_glob <- lm(formula = scale(mod_vig_PA) ~ scale(Age_num) + 
                                    sex+
#                                   scale(IntentionPA) +
#                                    scale(att_instru)+
                                   scale(PES_Global), data = pes)
summary(hlm_STEP2_mvpa_glob)
confint(hlm_STEP2_mvpa_glob)


# Both
hlm_mvpa_both <- lm(formula = scale(mod_vig_PA) ~ scale(Age_num) + 
                      sex + 
#                      scale(IntentionPA) +
#                      scale(att_instru)+
                      scale(pes_approach)+
                      scale(pes_avoid),data = pes)
summary(hlm_mvpa_both)

tab_model(
  hlm_mvpa_baseline,
  hlm_obtain_sample,
  hlm_mvpa_avoid_only,
  hlm_STEP2_mvpa_glob
)
################################################################################
# Sitting
# Baseline
hlm_sitting_baseline <- lm(formula = scale(sitting) ~ scale(Age_num) + 
                             sex, 
#                             scale(IntentionPA) +
#                             scale(att_instru), 
                              data = pes)
summary(hlm_sitting_baseline)

# Approach physical effort only
hlm_obtain_sitting_sample <- lm(formula = scale(sitting) ~ scale(Age_num) + 
                                  sex + 
#                                  scale(IntentionPA) +
#                                  scale(att_instru)  + 
                                  scale(pes_approach), data = pes)
summary(hlm_obtain_sitting_sample)

# Avoid physical effort only
hlm_sitting_avoid_only <- lm(formula = scale(sitting) ~ scale(Age_num) + 
                          sex + 
#                          scale(IntentionPA) +
#                          scale(att_instru)  + 
                          scale(pes_avoid), data = pes)
summary(hlm_sitting_avoid_only)

# Global
hlm_sitting_global <- lm(formula = scale(sitting) ~ scale(Age_num) + 
                         sex + 
                         #                          scale(IntentionPA)  +
                         #                          scale(att_instru)   + 
                         scale(PES_Global), data = pes)
summary(hlm_sitting_global)


# Both
hlm_sitting_both <- lm(formula = scale(sitting) ~ scale(Age_num) + 
                          sex + 
#                          scale(IntentionPA)  +
#                          scale(att_instru)   + 
                          scale(pes_approach) +
                          scale(pes_avoid), data = pes)
summary(hlm_sitting_both)


tab_model(
  hlm_sitting_baseline,
  hlm_obtain_sitting_sample,
  hlm_sitting_avoid_only,
  hlm_sitting_global
)

library(sjPlot)
library(sjmisc)




tab_model(hlm_mvpa_baseline)
tab_model(hlm_obtain_sample)
tab_model(hlm_mvpa_avoid_only)
tab_model(hlm_STEP2_mvpa_glob)
tab_model(hlm_mvpa_both)
tab_model(hlm_sitting_baseline)
tab_model(hlm_obtain_sitting_sample)
tab_model(hlm_sitting_avoid_only)
tab_model(hlm_sitting_both)

#########################################################################
# Second data analysis
# Extract necessary data

# Variable name conversion
names(pes)[names(pes) == "sq1_1"] <- "effort_approach_1_T2"
names(pes)[names(pes) == "sq1_3"] <- "effort_approach_2_T2"
names(pes)[names(pes) == "sq1_6"] <- "effort_approach_3_T2"
names(pes)[names(pes) == "sq1_8"] <- "effort_approach_4_T2"
names(pes)[names(pes) == "sq1_2"] <- "effort_avoid_1_T2"
names(pes)[names(pes) == "sq1_4"] <- "effort_avoid_2_T2"
names(pes)[names(pes) == "sq1_5"] <- "effort_avoid_3_T2"
names(pes)[names(pes) == "sq1_7"] <- "effort_avoid_4_T2"
head(pes)

# pes_efa <- pes %>% dplyr::select(contains("effort_"))
# pes_efa <- pes_efa %>% mutate_if(is.character, as.numeric)

pes <- mutate(pes, pes_approach_T2 = (effort_approach_1_T2 + effort_approach_2_T2 + effort_approach_3_T2 + effort_approach_4_T2 )/4)
pes <- mutate(pes, pes_avoid_T2 = (effort_avoid_1_T2 + effort_avoid_2_T2 + effort_avoid_3_T2 + effort_avoid_4_T2)/4)
pes <- mutate(pes, PES_Global_T2 = pes_approach_T2 -  pes_avoid_T2)

pes_clean <- subset(pes, 
                    select = c("no", "sex", "Age_num", 
                               "effort_approach_1", "effort_approach_2", 
                               "effort_approach_3", "effort_approach_4", 
                               "effort_avoid_1", "effort_avoid_2", 
                               "effort_avoid_3","effort_avoid_4",
                               "pes_approach", "pes_avoid", "PES_Global",
                               "effort_approach_1_T2", "effort_approach_2_T2", 
                               "effort_approach_3_T2", "effort_approach_4_T2", 
                               "effort_avoid_1_T2", "effort_avoid_2_T2", 
                               "effort_avoid_3_T2","effort_avoid_4_T2",
                               "pes_approach_T2", "pes_avoid_T2","PES_Global_T2"))
head(pes_clean)
# Remove missing values
pes_clean <- pes_clean[!(is.na(pes_clean$pes_approach_T2) & is.na(pes_clean$pes_avoid_T2)), ]
nrow(pes_clean)



# Mean age and standard deviation
mean(pes_clean$Age_num, na.rm = TRUE)
sd(pes_clean$Age_num, na.rm = TRUE)

# Sex distribution
table(pes_clean$sex)
prop.table(table(pes_clean$sex)) * 100  # Percentage

# Or only female percentage
sum(pes_clean$sex == 2, na.rm = TRUE) / nrow(pes_clean) * 100

library(irr)

# Kappa for each item
irr::kappa2(pes_clean[,c("effort_approach_1","effort_approach_1_T2")],weight="equal") # linear weight
irr::kappa2(pes_clean[,c("effort_approach_2","effort_approach_2_T2")],weight="equal") # linear weight
irr::kappa2(pes_clean[,c("effort_approach_3","effort_approach_3_T2")],weight="equal") # linear weight
irr::kappa2(pes_clean[,c("effort_approach_4","effort_approach_4_T2")],weight="equal") # linear weight
irr::kappa2(pes_clean[,c("effort_avoid_1","effort_avoid_1_T2")],weight="equal") # linear weight
irr::kappa2(pes_clean[,c("effort_avoid_2","effort_avoid_2_T2")],weight="equal") # linear weight
irr::kappa2(pes_clean[,c("effort_avoid_3","effort_avoid_3_T2")],weight="equal") # linear weight
irr::kappa2(pes_clean[,c("effort_avoid_4","effort_avoid_4_T2")],weight="equal") # linear weight



# ICC for the global dimension approach physical effort and avoid physical effort
irr::icc(pes_clean[,c("pes_approach","pes_approach_T2")],model="twoway",type="agreement") # linear weight
irr::icc(pes_clean[,c("pes_avoid","pes_avoid_T2")],model="twoway",type="agreement") # linear weight

### Global
irr::icc(pes_clean[,c("PES_Global","PES_Global_T2")],model="twoway",type="agreement") # linear weight


