# Reading in data
rm(list = ls())
library(dplyr)
library(ggplot2)
library(ggeasy)
library(ggdark)
library(viridis)
library(lme4)
library(rstan)
library(brms)
library(sjPlot) 
library(lattice) 
library(caret)

df_raw <- read.csv("C:\\Users\\deeks\\Documents\\MIDS\\IDS_702_Modeling_and_representation_of_data\\Final Project\\Gitdata\\IDS702_FinalProject_CrimesinSF\\20_intermediate_files\\sfpd_police_incidents_clean.csv", 
                   header=TRUE, sep=",", stringsAsFactors = FALSE, na.strings=c("NA","NaN", " ",""))
str(df_raw)
head(df_raw)

df_raw <- df_raw[!is.na(df_raw$IncCategory),]
# dropped 267 nulls

drop_cols <- c("DayOfMonth","WeekOfYear","End_Start_Month",
               "BusinessHour", "Year")
df_raw = df_raw[,!(names(df_raw) %in% drop_cols)]

# Checking nulls
sapply(df_raw, function(x) sum(is.na(x)))

# Converting to factors
factor_cols <- c("Resolution", "PoliceDistrict",
                 "Month", "Season","TimeOfDay",
                 "isWeekend","Holiday","IncCategory")
df_raw[factor_cols] <- lapply(df_raw[factor_cols], factor)
str(df_raw)

df_agg1 <- aggregate(list(countIncidents=df_raw$IncidntNum),
                          list(PoliceDistrict=df_raw$PoliceDistrict,
                               # Year=df_raw$Year,
                               Season = df_raw$Season,
                               Month=df_raw$Month,
                               # End_Start_Month = df_raw$End_Start_Month,
                               # WeekOfYear=df_raw$WeekOfYear,
                               # DayOfMonth=df_raw$DayOfMonth,
                               isWeekend=df_raw$isWeekend,
                               Holiday=df_raw$Holiday,
                               TimeOfDay=df_raw$TimeOfDay,
                               IncCategory=df_raw$IncCategory
                               ),length)
 
df_agg2 <- aggregate(list(Resolution_resp=df_raw$Resolution_resp),
                         list(PoliceDistrict=df_raw$PoliceDistrict,
                              Season = df_raw$Season,
                              # Year=df_raw$Year,
                              Month=df_raw$Month,
                              # End_Start_Month = df_raw$End_Start_Month,
                              # WeekOfYear=df_raw$WeekOfYear,
                              # DayOfMonth=df_raw$DayOfMonth,
                              isWeekend=df_raw$isWeekend,
                              Holiday=df_raw$Holiday,
                              TimeOfDay=df_raw$TimeOfDay,
                              IncCategory=df_raw$IncCategory
                              ),
                         sum)

merged_pd <- left_join(df_agg1, df_agg2, by = NULL, copy = FALSE)
merged_pd$resolution_rate <- merged_pd$Resolution_resp/merged_pd$countIncidents

# Checking resolution rates by Police Districts
ggplot(merged_pd,aes(x=PoliceDistrict, y=resolution_rate, fill=PoliceDistrict))+
  geom_boxplot() + #coord_flip() +
  # scale_fill_brewer(palette="Greens") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title="Resolution Rates for Police Districts",
  x="Police Districts",y="Resolution Rates") + 
  theme(legend.position="none") +
  # dark_theme_gray() +
  ggeasy::easy_center_title()

ggplot(merged_pd,aes(x=PoliceDistrict, y=countIncidents, fill=PoliceDistrict))+
  geom_boxplot() + #coord_flip() +
  # scale_fill_brewer(palette="Greens") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title="Count of Incidents for Police Districts",
       x="Police Districts",y="Count of Incidents") + 
  theme(legend.position="none") +
  # dark_theme_gray() +
  ggeasy::easy_center_title()

# ggplot(merged_pd,aes(x=Year, y=resolution_rate, fill=Year))+
#   geom_boxplot() + #coord_flip() +
#   # scale_fill_brewer(palette="Greens") +
#   scale_fill_viridis(discrete = TRUE) +
#   labs(title="Resolution Rates by Year",
#        x="Year",y="Resolution Rates") + 
#   theme(legend.position="none") +
#   # dark_theme_gray() +
#   ggeasy::easy_center_title()

ggplot(merged_pd,aes(x=Season, y=resolution_rate, fill=Season))+
  geom_boxplot() + #coord_flip() +
  # scale_fill_brewer(palette="Greens") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title="Resolution Rates by Season",
       x="Season",y="Resolution Rates") + 
  theme(legend.position="none") +
  # dark_theme_gray() +
  ggeasy::easy_center_title()

ggplot(merged_pd,aes(x=Month, y=resolution_rate, fill=Month))+
  geom_boxplot() + #coord_flip() +
  # scale_fill_brewer(palette="Greens") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title="Resolution Rates by Month",
       x="Month",y="Resolution Rates") + 
  theme(legend.position="none") +
  # dark_theme_gray() +
  ggeasy::easy_center_title()


ggplot(merged_pd,aes(x=isWeekend, y=resolution_rate, fill=isWeekend))+
  geom_boxplot() + #coord_flip() +
  # scale_fill_brewer(palette="Greens") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title="Resolution Rates by Weekend",
       x="Weekend",y="Resolution Rates") + 
  theme(legend.position="none") +
  # dark_theme_gray() +
  ggeasy::easy_center_title()

ggplot(merged_pd,aes(x=Holiday, y=resolution_rate, fill=Holiday))+
  geom_boxplot() + #coord_flip() +
  # scale_fill_brewer(palette="Greens") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title="Resolution Rates by Holiday",
       x="Holiday",y="Resolution Rates") + 
  theme(legend.position="none") +
  # dark_theme_gray() +
  ggeasy::easy_center_title()

ggplot(merged_pd,aes(x=TimeOfDay, y=resolution_rate, fill=TimeOfDay))+
  geom_boxplot() + #coord_flip() +
  # scale_fill_brewer(palette="Greens") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title="Resolution Rates by Time of Day",
       x="TIme of Day",y="Resolution Rates") + 
  theme(legend.position="none") +
  # dark_theme_gray() +
  ggeasy::easy_center_title()

ggplot(merged_pd,aes(x=IncCategory, y=resolution_rate, fill=IncCategory))+
  geom_boxplot() + #coord_flip() +
  # scale_fill_brewer(palette="Greens") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title="Resolution Rates by Category of Incident",
       x="Incident Category",y="Resolution Rates") + 
  theme(legend.position="none") +
  # dark_theme_gray() +
  ggeasy::easy_center_title()

# Interactions
ggplot(merged_pd,aes(x=Season, y=resolution_rate, fill=Season)) +
  geom_boxplot() + #coord_flip() +
  # scale_fill_brewer(palette="Reds") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title="Resolution rates in Police Districts by Season",
       x="Season",y="Resolution Rates") + 
  theme_classic() + theme(legend.position="none") +
  #scale_x_discrete(labels=c("0" = "No","1" = "Yes")) +
  facet_wrap( ~ PoliceDistrict, ncol=3) +
  ggeasy::easy_center_title()

ggplot(merged_pd,aes(x=Month, y=resolution_rate, fill=Month)) +
  geom_boxplot() + #coord_flip() +
  # scale_fill_brewer(palette="Reds") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title="Resolution rates in Police Districts by Month",
       x="Month",y="Resolution Rates") + 
  theme_classic() + theme(legend.position="none") +
  #scale_x_discrete(labels=c("0" = "No","1" = "Yes")) +
  facet_wrap( ~ PoliceDistrict, ncol=3) +
  ggeasy::easy_center_title()

ggplot(merged_pd,aes(x=isWeekend, y=resolution_rate, fill=isWeekend)) +
  geom_boxplot() + #coord_flip() +
  # scale_fill_brewer(palette="Reds") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title="Resolution rates in Police Districts by Weekend",
       x="Weekend",y="Resolution Rates") + 
  theme_classic() + theme(legend.position="none") +
  #scale_x_discrete(labels=c("0" = "No","1" = "Yes")) +
  facet_wrap( ~ PoliceDistrict, ncol=3) +
  ggeasy::easy_center_title()

ggplot(merged_pd,aes(x=Holiday, y=resolution_rate, fill=Holiday)) +
  geom_boxplot() + #coord_flip() +
  # scale_fill_brewer(palette="Reds") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title="Resolution rates in Police Districts by Holiday",
       x="Holiday",y="Resolution Rates") + 
  theme_classic() + theme(legend.position="none") +
  #scale_x_discrete(labels=c("0" = "No","1" = "Yes")) +
  facet_wrap( ~ PoliceDistrict, ncol=3) +
  ggeasy::easy_center_title()

ggplot(merged_pd,aes(x=TimeOfDay, y=resolution_rate, fill=TimeOfDay)) +
  geom_boxplot() + #coord_flip() +
  # scale_fill_brewer(palette="Reds") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title="Resolution rates in Police Districts by Time of Day",
       x="Time of Day",y="Resolution Rates") + 
  # theme_classic() + 
  dark_theme_gray() +
  theme(legend.position="none") +
  #scale_x_discrete(labels=c("0" = "No","1" = "Yes")) +
  facet_wrap( ~ PoliceDistrict, ncol=5) +
  ggeasy::easy_center_title()

ggplot(merged_pd,aes(x=IncCategory, y=resolution_rate, fill=IncCategory)) +
  geom_boxplot() + #coord_flip() +
  # scale_fill_brewer(palette="Reds") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title="Resolution rates in Police Districts by Incident Category",
       x="Incident Category",y="Resolution Rates") + 
  theme_classic() + theme(legend.position="none") +
  #scale_x_discrete(labels=c("0" = "No","1" = "Yes")) +
  facet_wrap( ~ PoliceDistrict, ncol=3) +
  ggeasy::easy_center_title()


# MODEL 1 - Fixed intercept only
start_time <- Sys.time()
model1 <- glmer(cbind(Resolution_resp, countIncidents-Resolution_resp) ~ 
                  Season + Month + isWeekend + Holiday +
                  TimeOfDay + IncCategory + (1 | PoliceDistrict),                         
                family=binomial(link="logit"), data=merged_pd
                # ,control=glmerControl(
                #   optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)
end_time <- Sys.time()
print(paste("The model training time:" , end_time - start_time))

summary(model1)
tab_model(model1)
dotplot(ranef(model1, condVar=TRUE))


# ANOVA for HOLIDAY
model1_minus_hol <- glmer(cbind(Resolution_resp, countIncidents-Resolution_resp) ~ 
                            Season + Month + isWeekend +
                            TimeOfDay + IncCategory + (1 | PoliceDistrict),                         
                          family=binomial(link="logit"), data=merged_pd
                          # ,control=glmerControl(
                          #   optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)
summary(model1_minus_hol)
# vif(model1_minus_hol)

anova(model1_minus_hol,model1,test="Chisq")
# p value is >0.05, fail to reject null hypothesis, which assumes all additional predictors are zero
# drop holiday
tab_model(model1_minus_hol)
dotplot(ranef(model1_minus_hol, condVar=TRUE))


model1_minus_hol_seas <- glmer(cbind(Resolution_resp, countIncidents-Resolution_resp) ~ Month + isWeekend +
                            TimeOfDay + IncCategory + (1 | PoliceDistrict),                         
                          family=binomial(link="logit"), data=merged_pd
                          # ,control=glmerControl(
                          #   optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)

# anova(model1_minus_hol,model1_minus_hol_seas,test="Chisq")
summary(model1_minus_hol_seas)
tab_model(model1_minus_hol_seas)
dotplot(ranef(model1_minus_hol_seas, condVar=TRUE))

# logit <- glm(cbind(Resolution_resp, countIncidents-Resolution_resp) ~ 
#                Season + Month + isWeekend +
#                TimeOfDay + PoliceDistrict,                         
#              family=binomial, data=merged_pd)

# Model 2 - including random slope and random intercept
start_time <- Sys.time()
model2 <- glmer(cbind(Resolution_resp, countIncidents-Resolution_resp) ~ 
                  Season + Month + isWeekend +
                  TimeOfDay + IncCategory +(TimeOfDay | PoliceDistrict),                         
                family=binomial(link="logit"), data=merged_pd
                # ,control=glmerControl(
                #   optimizer="bobyqa",optCtrl=list(maxfun=2e5))
)
end_time <- Sys.time()
print(paste("The model training time:" , end_time - start_time))

summary(model2)
tab_model(model2)
dotplot(ranef(model2, condVar=TRUE))

# Model 3 - Trying Poisson with random intercept
# model3 <- glmer(cbind(Resolution_resp, countIncidents-Resolution_resp) ~ 
#                   Season + Month + isWeekend +
#                   TimeOfDay + (1 | PoliceDistrict),                         
#                 family=poisson(link="log"), data=merged_pd
#                 # ,control=glmerControl(
#                 #   optimizer="bobyqa",optCtrl=list(maxfun=2e5))
# )
# summary(model3)
# tab_model(model3)
# dotplot(ranef(model3, condVar=TRUE))

