

load("Data_Gruendahletal.Rda")

library(Hmisc)
library(psych)
library(questionr)
library(lme4) 
library(lmerTest)
library(plyr)
library(performance)
library(car) 
library(ggplot2) 
library(stats)
library(sjmisc) 
library(sjPlot) 
library(car)
library(ggeffects)          
library(psych)
library(emmeans)            
library(parameters)        



#----------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------#

####################################
#### Descriptive Analysis
####################################


### social interaction characteristics


table(df_interact$SI_type_simple)
round(100*(prop.table(table(df_interact$SI_type_simple))), digits = 2)

table(df_interact$SI_gender_partner)
round(100*(prop.table(table(df_interact$SI_gender_partner))), digits = 2)

table(df_interact$SI_count)
round(100*(prop.table(table(df_interact$SI_count))), digits = 2)


table(df_interact$SI_num_partners) 
table(df_interact$SI_gender_partner, df_interact$SI_num_partners)

table(df_interact$SI_gender_partner, df_interact$SI_count)
round(100*(prop.table(table(df_interact$SI_gender_partner, df_interact$SI_count))), digits = 2)




#### descriptive analysis by gender


### PREP 

df_interact_BL$female <- ifelse(df_interact_BL$gender == 'female', 1, 0)

vars_BL = c("Participant", "female", "HR_BL", "RMSSD_BL")
df_vars_BL = df_interact_BL[vars_BL]
df_vars_BL$female<- as.factor(df_vars_BL$female)
df_vars_BL = aggregate(. ~Participant, data=df_vars_BL, mean, na.rm=T)

vars = c("Participant", "female", "age", "BMI", "HR_BL", "RMSSD_BL", "mean_HR_EMA","mean_RMSSD_EMA",  
         "mean_State_Anxiety", "mean_State_SI_Anxiety","SIAS_sum", "ADSK_post_sum", "STAI_Trait_sum") 
df_vars = df_interact[vars]
df_vars = aggregate(. ~Participant, data=df_vars, mean, na.rm=T)

df_vars$female<- as.factor(df_vars$female)


### whole sample

df_quest$female <- ifelse(df_quest$gender == 'female', 1, 0)
df_quest$num_interactions <- plyr::ddply(df_interact, .(Participant), nrow)
psych::describe(df_quest$num_interactions)
#df_quest$female[df_quest$gender=="female"] <- 1
#df_quest$female[df_quest$gender=="male"] <- 0

attach(df_quest)

psych::describe(age)
psych::describe(BMI)
psych::describe(SIAS_sum)
psych::describe(ADSK_post_sum)
table(Soc_Anx_group) #13.54% 
table(Depr_group) #12.5%

detach(df_quest)

### Baseline ECG

attach(df_vars_BL)

psych::describe(df_vars_BL)
describeBy(df_vars_BL, df_vars_BL$female)

leveneTest(HR_BL, female)
leveneTest(RMSSD_BL, female)

t.test(HR_BL~female, var.equal = T) 
t.test(RMSSD_BL~female, var.equal = T)

df_vars %>% rstatix::cohens_d(HR_BL~female) 
df_vars %>% rstatix::cohens_d(RMSSD_BL~female)

detach(df_vars_BL)

### EMA

attach(df_vars)
psych::describe(df_vars)

describeBy(df_vars, df_vars$female)

leveneTest(age, female)
leveneTest(BMI, female)
leveneTest(mean_HR_EMA, female)
leveneTest(mean_RMSSD_EMA, female)
leveneTest(mean_State_Anxiety, female)
leveneTest(mean_State_SI_Anxiety, female)
leveneTest(SIAS_sum, female)
leveneTest(ADSK_post_sum, female)
leveneTest(STAI_Trait_sum, female)
# all non-significant

t.test(age~female, var.equal = T)
t.test(BMI~female, var.equal = T) 
t.test(mean_HR_EMA~female, var.equal = T)
t.test(mean_RMSSD_EMA~female, var.equal = T)
t.test(mean_State_Anxiety~female, var.equal = T)
t.test(mean_State_SI_Anxiety~female, var.equal = T)
t.test(SIAS_sum~female, var.equal = T)
t.test(ADSK_post_sum~female, var.equal = T)
t.test(STAI_Trait_sum~female, var.equal = T) 

detach(df_vars)

# effect sizes

df_vars %>% rstatix::cohens_d(age~female)
df_vars %>% rstatix::cohens_d(BMI~female) 
df_vars %>% rstatix::cohens_d(mean_HR_EMA~female) 
df_vars %>% rstatix::cohens_d(mean_RMSSD_EMA~female)

df_vars %>% rstatix::cohens_d(mean_State_Anxiety~female)
df_vars %>% rstatix::cohens_d(mean_State_SI_Anxiety~female)

df_vars %>% rstatix::cohens_d(SIAS_sum~female)
df_vars %>% rstatix::cohens_d(ADSK_post_sum~female)
df_vars %>% rstatix::cohens_d(STAI_Trait_sum~female) 


remove(df_interact_BL, df_vars, df_vars_BL, vars, vars_BL)

#----------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------#



####################################
#### LINEAR MIXED MODELS
####################################




#-------------- manipulation check: anxiety relates to HR and HRV ----------------#


Anx_HR = lmer(State_Anxiety ~  
                HR_EMA_cw +
                female +
                SI_caffeine + 
                SI_nicotin +
                SI_alcohol +
                accel_EMA_cw +
                HR_BL_cb +
                (1|Participant), data=df_interact)
summary(Anx_HR)
Anova(Anx_HR, type=3)

Anx_RMSSD = lmer(State_Anxiety ~  
                   ln_RMSSD_EMA_cw +
                   female +
                   SI_caffeine + 
                   SI_nicotin +
                   SI_alcohol +
                   accel_EMA_cw +
                   ln_RMSSD_BL_cb +
                   (1|Participant), data=df_interact)
summary(Anx_RMSSD)
Anova(Anx_RMSSD, type=3)


Anx_RMSSD_HR = lmer(State_Anxiety ~  
                      ln_RMSSD_EMA_cw +
                      HR_EMA_cw +
                      female +
                      SI_duration_cw +
                      SI_caffeine + 
                      SI_nicotin +
                      SI_alcohol +
                      accel_EMA_cw +
                      ln_RMSSD_BL_cb +
                      HR_BL_cb +
                      (1|Participant), data=df_interact)
summary(Anx_RMSSD_HR)
Anova(Anx_RMSSD_HR, type=3)

check_collinearity(Anx_RMSSD_HR)

anova(Anx_HR, Anx_RMSSD_HR) # better
anova(Anx_RMSSD, Anx_RMSSD_HR) # better

remove(Anx_HR, Anx_RMSSD, Anx_RMSSD_HR)


#----------------------- STATE ANXIETY -----------------------#


## original

Anx_A = lmer(State_Anxiety ~  
               SI_familiarity_cw * female +
               female * SI_gender_partner +
               SIAS_cb +
               State_SI_Anxiety_cw +
               SI_count +
               SI_type_simple +
               SI_duration_cw + 
               SI_caffeine + 
               SI_nicotin +
               SI_alcohol +
               (1|Participant), data=df_interact)
summary(Anx_A)
Anova(Anx_A, type=3)

check_collinearity(Anx_A)


## no interactions

Anx_basic = lmer(State_Anxiety ~  
                   SI_familiarity_cw +
                   female + 
                   SI_gender_partner +
                   SIAS_cb +
                   State_SI_Anxiety_cw +
                   SI_count +
                   SI_type_simple +
                   SI_duration_cw + 
                   SI_caffeine + 
                   SI_nicotin +
                   SI_alcohol +
                   (1|Participant), data=df_interact)
summary(Anx_basic)
Anova(Anx_basic, type=3)

anova(Anx_basic, Anx_A)  # no difference

check_collinearity(Anx_basic)



## with gender IP x quantity IP

Anx_control = lmer(State_Anxiety ~  
                     SI_familiarity_cw * female +
                     female * SI_gender_partner +
                     SI_gender_partner * SI_count +
                     SIAS_cb +
                     State_SI_Anxiety_cw +
                     SI_type_simple +
                     SI_duration_cw + 
                     SI_caffeine + 
                     SI_nicotin +
                     SI_alcohol +
                     (1|Participant), data=df_interact)
summary(Anx_control)
Anova(Anx_control, type=3)

anova(Anx_control, Anx_A) # no difference





#----------------------- HEART RATE -----------------------#


### original

HR_A = lmer(HR_EMA ~  
              SI_familiarity_cw * female +
              female * SI_gender_partner + 
              SIAS_cb +
              State_SI_Anxiety_cw +
              SI_count +
              SI_type_simple +
              SI_duration_cw +
              SI_caffeine + 
              SI_nicotin +
              SI_alcohol +
              accel_EMA_cw +
              HR_BL_cb +
              (1|Participant), data=df_interact)
summary(HR_A)
Anova(HR_A, type=3)

check_collinearity(HR_A)


### no interactions

HR_basic = lmer(HR_EMA ~  
                  SI_familiarity_cw +
                  female + 
                  SI_gender_partner + 
                  SIAS_cb +
                  State_SI_Anxiety_cw +
                  SI_count +
                  SI_type_simple +
                  SI_duration_cw +
                  SI_caffeine + 
                  SI_nicotin +
                  SI_alcohol +
                  accel_EMA_cw +
                  HR_BL_cb +
                  (1|Participant), data=df_interact)
summary(HR_basic)
Anova(HR_basic, type=3)

anova(HR_basic, HR_A) # better


### with type x accel, gender (SI) x quantity (SI)

HR_control = lmer(HR_EMA ~  
                    SI_familiarity_cw * female +
                    female * SI_gender_partner +
                    SI_gender_partner * SI_count +
                    SI_type_simple * accel_EMA_cw +
                    SIAS_cb +
                    State_SI_Anxiety_cw +
                    SI_duration_cw +
                    SI_caffeine + 
                    SI_nicotin +
                    SI_alcohol +
                    HR_BL_cb +
                    (1|Participant), data=df_interact)
summary(HR_control)
Anova(HR_control, type=3)

anova(HR_control, HR_A) # no difference




#----------------------- HEART RATE VARIABILITY (RMSSD) -----------------------#


### original

RMSSD_A = lmer(ln_RMSSD_EMA ~  
                 SI_familiarity_cw * female +
                 female * SI_gender_partner + 
                 SIAS_cb +
                 State_SI_Anxiety_cw +
                 SI_count +
                 SI_type_simple +
                 SI_duration_cw +
                 SI_caffeine + 
                 SI_nicotin +
                 SI_alcohol +
                 accel_EMA_cw +
                 ln_RMSSD_BL_cb +
                 (1|Participant), data=df_interact)
summary(RMSSD_A)
Anova(RMSSD_A, type=3)

check_collinearity(RMSSD_A)


### no interactions

RMSSD_basic = lmer(ln_RMSSD_EMA ~  
                     SI_familiarity_cw +
                     female + 
                     SI_gender_partner + 
                     SIAS_cb +
                     State_SI_Anxiety_cw +
                     SI_count +
                     SI_type_simple +
                     SI_duration_cw +
                     SI_caffeine + 
                     SI_nicotin +
                     SI_alcohol +
                     accel_EMA_cw +
                     ln_RMSSD_BL_cb +
                     (1|Participant), data=df_interact)
summary(RMSSD_basic)
Anova(RMSSD_basic, type=3)

anova(RMSSD_basic, RMSSD_A) # better



### with type x accel, gender (SI) x quantity (SI)

RMSSD_control = lmer(ln_RMSSD_EMA ~  
                       SI_familiarity_cw * female +
                       female * SI_gender_partner +
                       SI_gender_partner * SI_count +
                       SI_type_simple * accel_EMA_cw +
                       SIAS_cb +
                       State_SI_Anxiety_cw +
                       SI_duration_cw +
                       SI_caffeine + 
                       SI_nicotin +
                       SI_alcohol +
                       ln_RMSSD_BL_cb +
                       (1|Participant), data=df_interact)
summary(RMSSD_control)
Anova(RMSSD_control, type=3)

anova(RMSSD_control, RMSSD_A) # no difference





#----------------------- test: with depression and age -----------------------#


### Heart Rate

HR_D = lmer(HR_EMA ~  
              SI_familiarity_cw * female +
              female * SI_gender_partner + 
              SIAS_cb +
              State_SI_Anxiety_cw +
              SI_count +
              SI_type_simple +
              SI_duration_cw +
              SI_caffeine + 
              SI_nicotin +
              SI_alcohol +
              accel_EMA_cw +
              HR_BL_cb +
              Age_cb +
              ADSK_post_sum +
              (1|Participant), data=df_interact)
summary(HR_D)
Anova(HR_D, type=3)

anova(HR_A, HR_D) # no difference 


### RMSSD

RMSSD_D = lmer(ln_RMSSD_EMA ~  
                 SI_familiarity_cw * female +
                 female * SI_gender_partner + 
                 SIAS_cb +
                 State_SI_Anxiety_cw +
                 SI_count +
                 SI_type_simple +
                 SI_duration_cw +
                 SI_caffeine + 
                 SI_nicotin +
                 SI_alcohol +
                 accel_EMA_cw +
                 ln_RMSSD_BL_cb +
                 Age_cb +
                 ADSK_post_sum +
                 (1|Participant), data=df_interact)
summary(RMSSD_D)
Anova(RMSSD_D, type=3)

anova(RMSSD_A, RMSSD_D) # no difference 


### State Anxiety

Anx_basic_D = lmer(State_Anxiety ~  
                     SI_familiarity_cw +
                     female + 
                     SI_gender_partner +
                     SIAS_cb +
                     State_SI_Anxiety_cw +
                     SI_count +
                     SI_type_simple +
                     SI_duration_cw + 
                     SI_caffeine + 
                     SI_nicotin +
                     SI_alcohol +
                     Age_cb +
                     ADSK_post_sum +
                     (1|Participant), data=df_interact)
summary(Anx_basic_D)
Anova(Anx_basic_D, type=3)

anova(Anx_basic, Anx_basic_D)  # better

### with interactions

Anx_D = lmer(State_Anxiety ~  
               SI_familiarity_cw * female +
               female * SI_gender_partner +
               SIAS_cb +
               State_SI_Anxiety_cw +
               SI_count +
               SI_type_simple +
               SI_duration_cw + 
               SI_caffeine + 
               SI_nicotin +
               SI_alcohol +
               time_since_SI +
               Age_cb +
               ADSK_post_sum +
               (1|Participant), data=df_interact)
summary(Anx_D)
Anova(Anx_D, type=3)

anova(Anx_basic_D, Anx_D) # no difference

remove(Anx_D)




#----------------------- with romantic partner -----------------------#


### Heart Rate

HR_partner = lmer(HR_EMA ~  
                    SI_familiarity_cw * female +
                    female * SI_gender_partner + 
                    SIAS_cb +
                    State_SI_Anxiety_cw +
                    SI_count +
                    SI_type_simple +
                    SI_duration_cw +
                    SI_caffeine + 
                    SI_nicotin +
                    SI_alcohol +
                    accel_EMA_cw +
                    HR_BL_cb +
                    SI_with_partner +
                    (1|Participant), data=df_interact)
summary(HR_partner)
Anova(HR_partner, type=3)

anova(HR_A, HR_partner) # marginal


### RMSSD

RMSSD_partner = lmer(ln_RMSSD_EMA ~  
                       SI_familiarity_cw * female +
                       female * SI_gender_partner + 
                       SIAS_cb +
                       State_SI_Anxiety_cw +
                       SI_count +
                       SI_type_simple +
                       SI_duration_cw +
                       SI_caffeine + 
                       SI_nicotin +
                       SI_alcohol +
                       accel_EMA_cw +
                       ln_RMSSD_BL_cb +
                       SI_with_partner +
                       (1|Participant), data=df_interact)
summary(RMSSD_partner)
Anova(RMSSD_partner, type=3)

anova(RMSSD_A, RMSSD_partner) # no difference



### State Anxiety

Anx_basic_partner = lmer(State_Anxiety ~  
                           SI_familiarity_cw +
                           female + 
                           SI_gender_partner +
                           SIAS_cb +
                           State_SI_Anxiety_cw +
                           SI_count +
                           SI_type_simple +
                           SI_duration_cw + 
                           SI_caffeine + 
                           SI_nicotin +
                           SI_alcohol +
                           Age_cb +
                           ADSK_post_sum +
                           SI_with_partner +
                           (1|Participant), data=df_interact)
summary(Anx_basic_partner)
Anova(Anx_basic_partner, type=3)

anova(Anx_basic_partner, Anx_basic_D)  # better





#----------------------- familiarity x female x BL -----------------------#


## check effect of Baseline on familiarity x gender (self) interaction

HR_test_BL = lmer(HR_EMA ~  
                    SI_familiarity_cw * female * HR_BL_cb +
                    female * SI_gender_partner + 
                    SIAS_cb +
                    State_SI_Anxiety_cw +
                    SI_count +
                    SI_type_simple +
                    SI_duration_cw +
                    SI_caffeine + 
                    SI_nicotin +
                    SI_alcohol +
                    accel_EMA_cw +
                    (1|Participant), data=df_interact)
summary(HR_test_BL)
Anova(HR_test_BL, type=3)

anova(HR_A, HR_test_BL) # no difference



RMSSD_test_BL = lmer(ln_RMSSD_EMA ~  
                       SI_familiarity_cw * female * ln_RMSSD_BL_cb +
                       female * SI_gender_partner +
                       SIAS_cb +
                       State_SI_Anxiety_cw +
                       SI_count + 
                       SI_type_simple + 
                       SI_duration_cw +
                       SI_caffeine + 
                       SI_nicotin +
                       SI_alcohol +
                       accel_EMA_cw + 
                       (1|Participant), data=df_interact)
summary(RMSSD_test_BL)
Anova(RMSSD_test_BL, type=3)

anova(RMSSD_A, RMSSD_test_BL) # better



#----------------------- pleasantness x familiarity, SIAS x familiarity, SIAS x SI type -----------------------#


Anx_mod_rev = lmer(State_Anxiety ~  
                      SI_familiarity_cw*SI_pleasantness_cw+       
                      SIAS_cb*SI_familiarity_cw+                  
                      SI_type_simple*SIAS_cb+   
                      female + 
                      SI_gender_partner +
                      State_SI_Anxiety_cw +
                      SI_count +
                      SI_duration_cw + 
                      SI_caffeine + 
                      SI_nicotin +
                      SI_alcohol +
                      Age_cb +
                      ADSK_post_sum +
                      (1|Participant), data=df_interact)
summary(Anx_mod_rev)
Anova(Anx_mod_rev, type=3)
anova(Anx_basic_D, Anx_mod_rev)


#HR
HR_mod_rev = lmer(HR_EMA ~  
                    SI_familiarity_cw*SI_pleasantness_cw+       
                    SIAS_cb*SI_familiarity_cw+                                
                    SI_type_simple*SIAS_cb+                  
                    SI_familiarity_cw * female +
                    female * SI_gender_partner +
                    State_SI_Anxiety_cw +
                    SI_count +
                    SI_duration_cw +
                    SI_caffeine + 
                    SI_nicotin +
                    SI_alcohol +
                    accel_EMA_cw +
                    HR_BL_cb +
                    (1|Participant), data=df_interact)
summary(HR_mod_rev)
Anova(HR_mod_rev, type=3)
anova(HR_mod_rev, HR_A)


###RMSSD
RMSSD_mod_rev = lmer(ln_RMSSD_EMA ~  
                       SIAS_cb*SI_familiarity_cw+                 # moderation Rev 1
                       SI_familiarity_cw*SI_pleasantness_cw+      # moderation Rev 1                 
                       SIAS_cb*SI_type_simple+                
                       SI_familiarity_cw * female +
                       female * SI_gender_partner + 
                       State_SI_Anxiety_cw +
                       SI_count +
                       SI_duration_cw +
                       SI_caffeine + 
                       SI_nicotin +
                       SI_alcohol +
                       accel_EMA_cw +
                       ln_RMSSD_BL_cb +
                       (1|Participant), data=df_interact)
summary(RMSSD_mod_rev)
Anova(RMSSD_mod_rev, type=3)
anova(RMSSD_mod_rev, RMSSD_A)



#----------------------- final models -----------------------#

HR_final <- HR_A
RMSSD_final <- RMSSD_A
Anx_final <- Anx_basic_D

# multicollinearity

check_collinearity(Anx_final)
check_collinearity(HR_final)
check_collinearity(RMSSD_final)

### model parameters

tab_model(Anx_final, HR_final, RMSSD_final, show.ci = F, show.se = T, show.stat = T)


#confidence intervals
ci(HR_final) # anderer Output als confint 
confint(HR_final, oldNames=F)
confint(RMSSD_final, oldNames=F)
confint(Anx_final, oldNames=F)

remove(Anx_A, Anx_basic, Anx_basic_partner, Anx_control, Anx_basic_D)
remove(HR_A, HR_basic, HR_control, HR_D, HR_partner, HR_test_BL)
remove(RMSSD_A, RMSSD_basic, RMSSD_control, RMSSD_D, RMSSD_partner)
remove(Anx_mod_rev, HR_mod_rev, RMSSD_mod_rev)


#----------------------- figures + inspection of interaction effects -----------------------#


#------- ANXIETY -------#

## Figure 1

plot1 <- df_interact %>%
  ggplot(aes(x = SI_familiarity_cw,
             y = State_Anxiety)) +
  geom_jitter(width = 0.13, color = "#4F7175", alpha = 0.5) + 
  geom_smooth(method = lm, color = "black", size = 0.5)+
  theme_classic() + 
  scale_x_continuous(name = "familiarity (cw)") + 
  scale_y_continuous(name = "state anxiety", limits = c(0, 90)) +
  theme(plot.title = element_blank(), axis.text = element_text(size=16, color="black"),
        axis.title.x=element_text(size=16, color="black"),
        axis.title.y=element_text(size=16, color="black"),
  )
plot1




#------- HEART RATE -------#



## familiarity x female

# simple slope analysis 

ss <- interactions::sim_slopes(HR_final, pred = SI_familiarity_cw, modx = female, digits = 3)
ss
huxtable::as_huxtable(ss)


## Figure 2A 

#no legend
fig1_HR_nolegend = qplot(x = SI_familiarity_cw, y = HR_EMA, data = df_interact, color = gender, shape=gender, alpha = 0.01)
fig1_HR_nolegend = fig1_HR_nolegend + 
  geom_smooth(aes(color = gender, fill = gender, shape=gender), method = lm, alpha=0.2, size=1.5) +
  scale_color_manual(values = c("#00AFBB",  "#FC4E07")) + 
  scale_fill_manual(values = c("#00AFBB","#FC4E07")) +
  theme_classic() + labs(x="familiarity (cw)", y="HR (bpm)")+
  theme(plot.title = element_blank(), axis.text = element_text(size=16, color="black"),
        axis.title.x=element_text(size=16, color="black"),
        axis.title.y=element_text(size=16, color="black"),
        legend.position = "none")
fig1_HR_nolegend

#top legend
fig1_HR_toplegend = qplot(x = SI_familiarity_cw, y = HR_EMA, data = df_interact, color = gender, shape=gender, alpha = 0.01, show.legend=F)
fig1_HR_toplegend = fig1_HR_toplegend + 
  geom_smooth(aes(color = gender, fill = gender, shape=gender), method = lm, alpha=0.2, size=1.5) +
  scale_color_manual(values = c("#00AFBB",  "#FC4E07")) + 
  scale_fill_manual(values = c("#00AFBB","#FC4E07")) + 
  theme_classic() + labs(x="familiarity (cw)", y="HR (bpm)")+
  theme(plot.title = element_blank(), axis.text = element_text(size=16, color="black"),
        axis.title.x=element_text(size=16, color="black"),
        axis.title.y=element_text(size=16, color="black"),
        legend.position = "top",
        legend.text=element_text(size=16), legend.title=element_text(size=16))
fig1_HR_toplegend+labs(colour="gender participant")+guides(fill = FALSE)



## gender interaction partner x female

# post hoc sample comparison
rbind(emmeans::emmeans(HR_final, pairwise~SI_gender_partner|female)$contrasts, adjust="bonferroni")
rbind(emmeans::emmeans(HR_final, pairwise~female|SI_gender_partner)$contrasts, adjust="bonferroni")


## Figure 3A

df_interact$SI_gender_partner_label[df_interact$SI_gender_partner == "female"] <- "female IP"
df_interact$SI_gender_partner_label[df_interact$SI_gender_partner == "male"] <- "male IP"
df_interact$SI_gender_partner_label[df_interact$SI_gender_partner == "mixed"] <- "mixed IP"


fig1_HR_gender <- ggboxplot(df_interact, x = "gender", y = "HR_EMA", color="gender",
                            palette = c("#00AFBB",  "#FC4E07"),
                            size = 1.2, width = 0.5, 
                            order = c("female", "male"), xlab="participant gender", 
                            ylab="HR (bpm)", add = "mean")+
                            facet_wrap(~SI_gender_partner_label) 
fig1_HR_gender <- fig1_HR_gender + theme(legend.position="none", 
                                         plot.title = element_blank(), axis.text = element_text(size=16, color="black"),
                                         axis.title.x=element_text(size=16, color="black"),
                                         axis.title.y=element_text(size=16, color="black"),                                                            
                                         legend.text=element_text(size=16), legend.title=element_text(size=16))
fig1_HR_gender 



#------- HEART RATE VARIABILITY -------#


## familiarity x female 

# simple slope analysis 

ss <- interactions::sim_slopes(RMSSD_final, pred = SI_familiarity_cw, modx = female, digits = 3)
ss
huxtable::as_huxtable(ss)

## Figure 2B

# without legend
fig1_RMSSD_nolegend = qplot(x = SI_familiarity_cw, y = ln_RMSSD_EMA, data = df_interact, color = gender, shape=gender, alpha = 0.01)
fig1_RMSSD_nolegend = fig1_RMSSD_nolegend + 
  geom_smooth(aes(color = gender, fill = gender, shape=gender), method = lm, alpha=0.2, size = 1.5) +
  scale_color_manual(values = c("#00AFBB",  "#FC4E07")) +
  scale_fill_manual(values = c("#00AFBB",  "#FC4E07")) +
  theme_classic() + labs(x="familiarity", y="RMSSD (ln)")+
  theme(plot.title = element_blank(), axis.text = element_text(size=16, color="black"),
        axis.title.x=element_text(size=16, color="black"),
        axis.title.y=element_text(size=16, color="black"),
        legend.position="none",
  )
fig1_RMSSD_nolegend


# top legend
fig1_RMSSD_toplegend = qplot(x = SI_familiarity_cw, y = ln_RMSSD_EMA, data = df_interact, color = gender, shape=gender, alpha = 0.01, show.legend=F)
fig1_RMSSD_toplegend = fig1_RMSSD_toplegend +  
  geom_smooth(aes(color = gender, fill = gender, shape=gender), method = lm, alpha=0.2, size=1.5) +
  scale_color_manual(values = c("#00AFBB",  "#FC4E07")) + 
  scale_fill_manual(values = c("#00AFBB",  "#FC4E07")) + 
  theme_classic() + labs(x="familiarity", y="RMSSD (ln)")+
  theme(plot.title = element_blank(), axis.text = element_text(size=16, color="black"),
        axis.title.x=element_text(size=16, color="black"),
        axis.title.y=element_text(size=16, color="black"),
        legend.position="top",
        legend.text=element_text(size=16), legend.title=element_text(size=16),
  )
fig1_RMSSD_toplegend+labs(colour="gender participant")+guides(fill = FALSE)



## Figure 2 (A + B)

panel1<-ggarrange(fig1_HR, fig1_RMSSD, ncol =2, nrow = 1, labels = c('A', 'B'), common.legend=T)
panel1



## gender interaction partner x female

# post hoc sample comparison

rbind(emmeans::emmeans(RMSSD_final, pairwise~SI_gender_partner|female)$contrasts, adjust="bonferroni")
rbind(emmeans::emmeans(RMSSD_final, pairwise~female|SI_gender_partner)$contrasts, adjust="bonferroni")

## Figure 3B

fig1_RMSSD_gender <- ggboxplot(df_interact, x = "gender", y = "ln_RMSSD_EMA", color="gender",
                               palette = c("#00AFBB",  "#FC4E07"),
                               size = 1.2, width = 0.5, 
                               order = c("female", "male"), xlab="participant gender", ylab="RMSSD (ln)", add = "mean")+
                                facet_wrap(~SI_gender_partner_label)
fig1_RMSSD_gender <- fig1_RMSSD_gender + theme(legend.position="none", 
                                               plot.title = element_blank(), axis.text = element_text(size=16, color="black"),
                                               axis.title.x=element_text(size=16, color="black"),
                                               axis.title.y=element_text(size=16, color="black"),                                                            
                                               legend.text=element_text(size=16), legend.title=element_text(size=16))
fig1_RMSSD_gender


## Figure 3 (A + B)

panel4<-ggarrange(fig1_HR_gender, fig1_RMSSD_gender, ncol =2, nrow = 1, labels = c('A', 'B'))
panel4




#------- BASELINE CHECK -------#

df_interact$ln_RMSSD_BL_cb_3<- as.factor(as.numeric(base::cut(df_interact$ln_RMSSD_BL_cb,3)))
df_interact$ln_RMSSD_BL_cb_3 = revalue(df_interact$ln_RMSSD_BL_cb_3, c("3"="high", "2"="medium", "1"="low"))

female_label <- c('0' = "male",'1' = "female")
df <- ggpredict(RMSSD_test_BL, terms = c("SI_familiarity_cw", "ln_RMSSD_BL_cb", "female"))

## Figure 4
fig1_RMSSD_BL <- ggplot(df, aes(x, predicted, color = group, linetype = group)) + 
  geom_line() +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=group), alpha=0.15)+
  facet_wrap(~facet, labeller = as_labeller(female_label))+
  xlab("familiarity (cw)") +
  ylab("RMSSD (ln) during interaction")+
  labs(fill = "RMMSD (cb)\nat baseline", 
       color = "RMMSD (cb)\nat baseline", 
       linetype = "RMMSD (cb)\nat baseline")+
  #guides(color = FALSE)+
  scale_fill_manual(values = c("#71D0A6", "#478268", "#1C342A"))+ #Anne: "#50C878", "#2AAA8A", "#097969"; 
  scale_color_manual(values = c("#71D0A6", "#478268", "#1C342A"))+ #https://htmlcolorcodes.com/color-picker/
  scale_linetype_manual(values = c("dotted", "dashed", "solid"))+ 
  theme_classic()+
  theme(text = element_text(size=18),
        axis.text=element_text(size=18),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        legend.text=element_text(size=18), legend.spacing = unit(10, "pt") ,
        legend.position = "bottom",
        legend.key.size = unit(2,"line")
  )
fig1_RMSSD_BL



## simple slope analysis 
ss <- interactions::sim_slopes(RMSSD_final, pred = SI_familiarity_cw, modx = female, digits = 3)
ss
huxtable::as_huxtable(ss)

df_interact_male <- subset(df_interact, female == 0)
df_interact_female <- subset(df_interact, female == 1)

RMSSD_test_BL_male = lmer(ln_RMSSD_EMA ~  
                            SI_familiarity_cw * ln_RMSSD_BL_cb_3 * ln_RMSSD_BL_cb +
                            SI_gender_partner +
                            SIAS_cb +
                            State_SI_Anxiety_cw +
                            SI_count + 
                            SI_type_simple + 
                            SI_duration_cw +
                            SI_caffeine + 
                            SI_nicotin +
                            SI_alcohol +
                            accel_EMA_cw +  
                            (1|Participant), data=df_interact_male)
summary(RMSSD_test_BL_male)

# simple slopes
ss <- interactions::sim_slopes(RMSSD_test_BL_male, pred = SI_familiarity_cw, modx = ln_RMSSD_BL_cb, digits = 3)
ss
huxtable::as_huxtable(ss)

RMSSD_test_BL_female = lmer(ln_RMSSD_EMA ~  
                              SI_familiarity_cw * ln_RMSSD_BL_cb_3 * ln_RMSSD_BL_cb +
                              SI_gender_partner +
                              SIAS_cb +
                              SI_count + 
                              SI_type_simple + 
                              SI_duration_cw +
                              SI_caffeine + 
                              SI_nicotin +
                              SI_alcohol +
                              accel_EMA_cw + 
                              (1|Participant), data=df_interact_female)
summary(RMSSD_test_BL_female)

# simple slopes
ss <- interactions::sim_slopes(RMSSD_test_BL_female, pred = SI_familiarity_cw, modx = ln_RMSSD_BL_cb, digits = 3)
ss
huxtable::as_huxtable(ss)





