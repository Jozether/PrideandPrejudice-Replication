library(tidyverse)
library(rio)
library(forestploter)
library(psych)
library(gridExtra)
library(grid)

setwd("/Users/josetorres/Desktop/UPenn/RBSI 2024/Final Project/Data/")

dat <- import("pewatp58.csv")

dat <- dat %>%
  filter(f_party_ != "Refused",
         f_educca != "Refused",
         f_sex != "Refused",
         f_agecat != "Refused",
         worrydpo != "Refused",
         knowdpor != "Refused") %>%
  select(hisporig, hispor_2, hispor_3, hispor_4, hispor_5, hispor_6,
         hispor_7, hispor_8, hispor_9, identerm, immburdn, immval_a, immval_b,
         immval_c, immval_d, immval_e, daca2018, asylumre, lan1_w58, idenchg_,
         idench_1, idench_2, idench_3, idench_4, idench_5, worrydpo, knowdpor,
         racesurv, cmmoncul, typicalu, racesu_1, f_party_, f_educca, f_sex,
         f_agecat, cmmoncul, lan1_w58, hispdisc, hispdi_1, hispdi_2, hispdi_3,
         hispdi_4, weight_w, citizen_)

dat <- dat %>%
  mutate(talk_abt_hispanic_pride = case_when(idenchg_ == "I have never done this" ~ -2,
                                             idenchg_ == "Less often" ~ -1,
                                             idenchg_ == "About the same000000" ~ 0,
                                             idenchg_ == "More often" ~ 1,
                                             idenchg_ == "Refused" ~ NA),
         self_idenity = case_when(identerm == "Hispanic or Latino" ~ 1,
                                  identerm == "Country of Hispanic origin" ~ 1,
                                  identerm == "American" ~ 0,
                                  identerm == "Refused" ~ NA),
         wear_hispanic_clothing = case_when(idench_1== "I have never done this" ~ -2,
                                            idench_1 == "Less often" ~ -1,
                                            idench_1 == "About the same000000" ~ 0,
                                            idench_1 == "More often" ~ 1,
                                            idench_1 == "Refused" ~ NA),
         speak_spanish_public = case_when(idench_2 == "I have never done this" ~ -2,
                                          idench_2 == "Less often" ~ -1,
                                          idench_2 == "About the same000000" ~ 0,
                                          idench_2 == "More often" ~ 1,
                                          idench_2 == "Refused" ~ NA),
         talk_abt_american_pride = case_when(idench_4 == "I have never done this" ~ -2,
                                             idench_4 == "Less often" ~ -1,
                                             idench_4 == "About the same000000" ~ 0,
                                             idench_4 == "More often" ~ 1,
                                             idench_4 == "Refused" ~ NA),
         wear_american_clothing = case_when(idench_3 == "I have never done this" ~ -2,
                                            idench_3 == "Less often" ~ -1,
                                            idench_3 == "About the same000000" ~ 0,
                                            idench_3 == "More often" ~ 1,
                                            idench_3 == "Refused" ~ NA),
         speak_only_english = case_when(idench_5 == "I have never done this" ~ -2,
                                        idench_5 == "Less often" ~ -1,
                                        idench_5 == "About the same000000" ~ 0,
                                        idench_5 == "More often" ~ 1,
                                        idench_5 == "Refused" ~ NA),
         linked_fate = case_when(racesurv == "A lot" ~ 2,
                                 racesurv == "Some" ~ 1,
                                 racesurv == "Not much" ~ 0,
                                 racesurv == "Not at all" ~ -1,
                                 racesurv == "Refused" ~ NA),
         immburdn = case_when(immburdn == "Immigrants today are a burden on our country because they take our jobs, housing and health care" ~ 1,
                              immburdn == "Immigrants today strengthen our country because of their hard work and talents" ~ 0,
                              immburdn == "Refused" ~ NA),
         immval_a = case_when(immval_a == "Very important goal" ~ 2,
                              immval_a == "Somewhat important goal" ~ 1,
                              immval_a == "Refused" ~ NA,
                              immval_a == "Not too important goal" ~ -1,
                              immval_a == "Not at all important goal" ~ -2),
         immval_b = case_when(immval_b == "Very important goal" ~ 2,
                              immval_b == "Somewhat important goal" ~ 1,
                              immval_b == "Refused" ~ NA,
                              immval_b == "Not too important goal" ~ -1,
                              immval_b == "Not at all important goal" ~ -2),
         immval_c = case_when(immval_c == "Very important goal" ~ 2,
                              immval_c == "Somewhat important goal" ~ 1,
                              immval_c == "Refused" ~ NA,
                              immval_c == "Not too important goal" ~ -1,
                              immval_c == "Not at all important goal" ~ -2),
         immval_d = case_when(immval_d == "Very important goal" ~ 2,
                              immval_d == "Somewhat important goal" ~ 1,
                              immval_d == "Refused" ~ NA,
                              immval_d == "Not too important goal" ~ -1,
                              immval_d == "Not at all important goal" ~ -2),
         immval_e = case_when(immval_e == "Very important goal" ~ 2,
                              immval_e == "Somewhat important goal" ~ 1,
                              immval_e == "Refused" ~ NA,
                              immval_e == "Not too important goal" ~ -1,
                              immval_e == "Not at all important goal" ~ -2),
         daca2018 = case_when(daca2018 == "Favor" ~ 1,
                              daca2018 == "Oppose" ~ 0,
                              daca2018 == "Refused" ~ NA),
         asylumre = case_when(asylumre == "No, the U.S. does not have this responsibility" ~ 0,
                              asylumre == "Refused" ~ NA,
                              asylumre == "Yes, the U.S. has this responsibility" ~ 1),
         pd1 = case_when(hispdisc == "No, has not happened" ~ 0,
                         hispdisc == "Yes, has happened" ~ 1),
         pd2 = case_when(hispdi_1 == "No, has not happened" ~ 0,
                         hispdi_1 == "Yes, has happened" ~ 1),
         pd3 = case_when(hispdi_2 == "No, has not happened" ~ 0,
                         hispdi_2 == "Yes, has happened" ~ 1),
         pd4 = case_when(hispdi_3 == "No, has not happened" ~ 0,
                         hispdi_3 == "Yes, has happened" ~ 1),
         pd5 = case_when(hispdi_4 == "No, has not happened" ~ 0,
                         hispdi_4 == "Yes, has happened" ~ 1)) %>%
  drop_na(talk_abt_hispanic_pride, wear_hispanic_clothing, speak_spanish_public, linked_fate) %>%
  mutate(hispanic_comfort = talk_abt_hispanic_pride + wear_hispanic_clothing + speak_spanish_public,
         hispanic_comfort1 = talk_abt_hispanic_pride + wear_hispanic_clothing + speak_spanish_public,
         hispanic_comfort = scale(hispanic_comfort),
         american_comfort = talk_abt_american_pride + wear_american_clothing + speak_only_english,
         american_comfort1 = talk_abt_american_pride + wear_american_clothing + speak_only_english,
         american_comfort = scale(american_comfort),
         pd = pd1 + pd2 + pd3 +pd4 + pd5) %>%
  select(hispanic_comfort, immburdn, immval_a, immval_b, immval_c, immval_d, immval_e, daca2018, asylumre, worrydpo,
         racesu_1, f_party_, f_educca, f_sex, f_agecat, talk_abt_hispanic_pride, self_idenity, wear_hispanic_clothing, speak_spanish_public,
         linked_fate, wear_american_clothing, talk_abt_american_pride, american_comfort, pd, worrydpo, knowdpor, weight_w, citizen_, hispanic_comfort1, american_comfort1)

# Cronbach's alpha for hispanic comfort score
alpha <- dat %>%
  select(talk_abt_hispanic_pride, wear_hispanic_clothing)
alpha(alpha)

#averages

mean(dat$american_comfort1, na.rm = T)

# hispanic comfort score distribution
dat %>%
  ggplot(aes(x = hispanic_comfort)) + 
  geom_bar(stat = "count", width = 0.45, color = "black", fill = "gray", position = "dodge") +
  theme_bw() +
  xlab("Score") +
  ylab(" ") +
  ggtitle("Distribution of Hispanic ID Comfort Score") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        title = element_text(size = 15),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black')) +
  scale_x_continuous(n.breaks = 10) +
  geom_vline(aes(xintercept = mean(hispanic_comfort, na.rm = TRUE)), 
             linetype = "dashed",
             color = "black")

# american comfort score distribution
dat %>%
  ggplot(aes(x = american_comfort)) + 
  geom_bar(stat = "count", width = 0.43, color = "black", fill = "gray", position = "dodge") +
  theme_bw() +
  xlab("Score") +
  ylab(" ") +
  ggtitle("Distribution of American ID Comfort Score") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        title = element_text(size = 15),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = 'black')) +
  scale_x_continuous(n.breaks = 10) +
  geom_vline(aes(xintercept = mean(hispanic_comfort, na.rm = TRUE)), 
             linetype = "dashed",
             color = "black")

# discrimination experience score distribution
dat %>%
  ggplot(aes(x = pd)) + 
  geom_bar(stat = "count") +
  theme_bw() +
  xlab("Discrimination Experience Score") +
  ylab(" ") +
  ggtitle("Distribution of Discrimination Experience Score") +
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size=15),
        title = element_text(size=15),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(n.breaks=10) +
  geom_vline(aes(xintercept = mean(pd)), 
             linetype = "dashed",
             color = "#FC4E07")
  
### hispanic comfort
# summary(lm(immburdn ~ hispanic_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd + citizen_ + linked_fate, data = dat, weights = weight_w)) # Are immigrants a burden (-1) or benefit (1)
# summary(lm(immval_a ~ hispanic_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd + citizen_ + linked_fate, data = dat, weights = weight_w)) # Taking in refugees from war
# summary(lm(immval_b ~ hispanic_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd + citizen_ + linked_fate, data = dat, weights = weight_w)) # Increase deportations of illegal immigrants in US
# summary(lm(immval_c ~ hispanic_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd + citizen_ + linked_fate, data = dat, weights = weight_w)) # Establish pathway for illegal immigrants
# summary(lm(immval_d ~ hispanic_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd + citizen_ + linked_fate, data = dat, weights = weight_w)) # Increase security along border to reduce illegal crossings
# summary(lm(immval_e ~ hispanic_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd + citizen_ + linked_fate, data = dat, weights = weight_w)) # Improve the security of the country’s borders
# summary(lm(daca2018 ~ hispanic_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd + citizen_ + linked_fate, data = dat, weights = weight_w)) # Congress passing law granting dreamers legal status
# summary(lm(asylumre ~ hispanic_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd + citizen_ + linked_fate, data = dat, weights = weight_w)) # US has responsibility for Central American asylum seekers
# 
# ### american comfort
# summary(lm(immburdn ~ american_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd + citizen_ + linked_fate, data = dat, weights = weight_w)) # Are immigrants a burden (-1) or benefit (1)
# summary(lm(immval_a ~ american_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd + citizen_ + linked_fate, data = dat, weights = weight_w)) # Taking in refugees from war
# summary(lm(immval_b ~ american_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd + citizen_ + linked_fate, data = dat, weights = weight_w)) # Increase deportations of illegal immigrants in US
# summary(lm(immval_c ~ american_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd + citizen_ + linked_fate, data = dat, weights = weight_w)) # Establish pathway for illegal immigrants
# summary(lm(immval_d ~ american_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd + citizen_ + linked_fate, data = dat, weights = weight_w)) # Increase security along border to reduce illegal crossings
# summary(lm(immval_e ~ american_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd + citizen_ + linked_fate, data = dat, weights = weight_w)) # Improve the security of the country’s borders
# summary(lm(daca2018 ~ american_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd + citizen_ + linked_fate, data = dat, weights = weight_w)) # Congress passing law granting dreamers legal status
# summary(lm(asylumre ~ american_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd + citizen_ + linked_fate, data = dat, weights = weight_w)) # US has responsibility for Central American asylum seekers


##### creating hispanic models

# Are immigrants a burden (1) or benefit (-1)
immig_burden <- lm(immburdn ~ hispanic_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)
immig_burden_coef <- unname(immig_burden$coefficients[2])
immig_burden_conf <- confint(immig_burden, 'hispanic_comfort', level=0.95)
immig_burden_se <- unname(sqrt(diag(vcov(immig_burden)))[2])
m1 <- lm(immburdn ~ hispanic_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)

# Taking in refugees from war
take_in_ref <- lm(immval_a ~ hispanic_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)
take_in_ref_coef <- unname(take_in_ref$coefficients[2])
take_in_ref_conf <- confint(take_in_ref, 'hispanic_comfort', level=0.95)
take_in_ref_se <- unname(sqrt(diag(vcov(take_in_ref)))[2])
m2 <- lm(immval_a ~ hispanic_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)

# Increase deportations of illegal immigrants in US
increase_deportations <- lm(immval_b ~ hispanic_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)
increase_deportations_coef <- unname(increase_deportations$coefficients[2])
increase_deportations_conf <- confint(increase_deportations, 'hispanic_comfort', level=0.95)
increase_deportations_se <- unname(sqrt(diag(vcov(increase_deportations)))[2])
m3 <- lm(immval_b ~ hispanic_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)

# Establish pathway for illegal immigrants
establish_pathway <- lm(immval_c ~ hispanic_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)
establish_pathway_coef <- unname(establish_pathway$coefficients[2])
establish_pathway_conf <- confint(establish_pathway, 'hispanic_comfort', level=0.95)
establish_pathway_se <- unname(sqrt(diag(vcov(establish_pathway)))[2])
m4 <- lm(immval_c ~ hispanic_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)

# Increase security along border to reduce illegal crossings
increase_sec_reduce_crossings <- lm(immval_d ~ hispanic_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)
increase_sec_reduce_crossings_coef <- unname(increase_sec_reduce_crossings$coefficients[2])
increase_sec_reduce_crossings_conf <- confint(increase_sec_reduce_crossings, 'hispanic_comfort', level=0.95)
increase_sec_reduce_crossings_se <- unname(sqrt(diag(vcov(increase_sec_reduce_crossings)))[2])
m5 <- lm(immval_d ~ hispanic_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)

# Improve the security of the country’s borders
improve_sec_borders <- lm(immval_e ~ hispanic_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)
improve_sec_borders_coef <- unname(improve_sec_borders$coefficients[2])
improve_sec_borders_conf <- confint(improve_sec_borders, 'hispanic_comfort', level=0.95)
improve_sec_borders_se <- unname(sqrt(diag(vcov(improve_sec_borders)))[2])
m6 <- lm(immval_e ~ hispanic_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)

# Congress passing law granting dreamers legal status
daca <- lm(daca2018 ~ hispanic_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)
daca_coef <- unname(daca$coefficients[2])
daca_conf <- confint(daca, 'hispanic_comfort', level = 0.95)
daca_se <- unname(sqrt(diag(vcov(daca)))[2])
m7 <- lm(daca2018 ~ hispanic_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)

# US has responsibility for Central American asylum seekers
asylum_central <- lm(asylumre ~ hispanic_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)
asylum_central_coef <- unname(asylum_central$coefficients[2])
asylum_central_conf <- confint(asylum_central, 'hispanic_comfort', level=0.95)
asylum_central_se <- unname(sqrt(diag(vcov(asylum_central)))[2])
m8 <- lm(asylumre ~ hispanic_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)

### Making paper table
 stargazer::stargazer(m1, m2, m3, m4, m5, m6, m7, m8, column.sep.width = "10pt")

##### creating American models

# Are immigrants a burden (1) or benefit (-1)
immig_burden_a <- lm(immburdn ~ american_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)
immig_burden_coef_a <- unname(immig_burden_a$coefficients[2])
immig_burden_conf_a <- confint(immig_burden_a, 'american_comfort', level=0.95)
immig_burden_se_a <- unname(sqrt(diag(vcov(immig_burden_a)))[2])
m1a <- lm(immburdn ~ american_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)

# Taking in refugees from war
take_in_ref_a <- lm(immval_a ~ american_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)
take_in_ref_coef_a <- unname(take_in_ref_a$coefficients[2])
take_in_ref_conf_a <- confint(take_in_ref_a, 'american_comfort', level=0.95)
take_in_ref_se_a <- unname(sqrt(diag(vcov(take_in_ref_a)))[2])
m2a <- lm(immval_a ~ american_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)

# Increase deportations of illegal immigrants in US
increase_deportations_a <- lm(immval_b ~ american_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)
increase_deportations_coef_a <- unname(increase_deportations_a$coefficients[2])
increase_deportations_conf_a <- confint(increase_deportations_a, 'american_comfort', level=0.95)
increase_deportations_se_a <- unname(sqrt(diag(vcov(increase_deportations_a)))[2])
m3a <- lm(immval_b ~ american_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)

# Establish pathway for illegal immigrants
establish_pathway_a <- lm(immval_c ~ american_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)
establish_pathway_coef_a <- unname(establish_pathway_a$coefficients[2])
establish_pathway_conf_a <- confint(establish_pathway_a, 'american_comfort', level=0.95)
establish_pathway_se_a <- unname(sqrt(diag(vcov(establish_pathway_a)))[2])
m4a <- lm(immval_c ~ american_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)

# Increase security along border to reduce illegal crossings
increase_sec_reduce_crossings_a <- lm(immval_d ~ american_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)
increase_sec_reduce_crossings_coef_a <- unname(increase_sec_reduce_crossings_a$coefficients[2])
increase_sec_reduce_crossings_conf_a <- confint(increase_sec_reduce_crossings_a, 'american_comfort', level=0.95)
increase_sec_reduce_crossings_se_a <- unname(sqrt(diag(vcov(increase_sec_reduce_crossings_a)))[2])
m5a <- lm(immval_d ~ american_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)

# Improve the security of the country’s borders
improve_sec_borders_a <- lm(immval_e ~ american_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)
improve_sec_borders_coef_a <- unname(improve_sec_borders_a$coefficients[2])
improve_sec_borders_conf_a <- confint(improve_sec_borders_a, 'american_comfort', level=0.95)
improve_sec_borders_se_a <- unname(sqrt(diag(vcov(improve_sec_borders_a)))[2])
m6a <- lm(immval_e ~ american_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)

# Congress passing law granting dreamers legal status
daca_a <- lm(daca2018 ~ american_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)
daca_coef_a <- unname(daca_a$coefficients[2])
daca_conf_a <- confint(daca_a, 'american_comfort', level = 0.95)
daca_se_a <- unname(sqrt(diag(vcov(daca_a)))[2])
m7a <- lm(daca2018 ~ american_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)

# US has responsibility for Central American asylum seekers
asylum_central_a <- lm(asylumre ~ american_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)
asylum_central_coef_a <- unname(asylum_central_a$coefficients[2])
asylum_central_conf_a <- confint(asylum_central_a, 'american_comfort', level=0.95)
asylum_central_se_a <- unname(sqrt(diag(vcov(asylum_central_a)))[2])
m8a <- lm(asylumre ~ american_comfort + f_party_ + f_educca + f_sex + f_agecat + worrydpo + knowdpor + pd, data = dat, weights = weight_w)

### Making paper table
stargazer::stargazer(m1a, m2a, m3a, m4a, m5a, m6a, m7a, m8a, column.sep.width = "10pt")


### creating the table
mean <- c(take_in_ref_coef, increase_deportations_coef, establish_pathway_coef,
          increase_sec_reduce_crossings_coef, improve_sec_borders_coef, immig_burden_coef, asylum_central_coef, daca_coef)
mean2 <- c(take_in_ref_coef_a, increase_deportations_coef_a, establish_pathway_coef_a,
           increase_sec_reduce_crossings_coef_a, improve_sec_borders_coef_a, immig_burden_coef_a, asylum_central_coef_a, daca_coef_a)
lower <- c(take_in_ref_conf[1], increase_deportations_conf[1], establish_pathway_conf[1],
           increase_sec_reduce_crossings_conf[1], improve_sec_borders_conf[1], immig_burden_conf[1], asylum_central_conf[1], daca_conf[1])
lower2 <- c(take_in_ref_conf_a[1], increase_deportations_conf_a[1], establish_pathway_conf_a[1],
            increase_sec_reduce_crossings_conf_a[1], improve_sec_borders_conf_a[1], immig_burden_conf_a[1], asylum_central_conf_a[1], daca_conf_a[1])
upper <- c(take_in_ref_conf[2], increase_deportations_conf[2], establish_pathway_conf[2],
            increase_sec_reduce_crossings_conf[2], improve_sec_borders_conf[2], immig_burden_conf[2], asylum_central_conf[2], daca_conf[2])
upper2 <- c(take_in_ref_conf_a[2], increase_deportations_conf_a[2], establish_pathway_conf_a[2],
            increase_sec_reduce_crossings_conf_a[2], improve_sec_borders_conf_a[2], immig_burden_conf_a[2], asylum_central_conf_a[2], daca_conf_a[2])
se <- c(take_in_ref_se, increase_deportations_se, establish_pathway_se, increase_sec_reduce_crossings_se,
        improve_sec_borders_se, immig_burden_se, asylum_central_se, daca_se)
se2 <- c(take_in_ref_se_a, increase_deportations_se_a, establish_pathway_se_a, increase_sec_reduce_crossings_se_a,
        improve_sec_borders_se_a, immig_burden_se_a, asylum_central_se_a, daca_se_a)

Question <- c("Taking in civilian refugees escaping war",
               "Increasing deportations of illegal immigrants",
               "Establishing pathway to citizenship for illegal immigrants",
               "Increasing security along border to reduce crossings",
               "Improving the security of the country’s borders",
               "Immigrants today are a burden on our country",
               "U.S. has responsibility to take in asylum seekers from Cent. Amer.",
               "Law granting children permanent legal status?")

meta <- cbind(Question, mean,lower, upper, se)
meta <- as.data.frame(meta)
meta <- meta %>%
  mutate(mean = as.numeric(mean),
         lower = as.numeric(lower),
         upper = as.numeric(upper),
         se = as.numeric(se) * 20) %>%
  add_row(Question = "How important is the following to U.S. immigration policy?", .before = 1) %>%
  add_row(Question = "Do you agree...", .before = 7) %>%
  add_row(Question = "Would you favor...", .before = 10)
meta$`Hispanic ID Comfort Score Effect` <- paste(rep(" ", 5), collapse = " ")
meta$Question <- ifelse(is.na(meta$mean),
                         meta$Question,
                         paste0("     ", meta$Question))

meta2 <- cbind(Question, mean2, lower2, upper2, se2)
meta2 <- as.data.frame(meta2)
meta2 <- meta2 %>%
  mutate(mean2 = as.numeric(mean2),
         lower2 = as.numeric(lower2),
         upper2 = as.numeric(upper2),
         se2 = as.numeric(se2) * 20) %>%
  add_row(Question = "How important is the following to U.S. immigration policy?", .before = 1) %>%
  add_row(Question = "Do you agree...", .before = 7) %>%
  add_row(Question = "Would you favor...", .before = 10)
meta2$`American ID Comfort Score Effect` <- paste(rep(" ", 5), collapse = " ")
meta2$Question <- ifelse(is.na(meta2$mean2),
                         meta2$Question,
                         paste0("     ", meta2$Question))

tm <- forest_theme(base_size = 12.5,
                   # Confidence interval point shape, line type/color/width
                   ci_pch = 15,
                   ci_col = "#762a83",
                   ci_fill = "black",
                   ci_alpha = 0.8,
                   ci_lty = 1,
                   ci_lwd = 2.5,
                   ci_Theight = 0.2, # Set an T end at the end of CI 
                   # Reference line width/type/color
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   # Vertical line width/type/color
                   vertline_lwd = 1,
                   vertline_lty = "dashed",
                   vertline_col = "grey20",
                   # Change summary color for filling and borders
                   summary_fill = "#4575b4",
                   summary_col = "#4575b4",
                   # Footnote font size/face/color
                   footnote_cex = 0.6,
                   footnote_fontface = "italic",
                   footnote_col = "blue",
                   core=list(bg_params=list(fill = c("#a6bddb", "#f6eff7", "#f6eff7",
                                                     "#f6eff7", "#f6eff7", "#f6eff7", 
                                                     "#a6bddb", "#f6eff7", "#f6eff7",
                                                     "#a6bddb", "#f6eff7"))),
                   colhead=list(fg_params=list(hjust=c(3.95,0.5), x=0.55)),
                   title_just = "center",
                   title_cex = 1.5,
                   title_fontface = "bold",
                   title_col = "black")


p <- forest(meta[,c(1, 6)],
            est = meta$mean, 
            lower = meta$lower,
            upper = meta$upper,
            sizes = meta$se,
            ci_column = 2,
            ref_line = 0,
            arrow_lab = c("Not Important/Disagree", "Important/Agree"),
            xlim = c(-0.15, 0.1),
            ticks_at = c(-.07, 0, 0.05),
            footnote = "Data from Pew American Trends Panel Wave 58 Fielded Dec 2019",
            title = "How does Identity Expression Relate to Attitudes Toward Immigration?",
            theme = tm)

p

p2 <- forest(meta2[,c(1, 6)],
             est = meta2$mean2, 
             lower = meta2$lower2,
             upper = meta2$upper2,
             sizes = meta2$se2,
             ci_column = 2,
             ref_line = 0,
             arrow_lab = c("Not Important/Disagree", "Important/Agree"),
             xlim = c(-0.25, 0.25),
             ticks_at = c(-0.2, -0.1, 0, 0.1, 0.2),
             footnote = "Data from Pew American Trends Panel Wave 58 Fielded Dec 2019",
             title = "How does Identity Expression Relate to Attitudes Toward Immigration?",
             theme = tm)

p2
