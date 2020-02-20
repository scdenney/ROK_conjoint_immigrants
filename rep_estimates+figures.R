### models and graphs for Ethnicities (journal) ###
## code by Steven Denney - steven.denney@utoronto.ca

## load packages
library(tidyverse)
library(reshape)
library(cregg)

## read .csv files
#read.csv()

#### OLS models from manuscript - figures 2 and 4-6 ####

amces_baseline <- cj(skconjoint, Immigrant_preferred ~ Application + Country + Language + Profession + Employment + Gender + Ethnicity, 
                     id = ~Respid, estimate = "amce")

amces_college <- cj(skconjoint, Immigrant_preferred ~ Application + Country + Language + Profession + Employment + Gender + Ethnicity, 
                    id = ~Respid, estimate = "amce", 
                    by = ~College)

amces_stateid <- cj(skconjoint, Immigrant_preferred ~ Application + Country + Language + Profession + Employment + Gender + Ethnicity, 
                    id = ~Respid, estimate = "amce", 
                    by = ~State.ID2)

amces_polid <- cj(skconjoint, Immigrant_preferred ~ Application + Country + Language + Profession + Employment + Gender + Ethnicity, 
                  id = ~Respid, estimate = "amce", 
                  by = ~Political.ID2)

## spacing for graphs
pdamce <- position_dodge(0.6) # move them .05 to the left and right


{
  
  (main <- ggplot(amces_baseline, aes(fct_rev(level), estimate)) +
     theme_light() +
     geom_point(aes(color=feature), size=1.5) + 
     geom_errorbar(aes(ymin=lower, ymax=upper, color=feature), width=0) +
     scale_color_manual(values = c("#CCA20C","#F86624","#662E9B","#E23482","#43BCCD","#82C458",
                                   "#79133E")) +
     coord_flip() +
     labs(y="",
          x="") +
     facet_grid(feature~., switch = "y", scales = "free_y", space = "free_y") +
     theme(legend.position = "none") )
  
  ggsave("Baseline_m.pdf", width = 9, height = 9)
  
  (main_g <- ggplot(amces_baseline, aes(fct_rev(level), estimate)) +
      theme_light() +
      geom_point(size=1.5) + 
      geom_errorbar(aes(ymin=lower, ymax=upper), width=0) +
      coord_flip() +
      labs(y="",
           x="") +
      facet_grid(feature~., switch = "y", scales = "free_y", space = "free_y") +
      theme(legend.position = "none") +
      scale_color_grey() )
  
  ggsave("Baseline_m_g.pdf", width = 9, height = 9)
  
}

# education // amces_college

{
  (pedu <- ggplot(amces_college, aes(level, estimate, colour=College)) + theme_light() +
     geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=pdamce) +
     coord_flip() +
     labs(y="",
          x="") +
     geom_point(position=pdamce) +
     facet_grid(feature~., switch = "y", scales = "free_y", space = "free_y") +
     guides(col = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE),
            guide_legend("")) +
     theme(legend.title = element_blank()) )
  
  ggsave("Baseline_x_edu.pdf", width = 9, height = 9)
  
  pedu_g <- pedu + scale_color_grey(start = 0.1, end = 0.65)
  
  ggsave("Baseline_x_edu_g.pdf", width = 9, height = 9)
  
}


# national identity // amces_stateid

{
  (pid <- ggplot(amces_stateid, aes(level, estimate, colour=State.ID2)) + theme_light() +
     geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=pdamce) +
     coord_flip() +
     labs(y="",
          x="") +
     geom_point(position=pdamce) +
     facet_grid(feature~., switch = "y", scales = "free_y", space = "free_y") +
     guides(col = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE),
            guide_legend("")) +
     theme(legend.title = element_blank()) )
    
    ggsave("Baseline_x_natid.pdf", width = 9, height = 9)
  
  pid_g <- pid + scale_color_grey(start = 0.1, end = 0.65)
  
  ggsave("Baseline_x_natid_g.pdf", width = 9, height = 9)
  
}


# political identification // amces_polid

{
  (ppol <- ggplot(amces_polid, aes(level, estimate, colour=Political.ID2)) + theme_light() +
     geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=pdamce) +
     coord_flip() +
     labs(y="",
          x="") +
     geom_point(position=pdamce) +
     facet_grid(feature~., switch = "y", scales = "free_y", space = "free_y") +
     guides(col = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE),
            guide_legend("")) +
     theme(legend.title = element_blank()) )
    
    ggsave("Baseline_x_polid.pdf", width = 9, height = 9)
  
  ppol_g <- ppol + scale_color_grey(start = 0.1, end = 0.65)
  
  ggsave("Baseline_x_polid_g.pdf", width = 9, height = 9)
  
}


#### For 
#### For figure 3 (estimated probabilities) ####

### conditional probilities
## est. prob. of being preferred by conditions/immigrant attributes
## https://github.com/DeclareDesign/estimatr
# for clustered SEs (+more)


library(estimatr)

m2 <- lm_robust(Immigrant_preferred ~ Application + Country + Language + Profession + Employment + Gender + Ethnicity, 
                data=skconjoint, clusters=Respid, ci=T)
summary(m2)

pp_profiles <- as.data.frame(matrix(ncol = 4, nrow = 12))
names(pp_profiles) <- c("Profile", "Admit", "CI", "Country")
pp_profiles$Profile <- c("Doctor \n Will Look for Work", 
                         "Doctor \n No Plans to Work", 
                         "Agricultural Worker \n Will Look for Work", 
                         "Agricultural Worker \n No Plans to Work")

pp_profiles$Country[1:4]="North Korea"
pp_profiles$Country[5:8]="United States"
pp_profiles$Country[9:12]="Vietnam"


nk1 <- with(skconjoint, data.frame(Application = 'Resettle',
                                   Country = 'North Korea',
                                   Language = 'Fluent Korean',
                                   Profession = 'Doctor',
                                   Employment = 'Will look for work after arrival',
                                   Gender = 'Female',
                                   Ethnicity = 'Ethnic Korean')) ##


preds1 <- predict(m2, nk1, se.fit = F, interval = c("confidence"), alpha = 0.05)
preds1 <- do.call(rbind.data.frame, preds1)

pp_profiles$Admit[1] = preds1$fit
pp_profiles$CI[1] = (preds1$fit) - (preds1$lwr)

# nk2
nk2 <- with(skconjoint, data.frame(Application = 'Resettle',
                                   Country = 'North Korea',
                                   Language = 'Fluent Korean',
                                   Profession = 'Doctor',
                                   Employment = 'No plans to look for work',
                                   Gender = 'Female',
                                   Ethnicity = 'Ethnic Korean')) ##

preds2 <- predict(m2, nk2, se.fit = FALSE, interval = c("confidence"), alpha = 0.05)
preds2 <- do.call(rbind.data.frame, preds2)

pp_profiles$Admit[2] = preds2$fit
pp_profiles$CI[2] = (preds2$fit) - (preds2$lwr)

# nk3
nk3 <- with(skconjoint, data.frame(Application = 'Resettle',
                                   Country = 'North Korea',
                                   Language = 'Fluent Korean',
                                   Profession = 'Agricultural worker',
                                   Employment = 'Will look for work after arrival',
                                   Gender = 'Female',
                                   Ethnicity = 'Ethnic Korean')) ##

preds3 <- predict(m2, nk3, se.fit = FALSE, interval = c("confidence"), alpha = 0.05)

preds3 <- do.call(rbind.data.frame, preds3)

pp_profiles$Admit[3] = preds3$fit
pp_profiles$CI[3] = (preds3$fit) - (preds3$lwr)

# nk4
nk4 <- with(skconjoint, data.frame(Application = 'Resettle',
                                   Country = 'North Korea',
                                   Language = 'Fluent Korean',
                                   Profession = 'Agricultural worker',
                                   Employment = 'No plans to look for work',
                                   Gender = 'Female',
                                   Ethnicity = 'Ethnic Korean')) ##

preds4 <- predict(m2, nk4, se.fit = FALSE, interval = c("confidence"), alpha = 0.05)
preds4 <- do.call(rbind.data.frame, preds4)

pp_profiles$Admit[4] = preds4$fit
pp_profiles$CI[4] = (preds4$fit) - (preds4$lwr)

## United States
us1 <- with(skconjoint, data.frame(Application = 'Resettle',
                                   Country = 'United States',
                                   Language = 'Fluent Korean',
                                   Profession = 'Doctor',
                                   Employment = 'Will look for work after arrival',
                                   Gender = 'Female',
                                   Ethnicity = 'Ethnic Korean')) ##

preds5 <- predict(m2, us1, se.fit = FALSE, interval = c("confidence"), alpha = 0.05)
preds5 <- do.call(rbind.data.frame, preds5)

pp_profiles$Admit[5] = preds5$fit
pp_profiles$CI[5] = (preds5$fit) - (preds5$lwr)

# us2
us2 <- with(skconjoint, data.frame(Application = 'Resettle',
                                   Country = 'United States',
                                   Language = 'Fluent Korean',
                                   Profession = 'Doctor',
                                   Employment = 'No plans to look for work',
                                   Gender = 'Female',
                                   Ethnicity = 'Ethnic Korean')) ##

preds6 <- predict(m2, us2, se.fit = FALSE, interval = c("confidence"), alpha = 0.05)
preds6 <- do.call(rbind.data.frame, preds6)

pp_profiles$Admit[6] = preds6$fit
pp_profiles$CI[6] = (preds6$fit) - (preds6$lwr)

# us3
us3 <- with(skconjoint, data.frame(Application = 'Resettle',
                                   Country = 'United States',
                                   Language = 'Fluent Korean',
                                   Profession = 'Agricultural worker',
                                   Employment = 'Will look for work after arrival',
                                   Gender = 'Female',
                                   Ethnicity = 'Ethnic Korean')) ##

preds7 <- predict(m2, us3, se.fit = FALSE, interval = c("confidence"), alpha = 0.05)
preds7 <- do.call(rbind.data.frame, preds7)

pp_profiles$Admit[7] = preds7$fit
pp_profiles$CI[7] = (preds7$fit) - (preds7$lwr)


# us4
us4 <- with(skconjoint, data.frame(Application = 'Resettle',
                                   Country = 'United States',
                                   Language = 'Fluent Korean',
                                   Profession = 'Agricultural worker',
                                   Employment = 'No plans to look for work',
                                   Gender = 'Female',
                                   Ethnicity = 'Ethnic Korean')) ##

preds8 <- predict(m2, us4, se.fit = FALSE, interval = c("confidence"), alpha = 0.05)
preds8 <- do.call(rbind.data.frame, preds8)

pp_profiles$Admit[8] = preds8$fit
pp_profiles$CI[8] = (preds8$fit) - (preds8$lwr)


## Vietnam
viet1 <- with(skconjoint, data.frame(Application = 'Resettle',
                                     Country = 'Vietnam',
                                     Language = 'Fluent Korean',
                                     Profession = 'Doctor',
                                     Employment = 'Will look for work after arrival',
                                     Gender = 'Female',
                                     Ethnicity = 'Ethnic Korean')) ##

preds9 <- predict(m2, viet1, se.fit = FALSE, interval = c("confidence"), alpha = 0.05)
preds9 <- do.call(rbind.data.frame, preds9)

pp_profiles$Admit[9] = preds9$fit
pp_profiles$CI[9] = (preds9$fit) - (preds9$lwr)

# viet2
viet2 <- with(skconjoint, data.frame(Application = 'Resettle',
                                     Country = 'Vietnam',
                                     Language = 'Fluent Korean',
                                     Profession = 'Doctor',
                                     Employment = 'No plans to look for work',
                                     Gender = 'Female',
                                     Ethnicity = 'Ethnic Korean')) ##

preds10 <- predict(m2, viet2, se.fit = FALSE, interval = c("confidence"), alpha = 0.05)
preds10 <- do.call(rbind.data.frame, preds10)

pp_profiles$Admit[10] = preds10$fit
pp_profiles$CI[10] = (preds10$fit) - (preds10$lwr)

# viet3
viet3 <- with(skconjoint, data.frame(Application = 'Resettle',
                                     Country = 'Vietnam',
                                     Language = 'Fluent Korean',
                                     Profession = 'Agricultural worker',
                                     Employment = 'Will look for work after arrival',
                                     Gender = 'Female',
                                     Ethnicity = 'Ethnic Korean')) ##

preds11 <- predict(m2, viet3, se.fit = FALSE, interval = c("confidence"), alpha = 0.05)
preds11 <- do.call(rbind.data.frame, preds11)

pp_profiles$Admit[11] = preds11$fit
pp_profiles$CI[11] = (preds11$fit) - (preds11$lwr)

# viet4
viet4 <- with(skconjoint, data.frame(Application = 'Resettle',
                                     Country = 'Vietnam',
                                     Language = 'Fluent Korean',
                                     Profession = 'Agricultural worker',
                                     Employment = 'No plans to look for work',
                                     Gender = 'Female',
                                     Ethnicity = 'Ethnic Korean')) ##

preds12 <- predict(m2, viet4, se.fit = FALSE, interval = c("confidence"), alpha = 0.05)
preds12 <- do.call(rbind.data.frame, preds12)

pp_profiles$Admit[12] = preds12$fit
pp_profiles$CI[12] = (preds12$fit) - (preds12$lwr)

## graph profiles

ggplot(pp_profiles, aes(x=reorder(Profile, +Admit), y=Admit, group=1)) +
  geom_errorbar(aes(ymin=Admit-CI, ymax=Admit+CI), width=.3) +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format(), limits=c()) +
  labs(y = "Pr(Immigrant Preferred for Admission)", x = "Profile") + 
  labs(title="Estimated Probability of Being Preferred for Admission \n for Selected Country Profiles") +
  coord_flip() +
  facet_wrap(~Country) +
  theme_light() ## not used in manuscript

pd <- position_dodge(0.4) # move the things .05 to the left and right

pr_imm = ggplot(pp_profiles, aes(x=Profile, y=Admit, colour=Country)) + theme_light() +
  geom_errorbar(aes(ymin=Admit-CI, ymax=Admit+CI), width=0, position=pd) +
  scale_color_manual(values = c("#CCA20C","#F86624","#662E9B")) +
  scale_y_continuous(labels = scales::percent_format(), limits=c()) +
  labs(x = "", y = "") + 
  geom_point(position=pd) + 
  labs(title = "") +
  theme(legend.title = element_blank())
  

pr_imm_g = ggplot(pp_profiles, aes(x=Profile, y=Admit, colour=Country)) + theme_light() +
  geom_errorbar(aes(ymin=Admit-CI, ymax=Admit+CI), width=0, position=pd) +
  scale_y_continuous(labels = scales::percent_format(), limits=c()) +
  labs(x = "", y = "") + 
  geom_point(position=pd) + 
  labs(title = "") +
  theme(legend.title = element_blank()) +
  scale_color_grey()

#### Models and figures for supplementary information ####

amces_age <- cj(skconjoint, Immigrant_preferred ~ Application + Country + Language + Profession + Employment + Gender + Ethnicity, 
                id = ~Respid, estimate = "amce", 
                by = ~Age_cohorts)

amces_gender <- cj(skconjoint, Immigrant_preferred ~ Application + Country + Language + Profession + Employment + Gender + Ethnicity, 
                   id = ~Respid, estimate = "amce", 
                   by = ~Resp_gender)

amces_income <- cj(skconjoint, Immigrant_preferred ~ Application + Country + Language + Profession + Employment + Gender + Ethnicity, 
                   id = ~Respid, estimate = "amce", 
                   by = ~Income_f)

amces_immigrants <- cj(skconjoint, Immigrant_preferred ~ Application + Country + Language + Profession + Employment + Gender + Ethnicity, 
                       id = ~Respid, estimate = "amce", 
                       by = ~Immigrants.Geo)

amces_generation <- cj(skconjoint, Immigrant_preferred ~ Application + Country + Language + Profession + Employment + Gender + Ethnicity, 
                       id = ~Respid, estimate = "amce", 
                       by = ~Generation)

amce_baseline_rating <- cj(skconjoint, Immigrant_supported ~ Application + Country + Language + Profession + Employment + Gender + Ethnicity, 
                           id = ~Respid, estimate = "amce") ## based on rating

amces_profiles <- cj(skconjoint, Immigrant_preferred ~ Application + Country + Language + Profession + Employment + Gender + Ethnicity, 
                     id = ~Respid, estimate = "amce",
                     by = ~Profiles)

amces_monitor <- cj(skconjoint, Immigrant_preferred ~ Application + Country + Language + Profession + Employment + Gender + Ethnicity, 
                    id = ~Respid, estimate = "amce",
                    by = ~Monitoring)

amces_atypical <- cj(skconjoint, Immigrant_preferred ~ Application + Country + Language + Profession + Employment + Gender + Ethnicity, 
                     id = ~Respid, estimate = "amce", 
                     by = ~Atypical)

# SI graphs

{
  (age <- plot(amces_age) + ggplot2::facet_wrap(~BY, ncol=5)+
     ggplot2::theme_light() + ggplot2::theme(legend.position="none") +
     labs(x="Effect on Pr(Immigrant Preferred for Admission") )
  
  ggsave("SI1_Baseline_x_age.png", width = 9, height = 9)
  
  (gender <- ggplot(amces_gender, aes(x=level, y=estimate, colour=Resp_gender)) + theme_light() +
      geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=pdamce) +
      coord_flip() +
      labs(y="Effect on Pr(Immigrant Preferred for Admission)",
           x="") +
      geom_point(position=pdamce) +
      facet_grid(feature~., switch = "y", scales = "free_y", space = "free_y") +
      guides(col = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE),
             guide_legend("")) +
      theme(legend.title = element_blank()) )
  
  ggsave("SI2_Baseline_x_gender.png", width = 9, height = 9)
  
  (income <- ggplot(amces_income, aes(x=level, y=estimate, colour=Income_f)) + theme_light() +
      geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=pdamce) +
      coord_flip() +
      labs(y="Effect on Pr(Immigrant Preferred for Admission)",
           x="") +
      geom_point(position=pdamce) +
      facet_grid(feature~., switch = "y", scales = "free_y", space = "free_y") +
      guides(col = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE),
             guide_legend("")) +
      theme(legend.title = element_blank()) )
  
  ggsave("SI3_Baseline_x_income.png", width = 9, height = 9)
  
  (imm <- ggplot(amces_immigrants, aes(x=level, y=estimate, colour=Immigrants.Geo)) + theme_light() +
      geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=pdamce) +
      coord_flip() +
      labs(y="Effect on Pr(Immigrant Preferred for Admission)",
           x="") +
      geom_point(position=pdamce) +
      facet_grid(feature~., switch = "y", scales = "free_y", space = "free_y") +
      guides(col = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE),
             guide_legend("")) +
      theme(legend.title = element_blank()) )
  
  ggsave("SI4_Baseline_x_immigrants.png", width = 9, height = 9)
  
  (gen <- ggplot(amces_generation, aes(x=level, y=estimate, colour=Generation)) + theme_light() +
      geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=pdamce) +
      coord_flip() +
      labs(y="Effect on Pr(Immigrant Preferred for Admission)",
           x="") +
      geom_point(position=pdamce) +
      facet_grid(feature~., switch = "y", scales = "free_y", space = "free_y") +
      guides(col = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE),
             guide_legend("")) +
      theme(legend.title = element_blank()) )
  
  ggsave("SI5_Baseline_x_generations.png", width = 9, height = 9)
  
  (preferred <- ggplot(amce_baseline_rating, aes(fct_rev(level), estimate)) +
      theme_light() +
      geom_point(aes(color=feature), size=1.5) + 
      geom_errorbar(aes(ymin=lower, ymax=upper, color=feature)) +
      scale_color_manual(values = c("#CCA20C","#F86624","#662E9B","#E23482","#43BCCD","#82C458",
                                    "#79133E")) +
      coord_flip() +
      labs(y="Effect on Pr(Immigrant Preferred for Admission)",
           x="") +
      facet_grid(feature~., switch = "y", scales = "free_y", space = "free_y") +
      theme(legend.position = "none") )
  
  ggsave("SI6_Baseline_2.png", width = 9, height = 9)
  
  (profiles <- plot(amces_profiles) + ggplot2::facet_wrap(~BY, ncol=6)+
      ggplot2::theme_light() + ggplot2::theme(legend.position="none") +
    labs(x="Effect on Pr(Immigrant Preferred for Admission") )
  
  ggsave("SI7_Baseline_x_profiles.png", width = 9, height = 9)
  
  (monitor <- ggplot(amces_monitor, aes(x=level, y=estimate, colour=Monitoring)) + theme_light() +
      geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=pdamce) +
      coord_flip() +
      labs(y="Effect on Pr(Immigrant Preferred for Admission)",
           x="") +
      geom_point(position=pdamce) +
      facet_grid(feature~., switch = "y", scales = "free_y", space = "free_y") +
      guides(col = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE),
             guide_legend("")) +
      theme(legend.title = element_blank()) )
  
  ggsave("SI8_Baseline_x_monitor.png", width = 9, height = 9)
  
  (atypical <- ggplot(amces_atypical, aes(x=level, y=estimate, colour=Atypical)) + theme_light() +
      geom_errorbar(aes(ymin=lower, ymax=upper), width=0, position=pdamce) +
      coord_flip() +
      labs(y="Effect on Pr(Immigrant Preferred for Admission)",
           x="") +
      geom_point(position=pdamce) +
      facet_grid(feature~., switch = "y", scales = "free_y", space = "free_y") +
      guides(col = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE),
             guide_legend("")) +
      theme(legend.title = element_blank()) )
  
  ggsave("SI10_Baseline_x_atypical.png", width = 9, height = 9)
}


#### Additional robustness check; randomzing question order ####

amce_baseline_ran <- cj(skconjoint_ran, Immigrant_preferred ~ Application + Country + Language + Profession + Employment + Gender + Ethnicity,
                        id = ~Respid, estimate = "amce")

{
  ggplot(amce_baseline_ran, aes(fct_rev(level), estimate)) +
    theme_light() +
    geom_point(aes(color=feature), size=1.5) + 
    geom_errorbar(aes(ymin=lower, ymax=upper, color=feature)) +
    scale_color_manual(values = c("#CCA20C","#F86624","#662E9B","#E23482","#43BCCD","#82C458",
                                  "#79133E")) +
    coord_flip() +
    labs(y="Effect on Pr(Immigrant Preferred for Admission)",
         x="") +
    facet_grid(feature~., switch = "y", scales = "free_y", space = "free_y") +
    theme(legend.position = "none")
  
  ggsave("SI9_Baseline_x_random.png", width = 9, height = 9)
  
}



#### Labs -- titles + captions ####

# from manuscirpt
main + labs(caption="Figure 2. Effects of immigrant attributes on probability of being preferred for admission. The average component marginal effects 
                    for the randomly generated attributes on the probability of being preferred for admission to South Korea. AMCEs for values of attributes 
                    shown with 95% confidence intervals and based on the baseline OLS model with standard clustered errors.") ## theme(plot.caption = element_text(size = 12))

ggsave("Figure 2. Baseline_m.pdf", width = 9, height = 9)

main_g + labs(caption="Figure 2. Effects of immigrant attributes on probability of being preferred for admission. The average component marginal effects 
                    for the randomly generated attributes on the probability of being preferred for admission to South Korea. AMCEs for values of attributes 
                    shown with 95% confidence intervals and based on the baseline OLS model with standard clustered errors.")

ggsave("Figure 2. Baseline_m_grey.pdf", width = 9, height = 9)

pr_imm + labs(caption="Figure 3. Estimated probability of being preferred for admission by selected profiles. As specified, only occupation and employment plans 
              vary. Estimates are based on the benchmark OLS model with clustered standards errors. The error bars show 95% confidenc intevals.") +
  theme(plot.caption = element_text(hjust=.25))

ggsave("Figure 3. PP_profiles.pdf", width = 9, height = 9)

pr_imm_g + labs(caption="Figure 3. Estimated probability of being preferred for admission by selected profiles. As specified, only occupation and employment plans 
              vary. Estimates are based on the benchmark OLS model with clustered standards errors. The error bars show 95% confidenc intevals.") +
  theme(plot.caption = element_text(hjust=.25))

ggsave("Figure 3. PP_profiles_grey.pdf", width = 9, height = 9)

pedu + labs(caption="Figure 4. Effects of immigrant attributes on probability of being preferred for admission by education levels.
                    The average component marginal effects for the randomly generated attributes on the probability of being preferred for 
                    admission to South Korea. AMCEs for values of attributes shown with 95% confidence intervals and based on the baseline 
                    OLS model with standard clustered errors.") 

ggsave("Figure 4. Baseline_x_edu.pdf", width = 9, height = 9)

pedu_g + labs(caption="Figure 4. Effects of immigrant attributes on probability of being preferred for admission by education levels.
                    The average component marginal effects for the randomly generated attributes on the probability of being preferred for 
                    admission to South Korea. AMCEs for values of attributes shown with 95% confidence intervals and based on the baseline 
                    OLS model with standard clustered errors.") 

ggsave("Figure 4. Baseline_x_edu_grey.pdf", width = 9, height = 9)

pid + labs(caption="Figure 5. Effects of immigrant attributes on probability of being preferred for admission by national identification.
                    The average component marginal effects for the randomly generated attributes on the probability of being preferred for 
                    admission to South Korea. AMCEs for values of attributes shown with 95% confidence intervals and based on the baseline 
                    OLS model with standard clustered errors.") 

ggsave("Figure 5. Baseline_x_natid.pdf", width = 9, height = 9)

pid_g + labs(caption="Figure 5. Effects of immigrant attributes on probability of being preferred for admission by national identification.
                    The average component marginal effects for the randomly generated attributes on the probability of being preferred for 
                    admission to South Korea. AMCEs for values of attributes shown with 95% confidence intervals and based on the baseline 
                    OLS model with standard clustered errors.") 

ggsave("Figure 5. Baseline_x_natid_grey.pdf", width = 9, height = 9)

ppol + labs(caption="Figure 6. Effects of immigrant attributes on probability of being preferred for admission by political identification.
                    The average component marginal effects for the randomly generated attributes on the probability of being preferred for 
                    admission to South Korea. AMCEs for values of attributes shown with 95% confidence intervals and based on the baseline 
                    OLS model with standard clustered errors.") 
 # theme(plot.caption = element_text(hjust=.25))

ggsave("Figure 6. Baseline_x_polid.pdf", width = 9, height = 9)

ppol_g + labs(caption="Figure 6. Effects of immigrant attributes on probability of being preferred for admission by political identification.
                    The average component marginal effects for the randomly generated attributes on the probability of being preferred for 
                    admission to South Korea. AMCEs for values of attributes shown with 95% confidence intervals and based on the baseline 
                    OLS model with standard clustered errors.")

ggsave("Figure 6. Baseline_x_polid_grey.pdf", width = 9, height = 9)



