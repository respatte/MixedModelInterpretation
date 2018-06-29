# LIBRARY IMPORTS ==================================================================================
library(lme4)
library(tidyverse)

# DATA GENERATION ==================================================================================
df <- tibble(Participant = parse_factor(rep(1:100, 2), levels = NULL),
             Intervention = parse_factor(rep(c("Video Game", "Mindfulness Meditation"), 100),
                                         levels = c("Video Game",                # Reference level
                                                    "Mindfulness Meditation")),  # Comparison level
             PrePost = parse_factor(c(rep("PreIntervention", 100),
                                      rep("PostIntervention", 100)),
                                    levels = c("PreIntervention",                # Reference level
                                               "PostIntervention")),             # Comparison level
             StressLevel = 150 +
               0      * (Intervention == "Mindfulness Meditation") +
               (-2)   * (PrePost == "PostIntervention") +
               (-100) * (Intervention == "Mindfulness Meditation")*(PrePost == "PostIntervention") +
               rnorm(200, 0, 25) +      # General variance
               rep(rnorm(100, 0, 5), 2) # Participant-specific variance
)
# The experiment is as follow: we measure stress levels in participants before and after an
# intervention. The intervnetion consists of either a session of mindfulness meditation,
# or a session of playing video games.

# DATA DESCRIPTION =================================================================================
# Plot
bplot <- ggplot(data = df,
                aes(y = StressLevel,
                    x = PrePost,
                    fill = Intervention)) +
  geom_bar(stat = "summary", fun.y = "mean",
           position = "dodge") +
  geom_errorbar(stat = "summary", fun.data = "mean_se",
                width = .5, position = position_dodge(.9))
(bplot)

# DATA ANALYSIS ====================================================================================
# Regular ANOVA
stress.anova <- aov(StressLevel ~ Intervention*PrePost,
                    data = df)
(summary(stress.anova))

# Regular lm: same computations as ANOVA but different way of showing results
## From the aov function help:
### The main difference from lm is in the way print, summary and so on handle the fit:
### this is expressed in the traditional language of the analysis of variance
### rather than that of linear models.
stress.lm <- lm(StressLevel ~ Intervention*PrePost,
                data = df)
(summary(stress.lm))
## Result breakdown:
### Factor levels:
#### Intervention = 0 for VideoGame, 1 for MindfulnessMeditation
#### PrePost = 0 for PreIntervention, 1 for PostIntervention
### General formula is:
#### StressLevel = 151.665 + (-2.471)*Intervention + (-6.887)*PrePost + (-95.381)*Intervention*PrePost
### StressLevel of participants in the VideoGame group pre-intervention is given when
### Intervention = 0 and PrePost = 0:
#### StressLevel(VideoGame, PreIntervention) = 151.665 + (-2.471)*0 + (-6.887)*0 + (-95.381)*0*0
####                                         = 151.665 (Intercept = mean for VideoGame pre-int.)
### StressLevel of participants in the Meditation group pre-intervention is given when
### Intervention = 1 and PrePost = 0:
#### StressLevel(Meditation, Pre) = 151.665 + (-2.471)*1 + (-6.887)*0 + (-95.381)*1*0
####                              = 151.665 - 2.471 (diff between groups, pre-intervention)
### StressLevel of participants in the VideoGame group post-intervention is given when
### Intervention = 0 and PrePost = 1:
#### StressLevel(Meditation, Pre) = 151.665 + (-2.471)*0 + (-6.887)*1 + (-95.381)*0*0
####                              = 151.665 - 6.887 (diff after intervention, VideoGame group only)
### StressLevel of participants in the Meditation group post-intervention is given when
### Intervention = 1 and PrePost = 0:
#### StressLevel(Meditation, Pre) = 151.665 + (-2.471)*1 + (-6.887)*1 + (-95.381)*1*1
####                              = 151.665 - 2.471 - 6.887 - 95.381
####                                                          (extra amount of difference post-int.
####                                                           when Meditation, compared to  the
####                                                           diff. pre-int. when Meditation)

# Mixed-effect model, just to compare output to ANOVA output and lm output
stress.lmer <- lmer(StressLevel ~ Intervention*PrePost + (1 | Participant),
                    data = df)
(summary(stress.lmer))

# Re-run aov and lm with different factor order for Intervention
df2 <- df
df2$Intervention <- relevel(df2$Intervention, ref = "Mindfulness Meditation") # New ref level
stress.anova2 <- aov(StressLevel ~ Intervention*PrePost,
                     data = df)
(summary(stress.anova2)) # No difference
stress.lm2 <- lm(StressLevel ~ Intervention*PrePost,
                data = df2)
(summary(stress.lm2))    # Now main effect of PrePost because this is for participants in the
                         # Mindfulness Meditation group only
## We can run the same result breakdown as before, but now we have different factor levels:
### Intervention = 0 for MindfulnessMeditation, 1 for VideoGame
