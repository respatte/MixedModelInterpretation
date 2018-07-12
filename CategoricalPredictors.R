# LIBRARY IMPORTS ==================================================================================
library(lme4)
library(tidyverse)
library(RColorBrewer)
source("geom_flat_violin.R")

# DATA GENERATION ==================================================================================
df <- tibble(Participant = parse_factor(rep(1:100, 2), levels = NULL),
             Intervention = parse_factor(rep(c("Video Game", "Mindfulness Meditation"), 100),
                                         levels = c("Video Game",                # Reference level
                                                    "Mindfulness Meditation")),  # Comparison level
             PrePost = parse_factor(c(rep("Pre Intervention", 100),
                                      rep("Post Intervention", 100)),
                                    levels = c("Pre Intervention",                # Reference level
                                               "Post Intervention")),             # Comparison level
             StressLevel = 150 +
               0      * (Intervention == "Mindfulness Meditation") +
               (-2)   * (PrePost == "Post Intervention") +
               (-100) * (Intervention=="Mindfulness Meditation")*(PrePost=="Post Intervention") +
               rnorm(200, 0, 25) +      # General variance
               rep(rnorm(100, 0, 5), 2) # Participant-specific variance
)
# The experiment is as follow: we measure stress levels in participants before and after an
# intervention. The intervnetion consists of either a session of mindfulness meditation,
# or a session of playing video games.

# DATA DESCRIPTION =================================================================================
# Plot
barplot <- ggplot(data = df,
                  aes(y = StressLevel,
                      x = PrePost,
                      fill = Intervention)) +
  geom_bar(stat = "summary", fun.y = "mean",
           position = "dodge",
           width = .5) +
  geom_errorbar(stat = "summary", fun.data = "mean_se",
                width = .25, position = position_dodge(.5),
                lwd = .2) +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw() +
  theme(legend.position = "top",
        legend.key.size = unit(.5, "lines"),
        legend.box.spacing = unit(0, "mm"),
        text = element_text(size = 8))
ggsave("CategoricalBarplot.pdf", barplot,
       width = 3, height = 2, dpi = 600)

raincloud <- ggplot(data = df,
                    aes(x = Intervention,
                        y = StressLevel,
                        fill = Intervention,
                        colour = Intervention)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0),
                   alpha = .7, width = .7,
                   colour = NA) +
  geom_point(position = position_jitter(width = .15, height = 0),
             size = .5, alpha = .5, show.legend = F) +
  geom_boxplot(width = .1, show.legend = F,
               outlier.shape = NA, alpha = .2,
               colour = "black", lwd = .2) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  coord_flip() + theme_bw() + facet_grid(.~PrePost) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top",
        legend.key.size = unit(.5, "lines"),
        legend.box.spacing = unit(0, "mm"),
        text = element_text(size = 8))
ggsave("CategoricalRaincloud.pdf", raincloud,
       width = 4, height = 2, dpi = 600)

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
