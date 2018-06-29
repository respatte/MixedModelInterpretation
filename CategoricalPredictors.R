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
               rnorm(200, 0, 25) + # General variance
               rep(rnorm(100, 0, 5), 2)
)

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

# Mixed-effect model, just to compare output to ANOVA output and lm output
stress.lmer <- lmer(StressLevel ~ Intervention*PrePost + (1 | Participant),
                    data = df)
(summary(stress.lm))
