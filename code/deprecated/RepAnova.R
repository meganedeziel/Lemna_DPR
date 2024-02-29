###############################
### REPEATED MEASURES ANOVA ###
###############################

library (rstatix)
library (emmeans)
library (multcomp)

# REPEATED MEASURES ANOVA
Rep_Anova <- RYT_Time %>%
  group_by(Plot) %>%
  anova_test(dv = RYT, wid = Block, within = Inv) %>%
  adjust_pvalue(method="fdr") %>%
  add_significance("p.adj") %>%
  get_anova_table()
data.frame(Rep_Anova)

# RUN PAIRWISE COMPARISONS FOR THE PLOTS WITH SIGNIFICANCE EFFECT OF YEAR. 

# AN EXAMPLE WITH THE 12N
plot_12N <- subset (RYT_Time, Plot=="12N")

# get (adjusted) weight means per year 
RYT_12N_lme <- lme(RYT ~ Inv, random = ~1 | Block/Inv, data = plot_12N) 
# This is another Rep.Meas.Anova for 12N. You need this to the next steps.

model_means <- emmeans(object = RYT_12N_lme,
                       specs = "Inv")

# This gives you the results of the comparisons between years in letters.
model_means_cld <- cld(object = model_means,
                       adjust = "sidak",
                       Letters = letters,
                       alpha = 0.05,
                       sort = FALSE)

model_means_cld

# Two-tailed t-tests to determine when (INV) RYT was differennt from 1
stat.test <- plot_12N %>%
  group_by(Inv) %>%
  t_test(RYT ~ 1, mu=1) %>%
  add_significance()
stat.test
