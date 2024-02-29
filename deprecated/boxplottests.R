### Figure
```{r}
NE_data<-NE_data %>%
  mutate(across(c("Composition", "Block", "Days"), 
                as.factor)) #transform Composition, Block, Days as factor
NE_data$Composition<-fct_relevel(NE_data$Composition, c("LmSp", "LmWc", "LmLt", "SpWc", "SpLt", "WcLt", "LmSpWc", "LmSpLt", "SpWcLt", "LmWcLt", "LmSpWcLt"))

ggplot(NE_data[NE_data$Days=="60",], aes(x = Composition, y = NE)) + 
  geom_boxplot(fill="#2D3184", alpha=0.7) +
  geom_point() +
  geom_hline(yintercept=0, linetype='dotted', col = 'red') +
  theme_classic() +
  theme(text=element_text(size=15), axis.text.x = element_text(size = 13, angle = 60, hjust=1), panel.grid.major.y = element_line( size=.1, color="black")) +
  labs(x="Composition", y="NE (mg)") 
```