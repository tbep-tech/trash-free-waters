library(tidyverse)
library(plotly)
library(reactable)
library(here)
library(english)
library(hrbrthemes)
# library(showtext)

data(tfwdat)

# get font
font_add_google("Roboto", "roboto")#, regular = 'C:/Windows/Fonts/Roboto.ttf')
fml <- "roboto"

showtext_auto()
showtext_opts(dpi = 400)

thm <- theme_ipsum(base_family = fml, plot_margin = margin(10, 10, 10, 10)) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title.x = element_text(hjust = 0.5, size = 12),
    axis.title.y = element_text(hjust = 0.5, size = 12),
    legend.position = 'top'
  )

# total lbs all sites and events ------------------------------------------

levs <- c("DebrisRecyclablesWeightLb", "DebrisTrashWeightLb", "LitterRecyclablesWeightLb", 
          "LitterTrashWeightLb")
labs <- c("Debris Recyclables", "Debris Trash", "Litter Recyclables", "Litter Trash")
cols <-  c('#00806E', '#427355', '#5C4A42', '#958984')
names(cols) <- labs

toplo <- tfwdat %>% 
  select(Site, Date, matches('^Litter|^Debris')) %>% 
  pivot_longer(cols = -matches('Site|Date')) %>% 
  group_by(name) %>% 
  summarise(
    nevnt = n(),
    dtrng = paste(format(min(Date), '%b %d, %Y'), 'to', format(max(Date), '%b %d, %Y')),
    nsite = length(unique(Site)),
    tot = sum(value, na.rm = T), 
    .groups = 'drop'
  ) %>% 
  mutate(
    name = factor(name, levels = levs, labels = labs), 
    per = paste(round(100 * tot / sum(tot), 1), '%')
  ) %>% 
  arrange(tot) %>% 
  mutate(name = factor(name, levels = .$name))

subttl <- paste0(unique(toplo$dtrng), ', ',  english(unique(toplo$nevnt)), ' events at ', english(unique(toplo$nsite)), ' sites')

p <- ggplot(toplo, aes(y = name, x = tot)) +
  geom_bar(stat = 'identity', alpha = 0.7, aes(color = name, fill = name), size = 0.7, width = 0.7) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  geom_text(aes(label = per), nudge_x = 10, hjust = 0) +
  scale_x_continuous(limits = c(0, max(toplo$tot) * 1.1)) +
  thm +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(),
    panel.grid.minor.x = element_line(), 
    legend.position = 'none'
  ) +
  labs(
    y = NULL,
    x = 'Total (lbs)',
    color = NULL,
    fill = NULL,
    title = 'Total weight to date',
    subtitle = subttl,
    caption = 'source: TBEP and partners'
  )

jpeg(here('figs/totlbs.jpg'), height = 5, width = 8, family = fml, units = 'in', res = 400)
print(p)
dev.off()

# total lbs all sites and events, by county -------------------------------

levs <- c("DebrisRecyclablesWeightLb", "DebrisTrashWeightLb", "LitterRecyclablesWeightLb", 
          "LitterTrashWeightLb")
labs <- c("Debris Recyclables", "Debris Trash", "Litter Recyclables", "Litter Trash")
cols <-  c('#00806E', '#427355', '#5C4A42', '#958984')
names(cols) <- labs

toplo <- tfwdat %>% 
  select(Site, Date, County, matches('^Litter|^Debris')) %>% 
  pivot_longer(cols = -matches('Site|Date|County')) %>% 
  group_by(County, name) %>% 
  summarise(
    nevnt = n(),
    dtrng = paste(format(min(Date), '%b %d, %Y'), 'to', format(max(Date), '%b %d, %Y')),
    nsite = length(unique(Site)),
    tot = sum(value, na.rm = T), 
    .groups = 'drop'
  ) %>% 
  mutate(
    name = factor(name, levels = levs, labels = labs), 
    per = paste(round(100 * tot / sum(tot), 1), '%'), 
    site = ifelse(nsite > 1, 'sites', 'site'),
    evnt = ifelse(nevnt > 1, 'events', 'event')
  ) %>% 
  rowwise() %>% 
  mutate(
    subttl = paste0(unique(dtrng), ', ', english(unique(nevnt)), ' ', evnt, ' at ', english(unique(nsite)), ' ', site), 
    County = paste0(County, ': ', subttl)
  )

p <- ggplot(toplo, aes(y = name, x = tot)) +
  geom_bar(stat = 'identity', alpha = 0.7, aes(color = name, fill = name), size = 0.7, width = 0.7) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  geom_text(aes(label = per), nudge_x = 5, hjust = 0) +
  scale_x_continuous(limits = c(0, max(toplo$tot) * 1.1)) +
  facet_wrap(~County, ncol = 1) + 
  thm +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(),
    panel.grid.minor.x = element_line(), 
    legend.position = 'none'
  ) +
  labs(
    y = NULL,
    x = 'Total (lbs)',
    color = NULL,
    fill = NULL,
    title = 'Total weight to date by county',
    caption = 'source: TBEP and partners'
  )

jpeg(here('figs/totlbscos.jpg'), height = 9, width = 8, family = fml, units = 'in', res = 400)
print(p)
dev.off()

