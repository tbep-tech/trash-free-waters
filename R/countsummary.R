library(tidyverse)
library(ggimage)
library(here)
# library(showtext)

# # get font
# font_add_google("Roboto", "roboto")#, regular = 'C:/Windows/Fonts/Roboto.ttf')
# fml <- "roboto"

# showtext_auto()
# showtext_opts(dpi = 300)

data(tfwdat)

col <- '#035172'

imgdat <- tibble(
  img = list.files(here('img'), full.names = T), 
  item1 = gsub('\\.png$', '', basename(img)), 
  y = 1
)

toplo <- tfwdat %>% 
  select(Date, Org, dataCards) %>% 
  unnest('dataCards') %>%
  filter(!item1 %in% 'WriteIn') %>% 
  group_by(item1) %>% 
  summarise(
    itemcnt = sum(itemcnt), 
    .groups = 'drop'
  ) %>% 
  mutate(
    item1 = factor(item1, levels = c('Plastic', 'Paper', 'Glass', 'Metal', 'Other'))
  ) %>% 
  arrange(item1) %>% 
  left_join(imgdat, by = 'item1') %>% 
  mutate(
    x = seq_along(item1), 
    itemcnt = formatC(itemcnt, big.mark = ',', digits = 0)
  ) %>% 
  unite('item1', item1, itemcnt, sep = '\n')



p <- ggplot(toplo, aes(x = x, y = y)) +
  geom_image(aes(image = img), size = 0.2) + 
  geom_text(aes(label = item1, y = y - 1), color = col, size = 5) + 
  # theme_void() +
  scale_x_continuous(expand = c(0.1, 0.1)) +
  scale_y_continuous(expand = c(0.1, 0.1)) +
  theme(
    plot.margin = margin(1, 1, 1, 1, "cm")
    ) + 
  # coord_cartesian(ylim = c(0.8, 1.1)) +
  coord_equal()

# ggsave(here('figs/totcnt.png'), device = 'png', width = 8, height = 6, dpi = 300)
# 
# png(here('figs/totcnt.png'), height = 3, width = 8, units = 'in', res = 300)
# print(p)
# dev.off()

