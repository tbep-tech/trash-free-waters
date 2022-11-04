library(tidyverse)
library(ggimage)
library(here)
library(showtext)

# get font
font_add_google("Roboto", "roboto")#, regular = 'C:/Windows/Fonts/Roboto.ttf')
fml <- "roboto"

showtext_auto()
showtext_opts(dpi = 300)

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

asp <- 4 # this is required to keep aspect ratio as 1:1, applied to height below
width <- 12

p <- ggplot(toplo, aes(x = x, y = y)) +
  geom_image(aes(image = img), size = 0.2, asp = asp) + 
  geom_text(aes(label = item1, y = y - 0.2), color = col, size = 5, family = fml, fontface = 'bold') + 
  theme_void() +
  scale_x_continuous(expand = c(0.1, 0.1)) +
  scale_y_continuous(expand = c(0.1, 0.1))

png(here('figs/totcnt.png'), height = width / asp, width = width, units = 'in', res = 300, family = fml)
print(p)
dev.off()

