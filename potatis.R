
# Libraries
library(tidyverse)
library(showtext)
library(sysfonts)
library(stringr)
library(ggstream)
library(cowplot)
library(here)
library(naniar)
library(lubridate)
library(forcats)

# data
potatis <- read_delim("data/JO1301K1_20220112-212110_1.csv", 
                      delim = ";", 
                      escape_double = FALSE, 
                      trim_ws = TRUE)

# Aes
showtext_auto()
font_add_google("Six Caps")
font_add_google("Fira Sans Extra Condensed")
font1 <- "Six Caps"
font2 <- "Fira Sans Extra Condensed"

# cleaning
typ_to_value <- tribble(
  ~Typ, ~value, ~value_order,
  "122 Potatis färsk",	"Fresh potatoes", 1, 
  "123 Potatis- och rotmospulver",	"Mashed Potatoes and Root Vegetables", 3,
  "124 Potatisstärkelse (potatismjöl)",	"Potato starch and potato flour", 6,
  "125 Kylda och djupfrysta potatisprodukter",	"Frozen potato products", 5, 
  "126 Konservpotatis","Canned potatoes", 2,
  "127 Andra beredda potatisprodukter (chips)",	"Chips", 4)

matt_to_family <- tribble(
  ~Matt, ~family,
  "Totalt, 1 000 ton eller miljoner liter",	"Total (in 1000 tonnes)",
  "Kilo eller liter per person och år",	"Kilograms per person and year")

# matt_to_value <- tribble(
#   ~Matt, ~value,
#   "Totalt, 1 000 ton eller miljoner liter",	"var1",
#   "Kilo eller liter per person och år",	"var2")

na_strings <- c(",,", "\\..")
# my_colors <- rev(c("#6d2f20", "#b75347", "#df7e66", 
#                    "#e09351", "#edc775", "#94b594", "#224b5e"))
my_colors <- c("#6d2f20", "#b75347", "#df7e66", 
                   "#e09351", "#edc775", "#94b594", "#224b5e")
value_label <- unique(potatis_clean$value)

potatis_clean <- gather(potatis, ar, varde_orig, `1960`:`2020`) %>% 
  left_join(typ_to_value) %>% 
  left_join(matt_to_family) %>% 
  filter(ar > 1965) %>% 
  mutate(value = fct_reorder(value, value_order),
         n = as.numeric(str_replace(varde_orig, ",", ".")),
         n_new = ifelse(value !="Total (in 1000 tonnes)", -n, n),
         year = ymd(ar, truncated = 2L)) %>% 
  select(value, year, family, n, n_new, value_order) %>% 
  arrange(family, value_order, year)
#%>% 
  #spread(., value, n) 

max_x <- max(unique(potatis_clean$year)) 
min_x <- min(unique(potatis_clean$year))


P1 <- potatis_clean %>% 
  filter(family != "Total (in 1000 tonnes)" ) %>% 
  ggplot() +
  geom_area(aes(x=year, y=n, fill=value),
            position = position_stack(reverse = TRUE)) +
  #geom_stream(aes(x=year, y=n, fill=rev(value), type="ridge") +
  ylim(0, 85) +
  scale_fill_manual(values=my_colors) +
  labs(title = "Potatoes, potatoes and less potatoes",
       subtitle = "In Sweden, we eat less and less fresh potatoes but more and more frozen potato products"
  ) +
  theme_minimal() + 
  theme(plot.background = element_rect(fill="#fbf7f0", color="#fbf7f0"),
        plot.margin = margin(0.2, 0.7, 1.1, 0.6, "cm"),
        plot.title = element_text(size = 48,
                                  family=font1, 
                                  face = "bold",
                                  hjust = 0.5,
                                  #x=0.5, 
                                  #y=1.095, 
                                  color="#4f2217"
                                  ),
        plot.subtitle = element_text(size = 18,
                                  family=font2, 
                                  hjust = 0.5,
                                  #x=0.5, 
                                  #y=1.095, 
                                  color="#4f2217"),
        legend.position = "bottom",
        #legend.margin=margin(t = 0, unit='cm'),
        legend.direction = "horizontal",
        legend.text = element_blank(),
        legend.title = element_blank(),
        legend.key.height = unit(0.7, "cm"),
        legend.key.width = unit(7, "cm"),
        legend.spacing.x = unit(0.05, 'cm'),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 16,
                                   hjust = 0.5,
                                   vjust = 1,
                                   family=font2, 
                                   color="#4f2217"),
        axis.text.y = element_blank()
        ) +
  scale_x_date(date_labels = "%Y", 
               date_breaks = "6 years",
               limits = as.Date(c(min_x, max_x)),
               expand = c(0,0)
               )  +
  guides(color = guide_legend(reverse=TRUE)) +
  geom_segment(aes(x = min_x, y = 80, 
                   xend = min_x + years(10), yend = 80), 
               col = "#4f2217", 
               #size = 0.5,
               linetype = "dotted") +
  geom_segment(aes(x = min_x + years(30), y = 60, 
                 xend = min_x + years(40), yend = 60), 
             col = "#4f2217", 
             #size = 0.5,
             linetype = "dotted"
) 
P1  

ggdraw(P1) +
  draw_text(text="80", x=0.190, y=0.825, size=18, family=font2, color="#4f2217") +
  draw_text(text="60", x=0.713, y=0.68, size=18, family=font2, color="#4f2217") +
  draw_text(text="Kilograms per person and year", 
            x=0.188, 
            y=0.265, 
            size=18, 
            family=font2, 
            color="#fbf7f0") +
  draw_text(text = "@leynu_ | Source: Jordbruksverkets, Sweden", 
            x=.16, y=0.016, 
            color="#4f2217", 
            size=12, 
            family=font2#, 
            #fontface="bold"
            ) +
  draw_text(text = "Mashed Potatoes and Root Vegetables", 
            x=.5, y=0.13, 
            color="#4f2217", 
            size=12, 
            family=font2) +
  draw_text(text = "Chips", 
            x=.5, y=0.09, 
            color="#4f2217", 
            size=12, 
            family=font2) +
draw_text(text = "Fresh potatoes", 
          x=.2, y=0.13, 
          color="#fbf7f0", 
          size=12, 
          #fontface="bold",
          family=font2) +
  draw_text(text = "Canned potatoes", 
            x=.2, y=0.09, 
            color="#fbf7f0", 
            size=12, 
            #fontface="bold",
            family=font2) +
draw_text(text = "Frozen potato products", 
          x=.8, y=0.13, 
          color="#4f2217", 
          size=12, 
          #fontface="bold",
          family=font2) +
  draw_text(text = "Potato starch and potato flour", 
            x=.8, y=0.09, 
            color="#4f2217", 
            size=12, 
            #fontface="bold",
            family=font2) 



ggsave("~/Desktop/Potatoes.png",
       width=9.44, 
       height=7.18)


