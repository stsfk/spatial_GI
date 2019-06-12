library(tidyverse)
library(stringr)
library(scales)

# Data --------------------------------------------------------------------

data_raw <- read_csv("C:/Users/User/Desktop/DEP Green Infrastructure/GI_assets_publicMap_construct.csv",
                     col_types = cols(
                       .default = col_character(),
                       Asset_ID = col_double(),
                       TDA_ID = col_double(),
                       DEP_Cont_1 = col_double(),
                       Asset_X_Co = col_double(),
                       Asset_Y_Co = col_double(),
                       BBL = col_double(),
                       Secondary = col_double(),
                       Community = col_double(),
                       City_Counc = col_double(),
                       Assembly_D = col_double(),
                       Asset_Leng = col_double(),
                       Asset_Widt = col_double(),
                       Calc__Rain = col_double()
                     ))


# Plot --------------------------------------------------------------------

# Count the number of different types of GIs (TO DO: add ticks to one axis only)

add_logticks  <- function (base = 10, sides = "bl", scaled = TRUE, 
                           short = unit(0.1, "cm"), mid = unit(0.2, "cm"),  long = unit(0.3, "cm"), 
                           colour = "black",  size = 0.5, linetype = 1, alpha = 1, color = NULL, 
                           data =data.frame(x = NA),... )   {
  # https://stackoverflow.com/questions/20128582/is-it-possible-to-have-annotation-logtics-appear-on-only-one-of-the-subplots-u
  
  if (!is.null(color)) 
    colour <- color
  layer(geom = "logticks", geom_params = list(base = base, 
                                              sides = sides, raw = raw, scaled = scaled, short = short, 
                                              mid = mid, long = long, colour = colour, size = size, 
                                              linetype = linetype, alpha = alpha, ...), 
        stat = "identity", data =data , mapping = NULL, inherit.aes = FALSE, 
        show_guide = FALSE)
}

ggplot(data_raw, aes(reorder(Asset_Type, Asset_Type, function(x) length(x)), fill = Borough)) +
  geom_bar(position = position_dodge2(preserve = "single")) +
  coord_flip() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(legend.position = "top",
        axis.title.y = element_blank())


# Count the total area of different types of GIs

data_plot <- data_raw %>%
  mutate(area = Asset_Leng*Asset_Widt) %>%
  group_by(Borough, Asset_Type) %>%
  summarise(n = n(),
            area = sum(area)) %>%
  filter(area != 0)

ggplot(data_plot, aes(x = Borough, y = area, fill = Asset_Type)) +
  geom_bar(position = position_dodge2(preserve = "single"),  stat="identity")+
  coord_flip()+
  theme_bw() +
  theme(legend.position = "top",
        axis.title.y = element_blank())

# tree, no tree
data_plot <- data_raw %>%
  mutate(tree_presence = ifelse(Tree_Speci == "No Tree", "No Tree", "Has Tree"))

ggplot(data_plot, aes(reorder(Asset_Type, Asset_Type, function(x) length(x)), fill = tree_presence)) +
  geom_bar(position = position_dodge2(preserve = "single")) +
  coord_flip() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  theme(legend.position = "top",
        axis.title.y = element_blank())

# tree species
data_plot <- data_raw %>%
  filter(Tree_Speci != "No Tree")

tree_names <- str_c(data_plot$Tree_Speci, collapse = "/") %>%
  str_split("/")
data_plot <- tibble(tree_names = tree_names[[1]])

ggplot(data_plot) +
  geom_bar(aes(reorder(tree_names, tree_names, function(x) length(x)))) +
  coord_flip()
















