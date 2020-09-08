library(tidyverse)
library(stringr)
library(scales)
library(ggalluvial)
library(TR8)
library(Taxonstand)
library(stringr)

# Data --------------------------------------------------------------------

data_raw <- read_csv("./data/GI_assets_publicMap_construct_new.csv",
                     col_types = cols(
                       .default = col_character(),
                       Asset_ID = col_double(),
                       DEP_Cont_1 = col_double(),
                       Asset_X_Co = col_double(),
                       Asset_Y_Co = col_double(),
                       BBL = col_double(),
                       Secondary = col_double(),
                       Community = col_double(),
                       City_Counc = col_double(),
                       Asset_Leng = col_double(),
                       Asset_Widt = col_double(),
                       Asset_Area = col_double()
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

# 
data_raw %>%
  count(Asset_Type) %>%
  arrange(desc(n))

data_raw %>%
  count(Tree_Latin) %>%
  arrange(desc(n))

data_raw %>%
  count(Status_Gro) %>%
  arrange(desc(n))

top_Asset_Type <- data_raw %>%
  count(Asset_Type) %>%
  arrange(desc(n)) %>%
  pull(Asset_Type)
top_Asset_Type <- top_Asset_Type[1:3]

data_plot <- data_raw
data_plot$Asset_Type[-which(data_plot$Asset_Type %in% top_Asset_Type)] <- "Other"
data_plot$Asset_Type[data_plot$Asset_Type == "ROW Infiltration Basin with Concrete Top"] <- "ROW w/ Cover"

data_plot <- data_plot %>%
  filter(Borough %in% c("Queens", "Brooklyn", "Bronx")) %>%
  mutate(Tree_Latin = replace(Tree_Latin, is.na(Tree_Latin), "No Tree"),
         Tree_Latin = replace(Tree_Latin, Tree_Latin != "No Tree", "Has Tree")) %>%
  group_by(Asset_Type, Borough, Status_Gro, Tree_Latin) %>%
  summarise(area = sum(Asset_Area),
         number = n()) %>%
  ungroup() %>%
  mutate(Asset_Type = factor(Asset_Type, levels = c(top_Asset_Type, "ROW w/ Cover", "Other")),
         Borough = factor(Borough, levels = c("Queens", "Brooklyn", "Bronx")),
         Tree_Latin = factor(Tree_Latin, levels = c("No Tree", "Has Tree")),
         Status_Gro = factor(Status_Gro, levels = c("Constructed", "In Construction", "Final Design")))

ggplot(data = data_plot,
       aes(axis1 = Borough, axis2 = Asset_Type, axis3 = Tree_Latin,
           y = number)) +
  scale_x_discrete(limits = c("Borough", "Type", "Status"), expand = c(.05, .05)) +
  labs(y = "Number of GIs") +
  geom_alluvium(aes(fill = Status_Gro), color = "black") +
  geom_stratum(width = 1/3) + 
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
  scale_fill_brewer(palette = "Set2") +
  ggtitle("Numbers of GIs grouped by Brough, Type, and Status") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "top")

ggsave("Number_GI_group.jpg", width = 25, height = 18, units = "cm", dpi = 400)



# Area
top_Asset_Type <- data_raw %>%
  group_by(Asset_Type) %>% 
  summarise(area = sum(Asset_Area)) %>%
  arrange(desc(area)) %>%
  pull(Asset_Type)
top_Asset_Type <- top_Asset_Type[1:6]

data_plot <- data_raw
data_plot$Asset_Type[-which(data_plot$Asset_Type %in% top_Asset_Type)] <- "Other"
data_plot$Asset_Type[data_plot$Asset_Type == "Multiple GI Components"] <- "Multiple GIs"
top_Asset_Type[top_Asset_Type == "Multiple GI Components"] <- "Multiple GIs"

data_plot <- data_plot %>%
  filter(Borough %in% c("Queens", "Brooklyn", "Bronx")) %>%
  mutate(Tree_Latin = replace(Tree_Latin, is.na(Tree_Latin), "No Tree"),
         Tree_Latin = replace(Tree_Latin, Tree_Latin != "No Tree", "Has Tree")) %>%
  group_by(Asset_Type, Borough, Status_Gro, Tree_Latin) %>%
  summarise(area = sum(Asset_Area),
            number = n()) %>%
  ungroup() %>%
  mutate(Asset_Type = factor(Asset_Type, levels = c(top_Asset_Type, "ROW w/ Cover", "Other")),
         Borough = factor(Borough, levels = c("Queens", "Brooklyn", "Bronx")),
         Tree_Latin = factor(Tree_Latin, levels = c("No Tree", "Has Tree")),
         Status_Gro = factor(Status_Gro, levels = c("Constructed", "In Construction", "Final Design")))

ggplot(data = data_plot,
       aes(axis1 = Borough, axis2 = Asset_Type, axis3 = Tree_Latin,
           y = area*0.09290304)) +
  scale_x_discrete(limits = c("Borough", "Type", "Status"), expand = c(.05, .05)) +
  labs(y = "Area of GIs [m²]") +
  geom_alluvium(aes(fill = Status_Gro), color = "black") +
  geom_stratum(width = 1/3) + 
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
  scale_fill_brewer(palette = "Set2") +
  ggtitle("Areas of GIs grouped by Brough, Type, and Status") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "top")

ggsave("Area_GI_group.jpg", width = 30, height = 18, units = "cm", dpi = 400)







trait_list <- available_traits() %>%
  filter(db = PLANTS)

