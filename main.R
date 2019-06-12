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

# Count the number of different types of GIs

add_logticks  <- function (base = 10, sides = "bl", scaled = TRUE, 
                           short = unit(0.1, "cm"), mid = unit(0.2, "cm"),  long = unit(0.3, "cm"), 
                           colour = "black",  size = 0.5, linetype = 1, alpha = 1, color = NULL, 
                           data =data.frame(x = NA),... )   {
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
























