
## 1. Load library ######
library(readxl)
library(tidyverse)


## 2. Load tables ######
tree_init <- read_xlsx(
  path = "data-source/raw data NNT 2024_en.xlsx", 
  sheet = "Data", 
  guess_max = 2700
  )

forest_area <- 51.07
bamboo_area <- 0.33
unk_area    <- 0.36 
plot_count  <- 67

## 3. Checks ######
names(tree_init)
summary(tree_init)

summary(tree_init$tree_dbh)
summary(tree_init$tree_bole_height)

table(tree_init$forest_management, tree_init$zone)

ggplot(tree_init) +
  geom_point(aes(x = tree_dbh, y = tree_bole_height, color = as.character(zone))) +
  labs(
    x = "DBH (cm)",
    y = "Bole height (m)",
    color = "Zone"
  )

ggplot(tree_init) +
  geom_point(aes(x = tree_dbh, y = tree_bole_height, color = as.character(zone))) +
  facet_wrap(~zone) +
  theme(legend.position = "none") +
  labs(
    x = "Diameter at breast height (cm)",
    y = "Bole height (m)",
    color = "Zone"
  )

## 4. Calculations ######

## + 4.1 Clean the data #####

tree <- tt |>
  mutate()

tt <- tree_init |> filter(!is.na(tree_dbh), !is.na(tree_bole_height))


## + 4.2 Tree DBH classes ######
tt1 <- tt |>
  mutate(
    tree_dbh_class = case_when(
      tree_dbh < 10 ~ "05-09",
      tree_dbh < 20 ~ "10-19",
      tree_dbh < 30 ~ "20-29",
      tree_dbh < 40 ~ "30-39",
      tree_dbh < 50 ~ "40-49",
      tree_dbh < 60 ~ "50-59",
      tree_dbh >= 60 ~ "60+",
      TRUE ~ NA_character_
    )
  )

## + 4.3 Tree Volume ######
tt2 <- tt1 |>
  mutate(tree_volume = round((tree_dbh / 200)^2 * pi * tree_bole_height * 0.65),3)


## + 4.4 Tree volume and weight at the forest level ######
table(tree_init$plot_size, useNA = "ifany")

tt3 <- tt2 |>
  mutate(
    measurement_area = case_when(
      plot_size == "5x5"   ~ 5 * 5 / 10000,
      plot_size == "10x10" ~ 10 * 10 / 10000,
      TRUE ~ forest_area
    )
  )

tt3b <- tt3 |> 
  mutate(
    tree_scale_factor = 1 / measurement_area
  )
  

tt4 <- tt3b |> 
  mutate(
    tree_weight_forest = tree_scale_factor * forest_area,
    tree_volume_forest = tree_volume * tree_scale_factor / plot_count * forest_area 
    )


## + 4.5 Aggregate tree to forest level

n_smalltree_zone <- tt4 |>
  filter(tree_dbh < 20) |>
  group_by(forest_management, zone, tree_dbh_class, plot_no) |>
  summarise(
    count = mean(tree_scale_factor),
    volume = mean(tree_volume_forest),
    .groups = "drop"
  ) |>
  pivot_wider(names_from = tree_dbh_class, values_from = c(count, volume))



