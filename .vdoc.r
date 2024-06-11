#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
library(tidyverse)
library(scales)
library(patchwork)
library(ggfx)
library(sf)
library(ggchicklet)
library(ggtext)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| echo: true
library(gapminder)

gapminder
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| echo: true
#| eval: false
#| code-line-numbers: "4"
library(tidyverse)

gapminder |>
  filter(country == params$country) |>
  ggplot(aes(
    x = year,
    y = lifeExp
  )) +
  geom_line()
#
#
#
#
#
#| fig-height: 3

gapminder |>
  filter(country == params$country) |>
  ggplot(aes(
    x = year,
    y = lifeExp
  )) +
  geom_line() +
  theme(axis.text = element_text(size = 14),
  axis.title = element_text(size = 14))
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| echo: true
#| eval: false
#| code-line-numbers: "4"
library(tidyverse)

gapminder |>
  filter(country == params$country) |>
  ggplot(aes(
    x = year,
    y = lifeExp
  )) +
  geom_line()
#
#
#
#
#
#| fig-height: 3

gapminder |>
  filter(country == "Albania") |>
  ggplot(aes(
    x = year,
    y = lifeExp
  )) +
  geom_line()
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# obtn::obtn_race_ethnicity |>
#   filter(year == 2023) |>
#   filter(geography %in% obtn::obtn_oregon_counties) |>
#   select(geography:value) |>
#   rename(
#     "county" = "geography",
#     "pct" = "value"
#   ) |>
#   mutate(pct_formatted = percent(pct, 0.1)) |>
#   mutate(population = fct_rev(population)) |>
#   write_rds("data/obtn_race_ethnicity.rds")
#
#
#
#
#
#
#
obtn_race_ethnicity <-
  read_rds("data/obtn_race_ethnicity.rds")
#
#
#
#
#
#| echo: true
obtn_race_ethnicity |>
  filter(county == "Multnomah")
#
#
#
#
#
#| echo: true
#| eval: false
race_ethnicity_bar_chart <- function(county_to_plot) {
  obtn_race_ethnicity |>
    filter(county == county_to_plot) |>
    ggplot(
      aes(
        x = pct,
        y = population
      )
    ) +
    geom_col(fill = "#004f39") +
    ...
}
#
#
#
#
#
#
race_ethnicity_bar_chart <- function(county_to_plot) {
  obtn_race_ethnicity_filtered <-
    obtn_race_ethnicity |>
    filter(county == county_to_plot)

  obtn_race_ethnicity_filtered |>
    ggplot(
      aes(
        x = pct,
        y = population
      )
    ) +
    geom_col(fill = "#004f39") +
    geom_text(
      data = obtn_race_ethnicity_filtered |> filter(population == "White"),
      aes(
        label = pct_formatted
      ),
      color = "white",
      hjust = 1.1
    ) +
    scale_x_continuous(
      expand = expansion(0, 0)
    ) +
    labs(title = county_to_plot) +
    theme_void() +
    theme(
      plot.title = element_text(
        size = 14,
        face = "bold"
      ),
      plot.margin = margin(rep(10, 4))
    )
}
#
#
#
#
#| echo: true
#| eval: false
race_ethnicity_bar_chart("Multnomah")
#
#
#
#
#
race_ethnicity_bar_chart("Multnomah")
#
#
#
#
#
#| fig-height: 3
race_ethnicity_bar_chart("Multnomah")
#
#
#
#
#
#| fig-height: 3
race_ethnicity_bar_chart("Baker")
#
#
#
#
#
#
#| echo: true
#| output: false
#| code-line-numbers: "2-9"
race_ethnicity_bar_chart("Multnomah") +
  geom_col(
    aes(
      x = 1
    ),
    fill = "transparent",
    color = "#A9C27F",
    linetype = "dotted"
  )
#
#
#
#
#
#
#| fig-height: 3.25
race_ethnicity_bar_chart("Multnomah") +
  geom_col(
    aes(
      x = 1
    ),
    fill = "transparent",
    color = "#A9C27F",
    linetype = "dotted"
  )
#
#
#
#
#
#| fig-height: 3.25
race_ethnicity_bar_chart("Baker") +
  geom_col(
    aes(
      x = 1
    ),
    fill = "transparent",
    color = "#A9C27F",
    linetype = "dotted"
  )
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
median_income <- read_csv("data/median_income.csv") |>
  rename("amount" = "value") |>
  mutate(amount_formatted = dollar(amount, 1)) |>
  select(geography, year, amount, amount_formatted) |>
  filter(year == 2024)

oregon_counties <-
  median_income |>
  distinct(geography) |>
  filter(!geography %in% c("Oregon", "Rural", "Urban")) |>
  pull(geography)
#
#
#
median_income_plot <- function(county_to_plot) {
  median_income |>
    filter(geography %in% c(county_to_plot, "Oregon")) |>
    mutate(geography = fct(geography, levels = c("Oregon", county_to_plot))) |>
    ggplot(
      aes(
        x = amount,
        y = geography,
        label = amount_formatted,
        fill = geography
      )
    ) +
    geom_col(show.legend = FALSE) +
    geom_text(
      color = "white",
      hjust = 1.2,
      size = 12
    ) +
    geom_text(
      aes(
        x = 2000,
        label = geography
      ),
      color = "white",
      hjust = 0,
      size = 12
    ) +
    scale_fill_manual(values = c(
      "gray",
      "darkgreen"
    )) +
    theme_void() +
    theme(plot.margin = margin(rep(20, 4)))
}
#
#
#
#
#
#| echo: true
median_income
#
#
#
#
#
#| echo: true
#| eval: false
median_income_plot <- function(county_to_plot) {
  median_income |>
    filter(geography %in% c(county_to_plot, "Oregon")) |>
    ggplot(
      aes(
        x = amount,
        y = geography,
        label = amount_formatted,
        fill = geography
      )
    ) +
    geom_col() +
    ...
}
#
#
#
#
#
#
#
#| eval: false
#| echo: true
median_income_plot("Jackson")
#
#
#
#
#
#| fig-height: 3
#| fig-width: 20
median_income_plot("Jackson")
#
#
#
#
#
#| fig-height: 3
#| fig-width: 20
median_income_plot("Jackson")
#
#
#
#
#
#| fig-height: 3
#| fig-width: 20
median_income_plot("Harney")
#
#
#
#
#
#| fig-height: 3
#| fig-width: 20
median_income_plot("Washington")
#
#
#
#
#
#
#| echo: true
max_median_income <-
  median_income |>
  slice_max(
    order_by = amount,
    n = 1
  ) |>
  pull(amount)
#
#
#
#
#
#| echo: true
max_median_income
#
#
#
#
#
#
#| eval: false
#| echo: true
#| code-line-numbers: "2-4"
median_income_plot("Jackson") +
  scale_x_continuous(
    limits = c(0, max_median_income)
  )
#
#
#
#
#
#| fig-height: 3
#| fig-width: 20
median_income_plot("Jackson") +
  scale_x_continuous(
    limits = c(0, max_median_income)
  )
#
#
#
#
#
#
#| fig-height: 3
#| fig-width: 20
median_income_plot("Jackson") +
  scale_x_continuous(
    limits = c(0, max_median_income)
  )
#
#
#
#
#
#| fig-height: 3
#| fig-width: 20
median_income_plot("Harney") +
  scale_x_continuous(
    limits = c(0, max_median_income)
  )
#
#
#
#
#
#| fig-height: 3
#| fig-width: 20
median_income_plot("Washington") +
  scale_x_continuous(
    limits = c(0, max_median_income)
  )
#
#
#
#
#
#
library(palmerpenguins)

set.seed(1234)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| echo: true
library(palmerpenguins)

penguins
#
#
#
#
#
#| echo: true
penguins_scatterplot <- function(number_of_dots) {
  penguins |>
    group_by(island) |>
    slice(1:(number_of_dots / 3)) |>
    ungroup() |>
    ggplot(aes(
      bill_length_mm,
      bill_depth_mm
    )) +
    geom_point() +
    theme_minimal()
}
#
#
#
#
#
#| echo: true
penguins_scatterplot(number_of_dots = 36)
#
#
#
#
#
#| echo: true
penguins_scatterplot(number_of_dots = 36) +
  geom_text(aes(label = island))
#
#
#
#
#
#
#
#
#
#
#
#
#| echo: true
#| eval: false
#| code-line-numbers: "4"
library(ggrepel)

penguins_scatterplot(number_of_dots = 36) +
  geom_text_repel(aes(label = island))
#
#
#
#
#
penguins_scatterplot(number_of_dots = 36) +
  geom_text_repel(aes(label = island))
#
#
#
#
#
#| echo: true
penguins_scatterplot(number_of_dots = 120) +
  geom_text_repel(aes(label = island))
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# pschousing::data_population_projection |>
#   select(area_name:age_cohort_grp, perc_proj) |>
#   rename(
#     "location" = "area_name",
#     "age_group" = "age_cohort_grp",
#     "pct" = "perc_proj"
#   ) |>
#   mutate(pct_formatted = percent(pct, 1)) |>
#   write_rds("data/population_projection.rds")
#
#
#
#
population_projection <-
  read_rds("data/population_projection.rds")
#
#
#
#| echo: true
population_projection |>
  filter(location == "Hartford")
#
#
#
#
#
#| echo: true
#| eval: false
population_projection_plot <- function(town_to_plot, county_to_plot) {
  population_projection |>
    filter(location %in% c(town_to_plot, county_to_plot, "Connecticut")) |>
    ggplot(aes(
      x = year,
      y = pct,
      color = location,
      group = location
    )) +
    geom_point() +
    geom_line() +
    ...
}
#
#
#
#
#
population_projection_plot <- function(town_to_plot, county_to_plot) {
  population_projection |>
    filter(location %in% c(town_to_plot, county_to_plot, "Connecticut")) |>
    mutate(location = fct(
      location,
      levels = c("Connecticut", county_to_plot, town_to_plot)
    )) |>
    ggplot(aes(
      x = year,
      y = pct,
      color = location,
      group = location
    )) +
    geom_point(size = 2) +
    geom_line(show.legend = FALSE) +
    labs(color = NULL) +
    facet_wrap(
      vars(age_group),
      nrow = 1
    ) +
    scale_y_continuous(
      limits = c(0, 0.4),
      labels = percent_format(1)
    ) +
    scale_color_manual(
      values = c(
        # town_to_plot = "#9f3515",
        # county_to_plot = "#fbbfb8",
        # "Connecticut" = "#c4c4c4"
        "#c4c4c4",
        "#fbbfb8",
        "#9f3515"
      )
    ) +
    guides(color = guide_legend(reverse = TRUE)) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "bottom",
      strip.text = element_text(
        face = "italic",
        size = 11
      ),
      axis.title = element_blank()
    )
}
#
#
#
#| echo: true
#| eval: false
population_projection_plot(
  town_to_plot = "Hartford",
  county_to_plot = "Hartford County"
)
#
#
#
#
#
population_projection_plot(
  town_to_plot = "Hartford",
  county_to_plot = "Hartford County"
)
#
#
#
#
#
#| echo: true
#| eval: false
#| code-line-numbers: "5-9"
population_projection_plot(
  town_to_plot = "Hartford",
  county_to_plot = "Hartford County"
) +
  geom_text(
    aes(
      label = pct_formatted
    )
  )
#
#
#
#
#
#| fig-height: 4
population_projection_plot(
  town_to_plot = "Hartford",
  county_to_plot = "Hartford County"
) +
  geom_text(
    aes(
      label = pct_formatted
    ),
    show.legend = FALSE
  )
#
#
#
#
#
#| echo: true
#| eval: false
#| code-line-numbers: "5-9"
population_projection_plot(
  town_to_plot = "Hartford",
  county_to_plot = "Hartford County"
) +
  geom_text_repel(
    aes(
      label = pct_formatted
    )
  )
#
#
#
#
#
#| fig-height: 4
population_projection_plot(
  town_to_plot = "Hartford",
  county_to_plot = "Hartford County"
) +
  geom_text_repel(
    aes(
      label = pct_formatted
    ),
    show.legend = FALSE
  )
#
#
#
#
#
#| echo: true
#| eval: false
#| code-line-numbers: "6,7"
population_projection_plot(
  town_to_plot = "Hartford",
  county_to_plot = "Hartford County"
) +
  geom_text(
    data = population_projection |> filter(location == "Hartford"),
    nudge_y = 0.02,
    aes(
      label = pct_formatted
    )
  )
#
#
#
#
#
#| fig-height: 4
population_projection_plot(
  town_to_plot = "Hartford",
  county_to_plot = "Hartford County"
) +
  geom_text(
    data = population_projection |> filter(location == "Hartford"),
    nudge_y = 0.02,
    aes(
      label = pct_formatted
    ),
    show.legend = FALSE
  )
#
#
#
#
#
#| echo: true
#| eval: false
#| code-line-numbers: "6,7"
population_projection_plot(
  town_to_plot = "Stamford",
  county_to_plot = "Fairfield County"
) +
  geom_text(
    data = population_projection |> filter(location == "Stamford"),
    nudge_y = 0.02,
    aes(
      label = pct_formatted
    )
  )
#
#
#
#
#
#| fig-height: 4
population_projection_plot(
  town_to_plot = "Stamford",
  county_to_plot = "Fairfield County"
) +
  geom_text(
    data = population_projection |> filter(location == "Stamford"),
    nudge_y = 0.02,
    aes(
      label = pct_formatted
    ),
    show.legend = FALSE
  )
#
#
#
#
#
library(shadowtext)
#
#
#
#
#| echo: true
#| eval: false
#| code-line-numbers: "1,7,9"

library(shadowtext)

population_projection_plot(
  town_to_plot = "Stamford",
  county_to_plot = "Fairfield County"
) +
  geom_shadowtext(
    data = population_projection |> filter(location == "Stamford"),
    bg.color = "white",
    nudge_y = 0.02,
    aes(
      label = pct_formatted
    )
  )
#
#
#
#
#
#| fig-height: 4
population_projection_plot(
  town_to_plot = "Stamford",
  county_to_plot = "Fairfield County"
) +
  geom_shadowtext(
    data = population_projection |> filter(location == "Stamford"),
    bg.color = "white",
    nudge_y = 0.02,
    aes(
      label = pct_formatted
    ),
    show.legend = FALSE
  )
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# pschousing::data_acs$affordability_occupancy_status |>
#   filter(occupancy_status == "Renter") |>
#   select(area_name, spending_to_income_grp, perc_spending) |>
#   rename(
#     "location" = "area_name",
#     "burden_level" = "spending_to_income_grp",
#     "pct" = "perc_spending"
#   ) |>
#   mutate(pct_formatted = percent(pct, 1)) |>
#   mutate(burden_level = fct_collapse(
#     burden_level,
#     "Not burdened" = "Not experiencing a burden (<30%)",
#     "Moderate burden" = "Cost-burdened (30%-50%)",
#     "Severe burden" = "Severely cost-burdened (>=50%)",
#     "Not computed" = "Not Computed"
#   )) |>
#   mutate(burden_level = fct(
#     as.character(burden_level),
#     levels = c(
#       "Not computed",
#       "Not burdened",
#       "Moderate burden",
#       "Severe burden"
#     )
#   )) |>
#   mutate(location = fct_inorder(location)) |>
#   mutate(location = fct_rev(location)) |>
#   write_rds("data/housing_cost_burden.rds")
#
#
#
#
housing_cost_burden <-
  read_rds("data/housing_cost_burden.rds")
#
#
#
housing_cost_burden
#
#
#
#
#
#| echo: true
#| eval: false

housing_cost_burden_plot <- function(town_to_plot, county_to_plot) {
  housing_cost_burden |>
    filter(location %in% c(town_to_plot, county_to_plot, "Connecticut")) |>
    ggplot(aes(
      x = pct,
      y = location,
      fill = burden_level,
      label = pct_formatted
    )) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5)) +
    ...
}
#
#
#
#
#
housing_cost_burden_plot <- function(town_to_plot, county_to_plot) {
  housing_cost_burden |>
    filter(location %in% c(town_to_plot, county_to_plot, "Connecticut")) |>
    ggplot(aes(
      x = pct,
      y = location,
      fill = burden_level,
      color = burden_level,
      label = pct_formatted
    )) +
    geom_col(color = "white") +
    geom_text(
      position = position_stack(vjust = 0.5),
      size = 10,
      fontface = "bold",
    ) +
    scale_fill_manual(
      name = "",
      values = pschousing::psc_colors("grey3", "grey2", "lightblue2", "lightblue"),
      guide = guide_legend(reverse = TRUE)
    ) +
    scale_color_manual(
      values = c("white", "white", "#969696", "white"),
      guide = FALSE
    ) +
    scale_x_continuous(expand = expansion(0, 0.01)) +
    labs(fill = NULL) +
    theme_void(base_size = 16) +
    theme(
      legend.position = "bottom",
      axis.text.y = element_text(hjust = 1),
      plot.margin = margin(rep(20, 4))
    )
}
#
#
#
#| echo: true
#| eval: false
housing_cost_burden_plot(
  town_to_plot = "Hartford",
  county_to_plot = "Hartford County"
)
#
#
#
#
#
housing_cost_burden_plot(
  town_to_plot = "Hartford",
  county_to_plot = "Hartford County"
)
#
#
#
#
#
#
#
#| echo: true
housing_cost_burden <-
  housing_cost_burden |>
  mutate(pct_formatted = if_else(pct > 0.07, pct_formatted, NA))
#
#
#
#
#
#| echo: true
housing_cost_burden
#
#
#
#
#
#| echo: true
#| eval: false
housing_cost_burden_plot(
  town_to_plot = "Hartford",
  county_to_plot = "Hartford County"
)
#
#
#
#
#
housing_cost_burden_plot(
  town_to_plot = "Hartford",
  county_to_plot = "Hartford County"
)
#
#
#
#
#
#| echo: true
#| eval: false
housing_cost_burden_plot(
  town_to_plot = "Stamford",
  county_to_plot = "Fairfield County"
)
#
#
#
#
#
housing_cost_burden_plot(
  town_to_plot = "Stamford",
  county_to_plot = "Fairfield County"
)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
pre_post_plot <- function(df) {
  df |>
    ggplot(aes(
      x = rating,
      y = question,
      fill = timing,
      label = rating,
      group = question
    )) +
    geom_vline(
      xintercept = 1:5,
      color = "grey90"
    ) +
    geom_line(
      color = "grey70",
      alpha = 0.75,
      linewidth = 1.5
    ) +
    geom_point(
      shape = 21,
      color = "white",
      size = 12,
      stroke = 2
    ) +
    scale_x_continuous(
      limits = c(1, 5),
      breaks = seq(1, 5, 1),
      position = "top",
      expand = expansion(0, 0)
    ) +
    scale_color_manual(
      values = c(
        "Pre" = "#6d8d24",
        "Post" = "#213921"
      )
    ) +
    scale_fill_manual(
      values = c(
        "Pre" = "#6d8d24",
        "Post" = "#213921"
      ),
      guide = guide_legend(reverse = TRUE)
    ) +
    labs(
      title = "Consider the limits of your data",
      fill = NULL
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.margin = margin(rep(20, 4)),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      # plot.title = element_text(face = "bold",
      #                           color = "grey30"),
      plot.title = element_blank(),
      plot.title.position = "plot",
      legend.position = "none",
      axis.text.x = element_text(color = "grey40")
    )
}
#
#
#
#
pre_post_data <-
  tribble(
    ~question,
    ~timing,
    ~rating,
    "Consider the limits of your data",
    "Pre",
    1.6,
    "Consider the limits of your data",
    "Post",
    4.2
  ) |>
  mutate(
    growth = rating - lag(rating, 1),
    growth = if_else(timing == "Post", growth, NA),
    growth_formatted = number(growth, 0.1),
    growth_formatted = str_glue("+{growth_formatted}"),
    growth_formatted = if_else(timing == "Post", growth, NA),
    growth_text_position = rating - growth / 2
  )
#
#
#
#| echo: true
pre_post_data
#
#
#
#
#
#| echo: true
#| eval: false

pre_post_plot <- function(df) {
  df |>
    ggplot(aes(
      x = rating,
      y = question,
      fill = timing
    )) +
    geom_line() +
    geom_point(shape = 21) +
    ...
}
#
#
#
#
#
#
#| echo: true
#| eval: false
pre_post_data |>
  pre_post_plot()
#
#
#
#
#
#| fig-height: 2
pre_post_data |>
  pre_post_plot()
#
#
#
#
#
#
#| echo: true
#| eval: false
#| code-line-numbers: 3-7|8-14|15-21
pre_post_data |>
  pre_post_plot() +
  # Ratings within points
  geom_text(
    aes(label = rating),
    color = "white"
  ) +
  # Pre/post labels
  geom_text(
    aes(
      label = timing,
      color = timing
    )
  ) +
  # Growth label
  geom_text(
    aes(
      x = growth_text_position,
      label = growth_formatted
    )
  )
#
#
#
#
#
#| fig-height: 2
pre_post_data |>
  pre_post_plot() +
  geom_text(color = "white") +
  geom_text(
    aes(
      label = timing,
      color = timing
    ),
    vjust = -2,
    fontface = "bold"
  ) +
  geom_text(
    aes(
      x = rating - growth / 2,
      label = growth_formatted
    ),
    vjust = -1
  )
#
#
#
#
#
pre_post_data <-
  tribble(
    ~question,
    ~timing,
    ~rating,
    "Consider the limits of your data",
    "Pre",
    3.5,
    "Consider the limits of your data",
    "Post",
    3.6
  ) |>
  mutate(
    growth = rating - lag(rating, 1),
    growth = if_else(timing == "Post", growth, NA),
    growth_formatted = number(growth, 0.1),
    growth_formatted = str_glue("+{growth_formatted}"),
    growth_formatted = if_else(timing == "Post", growth, NA),
    growth_text_position = rating - growth / 2
  )
#
#
#
#| echo: true
pre_post_data
#
#
#
#
#
#
#| fig-height: 2
pre_post_data |>
  pre_post_plot() +
  geom_text(color = "white") +
  geom_text(
    aes(
      label = timing,
      color = timing
    ),
    vjust = -2,
    fontface = "bold"
  ) +
  geom_text(
    aes(
      x = growth_text_position,
      label = growth_formatted
    ),
    vjust = -1
  )
#
#
#
#
#
#| echo: true
#| code-line-numbers: 3-6
pre_post_data <-
  pre_post_data |>
  mutate(rating_text_position = case_when(
    timing == "Pre" ~ rating - 0.2,
    timing == "Post" ~ rating + 0.2
  ))
#
#
#
#
#
#| echo: true
pre_post_data |> 
  select(question, timing, rating_text_position)
#
#
#
#
#
#| echo: true
#| eval: false
#| code-line-numbers: 3-8|9-14
pre_post_data |>
  pre_post_plot() +
  geom_text(
    aes(
      x = rating_text_position,
      label = rating
    )
  ) +
  geom_text(
    aes(
      x = rating_text_position,
      label = timing
    )
  ) +
  ...
#
#
#
#
#
#| echo: true
#| eval: false
pre_post_data |>
  pre_post_plot() +
  geom_text(
    aes(
      x = rating_text_position,
      label = rating
    )
  ) +
  geom_text(
    aes(
      x = rating_text_position,
      label = timing
    )
  ) +
  ...
#
#
#
#
#| fig-height: 2
pre_post_data |>
  pre_post_plot() +
  geom_text(
    aes(
      x = rating_text_position,
      color = timing
    ),
    fontface = "bold"
  ) +
  geom_text(
    aes(
      x = rating_text_position,
      label = timing,
      color = timing
    ),
    vjust = -2,
    fontface = "bold"
  ) +
  geom_text(
    aes(
      x = rating - growth / 2,
      label = growth_formatted
    ),
    vjust = -1
  )
#
#
#
#
#
#| echo: true
#| eval: false
#| code-line-numbers: 3-8
pre_post_data |>
  pre_post_plot() +
  geom_label(
    aes(
      x = 6,
      label = growth_formatted
    )
  ) +
  ...
#
#
#
#
#
#| fig-height: 2
pre_post_data |>
  mutate(rating_text_position = case_when(
    timing == "Pre" ~ rating - 0.3,
    timing == "Post" ~ rating + 0.3
  )) |>
  pre_post_plot() +
  geom_text(
    aes(
      x = rating_text_position,
      color = timing
    ),
    fontface = "bold"
  ) +
  geom_text(
    aes(
      x = rating_text_position,
      label = timing,
      color = timing
    ),
    vjust = -2,
    fontface = "bold"
  ) +
  geom_label(
    aes(
      x = 6,
      label = growth_formatted
    ),
    fill = "#cfd82d",
    label.size = unit(0, "pt"),
    label.padding = unit(4, "pt"),
    label.r = unit(3, "pt"),
    color = "grey30",
    fontface = "bold"
  ) +
  scale_x_continuous(
    position = "top",
    limits = c(1, 6),
    breaks = c(
      1,
      2,
      3,
      4,
      5,
      6
    ),
    labels = c(
      "1",
      "2",
      "3",
      "4",
      "5",
      "Growth"
    )
  )
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# pschousing::data_acs$units_structure_occupancy_status |>
#   filter(area_level == "Town") |>
#   group_by(area_name, units_structure_grp) |>
#   summarize(total = sum(estimate)) |>
#   mutate(pct = total / sum(total, na.rm = TRUE)) |>
#   ungroup() |>
#   filter(units_structure_grp == "Single-Family") |>
#   select(area_name, pct) |>
#   rename("location" = "area_name") |>
#   write_rds("data/single_family_homes.rds")
#
#
#
single_family_homes <-
  read_rds("data/single_family_homes.rds")
#
#
#
#| echo: true
single_family_homes
#
#
#
#
#
#| echo: true
#| eval: false
single_family_homes_plot <- function() {
  single_family_homes |>
    ggplot(
      aes(
        x = pct,
        y = 1
      )
    ) +
    geom_point(
      shape = 124,
      color = "grey80"
    ) +
    ...
}
#
#
#
#
#
#
#
#
single_family_homes_plot <- function() {
  single_family_homes |>
    ggplot(
      aes(
        x = pct,
        y = 1
      )
    ) +
    geom_hline(
      yintercept = 1,
      color = "grey90",
      linewidth = 0.25
    ) +
    geom_point(
      shape = 124,
      color = "grey80",
      size = 5
    ) +
    # scale_color_manual(
    #   values = c(
    #     "Y" = "#15397f",
    #     "N" = "grey80"
    #   )
    # ) +
    # scale_size_manual(values = c(
    #   "Y" = 15,
    #   "N" = 5
    # )) +
    scale_x_continuous(
      labels = percent_format(accuracy = 1),
      breaks = seq(0, 1, 0.25)
    ) +
    expand_limits(
      x = 0
    ) +
    theme_void() +
    theme(
      axis.text.x = element_text(),
      legend.position = "none",
      plot.margin = margin(rep(20, 4))
    )
}
#
#
#
#| echo: true
#| eval: false
single_family_homes_plot()
#
#
#
#
#
#| fig-height: 2
single_family_homes_plot()
#
#
#
#
#
#
#
#| echo: true
#| eval: false
single_family_homes_plot() +
  geom_point(
    data = single_family_homes |> filter(location == "Hartford"),
    shape = 124,
    color = "#15397f"
  )
#
#
#
#
#
#| fig-height: 2
single_family_homes_plot() +
  geom_point(
    data = single_family_homes |> filter(location == "Hartford"),
    shape = 124,
    color = "#15397f",
    size = 5
  )
#
#
#
#
#
#
#
#
#| echo: true
#| eval: false
#| code-line-numbers: "6"
single_family_homes_plot() +
  geom_point(
    data = single_family_homes |> filter(location == "Hartford"),
    shape = 124,
    color = "#15397f",
    size = 15
  )
#
#
#
#
#
#| fig-height: 2
single_family_homes_plot() +
  geom_point(
    data = single_family_homes |> filter(location == "Hartford"),
    shape = 124,
    color = "#15397f",
    size = 15
  )
#
#
#
#
#
#| echo: true
#| eval: false
#| code-line-numbers: "3"
single_family_homes_plot() +
  geom_point(
    data = single_family_homes |> filter(location == "Stamford"),
    shape = 124,
    color = "#15397f",
    size = 15
  )
#
#
#
#
#
#| fig-height: 2
single_family_homes_plot() +
  geom_point(
    data = single_family_homes |> filter(location == "Stamford"),
    shape = 124,
    color = "#15397f",
    size = 15
  )
#
#
#
#
#
#
#
#
#
#
#
#
#
read_rds("data/sf_region_ig1_2_map.rds") |>
  select(country_name, ind_region, disease, status) |>
  rename(
    "country" = "country_name",
    "region" = "ind_region"
  ) |>
  filter(disease == "Rubella") |>
  # select(-disease) |>
  write_rds("data/rubella.rds")

rubella <- read_rds("data/rubella.rds")

ia2030_colors <- list(
  navy = "#1f4675",
  light_blue = "#59b8cd",
  light_blue_back = "#e5f1f8",
  red = "#eb5a53",
  yellow = "#ffd300",
  stone = "#f0eae5",
  light_grey = "#e5ebee",
  # not official
  green = "#43a047",
  dark_grey = "#353535"
)

region_map <- function(opacity_level = 1) {
  ggplot() +
    geom_sf(
      data = rubella |>
        filter(region == 1) |>
        add_row(status = "Achieved", disease = "Rubella") |>
        add_row(status = "Re-established", disease = "Rubella") |>
        add_row(status = "Not achieved", disease = "Rubella") |>
        add_row(status = "No data", disease = "Rubella"),
      aes(fill = status),
      linewidth = 0.2,
      color = "white",
      alpha = opacity_level
    ) +
    geom_sf(
      data = rubella |>
        filter(region == 0),
      fill = "lightgrey",
      color = "grey",
      linewidth = 0,
      alpha = 0.5
    ) +
    # facet_wrap(
    #   vars(disease),
    #   labeller = labeller(disease = label_wrap_gen(20))
    # ) +
    scale_fill_manual(
      name = "",
      values = c(
        "Achieved" = ia2030_colors$navy,
        "Re-established" = ia2030_colors$yellow,
        "Not achieved" = ia2030_colors$red,
        "No data" = ia2030_colors$light_grey
      ),
      drop = FALSE
    ) +
    theme_void() +
    theme(
      strip.text = element_text(
        size = 9,
        family = "Inter"
      ),
      plot.margin = margin(rep(20, 4)),
      legend.position = "bottom"
    )
}
#
#
#
#
#
#
#
#| echo: true
#| eval: false
rubella
#
#
#
rubella |>
  select(-disease)
#
#
#
#
#
#
#| echo: true
#| eval: false
region_map <- function() {
  ggplot() +
    geom_sf(
      data = rubella |> filter(region == 1),
      aes(fill = status)
    ) +
    geom_sf(
      data = rubella |> filter(region == 0),
      fill = "lightgrey",
      alpha = 0.5
    ) +
    ...
}
#
#
#
#
#
#
#
# rubella <- read_rds("data/rubella.rds")
#
#
#
#
#
#| echo: true
region_map()
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
