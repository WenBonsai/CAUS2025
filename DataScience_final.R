library(readr)
library(dplyr)
library(broom)
library(tidyr)
library(tidyverse)
library(haven)
library(tibble)
library(fixest)
library(modelsummary)
library(knitr)
library(DescTools)  
library(kableExtra)
library(ggplot2)
library(scales)
library(WDI)

#loading main dataset

full_clean_panel <- read_csv("~/Desktop/Mannheim/Causal Data Science/Research Paper Replication/final_panel_clean.csv")
View(full_clean_panel)

full_clean_panel <- full_clean_panel |>
  rename(
    cash_eq = cash
  ) |>
  select(-cfo_actual)

# loading gdp deflator to adjust monetary variables in 2000 us dollars
gnp_deflator <- WDI(
  country = "US",
  indicator = "NY.GDP.DEFL.ZS",
  start = 1990,
  end = 2008
)

gnp_deflator$deflator_2000_base <- 
  gnp_deflator$NY.GDP.DEFL.ZS / 
  gnp_deflator$NY.GDP.DEFL.ZS[gnp_deflator$year == 2000] * 100


full_clean_panel <- full_clean_panel |>
  left_join(
    gnp_deflator |> select(year, NY.GDP.DEFL.ZS),
    by = "year"
  ) |>
  mutate(
    total_assets = total_assets / NY.GDP.DEFL.ZS * 100,
    capex  = capex / NY.GDP.DEFL.ZS * 100,
    ppe = ppe / NY.GDP.DEFL.ZS * 100,
    cash_eq  = cash_eq / NY.GDP.DEFL.ZS * 100,
    ncfo = ncfo / NY.GDP.DEFL.ZS * 100,
    debt = debt / NY.GDP.DEFL.ZS * 100,
    net_income = net_income / NY.GDP.DEFL.ZS * 100,
    ebitda_ttm = ebitda_ttm / NY.GDP.DEFL.ZS * 100,
    revenue = revenue / NY.GDP.DEFL.ZS * 100,
    mkt_cap = mkt_cap / NY.GDP.DEFL.ZS * 100,
    dividends = dividends / NY.GDP.DEFL.ZS * 100,
    stock_repurchased = stock_repurchased / NY.GDP.DEFL.ZS * 100
  ) |>
  select(-NY.GDP.DEFL.ZS)



##### Table 3: summary statistics - Panel A

summary_table <- full_clean_panel |>
  group_by(Country_of_Incorporation) |>
  summarise(
    unique_firms = n_distinct(Identifier),
    observations = n(),
    .groups = "drop"
  ) 

summary_table_a <- summary_table |>
  bind_rows(
    tibble(
      Country_of_Incorporation = "TOTAL",
      unique_firms = sum(summary_table$unique_firms),
      observations = sum(summary_table$observations)
    )
  )


#Latex output

kable(
  summary_table_a,
  format = "latex",
  booktabs = TRUE,
  align = "lrr",
  caption = "Table 3. Summary Statistics — Panel A: Firm Coverage by Country"
) |>
  kable_styling(
    latex_options = c("hold_position"),
    font_size = 10
  ) |>
  save_kable("table3_panelA.tex")


##### Table 3: summary statistics - Panel B

# Arranging data and avoiding duplication
full_clean_panel <- full_clean_panel |>
  distinct(Identifier, year, .keep_all = TRUE) |>
  arrange(Identifier, year)
  

full_clean_panel <- full_clean_panel |>
  arrange(Identifier, year) |>  # sort by firm and year
  group_by(Identifier) |>       # calculate within each firm
  mutate(
    Investment = capex / lag(total_assets),
    PPE_Growth = (ppe - lag(ppe)) / lag(total_assets),
    Asset_Growth = (total_assets - lag(total_assets)) / lag(total_assets)
  ) |>
  ungroup()

summary_table_b <- full_clean_panel |>
  summarise(
    N = c(
      sum(!is.na(Investment)),
      sum(!is.na(PPE_Growth)),
      sum(!is.na(Asset_Growth))
    ),
    Mean = c(
      mean(Investment, na.rm = TRUE),
      mean(PPE_Growth, na.rm = TRUE),
      mean(Asset_Growth, na.rm = TRUE)
    ),
    SD = c(
      sd(Investment, na.rm = TRUE),
      sd(PPE_Growth, na.rm = TRUE),
      sd(Asset_Growth, na.rm = TRUE)
    ),
    P10 = c(
      quantile(Investment, 0.10, na.rm = TRUE),
      quantile(PPE_Growth, 0.10, na.rm = TRUE),
      quantile(Asset_Growth, 0.10, na.rm = TRUE)
    ),
    Median = c(
      median(Investment, na.rm = TRUE),
      median(PPE_Growth, na.rm = TRUE),
      median(Asset_Growth, na.rm = TRUE)
    ),
    P90 = c(
      quantile(Investment, 0.90, na.rm = TRUE),
      quantile(PPE_Growth, 0.90, na.rm = TRUE),
      quantile(Asset_Growth, 0.90, na.rm = TRUE)
    )
  ) |>
  mutate(Variable = c("Investment", "PPE_Growth", "Asset_Growth")) |>
  select(Variable, N, Mean, SD, P10, Median, P90)


#Latex output

kable(
  summary_table_b,
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  align = "lrrrrrr",
  caption = "Table 3. Summary Statistics — Panel B: Investment Measures"
) |>
  kable_styling(
    latex_options = c("hold_position"),
    font_size = 10
  ) |>
  save_kable("table3_panelB.tex")

View(summary_table_b)


##### Merging firm sample data with tax data

tax_data_combined <- read_csv("~/Desktop/Mannheim/Causal Data Science/Research Paper Replication/tax_data_combined.csv")
View(tax_data_combined)

# Step 1: Add ISO3 country codes for the listed countries
#This is done to be able to merge the two datasets
full_clean_panel <- full_clean_panel |>
  mutate(
    country_code = recode(
      Country_of_Incorporation,
      "Australia" = "AUS",
      "Austria" = "AUT",
      "Belgium" = "BEL",
      "Canada" = "CAN" , 
      "Denmark" = "DNK",
      "Finland" = "FIN",
      "France" = "FRA",
      "Germany" = "DEU",
      "Greece" = "GRC",
      "Hungary" = "HUN",
      "Ireland" = "IRL",
      "Italy" = "ITA",
      "Japan" = "JPN",
      "Mexico" = "MEX",
      "Netherlands" = "NLD",
      "New Zealand" = "NZL",
      "Norway" = "NOR",
      "Poland" = "POL",
      "Portugal" = "PRT",
      "South Korea" = "KOR",
      "Spain" = "ESP",
      "Sweden" = "SWE",
      "Switzerland" = "CHE",
      "United Kingdom" = "GBR",
      "United States" = "USA"
    )
  )

# Step 2: Prepare tax dataset (must include country & year)
tax_data_clean <- tax_data_combined |>
  mutate(year = as.integer(year)) |>
  rename(country_code = countrycode) |>
  select(country_code, year,
         div_tax, cg_tax, corp_tax, avg_tax_c, eff_tax_c, div_penalty) |>
  distinct(country_code, year, .keep_all = TRUE)

# Step 3: Merge by country and year
full_clean_panel <- full_clean_panel |>
  mutate(year = as.integer(year)) |>
  left_join(
    tax_data_clean,
    by = c("country_code", "year")
  )

View(full_clean_panel) # inclusive of tax data

##### Table 3: summary statistics - Panel C

full_clean_panel <- full_clean_panel |>
  group_by(Identifier) |>
  mutate(
    cash       = ifelse(row_number() == 1, NA, cash_eq / lag(total_assets)),
    cash_flow  = ifelse(row_number() == 1, NA, ncfo / lag(total_assets)),
    EBITDA     = ifelse(row_number() == 1, NA, ebitda_ttm / lag(total_assets)),
    leverage   = debt / total_assets,
    q          = (mkt_cap + debt) / total_assets,
    # sales growth = log(revenue_t / revenue_{t-2})
    sales_growth = log(revenue / lag(revenue, 2)),
  ) |>
  group_by(country_code, year) |>
  mutate(
    # size = percentile rank of firm within country-year by total_assets
    size = percent_rank(total_assets)
  ) |>
  ungroup()

# Generating summary statistics
summary_table_c <- full_clean_panel |>
  summarise(
    N = c(
      sum(!is.na(div_tax)),
      sum(!is.na(eff_tax_c)),
      sum(!is.na(avg_tax_c)),
      sum(!is.na(cash_flow)),
      sum(!is.na(cash)),
      sum(!is.na(q)),
      sum(!is.na(leverage)),
      sum(!is.na(size))
    ),
    Mean = c(
      mean(div_tax, na.rm = TRUE),
      mean(eff_tax_c, na.rm = TRUE),
      mean(avg_tax_c, na.rm = TRUE),
      mean(cash_flow, na.rm = TRUE),
      mean(cash, na.rm = TRUE),
      mean(q, na.rm = TRUE),
      mean(leverage, na.rm = TRUE),
      mean(size, na.rm = TRUE)
    ),
    SD = c(
      sd(div_tax, na.rm = TRUE),
      sd(eff_tax_c, na.rm = TRUE),
      sd(avg_tax_c, na.rm = TRUE),
      sd(cash_flow, na.rm = TRUE),
      sd(cash, na.rm = TRUE),
      sd(q, na.rm = TRUE),
      sd(leverage, na.rm = TRUE),
      sd(size, na.rm = TRUE)
    ),
    P10 = c(
      quantile(div_tax, 0.10, na.rm = TRUE),
      quantile(eff_tax_c, 0.10, na.rm = TRUE),
      quantile(avg_tax_c, 0.10, na.rm = TRUE),
      quantile(cash_flow, 0.10, na.rm = TRUE),
      quantile(cash, 0.10, na.rm = TRUE),
      quantile(q, 0.10, na.rm = TRUE),
      quantile(leverage, 0.10, na.rm = TRUE),
      quantile(size, 0.10, na.rm = TRUE)
    ),
    Median = c(
      median(div_tax, na.rm = TRUE),
      median(eff_tax_c, na.rm = TRUE),
      median(avg_tax_c, na.rm = TRUE),
      median(cash_flow, na.rm = TRUE),
      median(cash, na.rm = TRUE),
      median(q, na.rm = TRUE),
      median(leverage, na.rm = TRUE),
      median(size, na.rm = TRUE)
    ),
    P90 = c(
      quantile(div_tax, 0.90, na.rm = TRUE),
      quantile(eff_tax_c, 0.90, na.rm = TRUE),
      quantile(avg_tax_c, 0.90, na.rm = TRUE),
      quantile(cash_flow, 0.90, na.rm = TRUE),
      quantile(cash, 0.90, na.rm = TRUE),
      quantile(q, 0.90, na.rm = TRUE),
      quantile(leverage, 0.90, na.rm = TRUE),
      quantile(size, 0.90, na.rm = TRUE)
    )
  ) |>
  mutate(Variable = c("div_tax", "eff_tax_c", "avg_tax_c","cash_flow", "cash",
                       "q", "leverage", "size")) |>
  select(Variable, N, Mean, SD, P10, Median, P90)


# LaTeX output
kable(
  summary_table_c,
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  align = "lrrrrrr",
  caption = "Table 3. Summary Statistics — Panel C: Taxes and Firm Characteristics"
) |>
  kable_styling(
    latex_options = c("hold_position"),
    font_size = 10
  ) |>
  save_kable("table3_panelC.tex")


View(summary_table_c)

#### Create distribution graphs
#### Figure A: cash, cash flow, leverage

variables_of_interest <- c("cash", "cash_flow", "leverage")

# Compute country-level averages per year
country_avg <- full_clean_panel |>
  group_by(country_code, year) |>
  summarise(across(all_of(variables_of_interest), mean, na.rm = TRUE), .groups = "drop") 

# Compute yearly statistics of country averages
stats_country_avg <- country_avg |>
  pivot_longer(cols = all_of(variables_of_interest),
               names_to = "variable",
               values_to = "value") |>
  group_by(year, variable) |>
  summarise(
    median_value = median(value, na.rm = TRUE),
    q1 = quantile(value, 0.25, na.rm = TRUE),
    q3 = quantile(value, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

# Plot
fig_country_avgs <- ggplot(stats_country_avg, aes(x = year)) +
  geom_line(aes(y = median_value), color = "navyblue", size = 1) +
  geom_line(aes(y = q1), color = "navyblue", linetype = "dotted") +
  geom_line(aes(y = q3), color = "navyblue", linetype = "dotted") +
  facet_wrap(~ variable, scales = "free_y") +
  labs(
    title = "Figure A: Cross-country median and interquartile range of firm-level averages",
    y = "Value",
    x = "Year"
  ) +
  theme_minimal()


ggsave(
  filename = "figure_country_level_averages.pdf",
  plot = fig_country_avgs,
  width = 8,
  height = 5
)






#### Figure B: Tax rate distribution across sample

# Select and reshape the tax variables
tax_long <- full_clean_panel |>
  select(div_tax, eff_tax_c, avg_tax_c) |>
  pivot_longer(cols = everything(),
               names_to = "tax_type",
               values_to = "tax_value") |>
  filter(!is.na(tax_value)) |>
  arrange(tax_type, tax_value) |>
  group_by(tax_type) |>
  mutate(
    obs_index = row_number()
  ) |>
  ungroup()

# Total observations for axis scaling
max_obs <- max(tax_long$obs_index)

# Define axis breaks
x_breaks <- seq(0, max_obs, by = 20000)
y_breaks <- seq(0, max(tax_long$tax_value, na.rm = TRUE), by = 20)

# Plot: Transposed CDF
tax_cdf_plot <- ggplot(tax_long, aes(x = obs_index, y = tax_value, color = tax_type, linetype = tax_type)) +
  geom_line(size = 1.2) +
  scale_x_continuous(
    name = "Cumulative Number of Observations",
    breaks = x_breaks,
    labels = scales::comma
  ) +
  scale_y_continuous(
    name = "Tax Rate (%)",
    breaks = y_breaks
  ) +
  scale_color_manual(
    name = "Tax Type",
    breaks = c("div_tax", "eff_tax_c", "avg_tax_c"),
    labels = c("Dividend Tax", "Effective Tax C", "Average Tax C"),
    values = c(
      "div_tax" = "navyblue",
      "eff_tax_c" = "lightblue",
      "avg_tax_c" = "blue"
    )
  ) +
  scale_linetype_manual(
    name = "Tax Type",
    breaks = c("div_tax", "eff_tax_c", "avg_tax_c"),
    labels = c("Dividend Tax", "Effective Tax C", "Average Tax C"),
    values = c(
      "div_tax" = "solid",
      "eff_tax_c" = "solid",
      "avg_tax_c" = "solid"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = c(0.25, 0.8),
    panel.grid.minor = element_blank()
  ) +
  ggtitle(
    "Figure B: Distribution of Tax Rates (1990–2008) - Transposed CDF",
    subtitle = "Dividend Tax, Effective Corporate Payout Tax, and Average Corporate Payout Tax"
  )

# Display plot
tax_cdf_plot

# Save as PDF
ggsave(
  filename = "tax_cdf_plot_three_blue.pdf",
  plot = tax_cdf_plot,
  width = 9,
  height = 5
)


##### Table 4: Difference in investment between high and low cash flow firms around payout tax changes (difference-in-difference)

##### Identify payout tax change events
pp_threshold <- 3   # Minimum tax change in percentage points
yr_min <- 1992L
yr_max <- 2006L



# Firm-level variables & country-year demeaning 
full_clean_panel <- full_clean_panel |>
  arrange(Company_Name, year) |>
  group_by(Company_Name) |>
  mutate(
    lag_assets = lag(total_assets),
    investment = capex / lag_assets,
    cash_flow  = ncfo / lag_assets
  ) |>
  ungroup() |>
  group_by(country_code, year) |>
  mutate(
    mean_inv_cy = mean(investment, na.rm = TRUE),
    mean_inv_cy = if_else(is.nan(mean_inv_cy), NA_real_, mean_inv_cy),
    inv_demeaned = investment - mean_inv_cy
  ) |>
  ungroup() |>
  select(-mean_inv_cy)

# Identify payout-tax events 
firm_counts <- full_clean_panel |>
  filter(!is.na(Identifier)) |>
  group_by(country_code, year) |>
  summarise(n_companies = n_distinct(Identifier), .groups = "drop")

tax_levels <- full_clean_panel |>
  filter(!is.na(avg_tax_c)) |>
  group_by(country_code, year) |>
  summarise(avg_tax_c = mean(avg_tax_c, na.rm = TRUE), .groups = "drop")

tax_events_raw <- tax_levels |>
  arrange(country_code, year) |>
  group_by(country_code) |>
  mutate(
    delta_tax = avg_tax_c - lag(avg_tax_c),
    event_type = case_when(
      delta_tax >= pp_threshold ~ "increase",
      delta_tax <= -pp_threshold ~ "decrease",
      TRUE ~ NA_character_
    )
  ) |>
  ungroup() |>
  filter(!is.na(event_type), between(year, yr_min, yr_max)) |>
  rename(event_year = year)

tax_events <- tax_events_raw |>
  inner_join(firm_counts |> filter(n_companies >= 30),
             by = c("country_code", "event_year" = "year")) |>
  select(country_code, event_year, event_type, delta_tax, avg_tax_c, n_companies)

# Drop overlapping opposite-sign events
tmp_events <- tax_events |> select(country_code, event_year, event_type)
opposite_pairs <- tmp_events |>
  inner_join(tmp_events, by = "country_code", suffix = c("", "_next")) |>
  filter(event_year_next > event_year,
         event_year_next - event_year <= 2,
         event_type_next != event_type)
events_to_drop <- bind_rows(
  opposite_pairs |> transmute(country_code, event_year, event_type),
  opposite_pairs |> transmute(country_code, event_year = event_year_next, event_type = event_type_next)
) |> distinct()
tax_events <- tax_events |> anti_join(events_to_drop, by = c("country_code", "event_year", "event_type"))

# Explicit exclusions
tax_events <- tax_events |>
  filter(
    !(country_code == "SWE" & event_year %in% c(1994, 1995)),
    !(country_code == "AUS" & event_year %in% c(2000, 2001)),
    !(country_code == "NOR" & event_year %in% c(2001, 2002)),
    !(country_code == "KOR" & event_year %in% c(1994, 1999, 2001)),
    !(country_code == "BEL" & event_year == 1995),
    !(country_code == "NOR" & event_year == 1992),
    !(country_code == "POL" & event_year == 1999),
    !(country_code == "ESP" & event_year == 1995),
    !(country_code == "IRL" & event_year == 2000),
    !(country_code == "NZL" & event_year == 2000),
    !(country_code == "FIN" & event_year == 1993)
  )

# Assign cash flow quintiles (bottom/top only) 
full_clean_panel <- full_clean_panel |>
  group_by(country_code, year) |>
  mutate(cf_quintile = ntile(cash_flow, 5)) |>
  ungroup()

# Expand each tax event into t-4 to t+2 horizon 
tax_event_horizons <- tax_events |>
  group_by(country_code, event_year, event_type) |>
  summarise(horizon = list(-5:2), .groups="drop") |>
  tidyr::unnest(horizon) |>
  mutate(year = event_year + horizon)

# Merge panel data with expanded tax events 
panel_events <- full_clean_panel |>
  inner_join(tax_event_horizons, by = c("country_code", "year"))

# Keep only bottom/top quintiles 
panel_events <- panel_events |>
  filter(cf_quintile %in% c(1,5))

# Define pre/post periods relative to the event 
panel_events <- panel_events |>
  mutate(period = case_when(
    horizon >= -5 & horizon <= -1 ~ "pre",
    horizon >= 0 & horizon <= 2   ~ "post"
  )) |>
  filter(!is.na(period))


View( panel_events |>
  group_by(event_type, event_year, period, cf_quintile) |>
  tally() |>
  ungroup() |>
  complete(event_type, event_year, period, cf_quintile = c(1,5)))


# Print table with unique events

tax_events_unique <- tax_events |>
  arrange(country_code, event_year, event_type) |>
  distinct(country_code, event_year, event_type, .keep_all = TRUE) |>
  mutate(
    delta_tax_pp = round(delta_tax, 3),   
    avg_tax_c    = round(avg_tax_c, 3)
  ) |>
  select(country_code, event_year, event_type, delta_tax_pp, avg_tax_c, n_companies)


View(tax_events_unique)


knitr::kable(
  tax_events_unique,
  format = "simple",
  col.names = c("Country", "Event year", "Type", "Δ tax (pp)", "Avg tax", "N firms")
)



###### Table 4
# Compute mean and SE per event, horizon, and cash flow quintile 
event_summary <- panel_events |>
  group_by(event_type, event_year, period, cf_quintile) |>
  summarise(
    mean_inv = mean(inv_demeaned, na.rm = TRUE),
    se_inv = sd(inv_demeaned, na.rm = TRUE)/sqrt(n()),
    .groups = "drop"
  ) |>
  pivot_wider(names_from = cf_quintile, values_from = c(mean_inv, se_inv)) |>
  rename(
    mean_low = mean_inv_1,
    mean_high = mean_inv_5,
    se_low = se_inv_1,
    se_high = se_inv_5
  ) |>
  mutate(
    diff_high_low = mean_high - mean_low,
    se_diff = sqrt(se_low^2 + se_high^2)
  )

# Average across events for each event type and period 
summary_table <- event_summary |>
  group_by(event_type, period) |>
  summarise(
    mean_low = mean(mean_low, na.rm = TRUE),
    mean_high = mean(mean_high, na.rm = TRUE),
    diff_high_low = mean(diff_high_low, na.rm = TRUE),
    se_low = sqrt(sum(se_low^2)/n()),
    se_high = sqrt(sum(se_high^2)/n()),
    se_diff = sqrt(sum(se_diff^2)/n()),
    .groups = "drop"
  )

# Compute difference-in-difference (post - pre) 
did_table <- summary_table |>
  group_by(event_type) |>
  summarise(
    diff_low = mean_low[period=="post"] - mean_low[period=="pre"],
    diff_high = mean_high[period=="post"] - mean_high[period=="pre"],
    diff_diff = diff_high_low[period=="post"] - diff_high_low[period=="pre"],
    se_diff_low = sqrt(se_low[period=="pre"]^2 + se_low[period=="post"]^2),
    se_diff_high = sqrt(se_high[period=="pre"]^2 + se_high[period=="post"]^2),
    se_diff_diff = sqrt(se_diff[period=="pre"]^2 + se_diff[period=="post"]^2),
    .groups = "drop"
  )

# Add significance stars 
add_stars <- function(estimate, se) {
  t_stat <- abs(estimate / se)
  stars <- ifelse(t_stat >= 2.58, "***",
                  ifelse(t_stat >= 1.96, "**",
                         ifelse(t_stat >= 1.65, "*", "")))
  paste0(round(estimate, 4), stars)
}

# Build final panel tables 
final_panel <- function(event_type_name) {
  panel <- summary_table |> filter(event_type==event_type_name)
  did <- did_table |> filter(event_type==event_type_name)
  
  tibble(
    Row = c("Pre-reform (t-4;t-1)", "Post-reform (t;t+2)", "Difference between periods"),
    `Low CF` = c(
      add_stars(panel$mean_low[panel$period=="pre"], panel$se_low[panel$period=="pre"]),
      add_stars(panel$mean_low[panel$period=="post"], panel$se_low[panel$period=="post"]),
      add_stars(did$diff_low, did$se_diff_low)
    ),
    `High CF` = c(
      add_stars(panel$mean_high[panel$period=="pre"], panel$se_high[panel$period=="pre"]),
      add_stars(panel$mean_high[panel$period=="post"], panel$se_high[panel$period=="post"]),
      add_stars(did$diff_high, did$se_diff_high)
    ),
    `Difference (High-Low)` = c(
      add_stars(panel$diff_high_low[panel$period=="pre"], panel$se_diff[panel$period=="pre"]),
      add_stars(panel$diff_high_low[panel$period=="post"], panel$se_diff[panel$period=="post"]),
      add_stars(did$diff_diff, did$se_diff_diff)
    )
  )
}

# Generate Panel A (tax increase) and Panel B (tax decrease) 
panel_A <- final_panel("increase")
panel_B <- final_panel("decrease")


kable(panel_A, format = "latex", booktabs = TRUE, 
      caption = "Firm Investment Response around Tax Increase Events",
      align = c("l", "c", "c", "c")) |>
  add_header_above(c(" " = 1, "Low Cash Flow" = 1, "High Cash Flow" = 1, "Difference (High-Low)" = 1)) |>
  kable_styling(latex_options = c("hold_position", "scale_down"))

# Panel B: Tax Decrease 
kable(panel_B, format = "latex", booktabs = TRUE, 
      caption = "Firm Investment Response around Tax Decrease Events",
      align = c("l", "c", "c", "c")) |>
  add_header_above(c(" " = 1, "Low Cash Flow" = 1, "High Cash Flow" = 1, "Difference (High-Low)" = 1)) |>
  kable_styling(latex_options = c("hold_position", "scale_down"))




# Assign High/Low cash flow 
full_clean_panel <- full_clean_panel |>
  group_by(country_code, year) |>
  mutate(
    cf_group = case_when(
      is.na(cash_flow) ~ NA_character_,
      cash_flow <= median(cash_flow, na.rm = TRUE) ~ "Low",
      cash_flow >  median(cash_flow, na.rm = TRUE) ~ "High"
    )
  ) |>
  ungroup()

# Expand each tax event into t-4 to t+2 horizon 
tax_event_horizons <- tax_events |>
  group_by(country_code, event_year, event_type) |>
  summarise(horizon = list(-4:2), .groups="drop") |>
  tidyr::unnest(horizon) |>
  mutate(year = event_year + horizon)

#  Merge panel data with expanded tax events 
panel_events_median <- full_clean_panel |>
  inner_join(tax_event_horizons, by = c("country_code", "year"))

#  Keep only High/Low cash flow groups 
panel_events_median <- panel_events_median |>
  filter(cf_group %in% c("Low", "High"))

#  Define pre/post periods relative to the event 
panel_events_median <- panel_events_median |>
  mutate(period = case_when(
    horizon >= -4 & horizon <= -1 ~ "pre",
    horizon >= 0 & horizon <= 2   ~ "post"
  )) |>
  filter(!is.na(period))

# Compute mean and SE per event, horizon, and cash flow group 
event_summary_median <- panel_events_median |>
  group_by(event_type, event_year, period, cf_group) |>
  summarise(
    mean_inv = mean(inv_demeaned, na.rm = TRUE),
    se_inv = sd(inv_demeaned, na.rm = TRUE)/sqrt(n()),
    .groups = "drop"
  ) |>
  pivot_wider(names_from = cf_group, values_from = c(mean_inv, se_inv)) |>
  rename(
    mean_low = mean_inv_Low,
    mean_high = mean_inv_High,
    se_low   = se_inv_Low,
    se_high  = se_inv_High
  ) |>
  mutate(
    diff_high_low = mean_high - mean_low,
    se_diff = sqrt(se_low^2 + se_high^2)
  )

# Average across events & compute difference-in-difference 
summary_table_median <- event_summary_median |>
  group_by(event_type, period) |>
  summarise(
    mean_low = mean(mean_low, na.rm = TRUE),
    mean_high = mean(mean_high, na.rm = TRUE),
    diff_high_low = mean(diff_high_low, na.rm = TRUE),
    se_low = sqrt(sum(se_low^2)/n()),
    se_high = sqrt(sum(se_high^2)/n()),
    se_diff = sqrt(sum(se_diff^2)/n()),
    .groups = "drop"
  )

did_table_median <- summary_table_median |>
  group_by(event_type) |>
  summarise(
    diff_low = mean_low[period=="post"] - mean_low[period=="pre"],
    diff_high = mean_high[period=="post"] - mean_high[period=="pre"],
    diff_diff = diff_high_low[period=="post"] - diff_high_low[period=="pre"],
    se_diff_low = sqrt(se_low[period=="pre"]^2 + se_low[period=="post"]^2),
    se_diff_high = sqrt(se_high[period=="pre"]^2 + se_high[period=="post"]^2),
    se_diff_diff = sqrt(se_diff[period=="pre"]^2 + se_diff[period=="post"]^2),
    .groups = "drop"
  )

# Build final panel tables with significance stars 
final_panel_median <- function(event_type_name) {
  panel <- summary_table_median |> filter(event_type==event_type_name)
  did <- did_table_median |> filter(event_type==event_type_name)
  
  tibble(
    Row = c("Pre-reform (t-4;t-1)", "Post-reform (t;t+2)", "Difference between periods"),
    `Low CF` = c(
      add_stars(panel$mean_low[panel$period=="pre"], panel$se_low[panel$period=="pre"]),
      add_stars(panel$mean_low[panel$period=="post"], panel$se_low[panel$period=="post"]),
      add_stars(did$diff_low, did$se_diff_low)
    ),
    `High CF` = c(
      add_stars(panel$mean_high[panel$period=="pre"], panel$se_high[panel$period=="pre"]),
      add_stars(panel$mean_high[panel$period=="post"], panel$se_high[panel$period=="post"]),
      add_stars(did$diff_high, did$se_diff_high)
    ),
    `Difference (High-Low)` = c(
      add_stars(panel$diff_high_low[panel$period=="pre"], panel$se_diff[panel$period=="pre"]),
      add_stars(panel$diff_high_low[panel$period=="post"], panel$se_diff[panel$period=="post"]),
      add_stars(did$diff_diff, did$se_diff_diff)
    )
  )
}

# Generate Panel A and Panel B 
panel_A_median <- final_panel_median("increase")
panel_B_median <- final_panel_median("decrease")

panel_A_median
panel_B_median




##### Table 5: Internal resources and investment under different taxes (linear regression)


# Arrange and create variables
full_clean_panel <- full_clean_panel |>
  arrange(Company_Name, year) |>
  mutate(
    Investment   = capex / lag(total_assets),
    cash_flow    = ncfo / lag(total_assets),
    sales_growth = log(revenue / lag(revenue, 2)),
    leverage     = debt / lag(total_assets),
    q            = (mkt_cap + debt) / lag(total_assets),
    size         = percent_rank(total_assets)
  )

# Remove outliers outside 1st and 99th percentile
dataset_table_5 <- full_clean_panel |>
  filter(
    between(Investment, quantile(Investment, 0.01, na.rm = TRUE), quantile(Investment, 0.99, na.rm = TRUE)),
    between(cash_flow, quantile(cash_flow, 0.01, na.rm = TRUE), quantile(cash_flow, 0.99, na.rm = TRUE)),
    between(sales_growth, quantile(sales_growth, 0.01, na.rm = TRUE), quantile(sales_growth, 0.99, na.rm = TRUE)),
    between(leverage, quantile(leverage, 0.01, na.rm = TRUE), quantile(leverage, 0.99, na.rm = TRUE)),
    between(q, quantile(q, 0.01, na.rm = TRUE), quantile(q, 0.99, na.rm = TRUE)), 
    between(size, quantile(size, 0.01, na.rm = TRUE), quantile(size, 0.99, na.rm = TRUE))
  )

# Models


m1 <- feols(
  Investment ~ cash_flow * div_tax + sales_growth + leverage + size + q |
    Identifier + country_code^year,
  cluster = ~ country_code^year,
  data = dataset_table_5
)

m2 <- feols(
  Investment ~ cash_flow * eff_tax_c + sales_growth + leverage + size + q |
    Identifier + country_code^year,
  cluster = ~ country_code^year,
  data = dataset_table_5
)

m3 <- feols(
  Investment ~ cash_flow * avg_tax_c + sales_growth + leverage + size + q |
    Identifier + country_code^year,
  cluster = ~ country_code^year,
  data = dataset_table_5
)


# Output table


modelsummary(
  list(
    "Dividend tax rate" = m1,
    "Country-weighted effective tax rate" = m2,
    "Country-weighted average tax rate" = m3
  ),
  stars = TRUE,
  statistic = "({std.error})",
  coef_map = c(
    "cash_flow:div_tax" = "Cash flow × Tax",
    "cash_flow:eff_tax_c" = "Cash flow × Tax",
    "cash_flow:avg_tax_c" = "Cash flow × Tax",
    "cash_flow" = "Cash flow",
    "sales_growth" = "Sales growth",
    "leverage" = "Leverage",
    "size" = "Size",
    "q" = "Q"
  ),
  gof_map = c("nobs", "r.squared"),
  title = "Firm investment and internal resources under various tax regimes",

)


###### Robustness checks 


##### Checking US and Japan dominance effects (Table A.V Appendix  )
# We identified the most issues with data points from Japan and the US. 
# For this reason, we test our results in a subsample, excluding the two countries. 

RC1_table5 <- dataset_table_5 |>
  filter(!(country_code %in% c("JPN", "USA")))

# Models

m1_RC1 <- feols(
  Investment ~ cash_flow * div_tax + sales_growth + leverage + size + q |
    Identifier + country_code^year,
  cluster = ~ country_code^year,
  data = RC1_table5
)

m2_RC1 <- feols(
  Investment ~ cash_flow * eff_tax_c + sales_growth + leverage + size + q |
    Identifier + country_code^year,
  cluster = ~ country_code^year,
  data = RC1_table5
)

m3_RC1 <- feols(
  Investment ~ cash_flow * avg_tax_c + sales_growth + leverage + size + q |
    Identifier + country_code^year,
  cluster = ~ country_code^year,
  data = RC1_table5
)


#Output table

modelsummary(
  list(
    "Dividend tax rate" = m1_RC1,
    "Country-weighted effective tax rate" = m2_RC1,
    "Country-weighted average tax rate" = m3_RC1
  ),
  stars = TRUE,
  statistic = "({std.error})",
  coef_map = c(
    "cash_flow:div_tax" = "Cash flow × Tax",
    "cash_flow:eff_tax_c" = "Cash flow × Tax",
    "cash_flow:avg_tax_c" = "Cash flow × Tax",
    "cash_flow" = "Cash flow",
    "sales_growth" = "Sales growth",
    "leverage" = "Leverage",
    "size" = "Size",
    "q" = "Q"
  ),
  gof_map = c("nobs", "r.squared"),
  title = "Robustness check: Firm investment and internal resources under various tax regimes (excluding USA and JPN)",
  
)


##### Winsorization at the 2%
RBC2_table5 <- full_clean_panel |>
  mutate(
    Investment = DescTools::Winsorize(
      Investment,
      val = quantile(Investment, c(0.02, 0.98), na.rm = TRUE)
    ),
    cash_flow = DescTools::Winsorize(
      cash_flow,
      val = quantile(cash_flow, c(0.02, 0.98), na.rm = TRUE)
    ),
    sales_growth = DescTools::Winsorize(
      sales_growth,
      val = quantile(sales_growth, c(0.02, 0.98), na.rm = TRUE)
    ),
    leverage = DescTools::Winsorize(
      leverage,
      val = quantile(leverage, c(0.02, 0.98), na.rm = TRUE)
    ),
    q = DescTools::Winsorize(
      q,
      val = quantile(q, c(0.02, 0.98), na.rm = TRUE)
    ),
    size = DescTools::Winsorize(
      size,
      val = quantile(size, c(0.02, 0.98), na.rm = TRUE)
    )
)

RBC2_m1 <- feols(
  Investment ~ cash_flow * div_tax + sales_growth + leverage + size + q |
    Identifier + country_code^year,
  cluster = ~ country_code^year,
  data = RBC2_table5
)

RBC2_m2 <- feols(
  Investment ~ cash_flow * eff_tax_c + sales_growth + leverage + size + q |
    Identifier + country_code^year,
  cluster = ~ country_code^year,
  data = RBC2_table5
)

RBC2_m3 <- feols(
  Investment ~ cash_flow * avg_tax_c + sales_growth + leverage + size + q |
    Identifier + country_code^year,
  cluster = ~ country_code^year,
  data = RBC2_table5
)


# Output table


modelsummary(
  list(
    "Dividend tax rate" = RBC2_m1,
    "Country-weighted effective tax rate" = RBC2_m2,
    "Country-weighted average tax rate" = RBC2_m3
  ),
  stars = TRUE,
  statistic = "({std.error})",
  coef_map = c(
    "cash_flow:div_tax" = "Cash flow × Tax",
    "cash_flow:eff_tax_c" = "Cash flow × Tax",
    "cash_flow:avg_tax_c" = "Cash flow × Tax",
    "cash_flow" = "Cash flow",
    "sales_growth" = "Sales growth",
    "leverage" = "Leverage",
    "size" = "Size",
    "q" = "Q"
  ),
  gof_map = c("nobs", "r.squared"),
  title = "Firm investment and internal resources under various tax regimes (2% winsorization)",
  output= "latex"
)

##### 5% winsorization
RBC3_table5 <- full_clean_panel |>
  mutate(
    Investment = DescTools::Winsorize(
      Investment,
      val = quantile(Investment, c(0.05, 0.95), na.rm = TRUE)
    ),
    cash_flow = DescTools::Winsorize(
      cash_flow,
      val = quantile(cash_flow, c(0.05, 0.95), na.rm = TRUE)
    ),
    sales_growth = DescTools::Winsorize(
      sales_growth,
      val = quantile(sales_growth, c(0.05, 0.95), na.rm = TRUE)
    ),
    leverage = DescTools::Winsorize(
      leverage,
      val = quantile(leverage, c(0.05, 0.95), na.rm = TRUE)
    ),
    q = DescTools::Winsorize(
      q,
      val = quantile(q, c(0.05, 0.95), na.rm = TRUE)
    ),
    size = DescTools::Winsorize(
      size,
      val = quantile(size, c(0.05, 0.95), na.rm = TRUE)
    )
  )

RBC3_m1 <- feols(
  Investment ~ cash_flow * div_tax + sales_growth + leverage + size + q |
    Identifier + country_code^year,
  cluster = ~ country_code^year,
  data = RBC3_table5
)

RBC3_m2 <- feols(
  Investment ~ cash_flow * eff_tax_c + sales_growth + leverage + size + q |
    Identifier + country_code^year,
  cluster = ~ country_code^year,
  data = RBC3_table5
)

RBC3_m3 <- feols(
  Investment ~ cash_flow * avg_tax_c + sales_growth + leverage + size + q |
    Identifier + country_code^year,
  cluster = ~ country_code^year,
  data = RBC3_table5
)


# Output table


modelsummary(
  list(
    "Dividend tax rate" = RBC3_m1,
    "Country-weighted effective tax rate" = RBC3_m2,
    "Country-weighted average tax rate" = RBC3_m3
  ),
  stars = TRUE,
  statistic = "({std.error})",
  coef_map = c(
    "cash_flow:div_tax" = "Cash flow × Tax",
    "cash_flow:eff_tax_c" = "Cash flow × Tax",
    "cash_flow:avg_tax_c" = "Cash flow × Tax",
    "cash_flow" = "Cash flow",
    "sales_growth" = "Sales growth",
    "leverage" = "Leverage",
    "size" = "Size",
    "q" = "Q"
  ),
  gof_map = c("nobs", "r.squared"),
  title = "Firm investment and internal resources under various tax regimes (5% winsorization)",
  output= "latex"
)

###### Using alternative measures for investment (Table A.VII Appendix )

full_clean_panel <- full_clean_panel |>
  arrange(Identifier, year) |>
  group_by(Identifier) |>
  mutate( ppe_growth   = (ppe - lag(ppe)) / lag(ppe),                 # Panel A (1)-(3)
          asset_growth = (total_assets - lag(total_assets)) / lag(total_assets), # Panel A (4)-(6)
          capex_ppe    = capex / lag(ppe),                             # Panel B (1)-(3)
          capex_fa     = capex / lag(total_assets)                     # Panel B (4)-(6)
  ) |>
  ungroup() |>
  group_by(country_code, year) |>
  mutate(size = percent_rank(total_assets)) |>
  ungroup()



# Remove outliers outside 1st and 99th percentile
RBC4_table_5 <- full_clean_panel |>
  filter(
    between(ppe_growth, quantile(ppe_growth, 0.01, na.rm = TRUE), quantile(ppe_growth, 0.99, na.rm = TRUE)),
    between(asset_growth, quantile(asset_growth, 0.01, na.rm = TRUE), quantile(asset_growth, 0.99, na.rm = TRUE)),
    between(capex_ppe, quantile(capex_ppe, 0.01, na.rm = TRUE), quantile(capex_ppe, 0.99, na.rm = TRUE)),
    between(capex_fa, quantile(capex_fa, 0.01, na.rm = TRUE), quantile(capex_fa, 0.99, na.rm = TRUE)),
    between(cash_flow, quantile(cash_flow, 0.01, na.rm = TRUE), quantile(cash_flow, 0.99, na.rm = TRUE)),
    between(sales_growth, quantile(sales_growth, 0.01, na.rm = TRUE), quantile(sales_growth, 0.99, na.rm = TRUE)),
    between(leverage, quantile(leverage, 0.01, na.rm = TRUE), quantile(leverage, 0.99, na.rm = TRUE)),
    between(q, quantile(q, 0.01, na.rm = TRUE), quantile(q, 0.99, na.rm = TRUE)), 
    between(size, quantile(size, 0.01, na.rm = TRUE), quantile(size, 0.99, na.rm = TRUE))
  )

#Panel A - Growth based investment measures

# Panel A: PPE growth
mA1 <- feols(
  ppe_growth ~ cash_flow : div_tax + sales_growth + leverage + size + q |
    Identifier + country_code^year,
  cluster = ~ country_code^year,
  data = RBC4_table_5
)

mA2 <- feols(
  ppe_growth ~ cash_flow : eff_tax_c + sales_growth + leverage + size + q |
    Identifier + country_code^year,
  cluster = ~ country_code^year,
  data = RBC4_table_5
)

mA3 <- feols(
  ppe_growth ~ cash_flow : avg_tax_c + sales_growth + leverage + size + q |
    Identifier + country_code^year,
  cluster = ~ country_code^year,
  data = RBC4_table_5
)

# Panel A: Asset growth
mA4 <- feols(
  asset_growth ~ cash_flow : div_tax + sales_growth + leverage + size + q |
    Identifier + country_code^year,
  cluster = ~ country_code^year,
  data = RBC4_table_5
)

mA5 <- feols(
  asset_growth ~ cash_flow : eff_tax_c + sales_growth + leverage + size + q |
    Identifier + country_code^year,
  cluster = ~ country_code^year,
  data = RBC4_table_5
)

mA6 <- feols(
  asset_growth ~ cash_flow : avg_tax_c + sales_growth + leverage + size + q |
    Identifier + country_code^year,
  cluster = ~ country_code^year,
  data = RBC4_table_5
)


# Panel B  - Capex based investment measures


# Panel B: CapEx / PPE
mB1 <- feols(
  capex_ppe ~ cash_flow : div_tax + sales_growth + leverage + size + q |
    Identifier + country_code^year,
  cluster = ~ country_code^year,
  data = RBC4_table_5
)

mB2 <- feols(
  capex_ppe ~ cash_flow : eff_tax_c + sales_growth + leverage + size + q |
    Identifier + country_code^year,
  cluster = ~ country_code^year,
  data = RBC4_table_5
)

mB3 <- feols(
  capex_ppe ~ cash_flow : avg_tax_c + sales_growth + leverage + size + q |
    Identifier + country_code^year,
  cluster = ~ country_code^year,
  data = RBC4_table_5
)

# Panel B: CapEx / Fixed Assets
mB4 <- feols(
  capex_fa ~ cash_flow : div_tax + sales_growth + leverage + size + q |
    Identifier + country_code^year,
  cluster = ~ country_code^year,
  data = RBC4_table_5
)

mB5 <- feols(
  capex_fa ~ cash_flow : eff_tax_c + sales_growth + leverage + size + q |
    Identifier + country_code^year,
  cluster = ~ country_code^year,
  data = RBC4_table_5
)

mB6 <- feols(
  capex_fa ~ cash_flow : avg_tax_c + sales_growth + leverage + size + q |
    Identifier + country_code^year,
  cluster = ~ country_code^year,
  data = RBC4_table_5)
  
  
modelsummary(
  list(
    "(1)" = mA1, "(2)" = mA2, "(3)" = mA3,
    "(4)" = mA4, "(5)" = mA5, "(6)" = mA6
  ),
  coef_map = c(
    "cash_flow:div_tax"    = "Cash Flow × Tax",
    "cash_flow:eff_tax_c" = "Cash Flow × Tax",
    "cash_flow:avg_tax_c" = "Cash Flow × Tax"
  ),
  statistic = "({std.error})",
  stars = c("*" = .10, "**" = .05, "***" = .01),
  gof_map = c("nobs", "r.squared"),
  add_rows = data.frame(
    term = c(
      "Baseline Controls",
      "Firm Fixed Effects",
      "Country–Year Fixed Effects",
      "Year × Cash Flow",
      "Country × Cash Flow"
    ),
    `(1)` = "Yes", `(2)` = "Yes", `(3)` = "Yes",
    `(4)` = "Yes", `(5)` = "Yes", `(6)` = "Yes",
    check.names = FALSE
  ),
  title = "Firm Investment and Internal Resources under Various Tax Regimes — Panel A",
  
  file = "panelA_investment_table.tex",
  colgroup = c(
    "PPE Growth"    = 3,
    "Assets Growth" = 3
  )
)
    

  #panel b
modelsummary(
  list(
    "(1)" = mB1, "(2)" = mB2, "(3)" = mB3,
    "(4)" = mB4, "(5)" = mB5, "(6)" = mB6
  ),
  coef_map = c(
    "cash_flow:div_tax"    = "Cash Flow × Tax",
    "cash_flow:eff_tax_c" = "Cash Flow × Tax",
    "cash_flow:avg_tax_c" = "Cash Flow × Tax"
  ),
  statistic = "({std.error})",
  stars = c("*" = .10, "**" = .05, "***" = .01),
  gof_map = c("nobs", "r.squared"),
  add_rows = data.frame(
    term = c(
      "Baseline Controls",
      "Firm Fixed Effects",
      "Country–Year Fixed Effects",
      "Year × Cash Flow",
      "Country × Cash Flow"
    ),
    `(1)` = "Yes", `(2)` = "Yes", `(3)` = "Yes",
    `(4)` = "Yes", `(5)` = "Yes", `(6)` = "Yes",
    check.names = FALSE
  ),
  title = "Firm Investment and Internal Resources under Various Tax Regimes — Panel B",

  file = "panelB_investment_table.tex",
  colgroup = c(
    "Capex/ PPE"    = 3,
    "Capex / FA" = 3
  )
)
  







###### Table 6

dataset_table6 <- full_clean_panel |>
  filter(
    between(Investment, quantile(Investment, 0.01, na.rm = TRUE), quantile(Investment, 0.99, na.rm = TRUE)),
    between(cash_flow, quantile(cash_flow, 0.01, na.rm = TRUE), quantile(cash_flow, 0.99, na.rm = TRUE)),
    between(EBITDA, quantile(EBITDA, 0.01, na.rm = TRUE), quantile(EBITDA, 0.99, na.rm = TRUE)),
    between(sales_growth, quantile(sales_growth, 0.01, na.rm = TRUE), quantile(sales_growth, 0.99, na.rm = TRUE)),
    between(leverage, quantile(leverage, 0.01, na.rm = TRUE), quantile(leverage, 0.99, na.rm = TRUE)),
    between(q, quantile(q, 0.01, na.rm = TRUE), quantile(q, 0.99, na.rm = TRUE)),
    between(size, quantile(size, 0.01, na.rm = TRUE), quantile(size, 0.99, na.rm = TRUE)), 
    between(cash, quantile(cash, 0.01, na.rm = TRUE), quantile(cash, 0.99, na.rm = TRUE))
  )

# Step 2: Run regressions
# Cash-based regressions
m1 <- feols(Investment ~ cash_flow * div_tax + sales_growth + leverage + size + q |
              Identifier + country_code^year,
            cluster = ~ country_code^year,
            data = dataset_table6)

m3 <- feols(Investment ~ cash_flow * eff_tax_c + sales_growth + leverage + size + q |
              Identifier + country_code^year,
            cluster = ~ country_code^year,
            data = dataset_table6)

m5 <- feols(Investment ~ cash_flow * avg_tax_c + sales_growth + leverage + size + q |
              Identifier + country_code^year,
            cluster = ~ country_code^year,
            data = dataset_table6)

# EBITDA-based regressions
m2 <- feols(Investment ~ EBITDA * div_tax + sales_growth + leverage + size + q |
              Identifier + country_code^year,
            cluster = ~ country_code^year,
            data = dataset_table6)

m4 <- feols(Investment ~ EBITDA * eff_tax_c + sales_growth + leverage + size + q |
              Identifier + country_code^year,
            cluster = ~ country_code^year,
            data = dataset_table6)

m6 <- feols(Investment ~ EBITDA * avg_tax_c + sales_growth + leverage + size + q |
              Identifier + country_code^year,
            cluster = ~ country_code^year,
            data = dataset_table6)

# Step 3: Display in one table
modelsummary(
  list(
    "Cash × Dividend tax"      = m1,
    "EBITDA × Dividend tax"    = m2,
    "Cash × Effective tax C"   = m3,
    "EBITDA × Effective tax C" = m4,
    "Cash × Average tax C"     = m5,
    "EBITDA × Average tax C"   = m6
  ),
  stars = TRUE,
  statistic = "({std.error})",
  coef_map = c(
    "cash_flow:div_tax" = "Cash × Tax",
    "EBITDA:div_tax"    = "EBITDA × Tax",
    "cash_flow:eff_tax_c" = "Cash × Tax",
    "EBITDA:eff_tax_c"    = "EBITDA × Tax",
    "cash_flow:avg_tax_c" = "Cash × Tax",
    "EBITDA:avg_tax_c"    = "EBITDA × Tax",
    "cash_flow" = "Cash",
    "EBITDA"    = "EBITDA",
    "sales_growth" = "Sales growth",
    "leverage" = "Leverage",
    "size" = "Size",
    "q" = "Q"
  ),
  gof_map = c("nobs", "r.squared"),
  title = "Firm investment and internal resources under various tax regimes—alternative measures",
  
)


###### Table 7


table7_m1 <- feols(
  Investment ~ cash_flow:div_tax +
    sales_growth + leverage + size + q +
    i(country_code, cash_flow) +
    i(year, cash_flow) |
    Identifier + country_code^year,
  cluster = ~ country_code^year,
  data = dataset_table_5
)

table7_m2 <- feols(
  Investment ~ cash_flow :eff_tax_c +
    sales_growth + leverage + size + q +
    i(country_code, cash_flow) +
    i(year, cash_flow) |
    Identifier + country_code^year,
  cluster = ~ country_code^year,
  data = dataset_table_5
)

table7_m3 <- feols(
  Investment ~ cash_flow : avg_tax_c +
    sales_growth + leverage + size + q +
    i(country_code, cash_flow) +
    i(year, cash_flow) |
    Identifier + country_code^year,
  cluster = ~ country_code^year,
  data = dataset_table_5
)


modelsummary(
  list(
    "Dividend tax rate"               = table7_m1,
    "Country-weighted effective tax rate" = table7_m2,
    "Country-weighted average tax rate"   = table7_m3
  ),
  stars     = TRUE,
  statistic = "({std.error})",
  coef_map  = c(
    "cash_flow:div_tax"        = "Cash flow × Tax",
    "cash_flow:eff_tax_c"      = "Cash flow × Tax",
    "cash_flow:avg_tax_c"      = "Cash flow × Tax",
    "sales_growth"             = "Sales growth",
    "leverage"                 = "Leverage",
    "size"                     = "Size",
    "q"                        = "Q"
  ),
  gof_map = c("nobs", "r.squared"),
  title   = "Firm investment and internal resources under various tax regimes—controlling for country-year and year-country cash flow effects",
  
)



####### Table 8 

#  Compute stock price appreciation 
full_clean_panel <- full_clean_panel |>
  arrange(Identifier, year) |>
  group_by(Identifier) |>
  mutate(
    stock_price     = mkt_cap / common_shares,
    stock_price_app = (stock_price / lag(stock_price)) - 1
  ) |>
  ungroup()

# Adding the dependent variable to the dataset, measured as common equity / total assets

full_clean_panel <- full_clean_panel |>
  arrange(Identifier, year) |>
  group_by(Identifier) |>
  mutate(
    equity_to_assets = (common_equity - lag(common_equity) + stock_repurchased) / lag(total_assets)
  ) |>
  ungroup()



#  Eliminate outliers for dependent and independent variables 

dataset_table_8 <- full_clean_panel |>
  filter(
    between(stock_price_app,  quantile(stock_price_app,  0.01, na.rm = TRUE), quantile(stock_price_app,  0.99, na.rm = TRUE)),
    between(equity_to_assets,  quantile(equity_to_assets,  0.01, na.rm = TRUE), quantile(equity_to_assets,  0.99, na.rm = TRUE)),
    between(cash_flow, quantile(cash_flow, 0.01, na.rm = TRUE), quantile(cash_flow, 0.99, na.rm = TRUE)),
    between(sales_growth, quantile(sales_growth, 0.01, na.rm = TRUE), quantile(sales_growth, 0.99, na.rm = TRUE)),
    between(leverage, quantile(leverage, 0.01, na.rm = TRUE), quantile(leverage, 0.99, na.rm = TRUE)),
    between(q, quantile(q, 0.01, na.rm = TRUE), quantile(q, 0.99, na.rm = TRUE)), 
    between(size, quantile(size, 0.01, na.rm = TRUE), quantile(size, 0.99, na.rm = TRUE))
  )

dataset_table_8 <- dataset_table_8 |>
  filter(equity_to_assets <= 0.15)


#  Run the regressions (firm FE + year FE) 

# (1) Dividend tax rate
model_div <- feols(
  equity_to_assets ~ div_tax   + cash_flow + stock_price_app + sales_growth +
    leverage + size + q |
    Identifier + year,
  cluster = ~ country_code^year,
  data    = dataset_table_8
)

# (2) Country-weighted effective tax rate
model_eff <- feols(
  equity_to_assets ~ eff_tax_c + cash_flow + stock_price_app + sales_growth +
    leverage + size + q |
    Identifier + year,
  cluster = ~ country_code^year,
  data    = dataset_table_8
)

# (3) Country-weighted average tax rate
model_avg <- feols(
  equity_to_assets ~ avg_tax_c + cash_flow + stock_price_app + sales_growth +
    leverage + size + q |
    Identifier + year,
  cluster = ~ country_code^year,
  data    = dataset_table_8
)

#  Display the regression results 
modelsummary(
  list(
    "Dividend tax rate"               = model_div,
    "Country-weighted effective tax rate" = model_eff,
    "Country-weighted average tax rate"   = model_avg
  ),
  stars     = TRUE,
  statistic = "({std.error})",
  coef_map  = c(
    "div_tax"        = "Tax",
    "eff_tax_c"      = "Tax",
    "avg_tax_c"      = "Tax",
    "cash_flow"      = "Cash flow",
    "stock_price_app"= "Stock price appreciation",
    "sales_growth"   = "Sales growth",
    "leverage"       = "Leverage",
    "size"           = "Size",
    "q"              = "Q"
  ),
  gof_map = c("nobs", "r.squared"),
  title   = "External equity financing and tax regimes",
 
)

#### Robustness check table 8: excluding USA and JPN

RC1_table8 <- dataset_table_8 |>
  filter(!(country_code %in% c("JPN", "USA")))

# (1) Dividend tax rate

model_div_RC1 <- feols(
  equity_to_assets ~ div_tax   + cash_flow + stock_price_app + sales_growth +
    leverage + size + q |
    Identifier + year,
  cluster = ~ country_code^year,
  data    = RC1_table8
)

# (2) Country-weighted effective tax rate

model_eff_RC1 <- feols(
  equity_to_assets ~ eff_tax_c + cash_flow + stock_price_app + sales_growth +
    leverage + size + q |
    Identifier + year,
  cluster = ~ country_code^year,
  data    = RC1_table8
)

# (3) Country-weighted average tax rate

model_avg_RC1 <- feols(
  equity_to_assets ~ avg_tax_c + cash_flow + stock_price_app + sales_growth +
    leverage + size + q |
    Identifier + year,
  cluster = ~ country_code^year,
  data    = RC1_table8
)

#  Display the regression results 

modelsummary(
  list(
    "Dividend tax rate"               = model_div_RC1,
    "Country-weighted effective tax rate" = model_eff_RC1,
    "Country-weighted average tax rate"   = model_avg_RC1
  ),
  stars     = TRUE,
  statistic = "({std.error})",
  coef_map  = c(
    "div_tax"        = "Tax",
    "eff_tax_c"      = "Tax",
    "avg_tax_c"      = "Tax",
    "cash_flow"      = "Cash flow",
    "stock_price_app"= "Stock price appreciation",
    "sales_growth"   = "Sales growth",
    "leverage"       = "Leverage",
    "size"           = "Size",
    "q"              = "Q"
  ),
  gof_map = c("nobs", "r.squared"),
  title   = "Robustness check: External equity financing and tax regimes (excluding USA and JPN)",
  
)


###### Table A.XIII Appendix: Debt substitution
dataset_table_10 <- dataset_table_8 |>
  mutate(
    delta_leverage = leverage - lag(leverage)
  )


dl_m1 <- feols(
  delta_leverage ~ div_tax + corp_tax + cash_flow +
    sales_growth + size + q |
    Identifier + year,
  cluster = ~ country_code^year,
  data = dataset_table_10
)

dl_m2 <- feols(
  delta_leverage ~eff_tax_c + corp_tax + cash_flow +
    sales_growth + size + q |
    Identifier + year,
  cluster = ~ country_code^year,
  data = dataset_table_10
)

dl_m3 <- feols(
  delta_leverage ~ avg_tax_c + corp_tax + cash_flow +
    sales_growth + size + q |
    Identifier + year,
  cluster = ~ country_code^year,
  data = dataset_table_10
)

modelsummary(
  list(
    "Dividend tax rate"               = dl_m1,
    "Country-weighted effective tax rate" = dl_m2,
    "Country-weighted average tax rate"   = dl_m3
  ),
  stars     = TRUE,
  statistic = "({std.error})",
  coef_map  = c(
    "div_tax"        = "Payout Tax",
    "eff_tax_c"      = "Payout Tax",
    "avg_tax_c"      = "Payout Tax",
    "corp_tax"      = "Corporate tax",
    "cash_flow"      = "Cash flow",
    "sales_growth"   = "Sales growth",
    "size"           = "Size",
    "q"              = "Q"
  ),
  gof_map = c("nobs", "r.squared"),
  title   = "Change in leverage and tax regimes",
  
)




###### Table 10

# 1. OECD personal income tax rates 2000-2008
csv_txt2 <- "
Country,2000,2001,2002,2003,2004,2005,2006,2007,2008
Australia,48.5,48.5,48.5,48.5,48.5,48.5,48.5,46.5,46.5
Austria,43.7,43.7,43.7,43.7,43.7,43.7,43.7,43.7,43.7
Belgium,60.5,60.0,55.6,53.5,53.5,53.5,53.5,53.7,53.7
Canada,47.9,46.4,46.4,46.4,46.4,46.4,46.4,46.4,46.4
Denmark,59.0,59.0,59.0,59.0,59.0,59.0,59.0,59.0,62.3
Finland,55.2,54.7,53.8,53.0,52.1,51.8,50.9,50.5,50.1
France,58.3,57.8,54.8,53.4,53.4,53.5,45.8,45.8,45.8
Germany,53.8,51.2,51.2,51.2,47.5,44.3,44.3,47.5,47.5
Greece,43.9,43.9,39.0,40.0,40.0,40.0,40.0,40.0,40.0
Hungary,40.0,40.0,40.0,40.0,38.0,38.0,36.0,36.0,36.0
Ireland,46.0,44.0,44.0,44.0,44.0,44.0,44.0,44.0,43.9
Italy,46.4,45.9,46.1,46.1,44.1,44.1,44.1,44.8,44.8
Japan,50.0,50.0,50.0,50.0,50.0,50.0,50.0,50.0,50.0
Korea,44.0,40.4,39.6,39.6,38.5,38.5,38.5,38.5,38.5
Mexico,40.0,40.0,35.0,34.0,33.0,30.0,29.0,28.0,28.0
Netherlands,60.0,52.0,52.0,52.0,52.0,52.0,52.0,52.0,52.0
New Zealand,39.0,39.0,39.0,39.0,39.0,39.0,39.0,39.0,39.0
Norway,47.5,47.5,47.5,47.5,47.5,43.5,43.5,47.5,47.5
Poland,40.0,40.0,40.0,40.0,40.0,40.0,40.0,40.0,40.0
Portugal,40.0,40.0,40.0,38.0,38.0,19.0,19.0,19.0,19.0
Spain,48.0,48.0,48.0,45.0,45.0,45.0,45.0,43.0,43.0
Sweden,55.4,55.5,55.5,55.5,55.5,55.5,55.5,55.4,55.4
Switzerland,43.8,43.2,42.7,42.1,42.1,42.1,42.1,42.1,42.1
United Kingdom,40.0,40.0,40.0,40.0,40.0,40.0,40.0,40.0,40.0
United States,46.7,46.1,45.4,41.6,41.5,41.4,41.4,41.4,41.9
"

tax_rates_25_wide <- read_csv(csv_txt2, show_col_types = FALSE)

tax_rates_25_long <- tax_rates_25_wide |>
  pivot_longer(-Country, names_to="year", values_to="tax_rate") |>
  mutate(year=as.integer(year))

tax_rates_25_iso <- tax_rates_25_long |>
  mutate(country_code = recode(Country,
                               "Australia"="AUS","Austria"="AUT","Belgium"="BEL","Canada"="CAN",
                               "Denmark"="DNK","Finland"="FIN","France"="FRA","Germany"="DEU",
                               "Greece"="GRC","Hungary"="HUN","Ireland"="IRL","Italy"="ITA",
                               "Japan"="JPN","Korea"="KOR","Mexico"="MEX","Netherlands"="NLD",
                               "New Zealand"="NZL","Norway"="NOR","Poland"="POL","Portugal"="PRT",
                               "Spain"="ESP","Sweden"="SWE","Switzerland"="CHE",
                               "United Kingdom"="GBR","United States"="USA"
  )) |>
  select(country_code, year, tax_rate) |>
  rename(top_personal_tax = tax_rate)


# 2. Merge with firm panel

merged_panel_2000 <- full_clean_panel |>
  mutate(year=as.integer(year)) |>
  left_join(tax_rates_25_iso, by=c("country_code","year")) |>
  mutate(
    tax_diff = top_personal_tax - corp_tax,
    high_diff = if_else(!is.na(tax_diff) & tax_diff >= 3, "Yes","No")
  )

# 3. Countries WITHOUT imputation
yrs <- function(ctry, y) tibble(country=ctry, year=y)

no_imputation <- bind_rows(
  yrs("Austria",2000:2008), yrs("Belgium",2000:2008), yrs("Denmark",2000:2008),
  yrs("France",2005:2008), yrs("Germany",2001:2008), yrs("Greece",2000:2008),
  yrs("Hungary",2000:2008), yrs("Ireland",2000:2008), yrs("Italy",2005:2008),
  yrs("Japan",2000:2008), yrs("Netherlands",2001:2008), yrs("Norway",2006:2008),
  yrs("Poland",2000:2008), yrs("Portugal",2000:2008), yrs("Spain",2006:2008),
  yrs("Sweden",2000:2008), yrs("Switzerland",2000:2008), yrs("United States",2000:2008)
)

no_imputation_iso <- no_imputation |>
  mutate(country_code = recode(country,
                               "Austria"="AUT","Belgium"="BEL","Denmark"="DNK","France"="FRA",
                               "Germany"="DEU","Greece"="GRC","Hungary"="HUN","Ireland"="IRL",
                               "Italy"="ITA","Japan"="JPN","Netherlands"="NLD","Norway"="NOR",
                               "Poland"="POL","Portugal"="PRT","Spain"="ESP","Sweden"="SWE",
                               "Switzerland"="CHE","United States"="USA"
  )) |>
  transmute(country_code, year=as.integer(year)) |>
  distinct()

merged_panel_2000 <- merged_panel_2000 |>
  semi_join(no_imputation_iso, by=c("country_code","year"))


# 4. Construct panel variables

panel_10 <- merged_panel_2000 |>
  arrange(Identifier, year) |>
  group_by(Identifier) |>
  mutate(
    dcash_to_assets = (cash - lag(cash))/lag(total_assets),
    delta_leverage = leverage - lag(leverage)
  ) |>
  ungroup() |>
  group_by(country_code, year) |>
  mutate(
    median_equity_to_assets = median(equity_to_assets, na.rm=TRUE),
    median_size_assets = median(total_assets, na.rm=TRUE),
    financial_constraint = if_else(
      equity_to_assets > median_equity_to_assets & total_assets < median_size_assets,
      "Yes","No"
    )
  ) |>
  ungroup()


# 5. Winsorization

panel_10 <- panel_10 |>
  filter(
    between(dcash_to_assets, quantile(dcash_to_assets,0.01,na.rm=TRUE), quantile(dcash_to_assets,0.99,na.rm=TRUE)),
    between(cash_flow, quantile(cash_flow,0.01,na.rm=TRUE), quantile(cash_flow,0.99,na.rm=TRUE)),
    between(sales_growth, quantile(sales_growth,0.01,na.rm=TRUE), quantile(sales_growth,0.99,na.rm=TRUE)),
    between(delta_leverage, quantile(delta_leverage,0.01,na.rm=TRUE), quantile(delta_leverage,0.99,na.rm=TRUE)),
    between(q, quantile(q,0.01,na.rm=TRUE), quantile(q,0.99,na.rm=TRUE)),
    between(size, quantile(size,0.01,na.rm=TRUE), quantile(size,0.99,na.rm=TRUE))
  )


# 6. Subsample with high corporate tax advantage

sub_high_diff <- panel_10 |> filter(high_diff=="Yes")
high_fc <- sub_high_diff |> filter(financial_constraint=="Yes")
low_fc  <- sub_high_diff |> filter(financial_constraint=="No")

# 7. Regressions


# High financial constraints panel
m_high_div <- feols( dcash_to_assets ~ cash_flow * div_tax + cash_flow + sales_growth + delta_leverage + size + q 
                     | Identifier + country_code,
                     cluster = ~ country_code^year, data = high_fc )

m_high_eff <- feols( dcash_to_assets ~ cash_flow * eff_tax_c + cash_flow + sales_growth + delta_leverage + size + q | 
                       Identifier + country_code, 
                     cluster = ~ country_code^year, data = high_fc ) 

m_high_avg <- feols( dcash_to_assets ~ cash_flow * avg_tax_c + cash_flow + sales_growth + delta_leverage + size + q | 
                       Identifier + country_code, 
                     cluster = ~ country_code^year, data = high_fc ) 

# Low financial constraints panel
m_low_div <- feols( dcash_to_assets ~ cash_flow * div_tax + cash_flow + sales_growth + delta_leverage + size + q |
                      Identifier + country_code,
                    cluster = ~ country_code^year, data = low_fc ) 

m_low_eff <- feols( dcash_to_assets ~ cash_flow * eff_tax_c + cash_flow + sales_growth + delta_leverage + size + q |
                      Identifier + country_code, 
                    cluster = ~ country_code^year, data = low_fc ) 

m_low_avg <- feols( dcash_to_assets ~ cash_flow * avg_tax_c + cash_flow + sales_growth + delta_leverage + size + q | 
                      Identifier + country_code, 
                    cluster = ~ country_code^year, data = low_fc )

# Output table

modelsummary(
  list(
    "High Financial Constraints: Dividend tax rate"        = m_high_div,
    "High Financial Constraints: Country-weighted effective tax rate" = m_high_eff,
    "High Financial Constraints: Country-weighted average tax rate"   = m_high_avg,
    "Low Financial Constraints: Dividend tax rate"         = m_low_div,
    "Low Financial Constraints: Country-weighted effective tax rate"  = m_low_eff,
    "Low Financial Constraints: Country-weighted average tax rate"    = m_low_avg
  ),
  stars     = TRUE,
  statistic = "({std.error})",
  coef_map  = c(
    "cash_flow:div_tax"       = "Cash flow × Tax",
    "cash_flow:eff_tax_c"     = "Cash flow × Tax",
    "cash_flow:avg_tax_c"     = "Cash flow × Tax",
    "cash_flow"               = "Cash flow",
    "sales_growth"            = "Sales growth",
    "delta_leverage"          = "ΔLeverage",
    "size"                    = "Size",
    "q"                       = "Q"
  ),
  
  gof_map = c("nobs", "r.squared"),
  title   = "Changes in cash holdings and tax regimes—financial constraints and corporate tax advantage",
)


dataset_table_10 <- dataset_table_8 |>
  mutate(
    delta_leverage = leverage - lag(leverage)
  )


dl_m1 <- feols(
  delta_leverage ~ div_tax + corp_tax + cash_flow +
    sales_growth + size + q |
    Identifier + year,
  cluster = ~ country_code^year,
  data = dataset_table_10
)

dl_m2 <- feols(
  delta_leverage ~eff_tax_c + corp_tax + cash_flow +
    sales_growth + size + q |
    Identifier + year,
  cluster = ~ country_code^year,
  data = dataset_table_10
)

dl_m3 <- feols(
  delta_leverage ~ avg_tax_c + corp_tax + cash_flow +
    sales_growth + size + q |
    Identifier + year,
  cluster = ~ country_code^year,
  data = dataset_table_10
)

modelsummary(
  list(
    "Dividend tax rate"               = dl_m1,
    "Country-weighted effective tax rate" = dl_m2,
    "Country-weighted average tax rate"   = dl_m3
  ),
  stars     = TRUE,
  statistic = "({std.error})",
  coef_map  = c(
    "div_tax"        = "Payout Tax",
    "eff_tax_c"      = "Payout Tax",
    "avg_tax_c"      = "Payout Tax",
    "corp_tax"      = "Corporate tax",
    "cash_flow"      = "Cash flow",
    "sales_growth"   = "Sales growth",
    "size"           = "Size",
    "q"              = "Q"
  ),
  gof_map = c("nobs", "r.squared"),
  title   = "Change in leverage and tax regimes",
  output= "latex"
)
  

write.csv(final_code_ds_project, "C:\\Users\\39324\\Downloads\\final_code_DS_project", row.names = FALSE)
