# US
file_us1 <- "/Users/wenlong/Downloads/USA1_November_23_2025_21_47_11.xlsx"
file_us2 <- "/Users/wenlong/Downloads/USA2_November_23_2025_21_49_49.xlsx"
file_us3 <- "/Users/wenlong/Downloads/USA3_November_23_2025_21_51_32.xlsx"


# Rest of world
file_rest1 <- "/Users/wenlong/Downloads/MULTI2_November_23_2025_21_24_32.xlsx"
file_rest2 <- "/Users/wenlong/Downloads/MULTI1_November_23_2025_21_6_26.xlsx"
file_rest3 <- "/Users/wenlong/Downloads/JAPAN1_November_23_2025_21_14_48.xlsx"
file_rest4 <- "/Users/wenlong/Downloads/Korea_November_23_2025_21_17_50.xlsx"
file_rest5 <- "/Users/wenlong/Downloads/Canada1_November_23_2025_21_20_23.xlsx"

# tax
tax <- read.csv("/Users/wenlong/Downloads/tax_data_combined.csv")



us1_company_year   <- read_company_years(file_us1,   latest_fy = 2024)
us2_company_year   <- read_company_years(file_us2,   latest_fy = 2024)
us3_company_year   <- read_company_years(file_us2,   latest_fy = 2024)


rest1_company_year <- read_company_years(file_rest1, latest_fy = 2024)
rest2_company_year <- read_company_years(file_rest2, latest_fy = 2024)
rest3_company_year <- read_company_years(file_rest3, latest_fy = 2024)
rest4_company_year <- read_company_years(file_rest4, latest_fy = 2024)
rest5_company_year <- read_company_years(file_rest5, latest_fy = 2024)


cat("\n\n===== DEBUG: Final values in wide format =====\n")
print(
  us1_company_year %>%
    select(Identifier, year, total_assets, capex, net_income) %>%
    head(20),
  n = Inf
)


us1_clean <- us1_company_year %>%
  filter_year_range(1990, 2008) %>%
  drop_financials_utilities() %>%
  require_min_consecutive_years() %>%
  drop_inconsistent_firms() %>%
  drop_small_firms()

us2_clean <- us2_company_year %>%
  filter_year_range(1990, 2008) %>%
  drop_financials_utilities() %>%
  require_min_consecutive_years() %>%
  drop_inconsistent_firms() %>%
  drop_small_firms()

us3_clean <- us3_company_year %>%
  filter_year_range(1990, 2008) %>%
  drop_financials_utilities() %>%
  require_min_consecutive_years() %>%
  drop_inconsistent_firms() %>%
  drop_small_firms()

rest1_clean <- rest1_company_year %>%
  filter_year_range(1990, 2008) %>%
  drop_financials_utilities() %>%
  require_min_consecutive_years_row() %>%
  drop_inconsistent_firms() %>%
  drop_small_firms()

rest2_clean <- rest2_company_year %>%
  filter_year_range(1990, 2008) %>%
  drop_financials_utilities() %>%
  require_min_consecutive_years_row() %>%
  drop_inconsistent_firms() %>%
  drop_small_firms()

# 🇯🇵 Japan (rest3) – use JP wrapper
rest3_company_year <- read_company_years_japan(file_rest3, latest_fy = 2024)

rest3_clean <- rest3_company_year %>%
  filter_year_range(1990, 2008) %>%
  drop_financials_utilities() %>%
  require_min_consecutive_years_row_japan(
    vars_required = c("total_assets", "net_income", "common_equity"),
    min_run = 4
  ) %>%
  drop_inconsistent_firms() %>%
  drop_small_firms()


rest4_clean <- rest4_company_year %>%
  filter_year_range(1990, 2008) %>%
  drop_financials_utilities() %>%
  require_min_consecutive_years_row() %>%
  drop_inconsistent_firms() %>%
  drop_small_firms()

rest5_clean <- rest5_company_year %>%
  filter_year_range(1990, 2008) %>%
  drop_financials_utilities() %>%
  require_min_consecutive_years_row() %>%
  drop_inconsistent_firms() %>%
  drop_small_firms()

full_clean_panel <- bind_rows(
  us1_clean,
  us2_clean,
  us3_clean,
  rest1_clean,
  rest2_clean,
  rest3_clean,
  rest4_clean,
  rest5_clean
)

full_clean_panel <- build_investment_measures(full_clean_panel)

library(dplyr)
library(stringr)

# Move EBIT from FI rows to the corresponding FY rows, then drop FI rows
library(dplyr)
library(stringr)

# 1) Move EBITDA (ebitda_ttm) from FI rows to FY rows with same Identifier + year
full_clean_panel_ebitda_fixed <- full_clean_panel %>%
  mutate(is_FI = str_detect(fy_label, "^FI")) %>%          # mark FI rows
  group_by(Identifier, year) %>%
  mutate(
    # EBITDA only from FI rows in this (Identifier, year) group
    ebitda_FI = if_else(is_FI, ebitda_ttm, NA_real_),
    ebitda_from_FI = suppressWarnings(max(ebitda_FI, na.rm = TRUE)),
    # if there was no FI EBITDA, avoid Inf from max(…, na.rm = TRUE)
    ebitda_from_FI = if_else(is.infinite(ebitda_from_FI), NA_real_, ebitda_from_FI),
    # for FY rows with missing EBITDA, fill from FI value
    ebitda_ttm = if_else(!is_FI & is.na(ebitda_ttm) & !is.na(ebitda_from_FI),
                         ebitda_from_FI,
                         ebitda_ttm)
  ) %>%
  ungroup() %>%
  # 2) Drop all FI rows from the final panel
  filter(!is_FI) %>%
  select(-is_FI, -ebitda_FI, -ebitda_from_FI)

# Optional: quick check
full_clean_panel_ebitda_fixed %>%
  select(Identifier, fy_label, year, ebitda_ttm) %>%
  arrange(Identifier, desc(year)) %>%
  head(40)


nrow(analysis_panel)
length(unique(analysis_panel$Identifier))



filter_year_range <- function(df, year_min = 1990, year_max = 2008,
                              year_col = "year") {
  df %>% 
    dplyr::filter(.data[[year_col]] >= year_min,
                  .data[[year_col]] <= year_max)
}

drop_financials_utilities <- function(df, naics_col = "naics") {
  df %>%
    mutate(
      naics_2d = as.integer(substr(as.character(.data[[naics_col]]), 1, 2))
    ) %>%
    filter(
      !is.na(naics_2d),
      !naics_2d %in% c(22L, 52L)
    ) %>%
    select(-naics_2d)
}

require_min_consecutive_years <- function(
    df,
    id_col   = "Identifier",
    year_col = "year",
    vars     = c("dividends", "net_income", "revenue", "total_assets"),
    min_run  = 4L
) {
  df %>%
    arrange(.data[[id_col]], .data[[year_col]]) %>%
    group_by(.data[[id_col]]) %>%
    mutate(
      good_row  = if_all(all_of(vars), ~ !is.na(.x)),
      break_seq = good_row == FALSE |
        is.na(lag(.data[[year_col]])) |
        .data[[year_col]] != lag(.data[[year_col]]) + 1,
      seq_id    = cumsum(coalesce(break_seq, TRUE)),
      run_len   = ave(good_row, seq_id, FUN = function(x) sum(x & !is.na(x)))
    ) %>%
    ungroup() %>%
    group_by(.data[[id_col]]) %>%
    mutate(
      has_long_run = any(run_len >= min_run & good_row)
    ) %>%
    ungroup() %>%
    filter(has_long_run) %>%
    select(-good_row, -break_seq, -seq_id, -run_len, -has_long_run)
}

drop_inconsistent_firms <- function(
    df,
    id_col        = "Identifier",
    dividends_col = "dividends",
    sales_col     = "revenue",
    shares_col    = "common_shares",
    equity_col    = "common_equity"
) {
  df %>%
    group_by(.data[[id_col]]) %>%
    mutate(
      bad_div_sales = any(
        !is.na(.data[[dividends_col]]) &
          !is.na(.data[[sales_col]]) &
          .data[[dividends_col]] > .data[[sales_col]]
      ),
      bad_shares = if (shares_col %in% names(df)) {
        any(!is.na(.data[[shares_col]]) & .data[[shares_col]] < 0)
      } else FALSE,
      bad_equity = if (equity_col %in% names(df)) {
        any(!is.na(.data[[equity_col]]) & .data[[equity_col]] < 0)
      } else FALSE
    ) %>%
    ungroup() %>%
    filter(!bad_div_sales, !bad_shares, !bad_equity) %>%
    select(-bad_div_sales, -bad_shares, -bad_equity)
}

drop_small_firms <- function(df, total_assets_col = "total_assets",
                             min_assets = 10e6) {
  df %>%
    filter(
      is.na(.data[[total_assets_col]]) |
        .data[[total_assets_col]] >= min_assets
    )
}

build_investment_measures <- function(
    df,
    id_col          = "Identifier",
    year_col        = "year",
    capex_col       = "capex",
    ppe_col         = "ppe",
    total_assets_col= "total_assets"
){
  df %>%
    arrange(.data[[id_col]], .data[[year_col]]) %>%
    group_by(.data[[id_col]]) %>%
    mutate(
      assets_lag = lag(.data[[total_assets_col]]),
      ppe_lag    = lag(.data[[ppe_col]]),
      
      Investment  = .data[[capex_col]] / assets_lag,
      PPE_Growth  = (.data[[ppe_col]] - ppe_lag) / assets_lag,
      Asset_Growth = (.data[[total_assets_col]] - assets_lag) / assets_lag
    ) %>%
    ungroup()
}

trim_outliers <- function(df, vars, p_low = 0.01, p_high = 0.99) {
  
  qs <- df %>%
    summarise(across(all_of(vars),
                     ~ quantile(.x, probs = c(p_low, p_high), na.rm = TRUE)))
  
  low  <- qs[1, ]
  high <- qs[2, ]
  
  crit <- rep(TRUE, nrow(df))
  for (v in vars) {
    if (v %in% names(df)) {
      crit <- crit & (
        is.na(df[[v]]) |
          (df[[v]] >= low[[v]] & df[[v]] <= high[[v]])
      )
    }
  }
  df[crit, , drop = FALSE]
}

# ============================================================
# 0) Packages
# ============================================================
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(zoo)

# ============================================================
# 1) Helper: print headers (to diagnose new exports)
#    - prints first 2 rows (the header block) for any file
# ============================================================
print_factset_headers <- function(path, n_cols = 40) {
  raw <- read_xlsx(path, col_names = FALSE, .name_repair = "minimal")
  cat("\n============================================================\n")
  cat("FILE:", path, "\n")
  cat("nrow:", nrow(raw), " ncol:", ncol(raw), "\n")
  cat("Row 1 (metric blocks):\n")
  print(as.character(raw[1, 1:min(n_cols, ncol(raw))]))
  cat("\nRow 2 (FY/FI labels):\n")
  print(as.character(raw[2, 1:min(n_cols, ncol(raw))]))
  cat("============================================================\n")
  invisible(raw)
}

# ============================================================
# 2) Robust "find ID columns" (prevents Identifier = NA error)
#    Works even if metric_block got filled / duplicated
# ============================================================
detect_id_cols <- function(raw, col_ids) {
  hdr1 <- as.character(raw[1, ])
  hdr2 <- as.character(raw[2, ])
  hdr12 <- paste0(hdr1, " | ", hdr2)
  
  find_first <- function(pattern_vec) {
    idx <- which(str_detect(hdr1, regex(pattern_vec, ignore_case = TRUE)))
    if (length(idx) == 0) idx <- which(str_detect(hdr2, regex(pattern_vec, ignore_case = TRUE)))
    if (length(idx) == 0) idx <- which(str_detect(hdr12, regex(pattern_vec, ignore_case = TRUE)))
    if (length(idx) == 0) return(NA_character_)
    col_ids[idx[1]]
  }
  
  id_col   <- find_first("Identifier")
  nm_col   <- find_first("Company Name")
  ctry_col <- find_first("Country of Incorporation")
  naics_col<- find_first("NAICS")
  
  list(
    id_col = id_col,
    nm_col = nm_col,
    ctry_col = ctry_col,
    naics_col = naics_col
  )
}

# ============================================================
# 3) Shared numeric parser (fixes insane values)
#    Handles:
#      - "184.523.136,00" -> 184523136.00
#      - "184,523,136.00" -> 184523136.00
#      - plain numeric already
# ============================================================
parse_number_any_locale <- function(x) {
  if (is.numeric(x)) return(x)
  
  s <- as.character(x)
  s <- str_replace_all(s, "\\s+", "")
  s[s %in% c("", "NA", "NaN", "NULL")] <- NA_character_
  
  # If string contains both '.' and ',' decide decimal separator by last occurrence
  both <- !is.na(s) & str_detect(s, "\\.") & str_detect(s, ",")
  if (any(both)) {
    last_dot   <- str_locate(s[both], "\\.(?!.*\\.)")[, 1]
    last_comma <- str_locate(s[both], ",(?!.*, )")[, 1] # fallback (won't always match)
    # safer: use base regexpr to find last positions
    last_dot   <- sapply(s[both], function(z) max(gregexpr("\\.", z)[[1]]))
    last_comma <- sapply(s[both], function(z) max(gregexpr(",", z)[[1]]))
    
    dec_is_comma <- last_comma > last_dot
    
    # European: "." thousands, "," decimal
    euro_idx <- which(both)[dec_is_comma]
    if (length(euro_idx) > 0) {
      s[euro_idx] <- gsub("\\.", "", s[euro_idx])
      s[euro_idx] <- sub(",", ".", s[euro_idx], fixed = TRUE)
    }
    
    # US: "," thousands, "." decimal
    us_idx <- which(both)[!dec_is_comma]
    if (length(us_idx) > 0) {
      s[us_idx] <- gsub(",", "", s[us_idx])
    }
  }
  
  # Only comma present -> treat comma as decimal if there is exactly 1 comma and last group length <= 2
  only_comma <- !is.na(s) & !str_detect(s, "\\.") & str_detect(s, ",")
  if (any(only_comma)) {
    comma_count <- str_count(s[only_comma], ",")
    # if multiple commas -> thousands separators
    multi <- comma_count > 1
    if (any(multi)) {
      s[which(only_comma)[multi]] <- gsub(",", "", s[which(only_comma)[multi]])
    }
    # single comma -> decimal
    single <- comma_count == 1
    if (any(single)) {
      idx <- which(only_comma)[single]
      s[idx] <- sub(",", ".", s[idx], fixed = TRUE)
    }
  }
  
  suppressWarnings(as.numeric(s))
}

# ============================================================
# 4) Reader: generic read_company_years (US + RoW)
#    - produces "year" ALWAYS
#    - keeps fy_label if present
#    - adds NAICS
#    - uses metric keys we used in the project
# ============================================================
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(zoo)

# ============================================================
# FIX: merge FI EBITDA into FY row for same firm-year and drop FI rows
# - Works even if columns are interleaved FY1 FI1 FY2 FI2 ...
# - FI rows are kept ONLY as a source of EBITDA, then removed
# ============================================================
read_company_years <- function(path, latest_fy = 2024) {
  
  raw <- read_xlsx(path, col_names = FALSE, .name_repair = "minimal")
  
  col_ids <- paste0("V", seq_len(ncol(raw)))
  colnames(raw) <- col_ids
  
  hdr1 <- as.character(raw[1, ])
  hdr2 <- as.character(raw[2, ])
  
  metric_block <- na.locf(hdr1, na.rm = FALSE)
  metric_block <- str_squish(as.character(metric_block))
  
  fy_label <- ifelse(hdr2 %in% c("", NA), NA_character_, as.character(hdr2))
  
  offset <- dplyr::case_when(
    fy_label %in% c("FY0", "FI0") ~ 0L,
    str_detect(fy_label, "^FY-\\d+$") ~ as.integer(str_remove(fy_label, "FY-")),
    str_detect(fy_label, "^FI-\\d+$") ~ as.integer(str_remove(fy_label, "FI-")),
    TRUE ~ NA_integer_
  )
  
  year <- if_else(!is.na(offset), latest_fy - offset, NA_integer_)
  
  col_info <- tibble(
    col_id       = col_ids,
    metric_block = metric_block,
    fy_label     = fy_label,
    offset       = offset,
    year         = year
  )
  
  # --- detect id columns (uses your existing helper)
  ids <- detect_id_cols(raw, col_ids)
  id_col   <- ids$id_col
  nm_col   <- ids$nm_col
  ctry_col <- ids$ctry_col
  naics_col<- ids$naics_col
  
  if (is.na(id_col) || is.na(nm_col) || is.na(ctry_col)) {
    stop("Identifier / Company Name / Country of Incorporation not found.")
  }
  
  # --- map metrics (same as your version)
  col_info <- col_info %>%
    mutate(
      metric_key = case_when(
        str_detect(metric_block, "Total Assets") ~ "total_assets",
        str_detect(metric_block, "Property Plant & Equipment - Net - Total") ~ "ppe",
        str_detect(metric_block, "Capital Expenditures - Total") ~ "capex",
        str_detect(metric_block, "Cash & Short Term Investments") ~ "cash_sti",
        str_detect(metric_block, "Debt - Total") ~ "debt",
        str_detect(metric_block, "Net Income after Tax") ~ "net_income",
        str_detect(metric_block, "Earnings before Interest, Taxes, Depreciation & Amortization \\(EBITDA\\), TTM") ~ "ebitda_ttm",
        str_detect(metric_block, "Revenue from Business Activities - Total") ~ "revenue",
        str_detect(metric_block, "Market Capitalization") ~ "mkt_cap",
        str_detect(metric_block, "Dividends Paid - Cash - Total - Cash Flow") ~ "dividends",
        str_detect(metric_block, "Stock - Common - Repurchased/Retired - Cash Flow") ~ "stock_repurchased",
        str_detect(metric_block, "Common Shares - Outstanding - Total") ~ "common_shares",
        str_detect(metric_block, "Common Equity - Total") ~ "common_equity",
        TRUE ~ NA_character_
      )
    )
  
  metric_cols <- col_info %>%
    filter(!is.na(metric_key), !is.na(fy_label), !is.na(year)) %>%
    select(col_id, metric_key, fy_label, year) %>%
    mutate(is_FI = str_detect(fy_label, "^FI"),
           is_FY = str_detect(fy_label, "^FY"))
  
  dat <- raw[-c(1, 2), ]
  
  long <- dat %>%
    mutate(
      Identifier               = .data[[id_col]],
      Company_Name             = .data[[nm_col]],
      Country_of_Incorporation = .data[[ctry_col]],
      naics = if (!is.na(naics_col)) .data[[naics_col]] else NA
    ) %>%
    pivot_longer(
      cols      = all_of(metric_cols$col_id),
      names_to  = "col_id",
      values_to = "value"
    ) %>%
    left_join(metric_cols, by = "col_id") %>%
    mutate(value = parse_number_any_locale(value)) %>%
    filter(!is.na(Identifier), !is.na(year))
  
  wide <- long %>%
    select(
      Identifier, Company_Name, Country_of_Incorporation, naics,
      fy_label, year, metric_key, value, is_FI, is_FY
    ) %>%
    distinct() %>%
    pivot_wider(names_from = metric_key, values_from = value)
  
  # ============================================================
  # Merge FI EBITDA into FY for same (Identifier, year), then drop FI rows
  # Rule:
  #   - For each (Identifier, year), take max(FI ebitda_ttm) as FI EBITDA
  #   - Fill FY ebitda_ttm if missing (or always overwrite if you prefer)
  #   - Keep FY rows if present; if only FI exists, keep that row as FY-like
  # ============================================================
  out <- wide %>%
    group_by(Identifier, year) %>%
    mutate(
      ebitda_from_FI = suppressWarnings(max(if_else(is_FI, ebitda_ttm, NA_real_), na.rm = TRUE)),
      ebitda_from_FI = if_else(is.infinite(ebitda_from_FI), NA_real_, ebitda_from_FI),
      has_FY = any(is_FY, na.rm = TRUE),
      # fill FY rows from FI if missing
      ebitda_ttm = if_else(is_FY & is.na(ebitda_ttm) & !is.na(ebitda_from_FI),
                           ebitda_from_FI,
                           ebitda_ttm),
      # if only FI exists for that year, keep the FI row but treat it as FY-like
      fy_label = if_else(!has_FY & is_FI, str_replace(fy_label, "^FI", "FY"), fy_label),
      is_FY = if_else(!has_FY & is_FI, TRUE, is_FY)
    ) %>%
    ungroup() %>%
    # keep FY rows only (after FI-only years are converted)
    filter(is_FY) %>%
    select(-is_FI, -is_FY, -has_FY, -ebitda_from_FI) %>%
    arrange(Identifier, desc(year))
  
  out
}


# ============================================================
# 5) Reader: Japan-specific (keeps ONLY FY rows or de-dupes FI)
#    In your workflow you re-downloaded Japan correctly, so this
#    version:
#      - reads as usual
#      - if FY/FI both exist for same (Identifier, year), keeps
#        the FY row as primary and uses FI only to fill EBITDA if needed
# ============================================================
read_company_years_japan <- function(path, latest_fy = 2024) {
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(zoo)
  library(tibble)
  
  raw <- read_xlsx(path, col_names = FALSE, .name_repair = "minimal")
  col_ids <- paste0("V", seq_len(ncol(raw)))
  colnames(raw) <- col_ids
  
  hdr1 <- as.character(raw[1, ])
  hdr2 <- as.character(raw[2, ])
  
  metric_block <- zoo::na.locf(hdr1, na.rm = FALSE)
  metric_block <- str_squish(as.character(metric_block))
  fy_label <- ifelse(hdr2 %in% c("", NA), NA_character_, as.character(hdr2))
  
  offset <- case_when(
    fy_label %in% c("FY0", "FI0") ~ 0L,
    str_detect(fy_label, "^FY-\\d+$") ~ as.integer(str_remove(fy_label, "FY-")),
    str_detect(fy_label, "^FI-\\d+$") ~ as.integer(str_remove(fy_label, "FI-")),
    TRUE ~ NA_integer_
  )
  year <- if_else(!is.na(offset), latest_fy - offset, NA_integer_)
  
  # Your existing helper
  ids <- detect_id_cols(raw, col_ids)
  id_col   <- ids$id_col
  nm_col   <- ids$nm_col
  ctry_col <- ids$ctry_col
  naics_col<- ids$naics_col
  
  if (is.na(id_col) || is.na(nm_col) || is.na(ctry_col)) {
    stop("Identifier / Company Name / Country of Incorporation not found.")
  }
  
  # ---- Metric mapping tuned to the Japanese export you showed (Revenue intentionally not mapped)
  col_info <- tibble(
    col_id       = col_ids,
    metric_block = metric_block,
    fy_label     = fy_label,
    year         = year,
    is_FI        = str_detect(fy_label, "^FI"),
    is_FY        = str_detect(fy_label, "^FY")
  ) %>%
    mutate(
      metric_key = case_when(
        str_detect(metric_block, regex("^Total Assets\\s*\\(USD\\)\\s*In the last \\d+\\s*FY$", ignore_case = TRUE)) ~ "total_assets",
        str_detect(metric_block, regex("^Property Plant\\s*&\\s*Equipment\\s*-\\s*Net\\s*-\\s*Total\\s*\\(USD\\)\\s*In the last \\d+\\s*FY$", ignore_case = TRUE)) ~ "ppe",
        str_detect(metric_block, regex("^Capital Expenditures\\s*-\\s*Actual\\s*\\(USD\\)\\s*In the last \\d+\\s*FY$", ignore_case = TRUE)) ~ "capex",
        str_detect(metric_block, regex("^Capital Expenditures\\s*-\\s*Total\\s*\\(USD\\)\\s*In the last \\d+\\s*FY$",  ignore_case = TRUE)) ~ "capex",
        str_detect(metric_block, regex("^Cash\\s*&\\s*Cash\\s*Equivalents\\s*-\\s*Total\\s*\\(USD\\)\\s*In the last \\d+\\s*FY$", ignore_case = TRUE)) ~ "cash_sti",
        # (You listed CFO variants; keep as optional operating cash flow variable)
        str_detect(metric_block, regex("^Cash Flow From Operations\\s*-\\s*Actual\\s*\\(USD\\)\\s*In the last \\d+\\s*FY$", ignore_case = TRUE)) ~ "cfo",
        str_detect(metric_block, regex("^Net Cash Flow from Operating Activities\\s*\\(USD\\)\\s*In the last \\d+\\s*FY$", ignore_case = TRUE)) ~ "cfo",
        str_detect(metric_block, regex("^Debt\\s*-\\s*Total\\s*\\(USD\\)\\s*In the last \\d+\\s*FY$", ignore_case = TRUE)) ~ "debt",
        str_detect(metric_block, regex("^Net Income\\s*-\\s*Actual\\s*\\(USD\\)\\s*In the last \\d+\\s*FY$", ignore_case = TRUE)) ~ "net_income",
        str_detect(metric_block, regex("^Net Income\\s+after\\s+Tax\\s*\\(USD\\)\\s*In the last \\d+\\s*FY$", ignore_case = TRUE)) ~ "net_income",
        str_detect(metric_block, regex("^Earnings before Interest, Taxes, Depreciation\\s*&\\s*Amortization\\s*\\(EBITDA\\),\\s*TTM\\s*\\(USD\\)\\s*In the last \\d+\\s*FI$", ignore_case = TRUE)) ~ "ebitda_ttm",
        str_detect(metric_block, regex("^Market Capitalization\\s*\\(USD\\)\\s*In the last \\d+\\s*FY$", ignore_case = TRUE)) ~ "mkt_cap",
        str_detect(metric_block, regex("^Dividends Paid\\s*-\\s*Cash\\s*-\\s*Total\\s*-\\s*Cash Flow\\s*\\(USD\\)\\s*In the last \\d+\\s*FY$", ignore_case = TRUE)) ~ "dividends",
        str_detect(metric_block, regex("^Stock\\s*-\\s*Common\\s*-\\s*Repurchased/Retired\\s*-\\s*Cash Flow\\s*\\(USD\\)\\s*In the last \\d+\\s*FY$", ignore_case = TRUE)) ~ "stock_repurchased",
        str_detect(metric_block, regex("^Common Shares\\s*-\\s*Outstanding\\s*-\\s*Total\\s*In the last \\d+\\s*FY$", ignore_case = TRUE)) ~ "common_shares",
        str_detect(metric_block, regex("^Common Equity\\s*-\\s*Total\\s*\\(USD\\)\\s*In the last \\d+\\s*FY$", ignore_case = TRUE)) ~ "common_equity",
        TRUE ~ NA_character_
      )
    )
  
  metric_cols <- col_info %>%
    filter(!is.na(metric_key), !is.na(fy_label), !is.na(year)) %>%
    select(col_id, metric_key, fy_label, year, is_FI, is_FY)
  
  dat <- raw[-c(1, 2), ]
  
  long <- dat %>%
    mutate(
      Identifier               = .data[[id_col]],
      Company_Name             = .data[[nm_col]],
      Country_of_Incorporation = .data[[ctry_col]],
      naics = if (!is.na(naics_col)) .data[[naics_col]] else NA
    ) %>%
    pivot_longer(
      cols      = all_of(metric_cols$col_id),
      names_to  = "col_id",
      values_to = "value"
    ) %>%
    left_join(metric_cols, by = "col_id") %>%
    mutate(value = parse_number_any_locale(value)) %>%
    filter(!is.na(Identifier), !is.na(year), !is.na(metric_key))
  
  # Aggregate duplicates to avoid list-cols
  long_agg <- long %>%
    group_by(Identifier, Company_Name, Country_of_Incorporation, naics, fy_label, year, is_FI, is_FY, metric_key) %>%
    summarise(value = suppressWarnings(max(value, na.rm = TRUE)), .groups = "drop") %>%
    mutate(value = if_else(is.infinite(value), NA_real_, value))
  
  wide <- long_agg %>%
    pivot_wider(names_from = metric_key, values_from = value)
  
  # Merge FI EBITDA -> FY and drop FI rows
  out <- wide %>%
    group_by(Identifier, year) %>%
    mutate(
      ebitda_from_FI = suppressWarnings(max(ifelse(is_FI, ebitda_ttm, NA_real_), na.rm = TRUE)),
      ebitda_from_FI = ifelse(is.infinite(ebitda_from_FI), NA_real_, ebitda_from_FI),
      has_FY = any(is_FY, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(is_FY | !has_FY) %>%
    mutate(
      ebitda_ttm = ifelse(is.na(ebitda_ttm) & !is.na(ebitda_from_FI), ebitda_from_FI, ebitda_ttm),
      fy_label   = ifelse(!is_FY & !has_FY, str_replace(fy_label, "^FI", "FY"), fy_label),
      is_FY      = ifelse(!is_FY & !has_FY, TRUE, is_FY)
    ) %>%
    filter(is_FY) %>%
    select(-is_FI, -is_FY, -has_FY, -ebitda_from_FI) %>%
    arrange(Identifier, desc(year))
  
  out
}


# ============================================================
# 6) Cleaning steps (same pipeline you used)
# ============================================================
filter_year_range <- function(df, year_min = 1990, year_max = 2008, year_col = "year") {
  df %>% filter(.data[[year_col]] >= year_min, .data[[year_col]] <= year_max)
}

drop_financials_utilities <- function(df, naics_col = "naics") {
  df %>%
    mutate(naics_2d = suppressWarnings(as.integer(substr(as.character(.data[[naics_col]]), 1, 2)))) %>%
    filter(!is.na(naics_2d), !naics_2d %in% c(22L, 52L)) %>%
    select(-naics_2d)
}

# --- consecutive-years: ROW version used for rest-of-world (FY/FI mixed)
#     This version:
#       - identifies "good rows" using REQUIRED vars
#       - computes consecutive run using "year"
#       - keeps firm if it has ANY run length >= 4
require_min_consecutive_years_row <- function(
    df,
    id_col   = "Identifier",
    year_col = "year",
    vars_required = c("total_assets", "capex", "net_income", "mkt_cap", "common_equity"),
    min_run  = 4L
) {
  vars_required <- vars_required[vars_required %in% names(df)]
  
  df %>%
    arrange(.data[[id_col]], .data[[year_col]]) %>%
    group_by(.data[[id_col]]) %>%
    mutate(
      good_row = if_all(all_of(vars_required), ~ !is.na(.x)),
      # break if not good OR year not consecutive
      break_seq = (!good_row) |
        is.na(lag(.data[[year_col]])) |
        (.data[[year_col]] != lag(.data[[year_col]]) + 1L),
      seq_id = cumsum(coalesce(break_seq, TRUE))
    ) %>%
    group_by(.data[[id_col]], seq_id) %>%
    mutate(run_len = sum(good_row, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(.data[[id_col]]) %>%
    mutate(has_long_run = any(run_len >= min_run & good_row, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(has_long_run) %>%
    select(-good_row, -break_seq, -seq_id, -run_len, -has_long_run)
}

# --- consecutive-years: US version (more permissive, excludes less-important vars)
require_min_consecutive_years <- function(
    df,
    id_col   = "Identifier",
    year_col = "year",
    vars_required = c("total_assets", "capex", "net_income", "mkt_cap", "common_equity", "debt"),
    min_run  = 4L
) {
  require_min_consecutive_years_row(
    df,
    id_col = id_col,
    year_col = year_col,
    vars_required = vars_required,
    min_run = min_run
  )
}

drop_inconsistent_firms <- function(
    df,
    id_col        = "Identifier",
    dividends_col = "dividends",
    sales_col     = "revenue",
    shares_col    = "common_shares",
    equity_col    = "common_equity"
) {
  df %>%
    group_by(.data[[id_col]]) %>%
    mutate(
      bad_div_sales = if (all(c(dividends_col, sales_col) %in% names(df))) {
        any(!is.na(.data[[dividends_col]]) & !is.na(.data[[sales_col]]) &
              .data[[dividends_col]] > .data[[sales_col]])
      } else FALSE,
      bad_shares = if (shares_col %in% names(df)) {
        any(!is.na(.data[[shares_col]]) & .data[[shares_col]] < 0)
      } else FALSE,
      bad_equity = if (equity_col %in% names(df)) {
        any(!is.na(.data[[equity_col]]) & .data[[equity_col]] < 0)
      } else FALSE
    ) %>%
    ungroup() %>%
    filter(!bad_div_sales, !bad_shares, !bad_equity) %>%
    select(-bad_div_sales, -bad_shares, -bad_equity)
}

drop_small_firms <- function(df, total_assets_col = "total_assets", min_assets = 10e6) {
  df %>% filter(is.na(.data[[total_assets_col]]) | .data[[total_assets_col]] >= min_assets)
}

# ============================================================
# 7) Measures (your final robust version used in regression)
# ============================================================
build_investment_measures <- function(df) {
  df %>%
    group_by(Identifier) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(
      invest_rate = ifelse(
        !is.na(capex) & !is.na(lag(total_assets)) & lag(total_assets) != 0,
        capex / lag(total_assets),
        NA_real_
      ),
      cf_assets = ifelse(
        !is.na(net_income) & !is.na(total_assets) & total_assets != 0,
        net_income / total_assets,
        NA_real_
      ),
      size = ifelse(!is.na(total_assets) & total_assets > 0, log(total_assets), NA_real_),
      leverage = ifelse(
        !is.na(debt) & !is.na(total_assets) & total_assets != 0,
        debt / total_assets,
        NA_real_
      ),
      q = ifelse(
        !is.na(total_assets) & total_assets != 0 &
          !is.na(mkt_cap) & !is.na(common_equity),
        (total_assets + mkt_cap - common_equity) / total_assets,
        NA_real_
      )
    ) %>%
    ungroup()
}

# ============================================================
# 8) Debug pipeline (counts rows + firms after each step)
# ============================================================
debug_clean_pipeline <- function(
    df,
    dataset_name = "dataset",
    year_min = 1990,
    year_max = 2008
) {
  cat("========== DEBUG:", dataset_name, "==========\n")
  cat(sprintf("Start (raw)                      : %8d rows, %5d firms\n",
              nrow(df), n_distinct(df$Identifier)))
  
  d1 <- df %>% filter_year_range(year_min, year_max)
  cat(sprintf("After filter_year_range          : %8d rows, %5d firms\n",
              nrow(d1), n_distinct(d1$Identifier)))
  
  d2 <- d1 %>% drop_financials_utilities()
  cat(sprintf("After drop_financials_utilities  : %8d rows, %5d firms\n",
              nrow(d2), n_distinct(d2$Identifier)))
  
  d3 <- d2 %>% require_min_consecutive_years()
  cat(sprintf("After require_min_consecutive_years : %8d rows, %5d firms\n",
              nrow(d3), n_distinct(d3$Identifier)))
  
  d4 <- d3 %>% drop_inconsistent_firms()
  cat(sprintf("After drop_inconsistent_firms    : %8d rows, %5d firms\n",
              nrow(d4), n_distinct(d4$Identifier)))
  
  d5 <- d4 %>% drop_small_firms()
  cat(sprintf("After drop_small_firms (final)   : %8d rows, %5d firms\n",
              nrow(d5), n_distinct(d5$Identifier)))
  cat("========================================\n\n")
  
  list(
    start = df,
    after_year_range = d1,
    after_drop_naics = d2,
    after_consecutive = d3,
    after_inconsistent = d4,
    clean = d5
  )
}

require_min_consecutive_years_row_japan2 <- function(
    df,
    id_col   = "Identifier",
    year_col = "year",
    core_required = c("total_assets", "net_income", "common_equity"),
    capex_col = "capex",
    min_run  = 4L,
    min_capex_share_in_run = 0.5   # at least 50% of years in the kept run have capex
) {
  core_required <- intersect(core_required, names(df))
  has_capex <- capex_col %in% names(df)
  
  df %>%
    arrange(.data[[id_col]], .data[[year_col]]) %>%
    group_by(.data[[id_col]]) %>%
    mutate(
      good_core = if_all(all_of(core_required), ~ !is.na(.x)),
      break_seq = (!good_core) |
        is.na(lag(.data[[year_col]])) |
        (.data[[year_col]] != lag(.data[[year_col]]) + 1L),
      seq_id = cumsum(coalesce(break_seq, TRUE))
    ) %>%
    group_by(.data[[id_col]], seq_id) %>%
    mutate(
      run_len = sum(good_core, na.rm = TRUE),
      capex_ok = if (has_capex) {
        mean(!is.na(.data[[capex_col]]), na.rm = TRUE) >= min_capex_share_in_run
      } else TRUE
    ) %>%
    ungroup() %>%
    group_by(.data[[id_col]]) %>%
    mutate(
      has_long_run = any(run_len >= min_run & good_core, na.rm = TRUE),
      has_good_run = any(run_len >= min_run & good_core & capex_ok, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(has_good_run) %>%
    select(-good_core, -break_seq, -seq_id, -run_len, -capex_ok, -has_long_run, -has_good_run)
}
