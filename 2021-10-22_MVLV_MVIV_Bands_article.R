## Market-Value-to-Long-term-holder-Value (MVLV) and Market-Value-to-Illiquid-Value (MVIV) metrics & bands
## By: Dilution-proof (@dilutionproof)

# Set locale ----
Sys.setlocale(category = "LC_TIME", locale="en_GB.UTF-8") 

# Libraries ----
library(tidyverse)
library(here)

# Data import ----
df_price <- read_csv(here::here("price-btc-24h.csv"))
df_mc <- read_csv(here::here("market-cap-btc-24h.csv"))
df_rc <- read_csv(here::here("realized-cap-btc-24h.csv"))
df_cs <- read_csv(here::here("circulating-supply-btc-24h.csv"))
df_ls <- read_csv(here::here("long-term-holder-supply-btc-24h.csv"))
df_is <- read_csv(here::here("illiquid-supply-btc-24h.csv"))

# Data management ----
df_price <- df_price %>% rename(date = timestamp, price = value)
df_mc <- df_mc %>% rename(date = timestamp, mc = value)
df_rc <- df_rc %>% rename(date = timestamp, rc = value)
df_cs <- df_cs %>% rename(date = timestamp, cs = value)
df_ls <- df_ls %>% rename(date = timestamp, ls = value)
df_is <- df_is %>% rename(date = timestamp, is = value)

df_bands <- full_join(df_price, df_mc, by = "date")
df_bands <- full_join(df_bands, df_rc, by = "date")
df_bands <- full_join(df_bands, df_cs, by = "date")
df_bands <- full_join(df_bands, df_ls, by = "date")
df_bands <- full_join(df_bands, df_is, by = "date")
remove(df_price, df_mc, df_rc, df_cs, df_ls, df_is)

df_bands <- df_bands %>%
  mutate(
    date = as.Date(date),
    mv = price * cs,
    mv_sd = NA,
    lv = price * ls,
    lv_365sma = NA,
    iv = price * is,
    iv_365sma = NA,
    bpt0 = NA,
    mc_sd = NA,
    price_sd = NA
  ) %>%
  arrange(date)
df_bands <- df_bands[-nrow(df_bands),] # last value ls incomplete

for (i in 365:nrow(df_bands)) {
  df_bands$lv_365sma[i] <- mean(df_bands$lv[(i-365):i], na.rm = TRUE)
  df_bands$iv_365sma[i] <- mean(df_bands$iv[(i-365):i], na.rm = TRUE)
  df_bands$bpt0[i] <- mean(df_bands$price[ifelse(i > 1459, (i-1460), 0):i], na.rm = TRUE)
  df_bands$mv_sd[i] <- sd(df_bands$mv[0:i], na.rm = TRUE)
  df_bands$mc_sd[i] <- sd(df_bands$mc[0:i], na.rm = TRUE)
  df_bands$price_sd[i] <- sd(df_bands$price[ifelse(i > 1459, (i-1460), 0):i], na.rm = TRUE)
}
remove(i)

df_bands <- df_bands %>%
  mutate(
    mvrv = (mc - rc) / mc_sd,
    mvrv0 = rc / cs,
    mvrv2 = (rc + 2*mc_sd) / cs,
    mvrv4 = (rc + 4*mc_sd) / cs,
    mvrv6 = (rc + 6*mc_sd) / cs,
    mvrv8 = (rc + 8*mc_sd) / cs,
    mvrv10 = (rc + 10*mc_sd) / cs,
    mvlv = (mv - lv_365sma) / mv_sd,
    mvlv0 = lv_365sma / cs,
    mvlv2 = (lv_365sma + 2*mv_sd) / cs,
    mvlv4 = (lv_365sma + 4*mv_sd) / cs,
    mvlv6 = (lv_365sma + 6*mv_sd) / cs,
    mvlv8 = (lv_365sma + 8*mv_sd) / cs,
    mvlv10 = (lv_365sma + 10*mv_sd) / cs,
    mviv = (mv - iv_365sma) / mv_sd,
    mviv0 = iv_365sma / cs,
    mviv2 = (iv_365sma + 2*mv_sd) / cs,
    mviv4 = (iv_365sma + 4*mv_sd) / cs,
    mviv6 = (iv_365sma + 6*mv_sd) / cs,
    mviv8 = (iv_365sma + 8*mv_sd) / cs,
    mviv10 = (iv_365sma + 10*mv_sd) / cs,
    bpt = (price - bpt0) / price_sd,
    bpt2 = bpt0 + 2*price_sd,
    bpt4 = bpt0 + 4*price_sd,
    bpt6 = bpt0 + 6*price_sd,
    bpt8 = bpt0 + 8*price_sd,
    bpt10 = bpt0 + 10*price_sd
  ) %>%
  select(
    date, cs, ls, is, rc, price, price_sd, mv, mv_sd, mc, mc_sd, lv, lv_365sma, iv, iv_365sma, starts_with("mvrv"),
    starts_with("mvlv"), starts_with("mviv"), bpt, starts_with("bpt")
  )

# LTH & illiquid supply ----
chart_ith_illiquid_supply <- ggplot(df_bands, aes(x = date)) +
  geom_line(aes(y = ls/1000000, colour = "3")) +
  geom_line(aes(y = is/1000000, colour = "2")) +
  geom_line(aes(y = cs/1000000, colour = "1")) +
  scale_y_continuous(
    breaks = seq(0, 100, 2), 
    minor_breaks = seq(0, 100, 1)
  ) +
  scale_x_date(
    breaks = "1 year",
    minor_breaks = NULL,
    date_labels = "%Y"
  ) +
  theme_minimal() + 
  labs(title = "The circulating bitcoin supply, Illiquid Supply* & Long-Term Holder (LTH) supply**", 
       subtitle = paste("Date: ", as.character(format(Sys.Date(), "%b %d, %Y")), ", data: Glassnode, chart: @dilutionproof", sep = ""), 
       x = "Time",
       y = "BTC (millions)",
       caption = "* Illiquid Supply: The total amount of bitcoin that that are labelled 'illiquid' by Glassnode\n** Long-Term Holder (LTH) supply: The total number of bitcoin that have been labelled 'long-term holder supply' by Glassnode") + 
  scale_colour_manual(
    name = "Supply",
    values = c("black", "red", "blue"),
    labels = c("Circulating", "Illiquid", "LTH")
  ) +
  annotate(
    "label",
    label = paste(
      "Last values:\nCirculating supply: ",
      as.character(format(round(last(df_bands$cs)/1000000, 1), nsmall = 1)),
      "M\nIlliquid supply: ",
      as.character(format(round(last(df_bands$is[!is.na(df_bands$is)])/1000000, 1), nsmall = 1)),
      "M\nLTH supply: ",
      as.character(format(round(last(df_bands$ls[!is.na(df_bands$ls)])/1000000, 1), nsmall = 1)),
      "M",
      sep = ""
    ),
    x = last(df_bands$date),
    y = 0,
    size = 3,
    hjust = 1,
    vjust = 0
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0, face = "italic"),
    plot.caption = element_text(hjust = 0, face = "italic"),
    legend.title = element_text(hjust = 0, face = "bold"),
    plot.title.position = "panel",
    plot.caption.position =  "panel"
  )

ggsave(here::here(paste(as.character(Sys.Date()), "_0_lth_illiquid_supply.png", sep = "")), chart_ith_illiquid_supply, height = 12.5 / 1.78, width = 12.5, dpi = 320, units = "in")

# MV & IV & LV ----
chart_mv_iv_lv <- ggplot(df_bands, aes(x = date)) +
  geom_line(aes(y = lv_365sma, colour = "3")) +
  geom_line(aes(y = iv_365sma, colour = "2")) +
  geom_line(aes(y = mv, colour = "1")) +
  scale_y_log10(
    labels=scales::dollar_format(),
    limits = c(min(df_bands$mv, na.rm = TRUE), max(df_bands$mv, na.rm = TRUE)),
    breaks = c(0,1,10,100,1000,10000,100000,1000000, 10000000, 100000000, 1000000000, 10000000000, 100000000000, 1000000000000,
               10000000000000), 
    minor_breaks = c(seq(0, 1, 0.1), seq(0, 10, 1), seq(0, 100, 10), seq(0, 1000, 100), seq(0, 10000, 1000), seq(0, 100000, 10000),
                     seq(0, 1000000, 100000), seq(0, 10000000, 1000000), seq(0, 100000000, 10000000), seq(0, 1000000000, 100000000),
                     seq(0, 10000000000, 1000000000), seq(0, 100000000000, 10000000000), seq(0, 1000000000000, 100000000000),
                     seq(0, 10000000000000, 1000000000000))
  ) +
  scale_x_date(
    limits = c(df_bands$date[min(which(!is.na(df_bands$mv)))], last(df_bands$date)),
    breaks = "1 year",
    minor_breaks = NULL,
    date_labels = "%Y"
  ) +
  theme_minimal() + 
  labs(title = "Bitcoin price and the 1-year moving averages of the Illiquid Value (IV)* & Long-term holder Value (LV)**", 
       subtitle = paste("Date: ", as.character(format(Sys.Date(), "%b %d, %Y")), ", data: Glassnode, chart: @dilutionproof", sep = ""), 
       x = "Time",
       y = "United States Dollars (USD)",
       caption = "* Illiquid Value (IV): The total USD value of the total supply of all bitcoin that are labelled 'illiquid' by Glassnode\n** Long-term holder Value (LV): The total USD value of the total supply of all bitcoin that is labelled 'long-term holder' by Glassnode") + 
  scale_colour_manual(
    name = "Legend",
    values = c("black", "red", "blue"),
    labels = c("MV", "IV", "LV")
  ) +
  annotate(
    "label",
    label = paste(
      "Current values:\nMV: $",
      as.character(format(round(last(df_bands$mv)/1000000000, 1), nsmall = 1)),
      "B\nIV: $",
      as.character(format(round(last(df_bands$iv_365sma)/1000000000, 1), nsmall = 1)),
      "B\nLV: $",
      as.character(format(round(last(df_bands$lv_365sma)/1000000000, 1), nsmall = 1)),
      "B",
      sep = ""
    ),
    x = last(df_bands$date),
    y = min(df_bands$mv, na.rm = TRUE),
    size = 3,
    hjust = 1,
    vjust = 0
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0, face = "italic"),
    plot.caption = element_text(hjust = 0, face = "italic"),
    legend.title = element_text(hjust = 0, face = "bold"),
    plot.title.position = "panel",
    plot.caption.position =  "panel"
  )

ggsave(here::here(paste(as.character(Sys.Date()), "_1_mv_iv_lv.png", sep = "")), chart_mv_iv_lv, height = 12.5 / 1.78, width = 12.5, dpi = 320, units = "in")

# MVIV & MVLV ----
chart_mviv_mvlv <- ggplot(df_bands, aes(x = date)) +
  geom_hline(yintercept = 0, color = "blue") +
  geom_hline(yintercept = 2, color = "green", linetype = 2) +
  geom_hline(yintercept = 4, color = "yellow", linetype = 2) +
  geom_hline(yintercept = 6, color = "orange", linetype = 2) +
  geom_hline(yintercept = 8, color = "red", linetype = 2) +
  geom_hline(yintercept = 10, color = "brown", linetype = 2) +
  geom_line(aes(y = mvlv, colour = "2"), alpha = 0.75) +
  geom_line(aes(y = mviv, colour = "1"), alpha = 0.75) +
  scale_y_continuous(
    breaks = seq(0, 10, 2),
    minor_breaks = NULL
  ) +
  scale_x_date(
    breaks = "1 year",
    minor_breaks = NULL,
    date_labels = "%Y",
    limits = c(df_bands$date[min(which(!is.na(df_bands$price)))], last(df_bands$date))
  ) +
  labs(
    title = "Bitcoin Market-Value-to-Illiquid-Value (MVIV)* & Market-Value-to-Long-term-holder-Value (MVLV)**", 
    subtitle = paste("Date: ", as.character(format(Sys.Date(), "%b %d, %Y")), ", data: Glassnode, chart: @dilutionproof", sep = ""), 
    x = "Time",
    y = "MVIV / MVLV",
    caption = "* MVIV: The difference between the value of all bitcoin and the value of the circulating bitcoin supply that is labelled 'illiquid' by Glassnode, divided by the standard deviation of the market value\n** MVLV: The difference between the value of all circulating bitcoin and that of all bitcoin that are labelled 'long-term holder' by Glassnode, divided by the standard deviation of the market value"
  ) +
  scale_colour_manual(
    name = "Legend",
    values = c("red", "blue"),
    labels = c("MVIV", "MVLV")
  ) +
  annotate(
    "label",
    label = paste(
      "Current MVIV: ",
      as.character(format(round(last(df_bands$mviv), 2), nsmall = 2)),
      "\nCurrent MVLV: ",
      as.character(format(round(last(df_bands$mvlv), 2), nsmall = 2)),
      sep = ""
    ),
    x = last(df_bands$date),
    y = max(df_bands$mviv[566:nrow(df_bands)], na.rm = TRUE),
    size = 3,
    hjust = 1,
    vjust = 1
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0, face = "italic"),
    plot.caption = element_text(hjust = 0, face = "italic"),
    legend.title = element_text(hjust = 0, face = "bold"),
    plot.title.position = "panel",
    plot.caption.position =  "panel"
  )

ggsave(here::here(paste(as.character(Sys.Date()), "_2_MVIV_MVLV.png", sep = "")), chart_mviv_mvlv, height = 12.5 / 1.78, width = 12.5, dpi = 320, units = "in")

# MVIV Bands ----
chart_mvivb <- ggplot(df_bands, aes(x = date)) +
  geom_line(aes(y = mviv0, colour = "6")) +
  geom_line(aes(y = mviv2, colour = "5"), linetype = 2) +
  geom_line(aes(y = mviv4, colour = "4"), linetype = 2) +
  geom_line(aes(y = mviv6, colour = "3"), linetype = 2) +
  geom_line(aes(y = mviv8, colour = "2"), linetype = 2) +
  geom_line(aes(y = mviv10, colour = "1"), linetype = 2) +
  geom_line(aes(y = price, colour = "7")) +
  scale_y_log10(
    labels=scales::dollar_format(),
    limits = c(min(df_bands$price, na.rm = TRUE), max(df_bands$mviv10, na.rm = TRUE)),
    breaks = c(0, 1, 10, 100, 1000, 10000, 100000, 1000000), 
    minor_breaks = c(
      seq(0, 1, 0.1),
      seq(0, 10, 1),
      seq(0, 100, 10),
      seq(0, 1000, 100),
      seq(0, 10000, 1000),
      seq(0, 100000, 10000),
      seq(0, 1000000, 100000))
  ) +
  scale_x_date(
    limits = c(df_bands$date[min(which(!is.na(df_bands$price)))], last(df_bands$date)),
    breaks = "1 year",
    minor_breaks = NULL,
    date_labels = "%Y"
  ) +
  theme_minimal() + 
  labs(title = "Bitcoin Market-Value-to-Illiquid-Value (MVIV) Bands*", 
       subtitle = paste("Date: ", as.character(format(Sys.Date(), "%b %d, %Y")), ", data: Coinmetrics, chart: @dilutionproof", sep = ""), 
       x = "Time",
       y = "United States Dollars (USD)",
       caption = "* MVIV Bands: Extrapolated prices for when market value deviates [Band #] standard deviations from the value of the 1-year moving average of the supply that Glassnode labels 'illiquid'") + 
  scale_colour_manual(
    name = "Bands",
    values = c("brown", "red", "orange", "yellow", "green", "blue", "black"),
    breaks = c('1', '2', '3', '4', '5', '6'),
    labels = c("MVIV10", "MVIV8", "MVIV6", "MVIV4", "MVIV2", "MVIV0")
  ) +
  annotate(
    "label",
    label = paste(
      "Current values:\nMVIV10: $",
      as.character(format(round(last(df_bands$mviv10)/1000, 1), nsmall = 1)),
      "k\nMVIV8: $",
      as.character(format(round(last(df_bands$mviv8)/1000, 1), nsmall = 1)),
      "k\nMVIV6: $",
      as.character(format(round(last(df_bands$mviv6)/1000, 1), nsmall = 1)),
      "k\nMVIV4: $",
      as.character(format(round(last(df_bands$mviv4)/1000, 1), nsmall = 1)),
      "k\nMVIV2: $",
      as.character(format(round(last(df_bands$mviv2)/1000, 1), nsmall = 1)),
      "k\nMVIV0: $",
      as.character(format(round(last(df_bands$mviv0)/1000, 1), nsmall = 1)),
      "k\nPrice: $",
      as.character(format(round(last(df_bands$price)/1000, 1), nsmall = 1)),
      "k",
      sep = ""
    ),
    x = last(df_bands$date),
    y = min(df_bands$price, na.rm = TRUE),
    size = 3,
    hjust = 1,
    vjust = 0
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0, face = "italic"),
    plot.caption = element_text(hjust = 0, face = "italic"),
    legend.title = element_text(hjust = 0, face = "bold"),
    plot.title.position = "panel",
    plot.caption.position =  "panel"
  )

ggsave(here::here(paste(as.character(Sys.Date()), "_3_mviv_bands.png", sep = "")), chart_mvivb, height = 12.5 / 1.78, width = 12.5, dpi = 320, units = "in")

# MVIV Bands (last 10y) ----
chart_mvivb_last10y <- ggplot(df_bands, aes(x = date)) +
  geom_line(aes(y = mviv0, colour = "6")) +
  geom_line(aes(y = mviv2, colour = "5"), linetype = 2) +
  geom_line(aes(y = mviv4, colour = "4"), linetype = 2) +
  geom_line(aes(y = mviv6, colour = "3"), linetype = 2) +
  geom_line(aes(y = mviv8, colour = "2"), linetype = 2) +
  geom_line(aes(y = mviv10, colour = "1"), linetype = 2) +
  geom_line(aes(y = price, colour = "7")) +
  scale_y_log10(
    labels=scales::dollar_format(),
    limits = c(
      ifelse(
        min(df_bands$mviv0[(nrow(df_bands)-10*365.25):nrow(df_bands)], na.rm = TRUE) < min(df_bands$price[(nrow(df_bands)-10*365.25):nrow(df_bands)], na.rm = TRUE),
        min(df_bands$mviv0[(nrow(df_bands)-10*365.25):nrow(df_bands)], na.rm = TRUE),
        min(df_bands$price[(nrow(df_bands)-10*365.25):nrow(df_bands)], na.rm = TRUE)
      ),
      max(df_bands$mviv10, na.rm = TRUE)
    ),
    breaks = c(0, 1, 10, 100, 1000, 10000, 100000, 1000000), 
    minor_breaks = c(
      seq(0, 1, 0.1),
      seq(0, 10, 1),
      seq(0, 100, 10),
      seq(0, 1000, 100),
      seq(0, 10000, 1000),
      seq(0, 100000, 10000),
      seq(0, 1000000, 100000))
  ) +
  scale_x_date(
    limits = c(last(df_bands$date)-10*365.25, last(df_bands$date)),
    breaks = "1 year",
    minor_breaks = NULL,
    date_labels = "%Y"
  ) +
  theme_minimal() + 
  labs(title = "Bitcoin Market-Value-to-Illiquid-Value (MVIV) Bands* during the last 10 years", 
       subtitle = paste("Date: ", as.character(format(Sys.Date(), "%b %d, %Y")), ", data: Coinmetrics, chart: @dilutionproof", sep = ""), 
       x = "Time",
       y = "United States Dollars (USD)",
       caption = "* MVIV Bands: Extrapolated prices for when market value deviates [Band #] standard deviations from the value of the 1-year moving average of the supply that Glassnode labels 'illiquid'") + 
  scale_colour_manual(
    name = "Bands",
    values = c("brown", "red", "orange", "yellow", "green", "blue", "black"),
    breaks = c('1', '2', '3', '4', '5', '6'),
    labels = c("MVIV10", "MVIV8", "MVIV6", "MVIV4", "MVIV2", "MVIV0")
  ) +
  annotate(
    "label",
    label = paste(
      "Current values:\nMVIV10: $",
      as.character(format(round(last(df_bands$mviv10)/1000, 1), nsmall = 1)),
      "k\nMVIV8: $",
      as.character(format(round(last(df_bands$mviv8)/1000, 1), nsmall = 1)),
      "k\nMVIV6: $",
      as.character(format(round(last(df_bands$mviv6)/1000, 1), nsmall = 1)),
      "k\nMVIV4: $",
      as.character(format(round(last(df_bands$mviv4)/1000, 1), nsmall = 1)),
      "k\nMVIV2: $",
      as.character(format(round(last(df_bands$mviv2)/1000, 1), nsmall = 1)),
      "k\nMVIV0: $",
      as.character(format(round(last(df_bands$mviv0)/1000, 1), nsmall = 1)),
      "k\nPrice: $",
      as.character(format(round(last(df_bands$price)/1000, 1), nsmall = 1)),
      "k",
      sep = ""
    ),
    x = last(df_bands$date),
    y = ifelse(
      min(df_bands$mviv0[(nrow(df_bands)-10*365.25):nrow(df_bands)], na.rm = TRUE) < min(df_bands$price[(nrow(df_bands)-10*365.25):nrow(df_bands)], na.rm = TRUE),
      min(df_bands$mviv0[(nrow(df_bands)-10*365.25):nrow(df_bands)], na.rm = TRUE),
      min(df_bands$price[(nrow(df_bands)-10*365.25):nrow(df_bands)], na.rm = TRUE)
    ),
    size = 3,
    hjust = 1,
    vjust = 0
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0, face = "italic"),
    plot.caption = element_text(hjust = 0, face = "italic"),
    legend.title = element_text(hjust = 0, face = "bold"),
    plot.title.position = "panel",
    plot.caption.position =  "panel"
  )

ggsave(here::here(paste(as.character(Sys.Date()), "_4_mviv_bands_last10y.png", sep = "")), chart_mvivb_last10y, height = 12.5 / 1.78, width = 12.5, dpi = 320, units = "in")

# MVIV Bands (last 5y) ----
chart_mvivb_last5y <- ggplot(df_bands, aes(x = date)) +
  geom_line(aes(y = mviv0, colour = "6")) +
  geom_line(aes(y = mviv2, colour = "5"), linetype = 2) +
  geom_line(aes(y = mviv4, colour = "4"), linetype = 2) +
  geom_line(aes(y = mviv6, colour = "3"), linetype = 2) +
  geom_line(aes(y = mviv8, colour = "2"), linetype = 2) +
  geom_line(aes(y = mviv10, colour = "1"), linetype = 2) +
  geom_line(aes(y = price, colour = "7")) +
  scale_y_log10(
    labels=scales::dollar_format(),
    limits = c(
      ifelse(
        min(df_bands$mviv0[(nrow(df_bands)-5*365.25):nrow(df_bands)], na.rm = TRUE) < min(df_bands$price[(nrow(df_bands)-5*365.25):nrow(df_bands)], na.rm = TRUE),
        min(df_bands$mviv0[(nrow(df_bands)-5*365.25):nrow(df_bands)], na.rm = TRUE),
        min(df_bands$price[(nrow(df_bands)-5*365.25):nrow(df_bands)], na.rm = TRUE)
      ),
      max(df_bands$mviv10, na.rm = TRUE)
    ),
    breaks = c(0, 1, 10, 100, 1000, 10000, 100000, 1000000), 
    minor_breaks = c(
      seq(0, 1, 0.1),
      seq(0, 10, 1),
      seq(0, 100, 10),
      seq(0, 1000, 100),
      seq(0, 10000, 1000),
      seq(0, 100000, 10000),
      seq(0, 1000000, 100000))
  ) +
  scale_x_date(
    limits = c(last(df_bands$date)-5*365.25, last(df_bands$date)),
    breaks = "1 year",
    minor_breaks = NULL,
    date_labels = "%Y"
  ) +
  theme_minimal() + 
  labs(title = "Bitcoin Market-Value-to-Illiquid-Value (MVIV) Bands* during the last 5 years", 
       subtitle = paste("Date: ", as.character(format(Sys.Date(), "%b %d, %Y")), ", data: Coinmetrics, chart: @dilutionproof", sep = ""), 
       x = "Time",
       y = "United States Dollars (USD)",
       caption = "* MVIV Bands: Extrapolated prices for when market value deviates [Band #] standard deviations from the value of the 1-year moving average of the supply that Glassnode labels 'illiquid'") + 
  scale_colour_manual(
    name = "Bands",
    values = c("brown", "red", "orange", "yellow", "green", "blue", "black"),
    breaks = c('1', '2', '3', '4', '5', '6'),
    labels = c("MVIV10", "MVIV8", "MVIV6", "MVIV4", "MVIV2", "MVIV0")
  ) +
  annotate(
    "label",
    label = paste(
      "Current values:\nMVIV10: $",
      as.character(format(round(last(df_bands$mviv10)/1000, 1), nsmall = 1)),
      "k\nMVIV8: $",
      as.character(format(round(last(df_bands$mviv8)/1000, 1), nsmall = 1)),
      "k\nMVIV6: $",
      as.character(format(round(last(df_bands$mviv6)/1000, 1), nsmall = 1)),
      "k\nMVIV4: $",
      as.character(format(round(last(df_bands$mviv4)/1000, 1), nsmall = 1)),
      "k\nMVIV2: $",
      as.character(format(round(last(df_bands$mviv2)/1000, 1), nsmall = 1)),
      "k\nMVIV0: $",
      as.character(format(round(last(df_bands$mviv0)/1000, 1), nsmall = 1)),
      "k\nPrice: $",
      as.character(format(round(last(df_bands$price)/1000, 1), nsmall = 1)),
      "k",
      sep = ""
    ),
    x = last(df_bands$date),
    y = ifelse(
      min(df_bands$mviv0[(nrow(df_bands)-5*365.25):nrow(df_bands)], na.rm = TRUE) < min(df_bands$price[(nrow(df_bands)-5*365.25):nrow(df_bands)], na.rm = TRUE),
      min(df_bands$mviv0[(nrow(df_bands)-5*365.25):nrow(df_bands)], na.rm = TRUE),
      min(df_bands$price[(nrow(df_bands)-5*365.25):nrow(df_bands)], na.rm = TRUE)
    ),
    size = 3,
    hjust = 1,
    vjust = 0
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0, face = "italic"),
    plot.caption = element_text(hjust = 0, face = "italic"),
    legend.title = element_text(hjust = 0, face = "bold"),
    plot.title.position = "panel",
    plot.caption.position =  "panel"
  )

ggsave(here::here(paste(as.character(Sys.Date()), "_5_mviv_bands_last5y.png", sep = "")), chart_mvivb_last5y, height = 12.5 / 1.78, width = 12.5, dpi = 320, units = "in")

# MVIV Bands (last 2y) ----
chart_mvivb_last2y <- ggplot(df_bands, aes(x = date)) +
  geom_line(aes(y = mviv0, colour = "6")) +
  geom_line(aes(y = mviv2, colour = "5"), linetype = 2) +
  geom_line(aes(y = mviv4, colour = "4"), linetype = 2) +
  geom_line(aes(y = mviv6, colour = "3"), linetype = 2) +
  geom_line(aes(y = mviv8, colour = "2"), linetype = 2) +
  geom_line(aes(y = mviv10, colour = "1"), linetype = 2) +
  geom_line(aes(y = price, colour = "7")) +
  scale_y_log10(
    labels=scales::dollar_format(),
    limits = c(
      ifelse(
        min(df_bands$mviv0[(nrow(df_bands)-2*365.25):nrow(df_bands)], na.rm = TRUE) < min(df_bands$price[(nrow(df_bands)-2*365.25):nrow(df_bands)], na.rm = TRUE),
        min(df_bands$mviv0[(nrow(df_bands)-2*365.25):nrow(df_bands)], na.rm = TRUE),
        min(df_bands$price[(nrow(df_bands)-2*365.25):nrow(df_bands)], na.rm = TRUE)
      ),
      max(df_bands$mviv10, na.rm = TRUE)
    ),
    breaks = c(0, 1, 10, 100, 1000, 10000, 100000, 1000000), 
    minor_breaks = c(
      seq(0, 1, 0.1),
      seq(0, 10, 1),
      seq(0, 100, 10),
      seq(0, 1000, 100),
      seq(0, 10000, 1000),
      seq(0, 100000, 10000),
      seq(0, 1000000, 100000))
  ) +
  scale_x_date(
    limits = c(last(df_bands$date)-2*365.25, last(df_bands$date)),
    breaks = "1 year",
    minor_breaks = NULL,
    date_labels = "%Y"
  ) +
  theme_minimal() + 
  labs(title = "Bitcoin Market-Value-to-Illiquid-Value (MVIV) Bands* during the last 2 years", 
       subtitle = paste("Date: ", as.character(format(Sys.Date(), "%b %d, %Y")), ", data: Coinmetrics, chart: @dilutionproof", sep = ""), 
       x = "Time",
       y = "United States Dollars (USD)",
       caption = "* MVIV Bands: Extrapolated prices for when market value deviates [Band #] standard deviations from the value of the 1-year moving average of the supply that Glassnode labels 'illiquid'") + 
  scale_colour_manual(
    name = "Bands",
    values = c("brown", "red", "orange", "yellow", "green", "blue", "black"),
    breaks = c('1', '2', '3', '4', '5', '6'),
    labels = c("MVIV10", "MVIV8", "MVIV6", "MVIV4", "MVIV2", "MVIV0")
  ) +
  annotate(
    "label",
    label = paste(
      "Current values:\nMVIV10: $",
      as.character(format(round(last(df_bands$mviv10)/1000, 1), nsmall = 1)),
      "k\nMVIV8: $",
      as.character(format(round(last(df_bands$mviv8)/1000, 1), nsmall = 1)),
      "k\nMVIV6: $",
      as.character(format(round(last(df_bands$mviv6)/1000, 1), nsmall = 1)),
      "k\nMVIV4: $",
      as.character(format(round(last(df_bands$mviv4)/1000, 1), nsmall = 1)),
      "k\nMVIV2: $",
      as.character(format(round(last(df_bands$mviv2)/1000, 1), nsmall = 1)),
      "k\nMVIV0: $",
      as.character(format(round(last(df_bands$mviv0)/1000, 1), nsmall = 1)),
      "k\nPrice: $",
      as.character(format(round(last(df_bands$price)/1000, 1), nsmall = 1)),
      "k",
      sep = ""
    ),
    x = last(df_bands$date),
    y = ifelse(
      min(df_bands$mviv0[(nrow(df_bands)-2*365.25):nrow(df_bands)], na.rm = TRUE) < min(df_bands$price[(nrow(df_bands)-2*365.25):nrow(df_bands)], na.rm = TRUE),
      min(df_bands$mviv0[(nrow(df_bands)-2*365.25):nrow(df_bands)], na.rm = TRUE),
      min(df_bands$price[(nrow(df_bands)-2*365.25):nrow(df_bands)], na.rm = TRUE)
    ),
    size = 3,
    hjust = 1,
    vjust = 0
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0, face = "italic"),
    plot.caption = element_text(hjust = 0, face = "italic"),
    legend.title = element_text(hjust = 0, face = "bold"),
    plot.title.position = "panel",
    plot.caption.position =  "panel"
  )

ggsave(here::here(paste(as.character(Sys.Date()), "_6_mviv_bands_last2y.png", sep = "")), chart_mvivb_last2y, height = 12.5 / 1.78, width = 12.5, dpi = 320, units = "in")

# MVLV Bands ----
chart_mvlvb <- ggplot(df_bands, aes(x = date)) +
  geom_line(aes(y = mvlv0, colour = "6")) +
  geom_line(aes(y = mvlv2, colour = "5"), linetype = 2) +
  geom_line(aes(y = mvlv4, colour = "4"), linetype = 2) +
  geom_line(aes(y = mvlv6, colour = "3"), linetype = 2) +
  geom_line(aes(y = mvlv8, colour = "2"), linetype = 2) +
  geom_line(aes(y = mvlv10, colour = "1"), linetype = 2) +
  geom_line(aes(y = price, colour = "7")) +
  scale_y_log10(
    labels=scales::dollar_format(),
    limits = c(min(df_bands$price, na.rm = TRUE), max(df_bands$mvlv10, na.rm = TRUE)),
    breaks = c(0, 1, 10, 100, 1000, 10000, 100000, 1000000), 
    minor_breaks = c(
      seq(0, 1, 0.1),
      seq(0, 10, 1),
      seq(0, 100, 10),
      seq(0, 1000, 100),
      seq(0, 10000, 1000),
      seq(0, 100000, 10000),
      seq(0, 1000000, 100000))
  ) +
  scale_x_date(
    limits = c(df_bands$date[min(which(!is.na(df_bands$price)))], last(df_bands$date)),
    breaks = "1 year",
    minor_breaks = NULL,
    date_labels = "%Y"
  ) +
  theme_minimal() + 
  labs(title = "Bitcoin Market-Value-to-Long-Term Holder-Value (MVLV) Bands*", 
       subtitle = paste("Date: ", as.character(format(Sys.Date(), "%b %d, %Y")), ", data: Coinmetrics, chart: @dilutionproof", sep = ""), 
       x = "Time",
       y = "United States Dollars (USD)",
       caption = "* MVLV Bands: Extrapolated prices for when market value deviates [Band #] standard deviations from the value of the 1-year moving average of the supply that are labelled 'long-term holder' by Glassnode") + 
  scale_colour_manual(
    name = "Bands",
    values = c("brown", "red", "orange", "yellow", "green", "blue", "black"),
    breaks = c('1', '2', '3', '4', '5', '6'),
    labels = c("MVLV10", "MVLV8", "MVLV6", "MVLV4", "MVLV2", "MVLV0")
  ) +
  annotate(
    "label",
    label = paste(
      "Current values:\nMVLV10: $",
      as.character(format(round(last(df_bands$mvlv10)/1000, 1), nsmall = 1)),
      "k\nMVLV8: $",
      as.character(format(round(last(df_bands$mvlv8)/1000, 1), nsmall = 1)),
      "k\nMVLV6: $",
      as.character(format(round(last(df_bands$mvlv6)/1000, 1), nsmall = 1)),
      "k\nMVLV4: $",
      as.character(format(round(last(df_bands$mvlv4)/1000, 1), nsmall = 1)),
      "k\nMVLV2: $",
      as.character(format(round(last(df_bands$mvlv2)/1000, 1), nsmall = 1)),
      "k\nMVLV0: $",
      as.character(format(round(last(df_bands$mvlv0)/1000, 1), nsmall = 1)),
      "k\nPrice: $",
      as.character(format(round(last(df_bands$price)/1000, 1), nsmall = 1)),
      "k",
      sep = ""
    ),
    x = last(df_bands$date),
    y = min(df_bands$price, na.rm = TRUE),
    size = 3,
    hjust = 1,
    vjust = 0
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0, face = "italic"),
    plot.caption = element_text(hjust = 0, face = "italic"),
    legend.title = element_text(hjust = 0, face = "bold"),
    plot.title.position = "panel",
    plot.caption.position =  "panel"
  )

ggsave(here::here(paste(as.character(Sys.Date()), "_7_mvlv_bands.png", sep = "")), chart_mvlvb, height = 12.5 / 1.78, width = 12.5, dpi = 320, units = "in")

# MVLV Bands (last 10y) ----
chart_mvlvb_last10y <- ggplot(df_bands, aes(x = date)) +
  geom_line(aes(y = mvlv0, colour = "6")) +
  geom_line(aes(y = mvlv2, colour = "5"), linetype = 2) +
  geom_line(aes(y = mvlv4, colour = "4"), linetype = 2) +
  geom_line(aes(y = mvlv6, colour = "3"), linetype = 2) +
  geom_line(aes(y = mvlv8, colour = "2"), linetype = 2) +
  geom_line(aes(y = mvlv10, colour = "1"), linetype = 2) +
  geom_line(aes(y = price, colour = "7")) +
  scale_y_log10(
    labels=scales::dollar_format(),
    limits = c(
      ifelse(
        min(df_bands$mvlv0[(nrow(df_bands)-10*365.25):nrow(df_bands)], na.rm = TRUE) < min(df_bands$price[(nrow(df_bands)-10*365.25):nrow(df_bands)], na.rm = TRUE),
        min(df_bands$mvlv0[(nrow(df_bands)-10*365.25):nrow(df_bands)], na.rm = TRUE),
        min(df_bands$price[(nrow(df_bands)-10*365.25):nrow(df_bands)], na.rm = TRUE)
      ),
      max(df_bands$mvlv10, na.rm = TRUE)
    ),
    breaks = c(0, 1, 10, 100, 1000, 10000, 100000, 1000000), 
    minor_breaks = c(
      seq(0, 1, 0.1),
      seq(0, 10, 1),
      seq(0, 100, 10),
      seq(0, 1000, 100),
      seq(0, 10000, 1000),
      seq(0, 100000, 10000),
      seq(0, 1000000, 100000))
  ) +
  scale_x_date(
    limits = c(last(df_bands$date)-10*365.25, last(df_bands$date)),
    breaks = "1 year",
    minor_breaks = NULL,
    date_labels = "%Y"
  ) +
  theme_minimal() + 
  labs(title = "Bitcoin Market-Value-to-Long-Term Holder-Value (MVLV) Bands* during the last 10 years", 
       subtitle = paste("Date: ", as.character(format(Sys.Date(), "%b %d, %Y")), ", data: Coinmetrics, chart: @dilutionproof", sep = ""), 
       x = "Time",
       y = "United States Dollars (USD)",
       caption = "* MVLV Bands: Extrapolated prices for when market value deviates [Band #] standard deviations from the value of the 1-year moving average of the supply that are labelled 'long-term holder' by Glassnode'") + 
  scale_colour_manual(
    name = "Bands",
    values = c("brown", "red", "orange", "yellow", "green", "blue", "black"),
    breaks = c('1', '2', '3', '4', '5', '6'),
    labels = c("MVLV10", "MVLV8", "MVLV6", "MVLV4", "MVLV2", "MVLV0")
  ) +
  annotate(
    "label",
    label = paste(
      "Current values:\nMVLV10: $",
      as.character(format(round(last(df_bands$mvlv10)/1000, 1), nsmall = 1)),
      "k\nMVLV8: $",
      as.character(format(round(last(df_bands$mvlv8)/1000, 1), nsmall = 1)),
      "k\nMVLV6: $",
      as.character(format(round(last(df_bands$mvlv6)/1000, 1), nsmall = 1)),
      "k\nMVLV4: $",
      as.character(format(round(last(df_bands$mvlv4)/1000, 1), nsmall = 1)),
      "k\nMVLV2: $",
      as.character(format(round(last(df_bands$mvlv2)/1000, 1), nsmall = 1)),
      "k\nMVLV0: $",
      as.character(format(round(last(df_bands$mvlv0)/1000, 1), nsmall = 1)),
      "k\nPrice: $",
      as.character(format(round(last(df_bands$price)/1000, 1), nsmall = 1)),
      "k",
      sep = ""
    ),
    x = last(df_bands$date),
    y = ifelse(
      min(df_bands$mvlv0[(nrow(df_bands)-10*365.25):nrow(df_bands)], na.rm = TRUE) < min(df_bands$price[(nrow(df_bands)-10*365.25):nrow(df_bands)], na.rm = TRUE),
      min(df_bands$mvlv0[(nrow(df_bands)-10*365.25):nrow(df_bands)], na.rm = TRUE),
      min(df_bands$price[(nrow(df_bands)-10*365.25):nrow(df_bands)], na.rm = TRUE)
    ),
    size = 3,
    hjust = 1,
    vjust = 0
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0, face = "italic"),
    plot.caption = element_text(hjust = 0, face = "italic"),
    legend.title = element_text(hjust = 0, face = "bold"),
    plot.title.position = "panel",
    plot.caption.position =  "panel"
  )

ggsave(here::here(paste(as.character(Sys.Date()), "_8_mvlv_bands_last10y.png", sep = "")), chart_mvlvb_last10y, height = 12.5 / 1.78, width = 12.5, dpi = 320, units = "in")

# MVLV Bands (last 5y) ----
chart_mvlvb_last5y <- ggplot(df_bands, aes(x = date)) +
  geom_line(aes(y = mvlv0, colour = "6")) +
  geom_line(aes(y = mvlv2, colour = "5"), linetype = 2) +
  geom_line(aes(y = mvlv4, colour = "4"), linetype = 2) +
  geom_line(aes(y = mvlv6, colour = "3"), linetype = 2) +
  geom_line(aes(y = mvlv8, colour = "2"), linetype = 2) +
  geom_line(aes(y = mvlv10, colour = "1"), linetype = 2) +
  geom_line(aes(y = price, colour = "7")) +
  scale_y_log10(
    labels=scales::dollar_format(),
    limits = c(
      ifelse(
        min(df_bands$mvlv0[(nrow(df_bands)-5*365.25):nrow(df_bands)], na.rm = TRUE) < min(df_bands$price[(nrow(df_bands)-5*365.25):nrow(df_bands)], na.rm = TRUE),
        min(df_bands$mvlv0[(nrow(df_bands)-5*365.25):nrow(df_bands)], na.rm = TRUE),
        min(df_bands$price[(nrow(df_bands)-5*365.25):nrow(df_bands)], na.rm = TRUE)
      ),
      max(df_bands$mvlv10, na.rm = TRUE)
    ),
    breaks = c(0, 1, 10, 100, 1000, 10000, 100000, 1000000), 
    minor_breaks = c(
      seq(0, 1, 0.1),
      seq(0, 10, 1),
      seq(0, 100, 10),
      seq(0, 1000, 100),
      seq(0, 10000, 1000),
      seq(0, 100000, 10000),
      seq(0, 1000000, 100000))
  ) +
  scale_x_date(
    limits = c(last(df_bands$date)-5*365.25, last(df_bands$date)),
    breaks = "1 year",
    minor_breaks = NULL,
    date_labels = "%Y"
  ) +
  theme_minimal() + 
  labs(title = "Bitcoin Market-Value-to-Long-Term Holder-Value (MVLV) Bands* during the last 5 years", 
       subtitle = paste("Date: ", as.character(format(Sys.Date(), "%b %d, %Y")), ", data: Coinmetrics, chart: @dilutionproof", sep = ""), 
       x = "Time",
       y = "United States Dollars (USD)",
       caption = "* MVLV Bands: Extrapolated prices for when market value deviates [Band #] standard deviations from the value of the 1-year moving average of the supply that are labelled 'long-term holder' by Glassnode'") + 
  scale_colour_manual(
    name = "Bands",
    values = c("brown", "red", "orange", "yellow", "green", "blue", "black"),
    breaks = c('1', '2', '3', '4', '5', '6'),
    labels = c("MVLV10", "MVLV8", "MVLV6", "MVLV4", "MVLV2", "MVLV0")
  ) +
  annotate(
    "label",
    label = paste(
      "Current values:\nMVLV10: $",
      as.character(format(round(last(df_bands$mvlv10)/1000, 1), nsmall = 1)),
      "k\nMVLV8: $",
      as.character(format(round(last(df_bands$mvlv8)/1000, 1), nsmall = 1)),
      "k\nMVLV6: $",
      as.character(format(round(last(df_bands$mvlv6)/1000, 1), nsmall = 1)),
      "k\nMVLV4: $",
      as.character(format(round(last(df_bands$mvlv4)/1000, 1), nsmall = 1)),
      "k\nMVLV2: $",
      as.character(format(round(last(df_bands$mvlv2)/1000, 1), nsmall = 1)),
      "k\nMVLV0: $",
      as.character(format(round(last(df_bands$mvlv0)/1000, 1), nsmall = 1)),
      "k\nPrice: $",
      as.character(format(round(last(df_bands$price)/1000, 1), nsmall = 1)),
      "k",
      sep = ""
    ),
    x = last(df_bands$date),
    y = ifelse(
      min(df_bands$mvlv0[(nrow(df_bands)-5*365.25):nrow(df_bands)], na.rm = TRUE) < min(df_bands$price[(nrow(df_bands)-5*365.25):nrow(df_bands)], na.rm = TRUE),
      min(df_bands$mvlv0[(nrow(df_bands)-5*365.25):nrow(df_bands)], na.rm = TRUE),
      min(df_bands$price[(nrow(df_bands)-5*365.25):nrow(df_bands)], na.rm = TRUE)
    ),
    size = 3,
    hjust = 1,
    vjust = 0
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0, face = "italic"),
    plot.caption = element_text(hjust = 0, face = "italic"),
    legend.title = element_text(hjust = 0, face = "bold"),
    plot.title.position = "panel",
    plot.caption.position =  "panel"
  )

ggsave(here::here(paste(as.character(Sys.Date()), "_9_mvlv_bands_last5y.png", sep = "")), chart_mvlvb_last5y, height = 12.5 / 1.78, width = 12.5, dpi = 320, units = "in")

# MVLV Bands (last 2y) ----
chart_mvlvb_last2y <- ggplot(df_bands, aes(x = date)) +
  geom_line(aes(y = mvlv0, colour = "6")) +
  geom_line(aes(y = mvlv2, colour = "5"), linetype = 2) +
  geom_line(aes(y = mvlv4, colour = "4"), linetype = 2) +
  geom_line(aes(y = mvlv6, colour = "3"), linetype = 2) +
  geom_line(aes(y = mvlv8, colour = "2"), linetype = 2) +
  geom_line(aes(y = mvlv10, colour = "1"), linetype = 2) +
  geom_line(aes(y = price, colour = "7")) +
  scale_y_log10(
    labels=scales::dollar_format(),
    limits = c(
      ifelse(
        min(df_bands$mvlv0[(nrow(df_bands)-2*365.25):nrow(df_bands)], na.rm = TRUE) < min(df_bands$price[(nrow(df_bands)-2*365.25):nrow(df_bands)], na.rm = TRUE),
        min(df_bands$mvlv0[(nrow(df_bands)-2*365.25):nrow(df_bands)], na.rm = TRUE),
        min(df_bands$price[(nrow(df_bands)-2*365.25):nrow(df_bands)], na.rm = TRUE)
      ),
      max(df_bands$mvlv10, na.rm = TRUE)
    ),
    breaks = c(0, 1, 10, 100, 1000, 10000, 100000, 1000000), 
    minor_breaks = c(
      seq(0, 1, 0.1),
      seq(0, 10, 1),
      seq(0, 100, 10),
      seq(0, 1000, 100),
      seq(0, 10000, 1000),
      seq(0, 100000, 10000),
      seq(0, 1000000, 100000))
  ) +
  scale_x_date(
    limits = c(last(df_bands$date)-2*365.25, last(df_bands$date)),
    breaks = "1 year",
    minor_breaks = NULL,
    date_labels = "%Y"
  ) +
  theme_minimal() + 
  labs(title = "Bitcoin Market-Value-to-Long-Term Holder-Value (MVLV) Bands* during the last 2 years", 
       subtitle = paste("Date: ", as.character(format(Sys.Date(), "%b %d, %Y")), ", data: Coinmetrics, chart: @dilutionproof", sep = ""), 
       x = "Time",
       y = "United States Dollars (USD)",
       caption = "* MVLV Bands: Extrapolated prices for when market value deviates [Band #] standard deviations from the value of the 1-year moving average of the supply that are labelled 'long-term holder' by Glassnode'") + 
  scale_colour_manual(
    name = "Bands",
    values = c("brown", "red", "orange", "yellow", "green", "blue", "black"),
    breaks = c('1', '2', '3', '4', '5', '6'),
    labels = c("MVLV10", "MVLV8", "MVLV6", "MVLV4", "MVLV2", "MVLV0")
  ) +
  annotate(
    "label",
    label = paste(
      "Current values:\nMVLV10: $",
      as.character(format(round(last(df_bands$mvlv10)/1000, 1), nsmall = 1)),
      "k\nMVLV8: $",
      as.character(format(round(last(df_bands$mvlv8)/1000, 1), nsmall = 1)),
      "k\nMVLV6: $",
      as.character(format(round(last(df_bands$mvlv6)/1000, 1), nsmall = 1)),
      "k\nMVLV4: $",
      as.character(format(round(last(df_bands$mvlv4)/1000, 1), nsmall = 1)),
      "k\nMVLV2: $",
      as.character(format(round(last(df_bands$mvlv2)/1000, 1), nsmall = 1)),
      "k\nMVLV0: $",
      as.character(format(round(last(df_bands$mvlv0)/1000, 1), nsmall = 1)),
      "k\nPrice: $",
      as.character(format(round(last(df_bands$price)/1000, 1), nsmall = 1)),
      "k",
      sep = ""
    ),
    x = last(df_bands$date),
    y = ifelse(
      min(df_bands$mvlv0[(nrow(df_bands)-2*365.25):nrow(df_bands)], na.rm = TRUE) < min(df_bands$price[(nrow(df_bands)-2*365.25):nrow(df_bands)], na.rm = TRUE),
      min(df_bands$mvlv0[(nrow(df_bands)-2*365.25):nrow(df_bands)], na.rm = TRUE),
      min(df_bands$price[(nrow(df_bands)-2*365.25):nrow(df_bands)], na.rm = TRUE)
    ),
    size = 3,
    hjust = 1,
    vjust = 0
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0, face = "italic"),
    plot.caption = element_text(hjust = 0, face = "italic"),
    legend.title = element_text(hjust = 0, face = "bold"),
    plot.title.position = "panel",
    plot.caption.position =  "panel"
  )

ggsave(here::here(paste(as.character(Sys.Date()), "_10_mvlv_bands_last2y.png", sep = "")), chart_mvlvb_last2y, height = 12.5 / 1.78, width = 12.5, dpi = 320, units = "in")

# Bands baselines ----
chart_bands_baselines <- ggplot(df_bands, aes(x = date)) +
  geom_line(aes(y = mvrv0, colour = "2")) +
  geom_line(aes(y = mvlv0, colour = "3")) +
  geom_line(aes(y = mviv0, colour = "4")) +
  geom_line(aes(y = bpt0, colour = "5")) +
  geom_line(aes(y = price, colour = "1")) +
  scale_y_log10(
    labels=scales::dollar_format(),
    limits = c(min(df_bands$price, na.rm = TRUE), max(df_bands$price, na.rm = TRUE)),
    breaks = c(0,1,10,100,1000,10000,100000,1000000, 10000000, 100000000, 1000000000, 10000000000, 100000000000, 1000000000000,
               10000000000000), 
    minor_breaks = c(seq(0, 1, 0.1), seq(0, 10, 1), seq(0, 100, 10), seq(0, 1000, 100), seq(0, 10000, 1000), seq(0, 100000, 10000),
                     seq(0, 1000000, 100000), seq(0, 10000000, 1000000), seq(0, 100000000, 10000000), seq(0, 1000000000, 100000000),
                     seq(0, 10000000000, 1000000000), seq(0, 100000000000, 10000000000), seq(0, 1000000000000, 100000000000),
                     seq(0, 10000000000000, 1000000000000))
  ) +
  scale_x_date(
    limits = c(df_bands$date[min(which(!is.na(df_bands$price)))], last(df_bands$date)),
    breaks = "1 year",
    minor_breaks = NULL,
    date_labels = "%Y"
  ) +
  theme_minimal() + 
  labs(title = "Bitcoin price and the baselines of the MVRV*, MVLV**, MVIV***, BPT**** Bands", 
       subtitle = paste("Date: ", as.character(format(Sys.Date(), "%b %d, %Y")), ", data: Glassnode, chart: @dilutionproof", sep = ""), 
       x = "Time",
       y = "United States Dollars (USD)",
       caption = "* MVRV: Market-Value-to-Realized-Value; the difference between the value of all bitcoin & that of all coins when they last moved, divided by the standard deviation of the market value\n** MVLV: Market-Value-to-Long-term-holder-Value; the difference between the value of all coins that are labelled 'long-term holder' by Glassnode, divided by the standard deviation of the market value\n*** MVIV: Market-Value-to-Illiquid-Value; the difference between the value of all bitcoin & that of Glassnode's 'illiquid supply', divided by the standard deviation of the market value\n**** BPT: Bitcoin Price Temperature; the difference between the bitcoin price and its 4-year moving average, divided by the rolling 4-year standard deviation of the bitcoin price") + 
  scale_colour_manual(
    name = "Bands",
    values = c("black", "blue", "green", "orange", "red"),
    labels = c("MVRV0", "MVLV0", "MVIV0", "BPT0"),
    breaks = c('2', '3', '4', '5')
  ) +
  annotate(
    "label",
    label = paste(
      "Current values:\nMVRV0: $",
      as.character(format(round(last(df_bands$mvrv0)/1000, 1), nsmall = 1)),
      "k\nMVLV0: $",
      as.character(format(round(last(df_bands$mvlv0)/1000, 1), nsmall = 1)),
      "k\nMVIV0: $",
      as.character(format(round(last(df_bands$mviv0)/1000, 1), nsmall = 1)),
      "k\nBPT0: $",
      as.character(format(round(last(df_bands$bpt0)/1000, 1), nsmall = 1)),
      "k\nPrice: $",
      as.character(format(round(last(df_bands$price)/1000, 1), nsmall = 1)),
      "k",
      sep = ""
    ),
    x = last(df_bands$date),
    y = min(df_bands$price, na.rm = TRUE),
    size = 3,
    hjust = 1,
    vjust = 0
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0, face = "italic"),
    plot.caption = element_text(hjust = 0, face = "italic"),
    legend.title = element_text(hjust = 0, face = "bold"),
    plot.title.position = "panel",
    plot.caption.position =  "panel"
  )

ggsave(here::here(paste(as.character(Sys.Date()), "_11_bands_baselines.png", sep = "")), chart_bands_baselines, height = 12.5 / 1.78, width = 12.5, dpi = 320, units = "in")

# Bands values ----
chart_bands_values <- ggplot(df_bands, aes(x = date)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_line(aes(y = mvrv, colour = "1"), alpha = 0.75) +
  geom_line(aes(y = mvlv, colour = "2"), alpha = 0.75) +
  geom_line(aes(y = mviv, colour = "3"), alpha = 0.75) +
  geom_line(aes(y = bpt, colour = "4"), alpha = 0.75) +
  scale_y_continuous(
  breaks = seq(0, 10, 2),
  minor_breaks = NULL
  ) +
  scale_x_date(
    breaks = "1 year",
    minor_breaks = NULL,
    date_labels = "%Y",
    limits = c(df_bands$date[min(which(!is.na(df_bands$price)))], last(df_bands$date))
  ) +
  labs(
    title = "Bitcoin MVRV*, MVLV**, MVIV*** & BPT****", 
    subtitle = paste("Date: ", as.character(format(Sys.Date(), "%b %d, %Y")), ", data: Glassnode, chart: @dilutionproof", sep = ""), 
    x = "Time",
    y = "MVRV / MVLV / MVIV / BPT",
    caption = "* MVRV: Market-Value-to-Realized-Value; the difference between the value of all bitcoin & that of all coins when they last moved, divided by the standard deviation of the market value\n** MVLV: Market-Value-to-Long-term-holder-Value; the difference between the value of all coins that are labelled 'long-term holder' by Glassnode, divided by the standard deviation of the market value\n*** MVIV: Market-Value-to-Illiquid-Value; the difference between the value of all bitcoin & that of Glassnode's 'illiquid supply', divided by the standard deviation of the market value\n**** BPT: Bitcoin Price Temperature; the difference between the bitcoin price and its 4-year moving average, divided by the rolling 4-year standard deviation of the bitcoin price"
  ) +
  scale_colour_manual(
    name = "Legend",
    values = c("blue", "green", "orange", "red"),
    labels = c("MVRV", "MVLV", "MVIV", "BPT")
  ) +
  annotate(
    "label",
    label = paste(
      "Current MVRV: ",
      as.character(format(round(last(df_bands$mvrv), 2), nsmall = 2)),
      "\nCurrent MVLV: ",
      as.character(format(round(last(df_bands$mvlv), 2), nsmall = 2)),
      "\nCurrent MVIV: ",
      as.character(format(round(last(df_bands$mviv), 2), nsmall = 2)),
      "\nCurrent BPT: ",
      as.character(format(round(last(df_bands$bpt), 2), nsmall = 2)),
      sep = ""
    ),
    x = last(df_bands$date),
    y = max(df_bands$bpt[566:nrow(df_bands)], na.rm = TRUE),
    size = 3,
    hjust = 1,
  vjust = 1
) +
theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0, face = "italic"),
    plot.caption = element_text(hjust = 0, face = "italic"),
    legend.title = element_text(hjust = 0, face = "bold"),
    plot.title.position = "panel",
    plot.caption.position =  "panel"
  )

ggsave(here::here(paste(as.character(Sys.Date()), "_12_bands_values.png", sep = "")), chart_bands_values, height = 12.5 / 1.78, width = 12.5, dpi = 320, units = "in")
