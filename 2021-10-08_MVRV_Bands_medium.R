## Title: Market Value to Realized Value (MVRV) Bands
## Author: Dilution-proof (@dilutionproof)
## Version: v1, 08-10-2021

# Set locale ----
Sys.setlocale(category = "LC_TIME", locale="en_GB.UTF-8") 

# Libraries ----
library(tidyverse)
library(here)

# Data import ----
df_cm <- read_csv(file = "https://coinmetrics.io/newdata/btc.csv")
df_dp <- df_cm

# Data management ----
df_dp$CapMrktCurUSD_sd <- 0
for (i in 0:nrow(df_dp)) {df_dp$CapMrktCurUSD_sd[i] <- sd(df_dp$CapMrktCurUSD[0:i], na.rm = TRUE)}
remove(i)
df_dp <- df_dp %>% mutate(
  rv = CapRealUSD / SplyCur,
  mvrv_z = (CapMrktCurUSD - CapRealUSD) / CapMrktCurUSD_sd,
  mvrv2 = (CapRealUSD + 2*CapMrktCurUSD_sd) / SplyCur,
  mvrv4 = (CapRealUSD + 4*CapMrktCurUSD_sd) / SplyCur,
  mvrv6 = (CapRealUSD + 6*CapMrktCurUSD_sd) / SplyCur,
  mvrv8 = (CapRealUSD + 8*CapMrktCurUSD_sd) / SplyCur,
  mvrv10 = (CapRealUSD + 10*CapMrktCurUSD_sd) / SplyCur,
)

# MV & RV ----
chart_mv_rv <- ggplot(df_dp, aes(x = date)) +
  geom_line(aes(y = CapRealUSD, colour = "2")) +
  geom_line(aes(y = CapMrktCurUSD, colour = "1")) +
  scale_y_log10(
    labels=scales::dollar_format(),
    limits = c(min(df_dp$CapMrktCurUSD, na.rm = TRUE), max(df_dp$CapMrktCurUSD, na.rm = TRUE)),
    breaks = c(0,1,10,100,1000,10000,100000,1000000, 10000000, 100000000, 1000000000, 10000000000, 100000000000, 1000000000000,
               10000000000000), 
    minor_breaks = c(seq(0, 1, 0.1), seq(0, 10, 1), seq(0, 100, 10), seq(0, 1000, 100), seq(0, 10000, 1000), seq(0, 100000, 10000),
                     seq(0, 1000000, 100000), seq(0, 10000000, 1000000), seq(0, 100000000, 10000000), seq(0, 1000000000, 100000000),
                     seq(0, 10000000000, 1000000000), seq(0, 100000000000, 10000000000), seq(0, 1000000000000, 100000000000),
                     seq(0, 10000000000000, 1000000000000))
  ) +
  scale_x_date(
    limits = c(df_dp$date[min(which(!is.na(df_dp$CapMrktCurUSD)))], last(df_dp$date)),
    breaks = "12 months",
    minor_breaks = NULL,
    date_labels = "%Y"
  ) +
  theme_minimal() + 
  labs(title = "Bitcoin Market Value (MV)* and Realized Value (RV)**", 
       subtitle = paste("Date: ", as.character(format(Sys.Date(), "%b %d, %Y")), ", data: Coinmetrics, chart: @dilutionproof", sep = ""), 
       x = "Time",
       y = "United States Dollars (USD)",
       caption = "* MV: The total USD value of all existing bitcoin\n** RV: The total USD value of all existing bitcoin at the last time they moved on-chain") + 
  scale_colour_manual(
    name = "Legend",
    values = c("black", "blue"),
    labels = c("MV", "RV")
    ) +
  annotate(
    "label",
    label = paste(
      "Current values:\nMV: $",
      as.character(format(round(last(df_dp$CapMrktCurUSD)/1000000000, 1), nsmall = 1)),
      "B\nRV: $",
      as.character(format(round(last(df_dp$CapRealUSD)/1000000000, 1), nsmall = 1)),
      "B",
      sep = ""
    ),
    x = last(df_dp$date),
    y = min(df_dp$PriceUSD, na.rm = TRUE),
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
chart_mv_rv

# MVRV Z-Score ----
chart_mvrvz <- ggplot(df_dp, aes(x = date, y = mvrv_z)) +
  geom_hline(yintercept = 0, color = "blue") +
  geom_hline(yintercept = 2, color = "green", linetype = 2) +
  geom_hline(yintercept = 4, color = "yellow", linetype = 2) +
  geom_hline(yintercept = 6, color = "orange", linetype = 2) +
  geom_hline(yintercept = 8, color = "red", linetype = 2) +
  geom_hline(yintercept = 10, color = "brown", linetype = 2) +
  geom_line() +
  scale_y_continuous(
    limits = c(min(df_dp$mvrv_z[566:nrow(df_dp)], na.rm = TRUE), max(df_dp$mvrv_z[566:nrow(df_dp)], na.rm = TRUE)),
    breaks = seq(0, 10, 2),
    minor_breaks = NULL,
  ) +
  scale_x_date(
    breaks = "1 year", minor_breaks = NULL, date_labels = "%Y",
    limits = c(as.Date("2010-07-22"), last(df_dp$date))
  ) + 
  labs(title = "Market-Value-to-Realized-Value (MVRV) Z-Score*", 
       subtitle = paste("Date: ", as.character(format(Sys.Date(), "%b %d, %Y")), ", data: Coinmetrics, chart: @dilutionproof", sep = ""), 
       x = "Time", y = "MVRV Z-Score",
       caption = "* MVRV Z-Score: The difference in the bitcoin market cap and realized cap, divided by the (all-time) standard-deviation of the bitcoin market cap") +
  annotate(
    "label",
    label = paste(
      "Current value:\nMVRV Z-score: ",
      as.character(format(round(last(df_dp$mvrv_z), 2), nsmall = 2)),
      sep = ""
    ),
    x = last(df_dp$date),
    y = max(df_dp$mvrv_z[566:nrow(df_dp)], na.rm = TRUE),
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
chart_mvrvz

# MVRV Bands ----
chart_mvrvb <- ggplot(df_dp, aes(x = date)) +
  geom_line(aes(y = rv, colour = "6")) +
  geom_line(aes(y = mvrv2, colour = "5"), linetype = 2) +
  geom_line(aes(y = mvrv4, colour = "4"), linetype = 2) +
  geom_line(aes(y = mvrv6, colour = "3"), linetype = 2) +
  geom_line(aes(y = mvrv8, colour = "2"), linetype = 2) +
  geom_line(aes(y = mvrv10, colour = "1"), linetype = 2) +
  geom_line(aes(y = PriceUSD, colour = "7")) +
  scale_y_log10(
    labels=scales::dollar_format(),
    limits = c(min(df_dp$PriceUSD, na.rm = TRUE), max(df_dp$mvrv10, na.rm = TRUE)),
    breaks = c(0,1,10,100,1000,10000,100000), 
    minor_breaks = c(seq(0, 1, 0.1), seq(0, 10, 1), seq(0, 100, 10), seq(0, 1000, 100), seq(0, 10000, 1000), seq(0, 100000, 10000))
  ) +
  scale_x_date(
    limits = c(df_dp$date[min(which(!is.na(df_dp$PriceUSD)))], last(df_dp$date)),
    breaks = "1 year",
    minor_breaks = NULL,
    date_labels = "%Y"
  ) +
  theme_minimal() + 
  labs(title = "Bitcoin price and Market-Value-to-Realized-Value (MVRV) Bands*", 
       subtitle = paste("Date: ", as.character(format(Sys.Date(), "%b %d, %Y")), ", data: Coinmetrics, chart: @dilutionproof", sep = ""), 
       x = "Time",
       y = "United States Dollars (USD)",
       caption = "* MVRV Bands: Extrapolated price values, where the bands represent the number of standard deviations (Z-scores) that the total bitcoin market value deviates from its realized value") + 
  scale_colour_manual(
    name = "Legend",
    values = c("brown", "red", "orange", "yellow", "green", "blue", "black"),
    labels = c("MVRV10", "MVRV8", "MVRV6", "MVRV4", "MVRV2", "MVRV0", "Price")
  ) +
  annotate(
    "label",
    label = paste(
      "Current values:\nMVRV10: $",
      as.character(format(round(last(df_dp$mvrv10)/1000, 1), nsmall = 1)),
      "k\nMVRV8: $",
      as.character(format(round(last(df_dp$mvrv8)/1000, 1), nsmall = 1)),
      "k\nMVRV6: $",
      as.character(format(round(last(df_dp$mvrv6)/1000, 1), nsmall = 1)),
      "k\nMVRV4: $",
      as.character(format(round(last(df_dp$mvrv4)/1000, 1), nsmall = 1)),
      "k\nMVRV2: $",
      as.character(format(round(last(df_dp$mvrv2)/1000, 1), nsmall = 1)),
      "k\nMVRV0: $",
      as.character(format(round(last(df_dp$rv)/1000, 1), nsmall = 1)),
      "k\nPrice: $",
      as.character(format(round(last(df_dp$PriceUSD)/1000, 1), nsmall = 1)),
      "k",
      sep = ""
    ),
    x = last(df_dp$date),
    y = min(df_dp$PriceUSD, na.rm = TRUE),
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
chart_mvrvb

# MVRV Bands (last 10y) ----
chart_mvrvb_last10y <- ggplot(df_dp, aes(x = date)) +
  geom_line(aes(y = rv, colour = "6")) +
  geom_line(aes(y = mvrv2, colour = "5"), linetype = 2) +
  geom_line(aes(y = mvrv4, colour = "4"), linetype = 2) +
  geom_line(aes(y = mvrv6, colour = "3"), linetype = 2) +
  geom_line(aes(y = mvrv8, colour = "2"), linetype = 2) +
  geom_line(aes(y = mvrv10, colour = "1"), linetype = 2) +
  geom_line(aes(y = PriceUSD, colour = "7")) +
  scale_y_log10(
    labels = scales::dollar_format(),
    limits = c(
      ifelse(
        min(df_dp$rv[(nrow(df_dp)-10*365.25):nrow(df_dp)], na.rm = TRUE) < min(df_dp$PriceUSD[(nrow(df_dp)-10*365.25):nrow(df_dp)], na.rm = TRUE),
        min(df_dp$rv[(nrow(df_dp)-10*365.25):nrow(df_dp)], na.rm = TRUE),
        min(df_dp$PriceUSD[(nrow(df_dp)-10*365.25):nrow(df_dp)], na.rm = TRUE)
      ),
      max(df_dp$mvrv10, na.rm = TRUE)
    ),
    breaks = c(0,1,10,100,1000,10000,100000), 
    minor_breaks = c(seq(0, 1, 0.1), seq(0, 10, 1), seq(0, 100, 10), seq(0, 1000, 100), seq(0, 10000, 1000), seq(0, 100000, 10000))
  ) +
  scale_x_date(
    limits = c(last(df_dp$date)-10*365.25, last(df_dp$date)),
    breaks = "year",
    minor_breaks = NULL,
    date_labels = "%Y"
  ) +
  theme_minimal() + 
  labs(title = "Bitcoin price and Market-Value-to-Realized-Value (MVRV) Bands* during the last 10 years", 
       subtitle = paste("Date: ", as.character(format(Sys.Date(), "%b %d, %Y")), ", data: Coinmetrics, chart: @dilutionproof", sep = ""), 
       x = "Time",
       y = "United States Dollars (USD)",
       caption = "* MVRV Bands: Extrapolated price values, where the bands represent the number of standard deviations (Z-scores) that the total bitcoin market value deviates from its realized value") + 
  scale_colour_manual(
    name = "Legend",
    values = c("brown", "red", "orange", "yellow", "green", "blue", "black"),
    labels = c("MVRV10", "MVRV8", "MVRV6", "MVRV4", "MVRV2", "MVRV0", "Price")
  ) +
  annotate(
    "label",
    label = paste(
      "Current values:\nMVRV10: $",
      as.character(format(round(last(df_dp$mvrv10)/1000, 1), nsmall = 1)),
      "k\nMVRV8: $",
      as.character(format(round(last(df_dp$mvrv8)/1000, 1), nsmall = 1)),
      "k\nMVRV6: $",
      as.character(format(round(last(df_dp$mvrv6)/1000, 1), nsmall = 1)),
      "k\nMVRV4: $",
      as.character(format(round(last(df_dp$mvrv4)/1000, 1), nsmall = 1)),
      "k\nMVRV2: $",
      as.character(format(round(last(df_dp$mvrv2)/1000, 1), nsmall = 1)),
      "k\nMVRV0: $",
      as.character(format(round(last(df_dp$rv)/1000, 1), nsmall = 1)),
      "k\nPrice: $",
      as.character(format(round(last(df_dp$PriceUSD)/1000, 1), nsmall = 1)),
      "k",
      sep = ""
    ),
    x = last(df_dp$date),
    y = ifelse(
      min(df_dp$rv[(nrow(df_dp)-10*365.25):nrow(df_dp)], na.rm = TRUE) < min(df_dp$PriceUSD[(nrow(df_dp)-10*365.25):nrow(df_dp)], na.rm = TRUE),
      min(df_dp$rv[(nrow(df_dp)-10*365.25):nrow(df_dp)], na.rm = TRUE),
      min(df_dp$PriceUSD[(nrow(df_dp)-10*365.25):nrow(df_dp)], na.rm = TRUE)
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
chart_mvrvb_last10y

# MVRV Bands (last 5y) ----
chart_mvrvb_last5y <- ggplot(df_dp, aes(x = date)) +
  geom_line(aes(y = rv, colour = "6")) +
  geom_line(aes(y = mvrv2, colour = "5"), linetype = 2) +
  geom_line(aes(y = mvrv4, colour = "4"), linetype = 2) +
  geom_line(aes(y = mvrv6, colour = "3"), linetype = 2) +
  geom_line(aes(y = mvrv8, colour = "2"), linetype = 2) +
  geom_line(aes(y = mvrv10, colour = "1"), linetype = 2) +
  geom_line(aes(y = PriceUSD, colour = "7")) +
  scale_y_log10(
    labels = scales::dollar_format(),
    limits = c(
      ifelse(
        min(df_dp$rv[(nrow(df_dp)-5*365.25):nrow(df_dp)], na.rm = TRUE) < min(df_dp$PriceUSD[(nrow(df_dp)-5*365.25):nrow(df_dp)], na.rm = TRUE),
        min(df_dp$rv[(nrow(df_dp)-5*365.25):nrow(df_dp)], na.rm = TRUE),
        min(df_dp$PriceUSD[(nrow(df_dp)-5*365.25):nrow(df_dp)], na.rm = TRUE)
      ),
      max(df_dp$mvrv10, na.rm = TRUE)
    ),
    breaks = c(0,1,10,100,1000,10000,100000), 
    minor_breaks = c(seq(0, 1, 0.1), seq(0, 10, 1), seq(0, 100, 10), seq(0, 1000, 100), seq(0, 10000, 1000), seq(0, 100000, 10000))
  ) +
  scale_x_date(
    limits = c(last(df_dp$date)-5*365.25, last(df_dp$date)),
    breaks = "year",
    minor_breaks = NULL,
    date_labels = "%Y"
  ) +
  theme_minimal() + 
  labs(title = "Bitcoin price and Market-Value-to-Realized-Value (MVRV) Bands* during the last 5 years", 
       subtitle = paste("Date: ", as.character(format(Sys.Date(), "%b %d, %Y")), ", data: Coinmetrics, chart: @dilutionproof", sep = ""), 
       x = "Time",
       y = "United States Dollars (USD)",
       caption = "* MVRV Bands: Extrapolated price values, where the bands represent the number of standard deviations (Z-scores) that the total bitcoin market value deviates from its realized value") + 
  scale_colour_manual(
    name = "Legend",
    values = c("brown", "red", "orange", "yellow", "green", "blue", "black"),
    labels = c("MVRV10", "MVRV8", "MVRV6", "MVRV4", "MVRV2", "MVRV0", "Price")
  ) +
  annotate(
    "label",
    label = paste(
      "Current values:\nMVRV10: $",
      as.character(format(round(last(df_dp$mvrv10)/1000, 1), nsmall = 1)),
      "k\nMVRV8: $",
      as.character(format(round(last(df_dp$mvrv8)/1000, 1), nsmall = 1)),
      "k\nMVRV6: $",
      as.character(format(round(last(df_dp$mvrv6)/1000, 1), nsmall = 1)),
      "k\nMVRV4: $",
      as.character(format(round(last(df_dp$mvrv4)/1000, 1), nsmall = 1)),
      "k\nMVRV2: $",
      as.character(format(round(last(df_dp$mvrv2)/1000, 1), nsmall = 1)),
      "k\nMVRV0: $",
      as.character(format(round(last(df_dp$rv)/1000, 1), nsmall = 1)),
      "k\nPrice: $",
      as.character(format(round(last(df_dp$PriceUSD)/1000, 1), nsmall = 1)),
      "k",
      sep = ""
    ),
    x = last(df_dp$date),
    y = ifelse(
      min(df_dp$rv[(nrow(df_dp)-5*365.25):nrow(df_dp)], na.rm = TRUE) < min(df_dp$PriceUSD[(nrow(df_dp)-5*365.25):nrow(df_dp)], na.rm = TRUE),
      min(df_dp$rv[(nrow(df_dp)-5*365.25):nrow(df_dp)], na.rm = TRUE),
      min(df_dp$PriceUSD[(nrow(df_dp)-5*365.25):nrow(df_dp)], na.rm = TRUE)
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
chart_mvrvb_last5y
