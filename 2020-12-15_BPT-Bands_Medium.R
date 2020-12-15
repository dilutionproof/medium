## 2020-12-15 Bitcoin Price Temperature (BPT) & BPT Bands scripts for Medium article
## Author: @dilutionproof

## 0 Data preparation ----
library(tidyverse)
df_btc <- read_csv(file = "https://coinmetrics.io/newdata/btc.csv")

# Per cycle variables
df_btc <- df_btc %>%
  mutate(cycle = ifelse(date < as.Date("2012-11-28"), 0,
                        ifelse(date < as.Date("2016-07-09"), 1,
                               ifelse(date < as.Date("2020-05-09"), 2, 3))),
         cycle = factor(cycle))

df_btc_cycle0 <- df_btc %>% filter(cycle == 0)
df_btc_cycle1 <- df_btc %>% filter(cycle == 1)
df_btc_cycle2 <- df_btc %>% filter(cycle == 2)
df_btc_cycle3 <- df_btc %>% filter(cycle == 3)

df_btc_cycle0 <- df_btc_cycle0 %>% mutate(day = 1:nrow(df_btc_cycle0))
df_btc_cycle1 <- df_btc_cycle1 %>% mutate(day = 1:nrow(df_btc_cycle1))
df_btc_cycle2 <- df_btc_cycle2 %>% mutate(day = 1:nrow(df_btc_cycle2))
df_btc_cycle3 <- df_btc_cycle3 %>% mutate(day = 1:nrow(df_btc_cycle3))

cycle_length_0 <- as.numeric(nrow(df_btc_cycle0))
cycle_length_1 <- as.numeric(nrow(df_btc_cycle1))
cycle_length_2 <- as.numeric(nrow(df_btc_cycle2))
cycle_length_3 <- as.numeric(nrow(df_btc_cycle3))
cycle_length <- NULL
cycle_length$cycle <- 0:2
cycle_length$length <- c(cycle_length_0, cycle_length_1, cycle_length_2)
cycle_length <- as.data.frame(cycle_length)
cycle_length_longest <- max(cycle_length$length)
cycle_length_shortest <- min(cycle_length$length)
cycle_length_average <- round(mean(cycle_length$length))
cycle_length_previous <- last(cycle_length$length)

df_btc <- bind_rows(df_btc_cycle0, df_btc_cycle1, df_btc_cycle2, df_btc_cycle3)

# 4-year moving average (using the to-date-available data the first 4 years)
df_btc$ma4y <- 0
for (i in 1:nrow(df_btc)){df_btc$ma4y[i] <- mean(df_btc$PriceUSD[ifelse(i < 1460, 0, (i-1460)):i], na.rm = TRUE)}

# Bitcoin Price Temperature (4-year moving average)
df_btc$sd4y <- 0
for (i in 1:nrow(df_btc)){df_btc$sd4y[i] <- sd(df_btc$PriceUSD[ifelse(i<1460,0,(i-1460)):i], na.rm = TRUE)}

df_btc$bpt <- 0
for (i in 1:nrow(df_btc)){df_btc$bpt[i] <- (df_btc$PriceUSD[i] - df_btc$ma4y[i]) / df_btc$sd4y[i]}

# BPT Bands
df_btc <- df_btc %>%
  mutate(
    ma4y_p1 = ma4y + sd4y,
    ma4y_p2 = ma4y + (2*sd4y),
    ma4y_p3 = ma4y + (3*sd4y),
    ma4y_p4 = ma4y + (4*sd4y),
    ma4y_p5 = ma4y + (5*sd4y),
    ma4y_p6 = ma4y + (6*sd4y),
    ma4y_p7 = ma4y + (7*sd4y),
    ma4y_p8 = ma4y + (8*sd4y),
    ma4y_p9 = ma4y + (9*sd4y),
    ma4y_p10 = ma4y + (10*sd4y),
    ma4y_p11 = ma4y + (11*sd4y),
    ma4y_p12 = ma4y + (12*sd4y),
    ma4y_p13 = ma4y + (13*sd4y),
    ma4y_m1 = ma4y - sd4y,
    ma4y_m2 = ma4y - (2*sd4y)
  )

temp <- df_btc %>%
  filter(ma4y_m1 > 0) %>%
  select(ma4y_m1)

df_btc <- df_btc %>%
  mutate(
    ma4y_m1l = ifelse(ma4y_m1 <= 0, min(temp$ma4y_m1), ma4y_m1),
    ma4y_m2l = ifelse(ma4y_m2 <= 0, min(temp$ma4y_m1), ma4y_m2)
  )

# Remove redundant variables:
remove(i, temp)

## 1 Bitcoin Log Price Chart ----
Sys.setlocale("LC_TIME", "C")
chart_logprice_ma4y <- paste(" $", as.character(round(df_btc$ma4y[max(which(!is.na(df_btc$ma4y)))], digits = 0)), sep="")
chart_logprice_curprice <- paste(" $", as.character(round(last(df_btc$PriceUSD), digits = 0)), sep="")
chart_logprice <- ggplot(df_btc, aes(x = date)) +
  geom_line(aes(y = ma4y, colour = "4y Moving\nAverage"), size = 0.25) +
  geom_vline(xintercept = as.Date(c("2012-11-28","2016-07-09","2020-05-09")), 
             linetype=2, size = 0.25, color = "white") +
  geom_vline(xintercept = df_btc$date[row_number_firstvalidprice+1460], 
             linetype=2, size = 0.25, color = "black", alpha = 0.5) +
  geom_line(aes(y = PriceUSD, colour = "Daily close"), size=0.5) +
  scale_y_log10(labels=scales::dollar_format(), 
                breaks = c(0,1,10,100,1000,10000,100000), 
                minor_breaks = c(seq(0, 1, 0.1), 
                                 seq(0, 10, 1), 
                                 seq(0, 100, 10), 
                                 seq(0, 1000, 100), 
                                 seq(0, 10000, 1000), 
                                 seq(0, 100000, 10000))) +
  scale_x_date(breaks = "1 year", 
               minor_breaks = NULL, 
               date_labels = "%Y", 
               limits = as.Date(c(df_btc$date[min(which(!is.na(df_btc$PriceUSD)))], last(df_btc$date)+30))) + 
  scale_colour_manual(name = "Legend:", values = c("black", "white")) +
  labs(title = "Bitcoin price in USD", 
       subtitle = paste("Logarithmic chart | Date = ",
                        as.character(format(Sys.Date(), "%a %b %d, %Y")), sep = ""), 
       x = "Time", y = "USD") +
  theme_dark() +
  annotate("text", x = as.Date("2012-11-28"), min(df_btc$PriceUSD, na.rm = T), label= "\nHalving 1", angle = 90, fontface = "plain", size = 3, color = "white", hjust = 0) +
  annotate("text", x = as.Date("2016-07-09"), min(df_btc$PriceUSD, na.rm = T), label= "\nHalving 2", angle = 90, fontface = "plain", size = 3, color = "white", hjust = 0) +
  annotate("text", x = as.Date("2020-05-09"), min(df_btc$PriceUSD, na.rm = T), label= "\nHalving 3", angle = 90, fontface = "plain", size = 3, color = "white", hjust = 0) +
  annotate("label", x = as.Date("2010-10-01"), y = 40000, size = 3,
           label = "Data: @coinmetrics\nChart: @dilutionproof") +
  annotate("text", x = df_btc$date[row_number_firstvalidprice+1460], y = min(df_btc$PriceUSD, na.rm = T), fontface = "plain", angle = 90, size = 3, alpha = 0.5, color = "black", hjust = 0,
           label= "Day 0-1459: MA based on to-date-available data\n") +
  annotate("text", x = df_btc$date[row_number_firstvalidprice+1460], y = min(df_btc$PriceUSD, na.rm = T), fontface = "plain", angle = 90, size = 3, alpha = 0.5, color = "black", hjust = 0,
           label= "\nDay 1460-now: MA based on 4-year time window") +
  annotate("text", x = last(df_btc$date), y = last(df_btc$ma4y), label = chart_logprice_ma4y, fontface = "plain", size = 3, color = "black", hjust = 0) +
  annotate("text", x = last(df_btc$date), y = last(df_btc$PriceUSD), label =  chart_logprice_curprice, fontface = "plain", size = 3, color = "white", hjust = 0)

ggsave(here::here(paste("Figures/", as.character(Sys.Date()), "_logprice.png", sep = "")), chart_logprice, height = 12.5 / 1.78, width = 12.5, dpi = 320, units = "in")

## 2 Bitcoin Price Temperature (BPT) ----
chart_bpt_bands_formula<- "Formula: btp[i] = (price[i] - mean(price[ifelse(i<1460,0,(i-1460)):i]) / sd(price[ifelse(i<1460,0,(i-1460)):i]"
chart_bpt_bands_date <- as.character(Sys.Date())
chart_bpt_lastz <- paste(" ", as.character(round(df_btc$bpt[max(which(!is.na(df_btc$bpt)))], digits = 2)), sep="")
chart_bpt <- ggplot(df_btc, aes(x = date)) +
  geom_vline(xintercept = as.Date(c("2012-11-28","2016-07-09","2020-05-09")), linetype=2, size = 0.25, color = "white") +
  geom_vline(xintercept = df_btc$date[row_number_firstvalidprice+1460], linetype=2, size = 0.25, color = "black", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype=1, size=0.25, color="#1600c3") +
  geom_hline(yintercept = 2, linetype=2, size=0.25, color="purple") +
  geom_hline(yintercept = 6, linetype=2, size=0.25, color="orange") +
  geom_hline(yintercept = 8, linetype=1, size=0.25, color="red") +
  geom_line(aes(y = bpt, colour = bpt), size=0.5) +
  scale_x_date(breaks = "1 year", minor_breaks = NULL, date_labels = "%Y", limits = as.Date(c(df_btc$date[min(which(!is.na(df_btc$PriceUSD)))], (last(df_btc$date) + 50)))) + 
  scale_y_continuous(breaks = seq(-10,25,1), minor_breaks = NULL) +
  scale_color_gradientn(colours = c("black", "blue", "green", "yellow", "orange", "red", "pink", "white"), name = "bpt", guide = FALSE)

build <- ggplot_build(chart_bpt)
df_hex <- build$data[[7]][, c("colour", "y")]
row_number_z2m <- which.min(abs(df_hex$y - -2))
row_number_z1m <- which.min(abs(df_hex$y - -1))
row_number_z0 <- which.min(abs(df_hex$y - 0))
row_number_z1 <- which.min(abs(df_hex$y - 1))
row_number_z2 <- which.min(abs(df_hex$y - 2))
row_number_z3 <- which.min(abs(df_hex$y - 3))
row_number_z4 <- which.min(abs(df_hex$y - 4))
row_number_z5 <- which.min(abs(df_hex$y - 5))
row_number_z6 <- which.min(abs(df_hex$y - 6))
row_number_z7 <- which.min(abs(df_hex$y - 7))
row_number_z8 <- which.min(abs(df_hex$y - 8))
row_number_z9 <- which.min(abs(df_hex$y - 9))
row_number_z10 <- which.min(abs(df_hex$y - 10))
row_number_z11 <- which.min(abs(df_hex$y - 11))
row_number_z12 <- which.min(abs(df_hex$y - 12))
last_bpt <- last(df_btc$bpt)
row_number_last <- which.min(abs(df_hex$y - last_bpt))

chart_bpt <- ggplot(df_btc, aes(x = date)) +
  geom_vline(xintercept = as.Date(c("2012-11-28","2016-07-09","2020-05-09")), linetype=2, size = 0.25, color = "white") +
  geom_vline(xintercept = df_btc$date[row_number_firstvalidprice+1460], linetype=2, size = 0.25, color = "black", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype=1, size=0.25, color=df_hex$colour[row_number_z0]) +
  geom_hline(yintercept = 2, linetype=2, size=0.25, color=df_hex$colour[row_number_z2]) +
  geom_hline(yintercept = 6, linetype=2, size=0.25, color=df_hex$colour[row_number_z6]) +
  geom_hline(yintercept = 8, linetype=1, size=0.25, color=df_hex$colour[row_number_z8]) +
  geom_line(aes(y = bpt, colour = bpt), size=0.5) +
  scale_x_date(breaks = "1 year", minor_breaks = NULL, date_labels = "%Y", 
               limits = as.Date(c(df_btc$date[min(which(!is.na(df_btc$PriceUSD)))], (last(df_btc$date))))) + 
  scale_y_continuous(breaks = seq(-10,25,1), minor_breaks = NULL) +
  scale_color_gradientn(colours = c("black", "blue", "green", "yellow", "orange", "red", "pink", "white"), name = "BPT", guide = FALSE) +
  labs(title = "Bitcoin Price Temperature (BPT)", 
       subtitle = paste("BPT = distance from 4-year moving average in # of standard deviations | Date = ",
                        as.character(format(Sys.Date(), "%a %b %d, %Y")), sep = ""), 
       x = "Time", y = "BPT") + 
  theme_dark() +
  annotate("text", x = as.Date("2012-11-28"), y = min(df_btc$bpt, na.rm = T), angle = 90, fontface = "plain", size = 3, color = "white", hjust = 0,
           label= "\nHalving 1", ) +
  annotate("text", x = as.Date("2016-07-09"), y = min(df_btc$bpt, na.rm = T), angle = 90, fontface = "plain", size = 3, color = "white", hjust = 0,
           label= "\nHalving 2", ) +
  annotate("text", x = as.Date("2020-05-09"), y = min(df_btc$bpt, na.rm = T), angle = 90, fontface = "plain", size = 3, color = "white", hjust = 0,
           label= "\nHalving 3", ) +
  annotate("text", x = df_btc$date[row_number_firstvalidprice+1460], y = max(df_btc$bpt, na.rm = T)+0.5, fontface = "plain", size = 3, alpha = 0.5, color = "black", hjust = 1,
           label= "Day 0-1459: BPT based on to-date-available data ") +
  annotate("text", x = df_btc$date[row_number_firstvalidprice+1460], y = max(df_btc$bpt, na.rm = T)+0.5, fontface = "plain", size = 3, alpha = 0.5, color = df_hex$colour[row_number_z0], hjust = 0,
           label= " Day 1460-now: BPT based on 4-year time window") +
  annotate("label", x = as.Date("2010-09-01"), y = max(df_btc$bpt, na.rm = T)+0.5, size = 3,
           label = "Data: @coinmetrics\nBPT: @dilutionproof") +
  annotate("label", x = as.Date("2019-08-01"), y = max(df_btc$bpt, na.rm = T)+0.5, parse = TRUE, fontface = "plain", size = 2,
           label = deparse(chart_bpt_bands_formula)) +
  annotate("text", x = last(df_btc$date), y = last(df_btc$bpt), parse = TRUE, fontface = "plain", size = 3, 
           color = df_hex$colour[max(which(!is.na(df_hex$y)))], hjust = 0,
           label = deparse(chart_bpt_lastz))

# Save to file
ggsave(here::here(paste("Figures/", as.character(Sys.Date()), "_bpt.png", sep = "")), chart_bpt, height = 12.5 / 1.78, width = 12.5, dpi = 320, units = "in")

## 3 BPT Bands (full) ----
chart_bpt_bands_formula<- "Formula: btp[i] = (price[i] - mean(price[ifelse(i<1460,0,(i-1460)):i]) / sd(price[ifelse(i<1460,0,(i-1460)):i]"
chart_bpt_z0price <- paste(" $", as.character(round(df_btc$ma4y[max(which(!is.na(df_btc$ma4y)))], digits = 0)), sep="")
chart_bpt_z2price <- paste(" $", as.character(round(df_btc$ma4y_p2[max(which(!is.na(df_btc$ma4y_p2)))], digits = 0)), sep="")
chart_bpt_curprice <- paste(" $", as.character(round(last(df_btc$PriceUSD), digits = 0)), sep="")
chart_bpt_z6price <- paste(" $", as.character(round(df_btc$ma4y_p6[max(which(!is.na(df_btc$ma4y_p6)))], digits = 0)), sep="")
chart_bpt_z8price <- paste(" $", as.character(round(df_btc$ma4y_p8[max(which(!is.na(df_btc$ma4y_p8)))], digits = 0)), sep="")

chart_bpt_bands <- ggplot(df_btc, aes(x = date)) +
  geom_line(aes(y = ma4y), size=0.25, color=df_hex$colour[row_number_z0]) +
  geom_line(aes(y = ma4y_p2), linetype = 2, size=0.25, color=df_hex$colour[row_number_z2]) +
  geom_line(aes(y = ma4y_p6), linetype = 2, size=0.25, color=df_hex$colour[row_number_z6]) +
  geom_line(aes(y = ma4y_p8), size=0.25, color=df_hex$colour[row_number_z8]) +
  geom_vline(xintercept = as.Date(c("2012-11-28","2016-07-09","2020-05-09")), linetype=2, size = 0.25, color = "white") +
  geom_vline(xintercept = df_btc$date[row_number_firstvalidprice+1460], linetype=2, size = 0.25, color = "black", alpha = 0.5) +
  geom_line(aes(y = PriceUSD, colour = bpt), size=0.5) +
  scale_y_log10(labels=scales::dollar_format(), 
                breaks = c(0,1,10,100,1000,10000,100000), 
                minor_breaks = c(seq(0, 1, 0.1), seq(0, 10, 1), seq(0, 100, 10), seq(0, 1000, 100), seq(0, 10000, 1000), seq(0, 100000, 10000))) +
  scale_x_date(breaks = "1 year", minor_breaks = NULL, date_labels = "%Y", limits = as.Date(c(df_btc$date[min(which(!is.na(df_btc$PriceUSD)))], last(df_btc$date)+30))) + 
  labs(title = "Bitcoin Price Temperature (BPT) Bands", 
       subtitle = paste("BPT = distance from 4-year moving average in # of standard deviations | Date = ",
                        as.character(format(Sys.Date(), "%a %b %d, %Y")), sep = ""), 
       x = "Time", y = "USD") +
  theme_dark() +
  scale_color_gradientn(colours = c("black", "blue", "green", "yellow", "orange", "red", "pink", "white"), 
                        breaks = c(0,2,6,8), labels = c(0,2,6,8), 
                        guide = guide_colorbar(title= "BPT\nBand:", barheight = 25)) +
  annotate("text", x = as.Date("2012-11-28"), y = min(df_btc$ma4y, na.rm = T), label= "\nHalving 1", angle = 90, fontface = "plain", size = 3, color = "white", hjust = 0) +
  annotate("text", x = as.Date("2016-07-09"), y = min(df_btc$ma4y, na.rm = T), label= "\nHalving 2", angle = 90, fontface = "plain", size = 3, color = "white", hjust = 0) +
  annotate("text", x = as.Date("2020-05-09"), y = min(df_btc$ma4y, na.rm = T)+0.1, label= "\nHalving 3", angle = 90, fontface = "plain", size = 3, color = "white") +
  annotate("text", x = df_btc$date[row_number_firstvalidprice+1460], y = min(df_btc$ma4y, na.rm = T), angle = 90, fontface = "plain", size = 3, alpha = 0.5, color = "black", hjust = 0,
           label= "Day 0-1459: BPT based on to-date-available data\n") +
  annotate("text", x = df_btc$date[row_number_firstvalidprice+1460], y = min(df_btc$ma4y, na.rm = T), angle = 90, fontface = "plain", size = 3, alpha = 0.5, color = df_hex$colour[row_number_z0], hjust = 0,
           label= "\nDay 1460-now: BPT based on 4-year time window") +
  annotate("label", x = as.Date("2010-10-01"), y = 40000, size = 3,
           label = "Data: @coinmetrics\nBPT: @dilutionproof") +
  annotate("label", x = last(df_btc$date)-365*1.5, min(df_btc$ma4y, na.rm = T), parse = TRUE, fontface = "plain", size = 2,
           label = deparse(chart_bpt_bands_formula)) +
  annotate("text", x = last(df_btc$date), y = last(df_btc$ma4y), label = chart_bpt_z0price, fontface = "plain", size = 3, color = df_hex$colour[row_number_z0], hjust = 0) +
  annotate("text", x = last(df_btc$date), y = last(df_btc$PriceUSD), label = chart_bpt_curprice, fontface = "plain", size = 3, color = df_hex$colour[row_number_last], hjust = 0) +
  annotate("text", x = last(df_btc$date), y = last(df_btc$ma4y_p8), label =  chart_bpt_z8price, fontface = "plain", size = 3, color = df_hex$colour[row_number_z8], hjust = 0)

# Save to file
ggsave(here::here(paste("Figures/", as.character(Sys.Date()), "_bpt-bands.png", sep = "")), chart_bpt_bands, height = 12.5 / 1.78, width = 12.5, dpi = 320, units = "in")

## 4 BPT Bands (same day) ----

# Variable for the previous date at the same point during the previous cycle:
temp_lastrow <- df_btc %>% slice_tail()
temp_lastrow_currcycle <- as.numeric(temp_lastrow$cycle) -1
temp_lastrow_prevcycle <- temp_lastrow_currcycle -1
temp_lastrow_curday <- as.numeric(last(df_btc$day))
temp_daterow <- df_btc %>% filter(cycle == temp_lastrow_prevcycle & day == temp_lastrow_curday)
temp_date <- temp_daterow$date
temp_ma4y <- temp_daterow$ma4y
temp_priceusd <- temp_daterow$PriceUSD
temp_prevcycle <- df_btc %>% filter(cycle == temp_lastrow_prevcycle)
temp_prevcycle_date1 <- first(temp_prevcycle$date)
remove(temp_lastrow, temp_lastrow_currcycle, temp_lastrow_prevcycle, temp_daterow, temp_prevcycle)

# Bitcoin Price Temperature (BPT) overlay - zoomed in at same day during previous cycle
chart_bpt_bands_sameday <- ggplot(df_btc, aes(x = date)) +
  geom_line(aes(y = ma4y), size=0.25, color=df_hex$colour[row_number_z0]) +
  geom_line(aes(y = ma4y_p2), linetype = 2, size=0.25, color=df_hex$colour[row_number_z2]) +
  geom_line(aes(y = ma4y_p6), linetype = 2, size=0.25, color=df_hex$colour[row_number_z6]) +
  geom_line(aes(y = ma4y_p8), size=0.25, color=df_hex$colour[row_number_z8]) +
  geom_vline(xintercept = as.Date(c("2012-11-28","2016-07-09","2020-05-09")), linetype=2, size = 0.25, color = "white") +
  geom_line(aes(y = PriceUSD, colour = bpt), size=0.5) +
  scale_y_log10(labels=scales::dollar_format(), 
                limits = c(ifelse(temp_ma4y < temp_priceusd, temp_ma4y, temp_priceusd), max(df_btc$PriceUSD)), 
                breaks = c(0,1,10,100,1000,10000,100000),
                minor_breaks= c(seq(0, 1000, 100), seq(0, 10000, 1000), seq(0, 100000, 10000))) +
  scale_x_date(breaks = "1 year", 
               minor_breaks = "1 month", 
               date_labels = "%Y", 
               limits = as.Date(c(temp_date, last(df_btc$date)+15))) + 
  labs(title = paste("Bitcoin Price Temperature (BPT) Bands since same post-halving day (", 
                     as.character(temp_lastrow_curday),
                     ") during previous halving cycle", sep = ""), 
       subtitle = paste("BPT = distance from 4-year moving average in # of standard deviations | Date = ",
                        as.character(format(Sys.Date(), "%a %b %d, %Y")), sep = ""), 
       x = "Time", y = "USD") +
  theme_dark() +
  scale_color_gradientn(colours = c("black", "blue", "green", "yellow", "orange", "red", "pink", "white"), 
                        breaks = c(0,2,6,8), labels = c(0,2,6,8), 
                        guide = guide_colorbar(title= "BPT\nBand:", barheight = 25)) +
  annotate("text", x = as.Date("2012-11-28"), y = ifelse(temp_ma4y < temp_priceusd, temp_ma4y, temp_priceusd),
           label= "\nHalving 1", angle = 90, fontface = "plain", size = 3, color = "white") +
  annotate("text", x = as.Date("2016-07-09"), y = ifelse(temp_ma4y < temp_priceusd, temp_ma4y, temp_priceusd),
           label= "\nHalving 2", angle = 90, fontface = "plain", size = 3, color = "white") +
  annotate("text", x = as.Date("2020-05-09"), y = ifelse(temp_ma4y < temp_priceusd, temp_ma4y, temp_priceusd)+175,
           label= "\nHalving 3", angle = 90, fontface = "plain", size = 3, color = "white") +
  annotate("label", x = temp_date+30, y = last(df_btc$ma4y_p8), 
           label = "Data: @coinmetrics\nBPT: @dilutionproof", size = 3) +
  annotate("label", x = Sys.Date()-200, y = ifelse(temp_ma4y < temp_priceusd, temp_ma4y, temp_priceusd), 
           label = deparse(chart_bpt_bands_formula), 
           parse = TRUE, fontface = "plain", size = 2) +
  annotate("text", x = last(df_btc$date), y = last(df_btc$ma4y), label = deparse(chart_bpt_z0price), parse = TRUE, fontface = "plain", size = 3, color = df_hex$colour[row_number_z0], hjust = 0) +
  annotate("text", x = last(df_btc$date), y = last(df_btc$ma4y_p2), label = deparse(chart_bpt_z2price), parse = TRUE, fontface = "plain", size = 3, color = df_hex$colour[row_number_z2], hjust = 0) +
  annotate("text", x = last(df_btc$date), y = last(df_btc$ma4y_p6), label = deparse(chart_bpt_z6price), parse = TRUE, fontface = "plain", size = 3, color = df_hex$colour[row_number_z6], hjust = 0) +
  annotate("text", x = last(df_btc$date), y = last(df_btc$PriceUSD), label = deparse(chart_bpt_curprice), parse = TRUE, fontface = "plain", size = 3, color = df_hex$colour[row_number_last], hjust = 0) +
  annotate("text", x = last(df_btc$date), y = last(df_btc$ma4y_p8), label =  deparse(chart_bpt_z8price), parse = TRUE, fontface = "plain", size = 3, color = df_hex$colour[row_number_z8], hjust = 0)

# Save to file
ggsave(here::here(paste("Figures/", as.character(Sys.Date()), "_bpt-bands-sameday.png", sep = "")), chart_bpt_bands_sameday, height = 12.5 / 1.78, width = 12.5, dpi = 320, units = "in")

# Also patch the previous two images together and safe them as a file:
library(patchwork)
chart_bpt_bands_2x <- chart_bpt_bands + chart_bpt_bands_sameday
ggsave(here::here(paste("Figures/", as.character(Sys.Date()), "_bpt-bands-2x.png", sep = "")), chart_bpt_bands_2x, height = 25 / 1.78 / 2, width = 25, dpi = 320, units = "in")

## 5 BPT per cycle ----
chart_bpt_percycle <- ggplot(df_btc, aes(x = day, y = bpt)) +
  geom_hline(yintercept = 0, linetype=1, size=0.5, color="black") +
  geom_line(aes(colour = cycle), size=0.25) +
  scale_x_continuous(breaks = seq(0,1460,365), minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(-10,25,1), minor_breaks = NULL) +
  scale_color_manual(name = "Cycle", 
                     values = c("blue", "green", "yellow", "orange", "red"), 
                     labels = seq(0,3,1)) +
  labs(title = "Bitcoin Price Temperature (BPT) per halving cycle", 
       subtitle = paste("BPT = distance from 4-year moving average in # of standard deviations | Date = ",
                        as.character(format(Sys.Date(), "%a %b %d, %Y")), sep = ""), 
       x= "Days since halving",  
       y = "BPT") + 
  theme_dark() +
  annotate("label", x = 100, y = -1.5, size = 3,
           label = "Data: @coinmetrics\nBPT: @dilutionproof") +
  annotate("label", x = 1050, y = 12, parse = TRUE, fontface = "plain", size = 3,
           label = deparse(chart_bpt_bands_formula))

ggsave(here::here(paste("Figures/", as.character(Sys.Date()), "_bpt-percycle.png", sep = "")), chart_bpt_percycle, height = 12.5 / 1.78, width = 12.5, dpi = 320, units = "in")

## 6 BPT cycle correlations ----
df_cor <- df_btc %>%
  select(cycle, day, PriceUSD, bpt) %>%
  pivot_wider(names_from = cycle, values_from = c(PriceUSD, bpt))

temp <- NULL
df_p2p1 <- NULL
for (i in 3:nrow(df_cor)) {
  temp <- cor.test(df_cor$PriceUSD_2[0:i], df_cor$PriceUSD_1[0:i])
  df_p2p1$p2p1_r[i] <- temp[["estimate"]]
}
df_p2p1 <- bind_rows(df_p2p1)
df_p2p1$day <- 1:nrow(df_p2p1)
df_cor <- full_join(df_cor, df_p2p1, by = "day")
df_cor <- df_cor %>%
  mutate(p2p1_r = ifelse(!is.na(PriceUSD_2), p2p1_r, NA),
         p2p1_r = ifelse(!is.na(PriceUSD_1), p2p1_r, NA))

temp <- NULL
df_p3p1 <- NULL
for (i in 3:nrow(df_cor)) {
  temp <- cor.test(df_cor$PriceUSD_3[0:i], df_cor$PriceUSD_1[0:i])
  df_p3p1$p3p1_r[i] <- temp[["estimate"]]
}
df_p3p1 <- bind_rows(df_p3p1)
df_p3p1$day <- 1:nrow(df_p3p1)
df_cor <- full_join(df_cor, df_p3p1, by = "day")

temp <- NULL
df_p3p2 <- NULL
for (i in 3:nrow(df_cor)) {
  temp <- cor.test(df_cor$PriceUSD_3[0:i], df_cor$PriceUSD_2[0:i])
  df_p3p2$p3p2_r[i] <- temp[["estimate"]]
}
df_p3p2 <- bind_rows(df_p3p2)
df_p3p2$day <- 1:nrow(df_p3p2)
df_cor <- full_join(df_cor, df_p3p2, by = "day")
df_cor <- df_cor %>%
  mutate(p3p1_r = ifelse(!is.na(PriceUSD_3), p3p1_r, NA),
         p3p2_r = ifelse(!is.na(PriceUSD_3), p3p2_r, NA))

temp <- NULL
df_bpt2bpt1 <- NULL
for (i in 3:nrow(df_cor)) {
  temp <- cor.test(df_cor$bpt_2[0:i], df_cor$bpt_1[0:i])
  df_bpt2bpt1$bpt2bpt1_r[i] <- temp[["estimate"]]
}
df_bpt2bpt1 <- bind_rows(df_bpt2bpt1)
df_bpt2bpt1$day <- 1:nrow(df_bpt2bpt1)
df_cor <- full_join(df_cor, df_bpt2bpt1, by = "day")
df_cor <- df_cor %>%
  mutate(bpt2bpt1_r = ifelse(!is.na(bpt_2), bpt2bpt1_r, NA),
         bpt2bpt1_r = ifelse(!is.na(bpt_1), bpt2bpt1_r, NA))

temp <- NULL
df_bpt3bpt1 <- NULL
for (i in 3:nrow(df_cor)) {
  temp <- cor.test(df_cor$bpt_3[0:i], df_cor$bpt_1[0:i])
  df_bpt3bpt1$bpt3bpt1_r[i] <- temp[["estimate"]]
  df_bpt3bpt1$bpt3bpt1_p[i] <- temp[["p.value"]]
}
df_bpt3bpt1 <- bind_rows(df_bpt3bpt1)
df_bpt3bpt1$day <- 1:nrow(df_bpt3bpt1)
df_cor <- full_join(df_cor, df_bpt3bpt1, by = "day")

temp <- NULL
df_bpt3bpt2 <- NULL
for (i in 3:nrow(df_cor)) {
  temp <- cor.test(df_cor$bpt_3[0:i], df_cor$bpt_2[0:i])
  df_bpt3bpt2$bpt3bpt2_r[i] <- temp[["estimate"]]
  df_bpt3bpt2$bpt3bpt2_p[i] <- temp[["p.value"]]
}
df_bpt3bpt2 <- bind_rows(df_bpt3bpt2)
df_bpt3bpt2$day <- 1:nrow(df_bpt3bpt2)
df_cor <- full_join(df_cor, df_bpt3bpt2, by = "day")
df_cor <- df_cor %>%
  mutate(bpt3bpt1_r = ifelse(!is.na(PriceUSD_3), bpt3bpt1_r, NA),
         bpt3bpt2_r = ifelse(!is.na(PriceUSD_3), bpt3bpt2_r, NA),
         bpt3bpt1_p = ifelse(!is.na(PriceUSD_3), bpt3bpt1_p, NA),
         bpt3bpt2_p = ifelse(!is.na(PriceUSD_3), bpt3bpt2_p, NA))

chart_cycle3 <- ggplot(df_cor, aes(x = day)) +
  geom_line(aes(y = p2p1_r, colour = "PriceUSD")) + 
  geom_line(aes(y = bpt2bpt1_r, colour = "bpt")) + 
  scale_x_continuous(limits = c(0, df_cor$day[max(which(!is.na(df_cor$PriceUSD_2)))])) +
  scale_y_continuous(limits = c(-1,1)) +
  scale_color_discrete(name = "Variable") +
  labs(title = "Correlation between halving cycle 2 and halving cycle 3", x = "Days after halving", y = "Correlation (r)") + 
  theme_linedraw()

ggsave(here::here(paste("Figures/", as.character(Sys.Date()), "_cor-2v3-pricevbpt.png", sep = "")), chart_cycle3, height = 12.5 / 1.78, width = 12.5, dpi = 320, units = "in")

chart_cycle4_p <- ggplot(df_cor, aes(x = day)) +
  geom_line(aes(y = p3p1_r, colour = "Cycle 4 vs\ncycle 2")) + 
  geom_line(aes(y = p3p2_r, colour = "Cycle 4 vs\ncycle 3")) + 
  scale_x_continuous(limits = c(0, df_cor$day[max(which(!is.na(df_cor$PriceUSD_3)))])) +
  scale_color_discrete(name = "Comparison") +
  labs(title = "Correlation between current halving cycle and previous halving cycles", subtitle = "Variable: Bitcoin PriceUSD (USD)", x = "Days after halving", y = "Correlation (r)") + 
  theme_linedraw()

ggsave(here::here(paste("Figures/", as.character(Sys.Date()), "_cor_price_4v23.png", sep = "")), chart_cycle4_p, height = 12.5 / 1.78, width = 12.5, dpi = 320, units = "in")

chart_cycle4_bpt_lastr <- paste("r=", as.character(round(df_cor$bpt3bpt2_r[max(which(!is.na(df_cor$bpt3bpt2_r)))], digits = 2)), "\n", sep="")
chart_cycle4_bpt <- ggplot(df_cor, aes(x = day)) +
  geom_line(aes(y = bpt3bpt2_r, colour = bpt3bpt2_p)) +
  scale_x_continuous(limits = c(0, df_cor$day[max(which(!is.na(df_cor$bpt_3)))]), breaks = seq(0, 1500, 25), minor_breaks = NULL) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.25), minor_breaks = NULL) +
  #  scale_color_discrete(name = "Comparison") +
  scale_color_gradientn(name = "p-value", colors = c("green", "red", "red", "red", "red", "red", "red"), breaks = c(0, 0.05, 0.5)) +
  labs(title = "Current (2020~2024) versus previous (2016-2020) halving cycle correlation", subtitle = "Variable: Bitcoin PriceUSD Temperature (BPT)", x = "Days after halving", y = "Correlation (r)") + 
  annotate("text", x = df_cor$day[max(which(!is.na(df_cor$bpt_3)))], y = df_cor$bpt3bpt2_r[max(which(!is.na(df_cor$bpt_3)))], label= deparse(chart_cycle4_bpt_lastr), parse = TRUE, fontface = "plain", size = 3, color = "white") +
  theme_dark()

build <- ggplot_build(chart_cycle4_bpt)
df_hex <- build$data[[1]][, c("colour", "y")]

chart_cycle4_bpt <- ggplot(df_cor, aes(x = day)) +
  geom_line(aes(y = bpt3bpt2_r, colour = bpt3bpt2_p)) +
  scale_x_continuous(limits = c(0, df_cor$day[max(which(!is.na(df_cor$bpt_3)))]), breaks = seq(0, 1500, 25), minor_breaks = NULL) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.25), minor_breaks = NULL) +
  scale_color_gradientn(name = "p-value", colors = c("green", "red", "red", "red", "red", "red", "red"), breaks = c(0, 0.05, 0.5)) +
  labs(title = "Current (2020~2024) versus previous (2016-2020) halving cycle correlation", 
       subtitle = paste("Variable = Bitcoin Price Temperature (BPT) | Date = ",
                        as.character(format(Sys.Date(), "%a %b %d, %Y")), sep = ""), 
       x = "Days after halving", y = "Correlation (r)") + 
  annotate("text", x = df_cor$day[max(which(!is.na(df_cor$bpt_3)))], y = df_cor$bpt3bpt2_r[max(which(!is.na(df_cor$bpt_3)))], 
           label= deparse(chart_cycle4_bpt_lastr), 
           parse = TRUE, fontface = "plain", size = 3, color = df_hex$colour[max(which(!is.na(df_hex$y)))]) +
  annotate("label", x = df_cor$day[max(which(!is.na(df_cor$bpt_3)))]-10, y = -0.9, size = 3,
           label = "Data: @coinmetrics\nBPT: @dilutionproof") +
  theme_dark()

ggsave(here::here(paste("Figures/", as.character(Sys.Date()), "_cor-2v3.png", sep = "")), chart_cycle4_bpt, height = 12.5 / 1.78, width = 12.5, dpi = 320, units = "in")


