# 02_make_plots

library(tidyverse)
x <- read_csv("Scratch_data/alldata.csv")

trnd <- x %>% 
  filter(!is.na(fte_days_available), staff_group == "all staff groups",
         reason %in% c("ALL REASONS", "All reasons"))
trnd <- trnd %>% 
  arrange(date)
trnd <- trnd %>% 
  mutate(daydiff = date - lag(date))
trnd_plt <- ggplot(trnd, aes(x = date, y = fte_days_lost/1000000)) +
  geom_point() +
  scale_y_continuous("Million days lost") +
  scale_x_date("Date") +
  geom_line()
trnd_plt
trnd <- trnd %>% 
  mutate(days = as.integer(date - min(date)))
trnd <- trnd %>% 
  mutate(p = 100*fte_days_lost/fte_days_available)
pct_ts <- ts(trnd$p,
             start = c(lubridate::year(trnd$date),
                       1),
             frequency = 12)
stl_fit <- stl(pct_ts, s.window = "periodic")
plot(stl_fit)
trend_ts <- stl_fit$time.series[, "trend"]
trnd <- trnd %>% 
  mutate(trend_ts = trend_ts)

trnd_plt2 <- ggplot(trnd, aes(x = date, y = p)) +
  geom_point() +
  scale_y_continuous("Days lost / days available (%)") +
  scale_x_date("Date") +
  geom_line(alpha = 0.2) +
  geom_line(mapping= aes(y = trend_ts), col = "blue", alpha = 0.5)
trnd_plt2
plts <- cowplot::plot_grid(trnd_plt, trnd_plt2, ncol =2)
png("trends.png", res = 150, height = 4, width = 5, unit = "in")
# plts
trnd_plt2
dev.off()


## Causes; go from 2019 as this is when the other non-standard causes disappear
causes <- x %>% 
  filter(!reason %in% c("All reasons", "ALL REASONS"), staff_group == "all staff groups",
         date >= as.Date("2019-01-01"))
causes <- causes %>% 
  mutate(yr = lubridate::year(date))
causes_smry <- causes %>% 
  group_by(yr) %>% 
  mutate(n = sum(fte_days_lost)) %>% 
  ungroup() %>% 
  group_by(yr, n, reason) %>% 
  summarise(x = sum(fte_days_lost )) %>% 
  ungroup() %>% 
  mutate(p = x/n)

## top ten not other or unspecified
causes_smry_top10 <- causes %>% 
  mutate(n = sum(fte_days_lost)) %>% 
  group_by(n, reason) %>% 
  summarise(x = sum(fte_days_lost )) %>% 
  ungroup() %>% 
  mutate(p = x/n) %>% 
  arrange(desc(p)) %>% 
  filter(!reason  %in% c("S98 Other known causes - not elsewhere classified",
                             "S99 Unknown causes / Not specified")) %>% 
  filter(p >= 0.02)
  # slice(1:10)
lvls <- c(causes_smry_top10$reason, "Other")
lbls <- str_remove(lvls, "^S[0-9]{1,2}") %>% str_trim()
causes_smry <- causes_smry %>% 
  mutate(reason_hrm = if_else(
    reason %in% causes_smry_top10$reason, reason, "Other"),
    reason_hrm = factor(reason_hrm, 
                        levels = lvls,
                        labels = lbls))
causes_smry <- causes_smry %>% 
  mutate(myalpha = if_else(reason_hrm == "Other", 1, 1))
plot_reason <- ggplot(causes_smry, aes(
    x    = factor(yr),     # treat year as discrete categories
    y    = 100*p,              # percent‐points (0–100)
    fill = factor(reason_hrm),
    alpha = myalpha
  )) +
  geom_col(position = "fill", color = "white", linewidth = 0.2) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0))
  ) +
  scale_alpha_identity() +
  labs(
    x    = "Year",
    y    = "Proportion",
    fill = "Reason"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10)
  )
png("scarf_plots.png", res = 150, height = 4, width = 8, unit = "in")
plot_reason
dev.off()
