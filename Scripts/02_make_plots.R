# 02_make_plots

library(tidyverse)
x <- read_csv("Scratch_data/alldata.csv")

trnd <- x %>% 
  filter(!is.na(fte_days_available), staff_group == "all staff groups",
         reason == "all reasons")
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
mdl <- mgcv::gam(cbind(round(fte_days_available), round(fte_days_available- fte_days_lost)) ~ s(days), 
           family = "binomial", data = trnd)
trnd_aug <- broom::augment(mdl) %>% 
  rename(est = .fitted)
trnd_plt2 <- ggplot(trnd, aes(x = date, y = 100*fte_days_lost/fte_days_available)) +
  geom_point() +
  scale_y_continuous("Days lost/Days available (%)") +
  scale_x_date("Date") +
  geom_line()
trnd_plt2
plts <- cowplot::plot_grid(trnd_plt, trnd_plt2, ncol =2)
png("trends.png", res = 150, height = 4, width = 8, unit = "in")
plts
dev.off()


## Causes
causes <- x %>% 
  filter(!reason == "all reasons", staff_group == "all staff groups")
lkp <- read_csv("Data/absence_reason_mapping.csv")
causes %>% 
  anti_join(lkp %>% rename(reason = original_term))
lkp_vct <- lkp$mapped_term
names(lkp_vct) <- lkp$original_term
causes <- causes %>% 
  mutate(reason_hrm = lkp_vct[reason])

causes_smry <- causes %>% 
  mutate(yr = lubridate::year(date)) %>% 
  group_by(data_type, yr) %>% 
  mutate(n = sum(fte_days_lost)) %>% 
  ungroup() %>% 
  group_by(data_type, yr, n, reason_hrm) %>% 
  summarise(x = sum(fte_days_lost )) %>% 
  ungroup() %>% 
  mutate(p = x/n)

## top ten not other or unspecified
causes_smry_top10 <- causes %>% 
  mutate(n = sum(fte_days_lost)) %>% 
  ungroup() %>% 
  group_by(n, reason_hrm) %>% 
  summarise(x = sum(fte_days_lost )) %>% 
  ungroup() %>% 
  mutate(p = x/n) %>% 
  arrange(desc(p)) %>% 
  filter(!reason_hrm  %in% c("s98 other known causes - not elsewhere classified",
                             "s99 unknown causes / not specified")) %>% 
  slice(1:10)
causes_smry <- causes_smry %>% 
  mutate(reason_hrm2 = if_else(
    reason_hrm %in% causes_smry_top10$reason_hrm, reason_hrm, "Other"
  ))

  
plot_reason <- ggplot(causes_smry, aes(
    x    = factor(yr),     # treat year as discrete categories
    y    = 100*p,              # percent‐points (0–100)
    fill = factor(reason_hrm2)
  )) +
  geom_col(position = "fill", color = "white", size = 0.2) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0))
  ) +
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
