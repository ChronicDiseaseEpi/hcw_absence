# 03_retention
filename <- "Data/NHS Workforce Statistics, December 2024 Reasons for Leaving.csv"
a <- read_csv(filename, skip = 6, col_names = FALSE)
a <- a %>%
  filter(!X2 == "")
qs <- read_csv(filename,  skip = 5, col_names = FALSE, n_max = 1) %>% 
  as.character()
yr <- read_csv(filename,  skip = 4, col_names = FALSE, n_max = 1) %>% 
  as.character()
yr <- if_else(yr == "NA", NA_character_, yr)
yr_lkp <- tibble(yr = yr)
yr_lkp <- yr_lkp %>% 
  mutate(grp = cumsum(!is.na(yr))) %>% 
  group_by(grp) %>% 
  mutate(yr = yr[1]) %>% 
  ungroup()
yr_lkp <- yr_lkp$yr
rm(yr)

a <- a %>% 
  gather("var", "value", -X1)
a <- a %>% 
  filter(!is.na(value))
a <- a %>% 
  mutate(var = str_sub(var, 2) %>% as.integer())
a <- a %>% 
  mutate(q = qs[var],
         yr = yr_lkp[var])
a <- a %>% 
  filter(!value %in% c("-", "..")) %>% 
  mutate(value = str_remove_all(value, "\\,") %>% as.integer())
a <- a %>% 
  rename(cause = X1)
# qlkp <- c(0, 0.25, 0.5, 0.75)
# names(qlkp) <- paste0("Q", seq_along(qlkp))
# a <- a %>% 
#   mutate(yr_num = yr + qlkp[q])
a <- a %>% 
  separate(yr, into = c("start_yr", "end_yr_suffix"), sep = "-", convert = TRUE) %>%
  mutate(
    start_yr       = as.integer(start_yr),
    end_yr         = as.integer(paste0("20", end_yr_suffix))
  ) %>%
  # 2. Pick which calendar‐year each quarter lives in:
  #    - Q1–Q3 → start_yr
  #    - Q4     → end_yr
  mutate(
    cal_year = case_when(
      q == "Q4" ~ end_yr,
      TRUE      ~ start_yr
    ),
    cal_month = case_when(
      q == "Q1" ~ 4L,   # April
      q == "Q2" ~ 7L,   # July
      q == "Q3" ~ 10L,  # October
      q == "Q4" ~ 1L    # January
    ),
    quarter_start = make_date(year = cal_year, month = cal_month, day = 1)
  ) %>% 
  select(cause, quarter_start, value, cal_year)
a <- a %>% 
  group_by(cause, cal_year) %>% 
  summarise(value = sum(value)) %>% 
  ungroup()
lkp <- c(
  "Total" = "Total",
  "Bank Staff not fulfilled minimum work requirement" = "Other/Unknown",
  "Death in Service" = "Death",
  "Dismissal - Capability" = "Dismissal",
  "Dismissal - Conduct" = "Dismissal",
  "Dismissal - Some Other Substantial Reason" = "Dismissal",
  "Dismissal - Statutory Reason" = "Dismissal",
  "Employee Transfer" = "Transfer",
  "End of Fixed Term Contract" = "End of Contract",
  "End of Fixed Term Contract - Completion of Training Scheme" = "End of Contract",
  "End of Fixed Term Contract - End of Work Requirement" = "End of Contract",
  "End of Fixed Term Contract - External Rotation" = "End of Contract",
  "End of Fixed Term Contract - Other" = "End of Contract",
  "Flexi Retirement" = "Retirement / Pension",
  "Initial Pension Ended" = "Retirement / Pension",
  "Merged Organisation - Duplicate Record" = "Other/Unknown",
  "Mutually Agreed Resignation - Local Scheme with Repayment" = "Resignation",
  "Mutually Agreed Resignation - Local Scheme without Repayment" = "Resignation",
  "Mutually Agreed Resignation - National Scheme with Repayment" = "Resignation",
  "Not Set in Legacy at Migration" = "Other/Unknown",
  "Pregnancy" = "Other/Unknown",
  "Redundancy - Compulsory" = "Redundancy",
  "Redundancy - Voluntary" = "Redundancy",
  "Retirement - Ill Health" = "Retirement / Pension",
  "Retirement Age" = "Retirement / Pension",
  "Voluntary Early Retirement - no Actuarial Reduction" = "Retirement / Pension",
  "Voluntary Early Retirement - with Actuarial Reduction" = "Retirement / Pension",
  "Voluntary Resignation - Adult Dependants" = "Resignation",
  "Voluntary Resignation - Child Dependants" = "Resignation",
  "Voluntary Resignation - Health" = "Resignation",
  "Voluntary Resignation - Incompatible Working Relationships" = "Resignation",
  "Voluntary Resignation - Lack of Opportunities" = "Resignation",
  "Voluntary Resignation - Other/Not Known" = "Resignation",
  "Voluntary Resignation - Pay and Reward Related" = "Resignation",
  "Voluntary Resignation - Promotion" = "Resignation",
  "Voluntary Resignation - Relocation" = "Resignation",
  "Voluntary Resignation - To undertake further education or training" = "Resignation",
  "Voluntary Resignation - Work Life Balance" = "Resignation",
  "Unknown" = "Other/Unknown",
  "Has Not Worked" = "Has Not Worked"
)
a <- a %>% 
  mutate(grp = lkp[cause])
a_nst <- a %>% 
  nest(.by = grp)
a_nst$data <- map(a_nst$data, ~ {
  .x %>% 
    inner_join(
      a_nst$data[[which(a_nst$grp == "Total")]] %>% 
    select(cal_year, total = value))
})
a_nst$plt <- map2(a_nst$grp, a_nst$data, ~ {
  ggplot(.y %>% filter(cal_year < 2025), aes(x = cal_year,
                 y = value/total)) +
    geom_point() +
    geom_line() +
    facet_wrap(~cause, scales = "free_y") +
    ggtitle(.x)
})

## specific health causes
lkp <- c(
  # "Total" = "Total",
  "Death in Service" = "Death",
  "Retirement - Ill Health" = "Retire - ill health",
  "Retirement Age" = "Retire - age",
  "Voluntary Resignation - Adult Dependants" = "Resign - adult depend.",
  "Voluntary Resignation - Child Dependants" = "Resign - child depend.",
  "Voluntary Resignation - Health" = "Resign - health"
)
health <- a %>% 
  filter(cause %in% names(lkp)) %>% 
  mutate(cause = lkp[cause],
         grp = case_when(
           cause == "Death" ~ "Death",
           cause == "Retirement - age" ~ "Age",
           TRUE ~ "Other"
         ))
health <- health %>% 
  inner_join(a_nst$data[[which(a_nst$grp == "Total")]] %>% 
               select(cal_year, total = value))
health <- health %>% 
  mutate(cause = str_replace(cause,
                             " \\- ",
                             "\n"))
plt_health <- ggplot(health, aes(x = cal_year,
                                 y = 100*value/total
                             )) +
  geom_point() +
  geom_line() +
  facet_wrap(~ cause, scales = "free_y") +
  scale_x_continuous("Calendar year",
    breaks = seq(2000, 2024, 4)) +
  scale_y_continuous("Reason for leaving / leavers (%)")
plt_health
png("leavers.png", res = 150, height = 4, width = 6, unit = "in")
plt_health
dev.off()
