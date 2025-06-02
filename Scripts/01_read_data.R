library(tidyverse)

a <- list.files("Data/nhsdigital/")
a <- tibble(filenames = a)
lng <- a %>% 
  filter(filenames == "MDS_ABSENCE_CSV_REASON_longterm_2022may_2015jan.csv")
shrt <- a %>% 
  filter(!filenames == "MDS_ABSENCE_CSV_REASON_longterm_2022may_2015jan.csv") %>% 
  separate(filenames, into = c("descr", "mnth_file"), sep = "\\,", remove = FALSE) %>% 
  separate(mnth_file, into = c("my", "file"), sep = "\\.") %>% 
  mutate(across(c(my, file), str_trim))  %>% 
  separate(my, into = c("mnth", "yr"), sep = " ") %>% 
  mutate(across(c(mnth, yr), str_trim),
         descr = str_to_lower(descr))
lkp <- 1:12
names(lkp) <- month.name
shrt <- shrt %>% 
  mutate(mnth_n = lkp[mnth])

shrt$csv <- map(shrt$filenames, ~ read_csv(paste0("Data/nhsdigital/", .x)) )
shrt <- shrt %>% 
  unnest(csv)
names(shrt) <- str_to_lower(names(shrt))
shrt <- shrt %>% 
  select(date:fte_days_available)
lng <- read_csv(paste0("Data/nhsdigital/", lng$filenames))
lng <- lng %>% 
  spread(Type, `FTE days`)
lng <- lng %>% 
  rename(date = Month, staff_group = `Staff group`, reason = Reason, 
         fte_days_lost= `FTE days lost`, fte_days_available = `FTE days available`)
names(lng)
lng <- lng %>% 
  mutate(date = lubridate::ymd(date))
shrt <- shrt %>% 
  mutate(date = lubridate::dmy(date))
tot <- bind_rows(old = lng, new = shrt, .id = "data_type")
tot <- tot %>% 
  mutate(staff_group = str_to_lower(staff_group),
         reason = str_to_lower(reason))

write_csv(tot, "Scratch_data/alldata.csv")

