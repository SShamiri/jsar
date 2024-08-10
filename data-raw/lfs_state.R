## code to prepare `lfs_sa4`
dat <- readxl::read_excel("data-raw/EQ08.xlsx", sheet = "Sheet1")
lkup <- readr::read_csv("data-raw/lookup_anzsco.csv") %>%
  filter(level ==1 & nfd == 0) %>%
  select(anzsco1_code = anzsco_code, anzsco1_name = anzsco_name_2016 )

lfs_state <- dat %>%
  mutate(
    date = as.Date(date),
    anzsco4_code = parse_number(occ),
    anzsco4_name = trimws(str_replace_all(occ, "[:digit:]", "")),
    anzsco1_code = as.numeric(substr(as.character(anzsco4_code),1,1))
  ) %>%
  select(-occ) %>%
  left_join(lkup ,by = join_by(anzsco1_code)) %>%
  group_by(state, anzsco1_code, anzsco1_name, date) %>%
  summarise(emp = sum(emp), .groups = 'drop') %>%
  mutate(
    state_name = case_when(
      state == 'Australian Capital Territory' ~ "ACT",
      state == 'New South Wales' ~ "NSW",
      state == 'Northern Territory' ~ "NT",
      state == 'Queensland' ~ "QLD",
      state == 'South Australia' ~ "SA",
      state == 'Tasmania' ~ "TAS",
      state == 'Victoria' ~ "VIC",
      state == 'Western Australia' ~ "WA"
    )
  ) %>%
  relocate(state_name) %>%
  select(-state)


usethis::use_data(lfs_state, overwrite = TRUE)
