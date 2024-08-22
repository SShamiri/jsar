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

## ######################### worldprod data
library(sfaR)
data(worldprod)

worldprod <- worldprod |>
  as_tibble() |>
  rename(country_name = country, country_id = code, year = yr, gdp = y,capital = k, log_gdp = ly,
         labour = l, h_capital = h, log_capital = lk, log_labour = ll, log_h = lh, initstat = initStat)

coldesc <- rep("", ncol(worldprod))
names(coldesc) <- names(worldprod)
dput(coldesc)

# Copy and paste the output into your script and assign this to an object called vars
vars <- c(country_name = "Country name",
          country_id = "Country identification",
          year = "Year identification",
          gdp = "GDP in 1987 U.S.dollars",
          capital = "Physical capital stock in 1987 U.S. dollars",
          labour = "Labour (number of individuals in the workforce between the age of15 and 64)",
          h_capital = "Human capital adjusted labour",
          log_gdp = "Log GDP",
          log_capital = "Log capital",
          log_labour = "Log labour",
          log_h = "Log human capital",
          initstat = "Log of the initial capital to labor ratio of each country, log_capital - labour, measured at the beginning of the sample period"
          )
glue::glue("#'   \\item{[colname]}{[coldesc]}",
           colname = names(vars),
           coldesc = vars,
           .open = "[",
           .close = "]")

usethis::use_r("worldprod") # once modified run doc
devtools::document()
usethis::use_data(worldprod, overwrite = TRUE)

