#| label: useful descriptive 

# For yaml
# include-in-header:
#   - text: |
#   \usepackage{cancel}


# count unique individuals
skimr::n_unique(dat$id)

# another way to count: doesn't require skimr
length(unique(dat$id))

# count participants by wave
dat |> 
  dplyr::filter(year_measured == 1) |>
  droplevels() |>
  dplyr::group_by(wave) |> 
  dplyr::count(wave)

# sampling by years
dat |> 
  dplyr::filter(year_measured == 1) |>
  droplevels() |>
  dplyr::count(sample_origin_year)


# more description 

# how many in 2018
dat |>
  filter(wave == 2018 & year_measured==1) |>
  select(hours_work, gender3, id, wave) |>
  drop_na() |>
  summarise(count_distinct = n_distinct(id))


# count unique individuals
skimr::n_unique(dat$id)

# another way to count: doesn't require skimr
length(unique(dat$id))

# count participants by wave
dat |> 
  dplyr::filter(year_measured == 1) |>
  droplevels() |>
  dplyr::group_by(wave) |> 
  dplyr::count(wave)

# sampling by years
dat |> 
  dplyr::filter(year_measured == 1) |>
  droplevels() |>
  dplyr::count(sample_origin_year)


# more description 

# how many in 2018
dat |>
  filter(wave == 2018 & year_measured==1) |>
  select(hours_work, gender3, id, wave) |>
  drop_na() |>
  summarise(count_distinct = n_distinct(id))



table(dat$gender3)
# perfectionism in 2018
# graph of gender x perfectionism
dev.off()
dt_graph  <- dat |>
  filter(any(wave == 2018 & year_measured == 1))|>
  select(perfectionism, male, id, wave) |>
  drop_na() 
dev.off()
dev.off()
dt_graph |> 
  ggplot(aes(x = male, y = perfectionism, colour = male)) +
  geom_boxplot(notch = TRUE) + geom_jitter(shape = 16,
                                           position = position_jitter(0.2),
                                           alpha = .1) + 
  labs(title = "Perfectionism by Gender: NZAVS years 2018-2019, N = 47823", y = "Doing my best never seems to be enough.\nMy performance rarely measures up to my standards.\nI am hardly ever satisfied with my performance.", x = "Male coded as 1, other identities coded as 0") + 
  scale_color_viridis_d(option = "D")
# graph of religious x perfectionism 
dat |>
  filter(any(wave == 2018 & year_measured==1)) |>
  select(perfectionism, religious_identification_level, id, wave) |>
  mutate(religious_identification = as.factor(religious_identification_level)) |>
  drop_na() |>
  ggplot(aes(x=as.factor(religious_identification_level), y= perfectionism, colour = factor(religious_identification_level))) +
  geom_boxplot(notch = TRUE) + geom_jitter(shape=16, position=position_jitter(0.2), alpha = .1) + labs(
    title = "Perfectionism by religious_identification: NZAVS years 2018-2019, N = 47823",
    y = "Doing my best never seems to be enough.\nMy performance rarely measures up to my standards.\nI am hardly ever satisfied with my performance.",
    x = "Male coded as 1, other identities coded as 0") + scale_color_viridis_d(option = "D")

```

