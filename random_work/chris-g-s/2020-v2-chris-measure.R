# graph for chris



# libraries
library("tidyverse") # data wrangle
library("patchwork") # add plots together
library("here") # directory management.
library("readr") # read data
library("janitor") # clean names
library("tidyr") # data wrangling
library("stringer") # data wrangling
library("ggokabeito") # graph colours (not used)
# note import files as tab delimited in to R as data frames

# read omega data
omega <- read_csv(here::here("random_work","chris-g-s", "c-g-s.csv"))


# check
colnames(omega)

# clean header names using the janitor package
omega_cleaned <- omega %>% clean_names()

#check
colnames(omega_cleaned)

# data wrangling -- data is a wee bit  messy
library(tidyr)
library(dplyr)
library(stringr)

# graph 1
omega_full <- omega_cleaned |> 
  select(scale, omega_full, low_ci_3, up_ci_4) |> 
  rename(score = omega_full) |> 
  rename(low_ci = starts_with("low_ci_")) |> 
  rename(up_ci = starts_with("up_ci_")) |> 
  mutate(type = rep("Omega-Full", nrow(omega_cleaned))) 

head(omega_full)

# graph 2
omega_short <- omega_cleaned %>%
  select(scale, omega_short, low_ci_6, up_ci_7) |> 
  rename(score = omega_short) |> 
  rename(low_ci = starts_with("low_ci_")) |> 
  rename(up_ci = starts_with("up_ci_")) |> 
  mutate(type = rep("Omega-Short", nrow(omega_cleaned))) 

head(omega_short)


# graph 3
short_remainder_reliability <- omega_cleaned %>%
  select(scale, short_remainder_reliability, low_ci_9, up_ci_10) |> 
  rename(score = short_remainder_reliability) |> 
  rename(low_ci = starts_with("low_ci_")) |> 
  rename(up_ci = starts_with("up_ci_")) |> 
  mutate(type = rep("Short-Remainder-Reliability", nrow(omega_cleaned))) 
  
head(short_remainder_reliability)
  



## Data for other panels

#graph 4
rmsea_bayes <- omega_cleaned %>%
  select(scale, rmsea_bayes, low_ci_12, up_ci_13) |> 
  rename(score = rmsea_bayes) |> 
  rename(low_ci = starts_with("low_ci_")) |> 
  rename(up_ci = starts_with("up_ci_")) |> 
  mutate(type = rep("RMSEA_Bayes", nrow(omega_cleaned))) 


head(rmsea_bayes)

# graph 5
rmsea_mlr <- omega_cleaned %>%
  select(scale, rmsea_mlr, low_ci_15, up_ci_16) |> 
  rename(score = rmsea_mlr) |> 
  rename(low_ci = starts_with("low_ci_")) |> 
  rename(up_ci = starts_with("up_ci_")) |> 
  mutate(type = rep("RMSEA_MLR", nrow(omega_cleaned))) 


head(rmsea_mlr)

# graph 6
cfi_bayes <- omega_cleaned %>%
  select(scale, cfi_bayes, low_ci_18, up_ci_19)|> 
  rename(score = cfi_bayes) |> 
  rename(low_ci = starts_with("low_ci_")) |> 
  rename(up_ci = starts_with("up_ci_")) |> 
  mutate(type = rep("CFI_Bayes", nrow(omega_cleaned))) 


head(cfi_bayes)

# graph 7
cfi_mlr <- omega_cleaned %>%
  select(scale, cfi_mlr) |> 
  rename(score = cfi_mlr) |> 
  mutate(low_ci = rep(NA, nrow(omega_cleaned))) |> 
  mutate(up_ci = rep(NA, nrow(omega_cleaned)))|> 
  mutate(type = rep("CFI_MLR", nrow(omega_cleaned))) 

head(cfi_mlr)

# graph 8
tli_bayes <- omega_cleaned %>%
  select(scale, tli_bayes,low_ci_22,up_ci_23) |> 
  rename(score = tli_bayes) |> 
  rename(low_ci = starts_with("low_ci_")) |> 
  rename(up_ci = starts_with("up_ci_")) |> 
  mutate(type = rep("TLI_Bayes", nrow(omega_cleaned))) 


head(tli_bayes)

# graph 9
tli_mlr <- omega_cleaned %>%
  select(scale, tli_mlr) |> 
  rename(score = tli_mlr) |> 
  mutate(low_ci = rep(NA, nrow(omega_cleaned))) |> 
  mutate(up_ci = rep(NA, nrow(omega_cleaned)))|> 
  mutate(type = rep("TLI_MLR", nrow(omega_cleaned)))  


head(tli_mlr)


# graph 10
srmr_mlr <- omega_cleaned %>%
  select(scale, srmr_mlr) |> 
  rename(score = srmr_mlr) |> 
  mutate(low_ci = rep(NA, nrow(omega_cleaned))) |> 
  mutate(up_ci = rep(NA, nrow(omega_cleaned)))|> 
  mutate(type = rep("SRMR_MLR", nrow(omega_cleaned)))  


head(srmr_mlr)


colnames(omega_cleaned)

# "Could you please do two panel graphs."
#
# First is re-doing the one with the omegas and short-remainder reliability, which is the first set of columns in the data file.

# step 2: merge data frames
dat_1 <- rbind(omega_full,omega_short)

# check
head(dat_1)



# step 3 convert omega_scale to numeric
dat_1$score <- as.numeric(dat_1$score)
dat_1$type <- as.factor(dat_1$type)



# arrange data by levels of scale
dat_1$scale <- factor(dat_1$scale, levels = unique(dat_1$scale))

#chris wants to reverse this
dat_1$scale <- forcats::fct_rev(dat_1$scale)


# build graph 
library(ggplot2)

# convert omega_scale to a numeric variable
dat_1$score <- as.numeric(dat_1$score)
dat_1$low_ci <- as.numeric(dat_1$low_ci)
dat_1$up_ci <- as.numeric(dat_1$up_ci)

levels(dat_1$type)
# plotting

# step 4 make omega graph

omega_graph <- ggplot(data = dat_1, aes(x = score, y = scale, colour = type)) +
  geom_point() +
  geom_errorbarh(aes(xmin = low_ci, xmax = up_ci), height = 0.2) +
  labs(
    title = "Validation: Long and Short Scale",
    x = "Omega Reliability",
    y = "Scale",
    colour = "Type"
  ) +
  scale_x_continuous(limits = c(0,1)) +
  scale_colour_manual(values = c("Omega-Full" = "blue", "Omega-Short" = "red")) +
  theme_classic() +
  theme(legend.position = "bottom") 

# view
omega_graph

# save graph
ggsave(
  omega_graph,
  path = here::here(here::here("figs" , "chris")),
  width = 10,
  height = 15,
  units = "in",
  filename = "omega_graph.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)


## graph for reliability

# step 5 make reliability graph
dat_1$score <- as.numeric(dat_1$score)
dat_1$type <- as.factor(dat_1$type)

# arrange data by levels of scale
dat_1$scale <- factor(dat_1$scale, levels = unique(dat_1$scale))

#chris wants to reverse this
dat_1$scale <- forcats::fct_rev(dat_1$scale)


# build graph 
library(ggplot2)

# convert omega_scale to a numeric variable
short_remainder_reliability$score <- as.numeric(short_remainder_reliability$score)
short_remainder_reliability$low_ci <- as.numeric(short_remainder_reliability$low_ci)
short_remainder_reliability$up_ci <- as.numeric(short_remainder_reliability$up_ci)

short_remainder_reliability$scale <- factor(short_remainder_reliability$scale, levels = unique(short_remainder_reliability$scale))

#chris wants to reverse this
short_remainder_reliability$scale <- forcats::fct_rev(short_remainder_reliability$scale)


# graph
reliability_graph_short <- ggplot(data = short_remainder_reliability,
                                  aes(x = score,
                                      y = scale)) +
  geom_point() +
  geom_errorbarh(aes(xmin = low_ci, xmax = up_ci), height = 0.2) +
  labs(
    title = "Short Scale Reliability",
    x = "Half-split Reliability",
    y = "Scale",
    colour = "Type"
  ) +  theme_classic() + scale_x_continuous(limits = c(0, 1))

# view
reliability_graph_short

# save graph
ggsave(
  reliability_graph_short,
  path = here::here(here::here("figs" , "chris")),
  width = 10,
  height = 15,
  units = "in",
  filename = "reliability_graph_short.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)

# step 6 make combo graph

# create combo graph using forest plot
all_graph <- omega_graph + reliability_graph_short + plot_annotation(tag_levels = "a")

all_graph
# save combo
ggsave(
  all_graph,
  path = here::here(here::here("figs" , "chris")),
  width = 20,
  height = 15,
  units = "in",
  filename = "all_graph.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)

# PANEL 2 
# for TLI, one for RMSEA and one for SRMR. There are two CFI values, one bayes, one MLR estimated, and the bayes estimate also has Cis. The MLR estimate does not. Same for the TLI. Then a plot for RMSEA for Bayes and MLR estaimtes, each with Cis, and then a graph for SRMR, which only has one value and no CI. So it would be a pannel with 5 figures in total, CFI, TLI, RMSEA, SRMR.
# 
# x-value ranges for CFI and TLI .50 to 1.00. x value ranges for RMSEA and SRMR 0 to .20.

## Next graph


# Panel 2a
colnames(omega_cleaned)

dat_rmsea <- rbind(rmsea_bayes,rmsea_mlr) |> 
  mutate(score = as.numeric(score),
         low_ci = as.numeric(low_ci),
         up_ci = as.numeric(up_ci),
         type = as.factor(type),
         scale = as.factor(scale)) |> 
  mutate( scale = forcats::fct_rev(scale))

colnames(omega_cleaned)

rmsea_graph <- ggplot(data = dat_rmsea, aes(x = score, y = scale, colour = type)) +
  geom_point() +
  geom_errorbarh(aes(xmin = low_ci, xmax = up_ci), height = 0.2) +
  labs(
    title = "RMSEA",
    x = "RMSEA",
    y = "Scale",
    colour = "Type"
  ) +
 # scale_color_okabe_ito()+
  scale_colour_manual(values = c("RMSEA_Bayes" = "red", "RMSEA_MLR" = "blue")) +
#  scale_x_continuous(limits = c(0,.2)) + 
  theme_classic() +
  theme(legend.position = "bottom") 

# view
rmsea_graph



# save graph
ggsave(
  rmsea_graph,
  path = here::here(here::here("figs" , "chris")),
  width = 10,
  height = 15,
  units = "in",
  filename = "rmsea_graph.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)



dat_cfi <- rbind(cfi_bayes, cfi_mlr) |> 
  mutate(score = as.numeric(score),
         low_ci = as.numeric(low_ci),
         up_ci = as.numeric(up_ci),
         type = as.factor(type),
         scale = as.factor(scale)) |> 
  mutate( scale = forcats::fct_rev(scale))
head(dat_cfi)



# next graph
cfi_graph <- ggplot(data = dat_cfi, aes(x = score, y = scale, colour = type)) +
  geom_point() +
  geom_errorbarh(aes(xmin = low_ci, xmax = up_ci), height = 0.2) +
  labs(
    title = "CFI",
    x = "CFI",
    y = "Scale",
    colour = "Type"
  ) +
  scale_colour_manual(values = c("CFI_Bayes" = "red", "CFI_MLR" = "blue")) +
 # scale_color_okabe_ito(order = 3:4)+
  scale_x_continuous(limits = c(.3,1)) + 
  theme_classic() +
  theme(legend.position = "bottom") 


cfi_graph


# save graph
ggsave(
  cfi_graph,
  path = here::here(here::here("figs" , "chris")),
  width = 10,
  height = 15,
  units = "in",
  filename = "cfi_graph.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)

cfi_graph
# view
cfi_graph + 
rmsea_graph


# next graph

dat_tli<- rbind(tli_bayes, tli_mlr) |> 
  mutate(score = as.numeric(score),
         low_ci = as.numeric(low_ci),
         up_ci = as.numeric(up_ci),
         type = as.factor(type),
         scale = as.factor(scale)) |> 
  mutate( scale = forcats::fct_rev(scale))
head(dat_tli)


tli_graph <- ggplot(data = dat_tli, aes(x = score, y = scale, colour = type)) +
  geom_point() +
  geom_errorbarh(aes(xmin = low_ci, xmax = up_ci), height = 0.2) +
  labs(
    title = "TLI",
    x = "TLI",
    y = "Scale",
    colour = "Type"
  ) +
#  scale_colour_viridis_d(option = "E") +
  scale_colour_manual(values = c("TLI_Bayes" = "red", "TLI_MLR" = "blue")) +
  #scale_color_okabe_ito(order = 5:6) +
  
#  scale_x_continuous(limits = c(.4,1)) + 
  theme_classic() +
  theme(legend.position = "bottom") 

# view
tli_graph

# save graph
ggsave(
  tli_graph,
  path = here::here(here::here("figs" , "chris")),
  width = 10,
  height = 15,
  units = "in",
  filename = "tli_graph.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)


# next graph


dat_srmr <- srmr_mlr |> 
  mutate(score = as.numeric(score),
         low_ci = as.numeric(low_ci),
         up_ci = as.numeric(up_ci),
         type = as.factor(type),
         scale = as.factor(scale)) |> 
  mutate( scale = forcats::fct_rev(scale))



srmr_graph <- ggplot(data = dat_srmr, aes(x = score, y = scale, colour = type)) +
  geom_point() +
  geom_errorbarh(aes(xmin = low_ci, xmax = up_ci), height = 0.2) +
  labs(
    title = "SRMR",
    x = "SRMR",
    y = "Scale",
    colour = "Type"
  ) +
  # scale_colour_viridis_d(option = "E") +
  scale_colour_manual(values = c("SRMR_MLR" = "black")) +
  scale_x_continuous(limits = c(0,.2)) + 
  theme_classic() +
  theme(legend.position = "bottom") 

srmr_graph

ggsave(
  srmr_graph,
  path = here::here(here::here("figs" , "chris")),
  width = 10,
  height = 15,
  units = "in",
  filename = "srmr_graph.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)


# combo

test_stats_graph <- rmsea_graph +
  tli_graph +
  srmr_graph +
  cfi_graph+ plot_annotation(tag_levels = "a")

ggsave(
  test_stats_graph,
  path = here::here(here::here("figs" , "chris")),
  width = 20,
  height = 20,
  units = "in",
  filename = "test_stats_graph.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)



## RMSEA and SRMR combod graph 

rmsea_srmr_combo_graph <- rmsea_graph + srmr_graph + plot_annotation(tag_levels = "a")
rmsea_srmr_combo_graph

ggsave(
  rmsea_srmr_combo_graph,
  path = here::here(here::here("figs" , "chris")),
  width = 20,
  height = 15,
  units = "in",
  filename = " rmsea_srmr_combo_graph.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)

###  CFI/TLI combo graph
cfi_tli_combo_graph <- cfi_graph + tli_graph + plot_annotation(tag_levels = "a")

cfi_tli_combo_graph

# save graph
ggsave(
  cfi_tli_combo_graph,
  path = here::here(here::here("figs" , "chris")),
  width = 20,
  height = 15,
  units = "in",
  filename = " cfi_tli_combo_graph.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 600
)



