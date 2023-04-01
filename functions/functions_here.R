# functions


# models ------------------------------------------------------------------
# function for obtaining causal contrasts on the risk difference scale
# takes time

# this code is based on Noah Griefer's blog here:
# https://ngreifer.github.io/blog/
# in particular this: https://ngreifer.github.io/blog/treatment-effects-mi/


##  df names a data frame in which the CVARS are measured at baseline (here, one wave before an exposure), followed by an exposure (one year after baseline), followed by an outcome Y (two wave after baseline, and one wave after the exposure.)

## d  will name the dataframe in propensity score models where we have used a balancing method to for confounding control.

## note that one should be able to choose either "g-comp (regression stratification), propensity_sore (weighting for the exposure), or both (doubly robust).  If only g-comp is used, then the "d" becomes redundant, i.e. df = d.

##  method = g-comp.

## as here, we also need a family variable


## for every outcomewide study we will need an X = (for each study there will be only 1 exposure), a Y = the outcome (there will typically be many, cvars which are composed of a) a consistent set of variables across all studies -- these are listed in the code. (e.g. Male, European, BornNZ, the big six personality, religion (relid), political orientation, political right..... Additionally within this set of standard baseline covariates we will have the baseline value of X, the exposure, and the baseline value of Y the outcome.

## This means we have a choice. Whether to Multiply impute all missingness at once, or whether to do MI one outcome at a time. Below we do MI all at once, but for outcome wide well-being studies we might want to reconsider because we don't want our MI models to be either too large or misspecified.)


## nsims is the number of simulations to obtain confidence intervals for the causal effect estimate.

## weights is used for post-stratification to obtain the population level causal effects or PATE. Recall there are different causal effects that may be of interest...



# glm_contrast_mi --------------------------------------------------------------



glm_contrast_mi <- function(dt_match, nsims, Y, X, baseline_vars, cl,family, value) {
    require("clarify")
    require("rlang") # for building dynamic expressions
    
    fits <-  lapply(complete(dt_match, "all"), function(d) {
      glm(as.formula(paste( paste(Y, "~", X , "*", "("), paste(baseline_vars, collapse = "+"),paste(")"))),
          weights = d$weights, # specify weights column from the dataset
          family = family,
          data = d
      )
    })
    
    sim.imp <- misim(fits, n = nsims)
    
    # Build dynamic expression for subsetting
    subset_expr <- rlang::expr(!!rlang::sym(X) == !!value)
    
    sim.att <- sim_ame(
      sim.imp,
      var = X,
      subset = eval(subset_expr),
      # Evaluate the subset_expr expression
      cl = cl,
      verbose = FALSE
    )
    
    sim_est <- transform(sim.att, `RD` = `E[Y(1)]` - `E[Y(0)]`)
    
    out <- summary(sim_est)
    
    out
  }






# function for obtaining causal contrasts on the risk difference scale


geeglm_mi_ate = function(ml, ml_match, nsims, Y, X, baseline_vars, cl, family = family, weights = weights ) {
  require("clarify") # simulate conf intervals
  require("geepack") # for gee using repeated measures
  # ml = object of the class mids
  # ml = matched dataset. 
  fits <- lapply(complete(ml, "all"), function(d) {
    geepack::glm(
      as.formula(paste(
        paste(Y, "~", X , "*", "("),
        paste(cvars, collapse = "+"),
        paste(")")
      )),
      weights = weights,
      id = id,
      family = family,
      data = d
    )
  })
  # sim coefficients
  sim.imp <- misim(fits, n = nsims)
  
  # Build dynamic expression for subsetting
  subset_expr <- rlang::expr(!!rlang::sym(X) == !!value)
  
  sim.att <- sim_ame(
    sim.imp,
    var = X,
    subset = eval(subset_expr), # Evaluate the subset_expr expression
    cl = cl,
    verbose = FALSE
  )
  # risk differ
  sim_est <- transform(sim.att, `RD` = `E[Y(1)]` - `E[Y(0)]`)
  
  # output
  out <-  summary(sim_est)
  
  out
  
}



# geeglm_ate --------------------------------------------------------------



# function for obtaining causal contrasts on the risk difference scale

geeglm_ate = function(df, nsims, Y, X, baseline_vars, cl, weights = weights) {
  require("clarify") # simulate conf intervals
  require("geepack") # for gee using repeated measures
  
  fit <-  geepack::geeglm(
    as.formula(paste(
      paste(Y, "~", X , "*", "("),
      paste(baseline_vars, collapse = "+"),
      paste(")")
    )),
    weights = NULL,
    # repeated measures
    family = family,
    data = df
  )
  # sim coefficients
  sim.imp <- sim(fit, n = nsims)
  
  sim.att <- sim_ame(
    sim.imp,
    var = X,
    subset = X  == 1,
    # !!sym(X) == subset_val,  fix later to make general
    # cores
    cl = cl,
    verbose = FALSE
  )
  
  # risk difference
  sim_est <- transform(sim.att, `RD` = `E[Y(1)]` - `E[Y(0)]`)
  
  # output
  out <-  summary(sim_est)
  
  out
}
  



# geeglm_mi_ate -----------------------------------------------------------


geeglm_mi_ate = function(df, d, nsims, Y, X, baseline_vars, cl, family = family, weights = weights ) {
  require("clarify") # simulate conf intervals
  require("geepack") # for gee using repeated measures
  
  fits <- lapply(complete(ml, "all"), function(d) {
    geepack::geeglm(
      # as.formula(paste(
      #   paste(Y, "~", X, "+"),
      #   paste(cvars, collapse = "+")
      # )),
      # interaction -
      as.formula(paste(
        paste(Y, "~", X , "*", "("),
        paste(cvars, collapse = "+"),
        paste(")")
      )),
      weights = weights,
      id = Id,
      corstr = "ar1",
      # repeated measures
      family = family,
      data = d
    )
  })
  # sim coefficients
  sim.imp <- misim(fits, n = nsims)
  
  sim.att <- sim_ame(
    sim.imp,
    var = X,
    subset = time  == 1,
    # !!sym(X) == subset_val,  fix later to make general
    # cores
    cl = cl,
    verbose = FALSE
  )
  
  # risk differ
  sim_est <- transform(sim.att, `RD` = `E[Y(1)]` - `E[Y(0)]`)
  
  # output
  out <-  summary(sim_est)
  
  out
  
}

## Function to create individual tables
tab_ate_ols <- function(x, new_name, delta, sd) {
  require("EValue")
  # x = output from function glm_contrast_mi: new_name is for the new table name
  # value = contrast
  # sd = standard deviation of the outcome
  require(dplyr)
  # make clarify object into a data frame
  x <- as.data.frame(x)
  out <- x %>%
    # take row that is needed
    dplyr::slice(3) %>%
    # use only three digits
    dplyr::mutate(across(where(is.numeric), round, digits = 4)) %>%
    # Estimand of interest is risk difference
    dplyr::rename("E[Y(1)]-E[Y(0)]" = Estimate) 
  
  #rename row
  rownames(out)[1] <- paste0(new_name)
  out <- as.data.frame(out)
  out
  # make evalue column, which is needed four evalues
  # Calculate the standard error
  tab0 <- out |>  dplyr::mutate(standard_error = abs(`2.5 %` - `97.5 %`) / 3.92)
  evalout <- as.data.frame(round(EValue::evalues.OLS(tab0[1, 1],
                                                     se = tab0[1, 4],
                                                     sd = sd,
                                                     delta = delta,
                                                     true = 0
  ),
  3
  ))
  
  evalout2 <- subset(evalout[2, ])
  evalout3 <- evalout2 |>
    select_if( ~ !any(is.na(.)))
  colnames(evalout3) <- c("Evalue", "E-val_bound")
  tab <- cbind.data.frame(tab0, evalout3) |> dplyr::select(-c(standard_error,Evalue)) # keep table minimal
  return(tab)
}



glm_contrast_mi <- function(dt_match, nsims, Y, X, baseline_vars, cl,family, delta) {
  require("clarify")
  require("rlang") # for building dynamic expressions
  
  fits <-  lapply(complete(dt_match, "all"), function(d) {
    glm(as.formula(paste( paste(Y, "~", X , "*", "("), paste(baseline_vars, collapse = "+"),paste(")"))),
        weights = d$weights, # specify weights column from the dataset
        family = family,
        data = d
    )
  })
  
  sim.imp <- misim(fits, n = nsims)
  
  # Build dynamic expression for subsetting
  subset_expr <- rlang::expr(!!rlang::sym(X) == !!delta)
  
  sim.att <- sim_ame(
    sim.imp,
    var = X,
    subset = eval(subset_expr),
    # Evaluate the subset_expr expression
    cl = cl,
    verbose = FALSE
  )
  
  sim_est <- transform(sim.att, `RD` = `E[Y(1)]` - `E[Y(0)]`)
  
  out <- summary(sim_est)
  
  out
}




glm_contrast_rr_mi <- function(dt_match, nsims, Y, X, baseline_vars, cl,family, delta) {
  require("clarify")
  require("rlang") # for building dynamic expressions
  
  fits <-  lapply(complete(dt_match, "all"), function(d) {
    glm(as.formula(paste( paste(Y, "~", X , "*", "("), paste(baseline_vars, collapse = "+"),paste(")"))),
        weights = d$weights, # specify weights column from the dataset
        family = family,
        data = d
    )
  })
  
  sim.imp <- misim(fits, n = nsims)
  
  # Build dynamic expression for subsetting
  subset_expr <- rlang::expr(!!rlang::sym(X) == !!delta)
  
  sim.att <- sim_ame(
    sim.imp,
    var = X,
    subset = eval(subset_expr),
    # Evaluate the subset_expr expression
    cl = cl,
    verbose = FALSE
  )
  
  sim_est <- transform(sim.att, RR = `E[Y(1)]`/`E[Y(0)]`)
  
  out <- summary(sim_est)
  
  out
}



tab_ate_rr <- function(x, new_name, delta, sd) {
  require("EValue")
  # x = output from function glm_contrast_mi: new_name is for the new table name
  # value = contrast
  # sd = standard deviation of the outcome
  require(dplyr)
  # make clarify object into a data frame
  x <- as.data.frame(x)
  out <- x %>%
    # take row that is needed
    dplyr::slice(3) %>%
    # use only three digits
    dplyr::mutate(across(where(is.numeric), round, digits = 4)) %>%
    # Estimand of interest is risk difference
    dplyr::rename("E[Y(1)]-E[Y(0)]" = Estimate) 
  
  #rename row
  rownames(out)[1] <- paste0(new_name)
  out <- as.data.frame(out)
  out
  # make evalue column, which is needed four evalues
  # Calculate the standard error
  tab0 <- out #|>  dplyr::mutate(standard_error = abs(`2.5 %` - `97.5 %`) / 3.92)
  evalout <- as.data.frame(round(EValue::evalues.RR(tab0[1, 1],
                                                    lo = tab0[1, 2],
                                                    hi = tab0[1, 3],
                                                    true = 1
  ),
  3
  ))
  evalout2 <- subset(evalout[2, ])
  evalout3 <- evalout2 |>
    select_if( ~ !any(is.na(.)))
  colnames(evalout3) <- c("Evalue", "E-val_bound")
  tab <- cbind.data.frame(tab0, evalout3) |> dplyr::select(-c(Evalue)) # keep table minimal
  return(tab)
}


# function for obtaining causal contrasts on the risk difference scale: geeeglm

geeglm_ate = function(df, nsims, Y, X, baseline_vars, cl) {
  require("clarify") # simulate conf intervals
  require("geepack") # for gee using repeated measures
  
  fit <-  geepack::geeglm(
    as.formula(paste(
      paste(Y, "~", X , "*", "("),
      paste(baseline_vars, collapse = "+"),
      paste(")")
    )),
    weights = NULL,
    # repeated measures
    family = family,
    data = df
  )
  # sim coefficients
  sim.imp <- sim(fit, n = nsims)
  
  sim.att <- sim_ame(
    sim.imp,
    var = X,
    subset = X  == 1,
    # !!sym(X) == subset_val,  fix later to make general
    # cores
    cl = cl,
    verbose = FALSE
  )
  
  # risk difference
  sim_est <- transform(sim.att, `RD` = `E[Y(1)]` - `E[Y(0)]`)
  
  # output
  out <-  summary(sim_est)
  
  out
  
}



# tables ------------------------------------------------------------------


## Function to create individual tables
tab_ate  <- function(x, new_name) {
  require(dplyr)
  # make clarify object into a data frame
  x <- as.data.frame(x)
  out  <- x  %>%
    # take row that is needed
    dplyr::slice(3) |>
    # use only three digits
    dplyr::mutate(across(where(is.numeric), round, digits = 4)) |>
    # Estimand of interest is risk difference
    dplyr::rename("E[Y(1)]-E[Y(0)]" = Estimate)
  
  #rename row
  rownames(out)[1] <- paste0(new_name)
  out
}


# function to create group tables
group_tab_ate <- function(df) {
  require(dplyr)
  # take group data frame, make a column for reliable estimates,
  out  <-  df |>
    arrange(desc(`E[Y(1)]-E[Y(0)]`)) |>
    dplyr::mutate(Estimate  = as.factor(ifelse(
      `E[Y(1)]-E[Y(0)]` > 0 & `2.5 %` > 0 ,
      "Positive",
      ifelse(`E[Y(1)]-E[Y(0)]` < 0 &
               `97.5 %` < 0 , "Negative",
             "Not Reliable")
    ))) |>
    rownames_to_column(var = "Group") |>
    #label for graph
    mutate(
      across(where(is.numeric), round, digits = 3),
      estimate_lab = paste0(`E[Y(1)]-E[Y(0)]`, " (", `2.5 %`, "-", `97.5 %`, ")")
    )
  
  out
}



# graphs ------------------------------------------------------------------

#function to make plots
group_plot_ate <- function(df, title, subtitle) {
  # Convert the title string to a symbol
  title_sym <- sym(title)
  
  out <- ggplot(
    data = df,
    aes(
      y = reorder(Group, `E[Y(1)]-E[Y(0)]`),
      x = `E[Y(1)]-E[Y(0)]`,
      xmin = `2.5 %`,
      xmax = `97.5 %`,
      fill = Estimate
    )
  ) +
    geom_col(position = position_dodge(width = 0.3)) +
    geom_errorbarh(height = .3, position = position_dodge(width = 0.3)) +
    geom_vline(xintercept = 0, linetype = "solid") +
    geom_vline(
      xintercept = c(-.25, -.1, .1, .25),
      linetype = "twodash",
      alpha = .5
    ) +
    theme_classic(base_size = 10) +
    scale_fill_manual(values = c("gray", "orange")) + # dodgerblue
    labs(
      x = "Causal Difference (SD)",
      y = "",
      title = title,
      subtitle = subtitle
    ) +
    #labels so that the graph can also be a table
    geom_text(
      aes(x = -.24, label = estimate_lab),
      size = 4,
      hjust = 0,
      fontface = ifelse(df$Estimate != "Not Reliable", "bold", "plain")
    ) +
    coord_cartesian(xlim = c(-.25, .25)) +
    theme(
      panel.border = element_blank(),
      axis.line = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.x = element_text(size = 14),  # Increase x-axis label font size
      axis.title.y = element_text(size = 14),
      plot.title = element_text(face = "bold", size = 18),  # Increase title font size
      plot.subtitle = element_text(size = 14),  # Increase title font size
      axis.text = element_text(size = 12)  # Increase axis text font size
    ) + theme(legend.position = "top",  # or "top"
              legend.direction = "horizontal")
  # return
  out
}


# not needed
# cvars = mf |>
#   dplyr::select(-c(
#     .imp,
#     .id,
#     Id,
#     time,
#     weights
#   )) |> # include?
#   dplyr::select(!starts_with("Warm.")) |>
#   colnames()
#
# cvars
#
#
# # hack
# my_vec <- c("Partner", "Euro", "GenCohort", "Male", "NZSEI13",
#             "NZDep2018", "Rural_GCH2018", "REGC_2022", "CONSCIENTIOUSNESS",
#             "OPENNESS", "HONESTY_HUMILITY", "EXTRAVERSION", "NEUROTICISM",
#             "AGREEABLENESS", "edu_n", "Employed", "BornNZ", "Pol.Orient",
#             "Pol.Wing", "Parent", "Relid")
#
# my_vec <- setdiff(my_vec, c("Rural_GCH2018", "REGC_2022"))
#my_vec


# sens plot ---------------------------------------------------------------







# convention for covid timeline if needed
# CONVENTIONS FOR COVID TIMELINE
# dat$COVID19.Timeline
# bels:
#   value                                                                                              label
# 0.0                                                                                   The Before Times
# 1.0                                31.12.2019 -- 27.02.2020 [First cluster of cases in Wuhan reported]
# 1.1                                      28.02.2020 -- 25.02.2020 [First case recorded in New Zealand]
# 1.2                                                           26.03.2020 -- 27.04-2020 [Alert Level 4]
# 1.3                                                           28.04.2020 -- 13.05.2020 [Alert Level 3]
# 1.4                                                          14.05.2020 -- 08.06.2020 [Alert Level 2].
# 1.5                                                           09.06.2020 -- 11.08.2020 [Alert Level 1]
# 2.1 12.08.2020 -- 30.08.2020 [Second Outbreak - Auckland Alert Level 3, Rest of Country Alert Level 2]
# 2.2                 30.08.2020 -- 21.09.2020 [Auckland Alert Level 2.5, Rest of Country Alert Level 2]
# 2.3                    22.09.2020 -- 07.10.2020 [Auckland Alert Level 2, Rest of Country Alert Level 1
# 2.4                                                              08.10.2020 -- onwards [Alert Level 1]

# dat$REGC_2018 -- DON'T USE RATHER USE REGC_2022

# labels:
#   value                     label
# 1          Northland Region
# 2           Auckland Region
# 3            Waikato Region
# 4      Bay of Plenty Region
# 5           Gisborne Region
# 6         Hawkes Bay Region
# 7           Taranaki Region
# 8 Manawatu-Whanganui Region
# 9         Wellington Region
# 12         West Coast Region
# 13         Canterbury Region
# 14              Otago Region
# 15          Southland Region
# 16             Tasman Region
# 17             Nelson Region
# 18        Marlborough Region
# 99       Area Outside Region
