
#functions for pooled parameters 
library(msm)
# read data

### Function for graph 
library(msm)

msm_graph  <- function(x, l_str, u_str ){
  p20 <-
    pmatrix.msm(x, covariates = list(age = 20), ci = "normal")
  p20.Convert <- p20$estimates[1, 2]
  p20.Convert.L <- p20$L[1, 2]
  # get upper bound
  p20.Convert.U <- p20$U[1, 2]
  con.20 <- c(20, p20.Convert, p20.Convert.L, p20.Convert.U)
  ## 30 year olds
  p30 <-
    pmatrix.msm(x, covariates = list(age = 30), ci = "normal")
  p30.Convert <- p30$estimates[1, 2]
  # set lower bound
  p30.Convert.L <- p30$L[1, 2]
  # get upper bound
  p30.Convert.U <- p30$U[1, 2]
  #combine
  con.30 <- c(30, p30.Convert, p30.Convert.L, p30.Convert.U)
  ## 40 year old
  p40 <-
    pmatrix.msm(x, covariates = list(age = 40), ci = "normal")
  # Get Transition to Relid Estimate
  p40.Convert <- p40$estimates[1, 2]
  # set lower bound
  p40.Convert.L <- p40$L[1, 2]
  # get upper bound
  p40.Convert.U <- p40$U[1, 2]
  con.40 <- c(40, p40.Convert, p40.Convert.L, p40.Convert.U)
  ## 50 year old
  p50 <-
    pmatrix.msm(x, covariates = list(age = 50), ci = "normal")
  # Get Transition to Relid Estimate
  p50.Convert <- p50$estimates[1, 2]
  # set lower bound
  p50.Convert.L <- p50$L[1, 2]
  # get upper bound
  p50.Convert.U <- p50$U[1, 2]
  con.50 <- c(50, p50.Convert, p50.Convert.L, p50.Convert.U)
  ## 60 year olds
  p60 <-
    pmatrix.msm(x, covariates = list(age = 60), ci = "normal")
  # Get Transition to Relid Estimate
  p60.Convert <- p60$estimates[1, 2]
  # set lower bound
  p60.Convert.L <- p60$L[1, 2]
  # get upper bound
  p60.Convert.U <- p60$U[1, 2]
  con.60 <- c(60, p60.Convert, p60.Convert.L, p60.Convert.U)
  ## 70 year olds
  p70 <-
    pmatrix.msm(x, covariates = list(age = 70), ci = "normal")
  # Get Transition to Relid Estimate
  p70.Convert <- p70$estimates[1, 2]
  # set lower bound
  p70.Convert.L <- p70$L[1, 2]
  # get upper bound
  p70.Convert.U <- p70$U[1, 2]
  con.70 <- c(70, p70.Convert, p70.Convert.L, p70.Convert.U)
  ### ## 80 year olds
  p80 <-
    pmatrix.msm(x, covariates = list(age = 80), ci = "normal")
  # Get Transition to Relid Estimate
  p80.Convert <- p80$estimates[1, 2]
  # set lower bound
  p80.Convert.L <- p80$L[1, 2]
  # get upper bound
  p80.Convert.U <- p80$U[1, 2]
  con.80 <- c(80, p80.Convert, p80.Convert.L, p80.Convert.U)
  # ### ## 90 year olds
  p90 <-
    pmatrix.msm(x, covariates = list(age = 90), ci = "normal")
  # # Get Transition to Relid Estimate
  p90.Convert <- p90$estimates[1, 2]
  # # set lower bound
  p90.Convert.L <- p90$L[1, 2]
  p90.Convert.U <- p90$U[1, 2]
  con.90 <- c(90, p90.Convert, p90.Convert.L, p90.Convert.U)
  # ### create data frame for conversion plot
  conplot <-
    data.frame(rbind(con.20, con.30, con.40, con.50, con.60, con.70, con.80, con.90))
  colnames(conplot) <-
    c("age", "conv_probability", "conv_lower", "conv_upper")
  ##### CREAT PLOT FOR DECONVERSION
  ## 20 year olds
  p20 <-
    pmatrix.msm(x, covariates = list(age = 20), ci = "normal")
  # Get Transition to Relid Estimate
  p20.decon <- p20$estimates[2, 1]
  p20.decon.L <- p20$L[2, 1]
  p20.decon.U <- p20$U[2, 1]
  decon.20 <- c(20, p20.decon, p20.decon.L, p20.decon.U)
  p30 <-
    pmatrix.msm(x, covariates = list(age = 30), ci = "normal")
  p30.decon <- p30$estimates[2, 1]
  p30.decon.L <- p30$L[2, 1]
  p30.decon.U <- p30$U[2, 1]
  decon.30 <- c(30, p30.decon, p30.decon.L, p30.decon.U)
  p40 <-
    pmatrix.msm(x, covariates = list(age = 40), ci = "normal")
  p40.decon <- p40$estimates[2, 1]
  p40.decon.L <- p40$L[2, 1]
  p40.decon.U <- p40$U[2, 1]
  decon.40 <- c(40, p40.decon, p40.decon.L, p40.decon.U)
  p50 <-
    pmatrix.msm(x, covariates = list(age = 50), ci = "normal")
  p50.decon <- p50$estimates[2, 1]
  p50.decon.L <- p50$L[2, 1]
  p50.decon.U <- p50$U[2, 1]
  decon.50 <- c(50, p50.decon, p50.decon.L, p50.decon.U)
  p60 <-
    pmatrix.msm(x, covariates = list(age = 60), ci = "normal")
  p60.decon <- p60$estimates[2, 1]
  p60.decon.L <- p60$L[2, 1]
  p60.decon.U <- p60$U[2, 1]
  decon.60 <- c(60, p60.decon, p60.decon.L, p60.decon.U)
  p70 <-
    pmatrix.msm(x, covariates = list(age = 70), ci = "normal")
  p70.decon <- p70$estimates[2, 1]
  p70.decon.L <- p70$L[2, 1]
  p70.decon.U <- p70$U[2, 1]
  decon.70 <- c(70, p70.decon, p70.decon.L, p70.decon.U)
  p80 <-
    pmatrix.msm(x, covariates = list(age = 80), ci = "normal")
  p80.decon <- p80$estimates[2, 1]
  p80.decon.L <- p80$L[2, 1]
  p80.decon.U <- p80$U[2, 1]
  decon.80 <- c(80, p80.decon, p80.decon.L, p80.decon.U)
  p90 <-
    pmatrix.msm(x, covariates = list(age = 90), ci = "normal")
  p90.decon <- p90$estimates[2, 1]
  p90.decon.L <- p90$L[2, 1]
  p90.decon.U <- p90$U[2, 1]
  decon.90 <- c(90, p90.decon, p90.decon.L, p90.decon.U)
  
  ### create data frame for conversion plot
  deconplot <-
    data.frame(rbind(
      decon.20,
      decon.30,
      decon.40,
      decon.50,
      decon.60,
      decon.70,
      decon.80,
      decon.90
    ))
  colnames(deconplot) <-
    c("age", "decon_probability", "decon_lower", "decon_upper")
  #grid.arrange(plot.con,plot.decon, ncol=2)
  ### SUPERIMPOSE
  deconplot$Group <- as.factor(rep(l_str, nrow(deconplot)))
  conplot$Group <- as.factor(rep(u_str, nrow(conplot)))
  deconplot2 <- deconplot %>%
    dplyr::mutate(
      conv_probability = decon_probability,
      conv_lower = decon_lower,
      conv_upper = decon_upper
    ) %>%
    dplyr::select(-c(decon_probability, decon_upper, decon_lower))
  
  comboplot <- rbind(conplot, deconplot2)

}

markov_manifest <- function(df, outcome ) {
# matrix for msms model on these data sets
  q_mat <- rbind(c(.9, .1),c(.1, .9))
  #intial values 
  crude_inits <-
    msm::crudeinits.msm(outcome  ~ yearW, Id, # Id = identifier
                        data = df,
                        qmatrix = q_mat)
  out  <- msm(
    outcome ~ yearW,
    Id,
    data = a_imp_5_r,
    covariates =  ~ Age,
    qmatrix = crude_inits,
    ematrix = rbind(c(.1, .1), c(.1, .1)), 
    est.initprobs = TRUE,
    exacttimes = TRUE,
    gen.inits = TRUE
  )
}



