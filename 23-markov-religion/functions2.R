#functions for pooled parameters
library(msm)
library(dplyr)
## function for saving

save_here <- function(df, name) {
  x = df
  saveRDS( x,  here::here("_posts", "religious_simpsons_paradox",  "mods", paste0(name, '')))
}


# function for reading

read_here <- function(name) {
  df = readRDS(here::here("_posts", "religious_simpsons_paradox", "mods", paste0(name, '')))
  df
}
# read data

# msm_10_r_imp1 <- readRDS(here::here( "_posts","religious_simpsons_paradox", "mods","msm_10_r_imp1" ))
# msm_10_r_imp2 <- readRDS(here::here( "_posts","religious_simpsons_paradox", "mods","msm_10_r_imp2" ))
# msm_10_r_imp3 <- readRDS(here::here( "_posts","religious_simpsons_paradox", "mods","msm_10_r_imp3" ))
# msm_10_r_imp4 <- readRDS(here::here( "_posts","religious_simpsons_paradox", "mods","msm_10_r_imp4" ))
# msm_10_r_imp5 <- readRDS(here::here( "_posts","religious_simpsons_paradox", "mods","msm_10_r_imp5" ))
#
# m1<- msm_10_r_imp1
# m2<- msm_10_r_imp2
# m3<- msm_10_r_imp3
# m4<- msm_10_r_imp4
# m5<- msm_10_r_imp5

pmatrix.msm(m1,  covariates = list(Age = 80), ci = "normal")


### Function for graph
library(msm)
# religious works
# r1 <- msm_graph(msm_10_r_imp1, "Disaffiliation", "Affiliation")
# r2 <- msm_graph(msm_10_r_imp2, "Disaffiliation", "Affiliation")
# r3 <- msm_graph(msm_10_r_imp3, "Disaffiliation", "Affiliation")
# r4 <- msm_graph(msm_10_r_imp4, "Disaffiliation", "Affiliation")
# r5 <- msm_graph(msm_10_r_imp5, "Disaffiliation", "Affiliation")
#

# God
# g1 <- msm_graph(msm_10_g_imp1, "Disbelief", "Belief")
# g2 <- msm_graph(msm_10_g_imp2, "Disbelief", "Belief")
# g3 <- msm_graph(msm_10_g_imp3, "Disbelief", "Belief")
# g4 <- msm_graph(msm_10_g_imp4, "Disbelief", "Belief")
# g5 <- msm_graph(msm_10_g_imp5, "Disbelief", "Belief")



# Spirit
# s1 <- msm_graph(msm_10_s_imp1, "Disbelief", "Belief")
# s2 <- msm_graph(msm_10_s_imp2, "Disbelief", "Belief")
# s3 <- msm_graph(msm_10_s_imp3, "Disbelief", "Belief")
# s4 <- msm_graph(msm_10_s_imp4, "Disbelief", "Belief")
# s5 <- msm_graph(msm_10_s_imp5, "Disbelief", "Belief")
#

# manifest

# religious works
# mr1 <- msm_graph(msm_10_r_MAN_imp1, "Disaffiliation", "Affiliation")
# mr2 <- msm_graph(msm_10_r_MAN_imp2, "Disaffiliation", "Affiliation")
# mr3 <- msm_graph(msm_10_r_MAN_imp3, "Disaffiliation", "Affiliation")
# mr4 <- msm_graph(msm_10_r_MAN_imp4, "Disaffiliation", "Affiliation")
# mr5 <- msm_graph(msm_10_r_MAN_imp5, "Disaffiliation", "Affiliation")


# God
# mg1 <- msm_graph(msm_10_g_MAN_imp1, "Disbelief", "Belief")
# mg2 <- msm_graph(msm_10_g_MAN_imp2, "Disbelief", "Belief")
# mg3 <- msm_graph(msm_10_g_MAN_imp3, "Disbelief", "Belief")
# mg4 <- msm_graph(msm_10_g_MAN_imp4, "Disbelief", "Belief")
# mg5 <- msm_graph(msm_10_g_MAN_imp5, "Disbelief", "Belief")



# Spirit
# ms1 <- msm_graph(msm_10_s_MAN_imp1, "Disbelief", "Belief")
# ms2 <- msm_graph(msm_10_s_MAN_imp2, "Disbelief", "Belief")
# ms3 <- msm_graph(msm_10_s_MAN_imp3, "Disbelief", "Belief")
# ms4 <- msm_graph(msm_10_s_MAN_imp4, "Disbelief", "Belief")
# ms5 <- msm_graph(msm_10_s_MAN_imp5, "Disbelief", "Belief")




## https://bookdown.org/mwheymans/bookmi/measures-of-missing-data-information.html#eq:lambda
## RIV = Relative increase in variance  = LAMDA
# lambda = ( (v_b + (v_b/m) )/ v_total)
# riv = lambda = ( (v_b + (v_b/m) )/ v_w)
# df_old = (m -1 ) * (1 + (1/riv^2))
# df_observed  = ( (((n-k) + 1)/ ((n-k) + 3) ) * ((n-k) * (1- lambda)) )
# df_adjusted = ( (df_old * df_adjusted ) / (df_old + df_adjusted)  )
# alpha = 0.05
# t_score = qt( p=alpha/2, df= df_adjusted,lower.tail=F )
# margin_error = t_score * se_pooled
#

#This correctly calculates ci's


msm_se_pooled_ci <- function(df1,df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12,
                             df13, df14, df15, df16, df17, df18, df19, df20){
  m = 20
  n = 4441
  k = 1
  alpha = .05
  out <- cbind(df1$conv_probability,
               df2$conv_probability,
               df3$conv_probability,
               df4$conv_probability,
               df5$conv_probability,
               df6$conv_probability,
               df7$conv_probability,
               df8$conv_probability,
               df9$conv_probability,
               df10$conv_probability,
               df11$conv_probability,
               df12$conv_probability,
               df13$conv_probability,
               df14$conv_probability,
               df15$conv_probability,
               df16$conv_probability,
               df17$conv_probability,
               df18$conv_probability,
               df19$conv_probability,
               df20$conv_probability)
  age <- df1$age
  Group <- df1$Group
  col_m <- data.frame(rowMeans(out))
  colnames(col_m) <-  "conv_probability_m"
  var1 = ((df1$conv_probability - col_m) ^ 2)
  var2 = ((df2$conv_probability - col_m) ^ 2)
  var3 = ((df3$conv_probability - col_m) ^ 2)
  var4 = ((df4$conv_probability - col_m) ^ 2)
  var5 = ((df5$conv_probability - col_m) ^ 2)
  var6 = ((df6$conv_probability - col_m) ^ 2)
  var7 = ((df7$conv_probability - col_m) ^ 2)
  var8 = ((df8$conv_probability - col_m) ^ 2)
  var9 = ((df9$conv_probability - col_m) ^ 2)
  var10 = ((df10$conv_probability - col_m) ^ 2)
  var11 = ((df11$conv_probability - col_m) ^ 2)
  var12 = ((df12$conv_probability - col_m) ^ 2)
  var13 = ((df13$conv_probability - col_m) ^ 2)
  var14 = ((df14$conv_probability - col_m) ^ 2)
  var15 = ((df15$conv_probability - col_m) ^ 2)
  var16 = ((df16$conv_probability - col_m) ^ 2)
  var17 = ((df17$conv_probability - col_m) ^ 2)
  var18 = ((df18$conv_probability - col_m) ^ 2)
  var19 = ((df19$conv_probability - col_m) ^ 2)
  var20 = ((df20$conv_probability - col_m) ^ 2)
  v_b = as.data.frame(
    1 / (m - 1) * (var1 + var2 + var3 + var4 + var5 + var6 + var7  + var8 + var9 + var10 +
                     var11 + var12 + var13 + var14 + var15 + var16 + var17  + var18 + var19 + var20
    )
  ) # var between
  se1 <- as.data.frame((df1$conv_upper - df1$conv_lower) / 3.92)
  colnames(se1) <- "se1"
  se2 <- as.data.frame((df2$conv_upper - df2$conv_lower)/3.92)
  colnames(se2) <- "se2"
  se3 <- as.data.frame((df3$conv_upper - df3$conv_lower)/3.92)
  colnames(se3) <- "se3"
  se4 <- as.data.frame((df4$conv_upper - df4$conv_lower)/3.92)
  colnames(se4) <- "se4"
  se5 <- as.data.frame((df5$conv_upper - df5$conv_lower)/3.92)
  colnames(se5) <- "se5"
  se6 <- as.data.frame((df6$conv_upper - df6$conv_lower) / 3.92)
  colnames(se6) <- "se6"
  se7 <- as.data.frame((df7$conv_upper - df7$conv_lower)/3.92)
  colnames(se7) <- "se7"
  se8 <- as.data.frame((df8$conv_upper - df8$conv_lower)/3.92)
  colnames(se8) <- "se8"
  se9 <- as.data.frame((df9$conv_upper - df9$conv_lower)/3.92)
  colnames(se9) <- "se9"
  se10 <- as.data.frame((df10$conv_upper - df10$conv_lower)/3.92)
  colnames(se10) <- "se10"
  se11 <- as.data.frame((df11$conv_upper - df11$conv_lower) / 3.92)
  colnames(se11) <- "se11"
  se12 <- as.data.frame((df12$conv_upper - df12$conv_lower)/3.92)
  colnames(se12) <- "se12"
  se13 <- as.data.frame((df13$conv_upper - df13$conv_lower)/3.92)
  colnames(se13) <- "se13"
  se14 <- as.data.frame((df14$conv_upper - df14$conv_lower)/3.92)
  colnames(se14) <- "se14"
  se15 <- as.data.frame((df15$conv_upper - df15$conv_lower)/3.92)
  colnames(se15) <- "se15"
  se16 <- as.data.frame((df16$conv_upper - df16$conv_lower) / 3.92)
  colnames(se16) <- "se16"
  se17 <- as.data.frame((df17$conv_upper - df17$conv_lower)/3.92)
  colnames(se17) <- "se17"
  se18 <- as.data.frame((df18$conv_upper - df18$conv_lower)/3.92)
  colnames(se18) <- "se18"
  se19 <- as.data.frame((df19$conv_upper - df19$conv_lower)/3.92)
  colnames(se19) <- "se19"
  se20 <- as.data.frame((df20$conv_upper - df20$conv_lower)/3.92)
  colnames(se20) <- "se20"
  out2 = cbind( se1 , se2, se3 , se4, se5, se6, se7, se8, se9, se10, se11, se12, se13, se14, se15, se16,
                se17, se18, se19, se20)
  v_w <- as.data.frame( (out2$se1^2 + out2$se2^2 + out2$se3^2 + out2$se4^2 + out2$se5^2 +
                           out2$se6^2 + out2$se7^2 + out2$se8^2 + out2$se9^2 + out2$se10^2 +
                           out2$se11^2 + out2$se12^2 + out2$se13^2 + out2$se14^2 + out2$se15^2 +
                           out2$se16^2 + out2$se17^2 + out2$se18^2 + out2$se19^2 + out2$se20^2) /(m))# Var withing
  colnames(v_w) <- "v_w"
  v_total = as.data.frame(v_w + v_b + v_b/m) # var total
  colnames(v_total) <- "v_total"
  se_pooled = sqrt(v_total)
  colnames(se_pooled) <- "se_pooled"
  #pooled_ci=   se_pooled * 1.96 # (col_m  + (1.96 * (se_pooled/66.64))) # not used
  lambda = ( (v_b + (v_b/m) ) / v_total)
  riv = ( (v_b + (v_b/m) )/ v_w )
  df_old = ( (m - 1 ) * (1 + (1/(riv^2)) ) )
  df_observed  = ( ( ( (n-k) + 1 )/ ( (n-k) + 3) ) * ((n-k) * (1- lambda) ) )
  df_adjusted = ( (df_old * df_observed ) / (df_old + df_observed)  )
  colnames(df_adjusted) <- "df_adjusted"
  df_adjusted
  alpha = 0.05
  t_score = as.data.frame( qt( p = alpha/2, df = df_adjusted$df_adjusted, lower.tail = F ) )
  colnames(t_score) <- "t_score"
  margin_error = as.data.frame ( t_score * se_pooled )
  colnames(margin_error) <- "margin_error"
  pool_coef = as.data.frame(cbind(col_m, se_pooled, margin_error))
  colnames(pool_coef) <- c("pooled_mean", "se_pooled", "margin_error")
  pooled_out <- as.data.frame(cbind(age, Group, pool_coef))
  pooled_out
}


msm_se_pooled_ci()

length(unique(all))


# r_pool_plot <- msm_se_pooled_ci(r1,r2,r3,r4,r5)
# r_pool_plot_man <- msm_se_pooled_ci(mr1,mr2,mr3,mr4,mr5)
# g_pool_plot <- msm_se_pooled_ci(g1,g2,g3,g4,g5)
# g_pool_plot_man <- msm_se_pooled_ci(mg1,mg2,mg3,mg4,mg5)
# s_pool_plot <- msm_se_pooled_ci(s1,s2,s3,s4,s5)
# s_pool_plot_man <- msm_se_pooled_ci(ms1,ms2,ms3,ms4,ms5)


#
# markov_manifest <- function(df, outcome ) {
#   # matrix for msms model on these data sets
#   q_mat <- rbind(c(.9, .1),c(.1, .9))
#   #intial values
#   crude_inits <-
#     msm::crudeinits.msm(outcome  ~ yearW, Id, # Id = identifier
#                         data = df,
#                         qmatrix = q_mat)
#   out  <- msm(
#     outcome ~ yearW,
#     Id,
#     data = a_imp_5_r,
#     covariates =  ~ Age,
#     qmatrix = crude_inits,
#     ematrix = rbind(c(.1, .1), c(.1, .1)),
#     est.initprobs = TRUE,
#     exacttimes = TRUE,
#     gen.inits = TRUE
#   )
# }
#


combine_man_imp_r <- function(amelia_obj, m) {
  out  <- list()
  model <- list()
  graph <- list()
  name1 <- "Disbelief"
  name2 <- "Belief"

  q_mat <- rbind(c(.9, .1),c(.1, .9))
  for (i in 1:m) {
    out[[i]] <- amelia_obj$imputations[[i]]
    out[[i]] %>%
      dplyr::arrange(Id, yearW)
  }
  #intial values
  crude_inits <-
    msm::crudeinits.msm(Religious1  ~ yearW, Id, # Id = identifier
                        data = out[[i]],
                        qmatrix = q_mat)
  for (i in 1:m){
    model[[i]]  <- msm(
      Religious1 ~ yearW,
      Id,
      data = out[[i]],
      covariates =  ~ Age,
      qmatrix = crude_inits,
      # ematrix = rbind(c(.1, .1), c(.1, .1)),
      est.initprobs = TRUE,
      exacttimes = TRUE,
      gen.inits = TRUE
    )
  }
  return(model)
}




combine_imp_r <- function(amelia_obj, m) {
  out  <- list()
  model <- list()
  graph <- list()
  name1 <- "Disbelief"
  name2 <- "Belief"
  q_mat <- rbind(c(.9, .1),c(.1, .9))
  for (i in 1:m) {
    out[[i]] <- amelia_obj$imputations[[i]]
    out[[i]] %>%
      dplyr::arrange(Id, yearW)
  }
  #intial values
  crude_inits <-
    msm::crudeinits.msm(Religious1  ~ yearW, Id, # Id = identifier
                        data = out[[i]],
                        qmatrix = q_mat)
  for (i in 1:m){
    model[[i]]  <- msm(
      Religious1 ~ yearW,
      Id,
      data = out[[i]],
      covariates =  ~ Age,
      qmatrix = crude_inits,
      ematrix = rbind(c(.1, .1), c(.1, .1)),
      est.initprobs = TRUE,
      exacttimes = TRUE,
      gen.inits = TRUE
    )
  }
  return(model)
}



combine_imp_g <- function(amelia_obj, m) {
  out  <- list()
  model <- list()
  graph <- list()
  name1 <- "Disbelief"
  name2 <- "Belief"

  q_mat <- rbind(c(.9, .1),c(.1, .9))
  for (i in 1:m) {
    out[[i]] <- amelia_obj$imputations[[i]]
    out[[i]] %>%
      dplyr::arrange(Id, yearW)
  }
  #intial values
  crude_inits <-
    msm::crudeinits.msm(bg1  ~ yearW, Id, # Id = identifier
                        data = out[[i]],
                        qmatrix = q_mat)
  for (i in 1:m){
    model[[i]]  <- msm(
      bg1 ~ yearW,
      Id,
      data = out[[i]],
      covariates =  ~ Age,
      qmatrix = crude_inits,
      ematrix = rbind(c(.1, .1), c(.1, .1)),
      est.initprobs = TRUE,
      exacttimes = TRUE,
      gen.inits = TRUE
    )
  }
  return(model)
}


combine_imp_s <- function(amelia_obj, m) {
  out  <- list()
  model <- list()
  graph <- list()
  name1 <- "Disbelief"
  name2 <- "Belief"

  q_mat <- rbind(c(.9, .1),c(.1, .9))
  for (i in 1:m) {
    out[[i]] <- amelia_obj$imputations[[i]]
    out[[i]] %>%
      dplyr::arrange(Id, yearW)
  }
  #intial values
  crude_inits <-
    msm::crudeinits.msm(bs1  ~ yearW, Id, # Id = identifier
                        data = out[[i]],
                        qmatrix = q_mat)
  for (i in 1:m){
    model[[i]]  <- msm(
      bs1 ~ yearW,
      Id,
      data = out[[i]],
      covariates =  ~ Age,
      qmatrix = crude_inits,
      ematrix = rbind(c(.1, .1), c(.1, .1)),
      est.initprobs = TRUE,
      exacttimes = TRUE,
      gen.inits = TRUE
    )
  }
  return(model)
}



here::here()
# read data for models

dat_all_10_r <- read_here("dat_all_10_r")




# religion_man  - manifest outputs
save_here(religion_man, "religion_man")


religion_imp <- combine_imp_r(dat_all_10_r, 20)

# religion impute 20 datasets
save_here(religion_imp, "religion_imp")

religion_imp <- read_here("religion_imp")

graphs_r<- NULL
m<- 20

for (i in 1:m) {
  graphs_r[[i]] <- msm_graph(religion_imp[[i]],"Disaffiliation", "Affiliation")
}

## Save manifest r (as "graphs"  -- did this earlier as a test on the manual r, file)
#save_here(graphs, "graphs_man_r")

save_here(graphs_r, "graphs_r")

for (i in 1:m) {
  graphs_r[[i]] <- msm_graph(religion_imp[[i]],"Disaffiliation", "Affiliation")
}
df<- NULL
for (i in 1:m) {
  df[[i]] <- as.data.frame(graphs_r[[i]])
}



new_20_imp_r <- msm_se_pooled_ci(df[[1]], df[[2]], df[[3]], df[[4]],
                                 df[[5]], df[[6]], df[[7]], df[[8]],
                                 df[[9]], df[[10]], df[[11]], df[[12]],
                                 df[[13]], df[[14]], df[[15]], df[[16]],
                                 df[[17]], df[[18]], df[[19]], df[[20]])






# IGNORE FROM BELOW


# 5 year imputations ------------------------------------------------------
library(msm)

dat_all_5_ru <- read_here("dat_all_5_ru")  # 20 imputations
dat_all_5_gu <- read_here("dat_all_5_gu")  # 20 imputations
dat_all_5_su <- read_here("dat_all_5_su")  # 20 imputations

r5 <- combine_imp_r(dat_all_5_ru, 20)
g5 <- combine_imp_g(dat_all_5_gu, 5)
s5 <- combine_imp_s(dat_all_5_su, 5)

# save models
save_here(r5, "r5")
r5<- read_here("r5")

save_here(g5, "g5")
g5<- read_here("g5")

save_here(s5, "s5")
s5<- read_here("s5")



# df<- NULL
# for (i in 1:m) {
#   df[[i]] <- as.data.frame(graphs_r[[i]])
# }


## Save manifest r (as "graphs"  -- did this earlier as a test on the manual r, file)
#save_here(graphs, "graphs_man_r")

# for (i in 1:m) {
#   graphs_r[[i]] <- msm_graph(religion_imp[[i]],"Disaffiliation", "Affiliation")
# }
# df<- NULL
# for (i in 1:m) {
#   df[[i]] <- as.data.frame(graphs_r[[i]])
# }


df<- NULL

for (i in 1:m) {
  df[[i]] <- as.data.frame(graphs_r[[i]])
}
#
# msm_se_pooled_ci <- function(df1,df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12,
#                              df13, df14, df15, df16, df17, df18, df19, df20){
#   m = 20
#   n = 21936
#   k = 1
#   alpha = .05
#   out <- cbind(df1$conv_probability,
#                df2$conv_probability,
#                df3$conv_probability,
#                df4$conv_probability,
#                df5$conv_probability,
#                df6$conv_probability,
#                df7$conv_probability,
#                df8$conv_probability,
#                df9$conv_probability,
#                df10$conv_probability,
#                df11$conv_probability,
#                df12$conv_probability,
#                df13$conv_probability,
#                df14$conv_probability,
#                df15$conv_probability,
#                df16$conv_probability,
#                df17$conv_probability,
#                df18$conv_probability,
#                df19$conv_probability,
#                df20$conv_probability)
#   age <- df1$age
#   Group <- df1$Group
#   col_m <- data.frame(rowMeans(out))
#   colnames(col_m) <-  "conv_probability_m"
#   var1 = ((df1$conv_probability - col_m) ^ 2)
#   var2 = ((df2$conv_probability - col_m) ^ 2)
#   var3 = ((df3$conv_probability - col_m) ^ 2)
#   var4 = ((df4$conv_probability - col_m) ^ 2)
#   var5 = ((df5$conv_probability - col_m) ^ 2)
#   var6 = ((df1$conv_probability - col_m) ^ 2)
#   var7 = ((df2$conv_probability - col_m) ^ 2)
#   var8 = ((df3$conv_probability - col_m) ^ 2)
#   var9 = ((df4$conv_probability - col_m) ^ 2)
#   var10 = ((df5$conv_probability - col_m) ^ 2)
#   var11 = ((df1$conv_probability - col_m) ^ 2)
#   var12 = ((df2$conv_probability - col_m) ^ 2)
#   var13 = ((df3$conv_probability - col_m) ^ 2)
#   var14 = ((df4$conv_probability - col_m) ^ 2)
#   var15 = ((df5$conv_probability - col_m) ^ 2)
#   var16 = ((df1$conv_probability - col_m) ^ 2)
#   var17 = ((df2$conv_probability - col_m) ^ 2)
#   var18 = ((df3$conv_probability - col_m) ^ 2)
#   var19 = ((df4$conv_probability - col_m) ^ 2)
#   var20 = ((df5$conv_probability - col_m) ^ 2)
#   v_b = as.data.frame(
#     1 / (m - 1) * (var1 + var2 + var3 + var4 + var5 + var6 + var7  + var8 + var9 + var10 +
#                      var11 + var12 + var13 + var14 + var15 + var16 + var17  + var18 + var19 + var20
#     )
#   ) # var between
#   se1 <- as.data.frame((df1$conv_upper - df1$conv_lower) / 3.92)
#   colnames(se1) <- "se1"
#   se2 <- as.data.frame((df2$conv_upper - df2$conv_lower)/3.92)
#   colnames(se2) <- "se2"
#   se3 <- as.data.frame((df3$conv_upper - df3$conv_lower)/3.92)
#   colnames(se3) <- "se3"
#   se4 <- as.data.frame((df4$conv_upper - df4$conv_lower)/3.92)
#   colnames(se4) <- "se4"
#   se5 <- as.data.frame((df5$conv_upper - df5$conv_lower)/3.92)
#   colnames(se5) <- "se5"
#   se6 <- as.data.frame((df6$conv_upper - df6$conv_lower) / 3.92)
#   colnames(se6) <- "se6"
#   se7 <- as.data.frame((df7$conv_upper - df7$conv_lower)/3.92)
#   colnames(se7) <- "se7"
#   se8 <- as.data.frame((df8$conv_upper - df8$conv_lower)/3.92)
#   colnames(se8) <- "se8"
#   se9 <- as.data.frame((df9$conv_upper - df9$conv_lower)/3.92)
#   colnames(se9) <- "se9"
#   se10 <- as.data.frame((df10$conv_upper - df10$conv_lower)/3.92)
#   colnames(se10) <- "se10"
#   se11 <- as.data.frame((df11$conv_upper - df11$conv_lower) / 3.92)
#   colnames(se11) <- "se11"
#   se12 <- as.data.frame((df12$conv_upper - df12$conv_lower)/3.92)
#   colnames(se12) <- "se12"
#   se13 <- as.data.frame((df13$conv_upper - df13$conv_lower)/3.92)
#   colnames(se13) <- "se13"
#   se14 <- as.data.frame((df14$conv_upper - df14$conv_lower)/3.92)
#   colnames(se14) <- "se14"
#   se15 <- as.data.frame((df15$conv_upper - df15$conv_lower)/3.92)
#   colnames(se15) <- "se15"
#   se16 <- as.data.frame((df16$conv_upper - df16$conv_lower) / 3.92)
#   colnames(se16) <- "se16"
#   se17 <- as.data.frame((df17$conv_upper - df17$conv_lower)/3.92)
#   colnames(se17) <- "se17"
#   se18 <- as.data.frame((df18$conv_upper - df18$conv_lower)/3.92)
#   colnames(se18) <- "se18"
#   se19 <- as.data.frame((df19$conv_upper - df19$conv_lower)/3.92)
#   colnames(se19) <- "se19"
#   se20 <- as.data.frame((df20$conv_upper - df20$conv_lower)/3.92)
#   colnames(se20) <- "se20"
#   out2 = cbind( se1 , se2, se3 , se4, se5, se6, se7, se8, se9, se10, se11, se12, se13, se14, se15, se16,
#                 se17, se18, se19, se20)
#   v_w <- as.data.frame( (out2$se1^2 + out2$se2^2 + out2$se3^2 + out2$se4^2 + out2$se5^2 +
#                            out2$se6^2 + out2$se7^2 + out2$se8^2 + out2$se9^2 + out2$se10^2 +
#                            out2$se11^2 + out2$se12^2 + out2$se13^2 + out2$se14^2 + out2$se15^2 +
#                            out2$se16^2 + out2$se17^2 + out2$se18^2 + out2$se19^2 + out2$se20^2) /(m))# Var withing
#   colnames(v_w) <- "v_w"
#   v_total = as.data.frame(v_w + v_b + v_b/m) # var total
#   colnames(v_total) <- "v_total"
#   se_pooled = sqrt(v_total)
#   colnames(se_pooled) <- "se_pooled"
#   #pooled_ci=   se_pooled * 1.96 # (col_m  + (1.96 * (se_pooled/66.64))) # not used
#   lambda = ( (v_b + (v_b/m) ) / v_total)
#   riv = ( (v_b + (v_b/m) )/ v_w )
#   df_old = ( (m - 1 ) * (1 + (1/(riv^2)) ) )
#   df_observed  = ( ( ( (n-k) + 1 )/ ( (n-k) + 3) ) * ((n-k) * (1- lambda) ) )
#   df_adjusted = ( (df_old * df_observed ) / (df_old + df_observed)  )
#   colnames(df_adjusted) <- "df_adjusted"
#   df_adjusted
#   alpha = 0.05
#   t_score = as.data.frame( qt( p = alpha/2, df = df_adjusted$df_adjusted, lower.tail = F ) )
#   colnames(t_score) <- "t_score"
#   margin_error = as.data.frame ( t_score * se_pooled )
#   colnames(margin_error) <- "margin_error"
#   pool_coef = as.data.frame(cbind(col_m, se_pooled, margin_error))
#   colnames(pool_coef) <- c("pooled_mean", "se_pooled", "margin_error")
#   pooled_out <- as.data.frame(cbind(age, Group, pool_coef))
#   pooled_out
# }

## Convergence an issue for some models
r5_pooled_list <- list(
  # r5[[1]],
  r5[[2]],
  r5[[3]],
  # r5[[4]],
  r5[[5]],
  r5[[6]],
  r5[[7]],
  r5[[8]],
  #  r5[[9]],
  #  r5[[10]],
  r5[[11]],
  r5[[12]],
  r5[[13]],
  r5[[14]],
  r5[[15]],
  r5[[16]],
  r5[[17]],
  r5[[18]],
  r5[[19]],
  r5[[20]]
)

# just use 5

## RELIGIOUS
r5_pooled_list2 <- list(
  r5[[11]],
  r5[[12]],
  r5[[13]],
  r5[[14]],
  r5[[15]]
)


g5_pooled_list2 <- list(
  g5[[1]],
  g5[[2]],
  g5[[3]],
  g5[[4]],
  g5[[5]]
)

s5_pooled_list2 <- list(
  s5[[1]],
  s5[[2]],
  s5[[3]],
  s5[[4]],
  s5[[5]]
)

library(msm)
library(dplyr)
pmatrix.msm(s5[[1]], covariates = list(Age = 90), ci = "normal")

r5_pooled_list2
m = 15
for (i in 11:m) {
  graphs_r[[i]] <-  msm_graph(list(r5[[i]]), "Age", seq(20, 90, 10), "disaffiliation", "affiliation")
}

dfrr<- NULL
for (i in 11:m) {
  dfrr[[i]] <- as.data.frame(graphs_r[[i]])
}

rr1<- dfrr[[11]]
rr2<- dfrr[[12]]
rr3<- dfrr[[13]]
rr4<- dfrr[[14]]
rr5<- dfrr[[15]]



m = 5
graphs_g <- NULL
for (i in 1:m) {
  graphs_g[[i]] <-  msm_graph(list(g5[[i]]), "Age", seq(20, 90, 10), "disbelief", "belief")
}

dfg<- NULL
for (i in 1:m) {
  dfg[[i]] <- as.data.frame(graphs_g[[i]])
}

gg1<- dfg[[1]]
gg2<- dfg[[2]]
gg3<- dfg[[3]]
gg4<- dfg[[4]]
gg5<- dfg[[5]]


# spirit prep

m = 5
graphs_s <- NULL
for (i in 1:m) {
  graphs_s[[i]] <-  msm_graph(list(s5[[i]]), "Age", seq(20, 90, 10), "disbelief", "belief")
}

dfs<- NULL
for (i in 1:m) {
  dfs[[i]] <- as.data.frame(graphs_s[[i]])
}

ss1<- dfs[[1]]
ss2<- dfs[[2]]
ss3<- dfs[[3]]
ss4<- dfs[[4]]
ss5<- dfs[[5]]



# Function
msm_se_pooled_ci_short <-
  function(df1,
           df2,
           df3,
           df4,
           df5) {
    m = 5
    n = 21936
    k = 1
    alpha = .05
    out <- cbind(
      df1$prob,
      df2$prob,
      df3$prob,
      df4$prob,
      df5$prob
    )
    age <- df1$Age
    Group <- df1$group
    col_m <- data.frame(rowMeans(out))
    colnames(col_m) <-  "conv_probability_m"
    var1 = ((df1$prob - col_m) ^ 2)
    var2 = ((df2$prob - col_m) ^ 2)
    var3 = ((df3$prob - col_m) ^ 2)
    var4 = ((df4$prob - col_m) ^ 2)
    var5 = ((df5$prob - col_m) ^ 2)
    v_b = as.data.frame(1 / (m - 1) * (var1 + var2 + var3 + var4 + var5)) # var between
    se1 <- as.data.frame((df1$prob_upper - df1$prob_lower) / 3.92)
    colnames(se1) <- "se1"
    se2 <- as.data.frame((df2$prob_upper - df2$prob_lower) / 3.92)
    colnames(se2) <- "se2"
    se3 <- as.data.frame((df3$prob_upper - df3$prob_lower) / 3.92)
    colnames(se3) <- "se3"
    se4 <- as.data.frame((df4$prob_upper - df4$prob_lower) / 3.92)
    colnames(se4) <- "se4"
    se5 <- as.data.frame((df5$prob_upper - df5$prob_lower) / 3.92)
    colnames(se5) <- "se5"
    out2 = cbind(se1 , se2, se3 , se4, se5)
    v_w <-
      as.data.frame(
        (out2$se1 ^ 2 + out2$se2 ^ 2 + out2$se3 ^ 2 + out2$se4 ^ 2 + out2$se5 ^ 2 / (m))
      )# Var withing
    colnames(v_w) <- "v_w"
    v_total = as.data.frame(v_w + v_b + v_b / m) # var total
    colnames(v_total) <- "v_total"
    se_pooled = sqrt(v_total)
    colnames(se_pooled) <- "se_pooled"
    #pooled_ci=   se_pooled * 1.96 # (col_m  + (1.96 * (se_pooled/66.64))) # not used
    lambda = ((v_b + (v_b / m)) / v_total)
    riv = ((v_b + (v_b / m)) / v_w)
    df_old = ((m - 1) * (1 + (1 / (riv ^ 2))))
    df_observed  = ((((n - k) + 1) / ((n - k) + 3)) * ((n - k) * (1 - lambda)))
    df_adjusted = ((df_old * df_observed) / (df_old + df_observed))
    colnames(df_adjusted) <- "df_adjusted"
    df_adjusted
    alpha = 0.05
    t_score = as.data.frame(qt(
      p = alpha / 2,
      df = df_adjusted$df_adjusted,
      lower.tail = F
    ))
    colnames(t_score) <- "t_score"
    margin_error = as.data.frame (t_score * se_pooled)
    colnames(margin_error) <- "margin_error"
    pool_coef = as.data.frame(cbind(col_m, se_pooled, margin_error))
    colnames(pool_coef) <-
      c("pooled_mean", "se_pooled", "margin_error")
    pooled_out <- as.data.frame(cbind(age, Group, pool_coef))
    pooled_out
  }


pooled_5_r <- msm_se_pooled_ci_short( rr1, rr2, rr3, rr4, rr5)
pooled_5_r

pooled_5_g <- msm_se_pooled_ci_short( gg1, gg2, gg3, gg4, gg5)
pooled_5_g

pooled_5_s <- msm_se_pooled_ci_short( ss1, ss2, ss3, ss4, ss5)
pooled_5_s

save_here(pooled_5_r, "pooled_5_r")
save_here(pooled_5_g, "pooled_5_g")
save_here(pooled_5_s, "pooled_5_s")


# 5 year graphs -----------------------------------------------------------
library(ggsci)
library(ggplot2)
plot_5_r_30_pooled <-
  ggplot(
    aes(
      x = age,
      y = pooled_mean,
      ymin = pooled_mean - margin_error,
      ymax = pooled_mean + margin_error,
      colour = Group
    ),
    data = pooled_5_r
  ) +
  geom_point(alpha = 1, stroke = 2) +
  geom_linerange(alpha = 1) +
  geom_smooth(se = F) +
  scale_y_continuous(limits = c(0, .30))  +
  labs(x = "Age", y = "Annual Probability of Religious (Dis)Affiliation w/ 95 CI ") + theme_bw() + labs(title = "Hidden Markov Model: (Dis)Afilliation Pooled Imputation Model",
                                                                                                        subtitle= "N = 21936, years 2016-2021") +
  #scale_colour_manual(values=c( "#E69F00", "#56B4E9")) +
  scale_color_aaas() +
  theme(legend.position = "bottom")


save_here(plot_5_r_30_pooled, "plot_5_r_30_pooled")




plot_5_g_30_pooled <-
  ggplot(
    aes(
      x = age,
      y = pooled_mean,
      ymin = pooled_mean - margin_error,
      ymax = pooled_mean + margin_error,
      colour = Group
    ),
    data = pooled_5_g
  ) +
  geom_point(alpha = 1, stroke = 2) +
  geom_linerange(alpha = 1) +
  geom_smooth(se = F) +
  scale_y_continuous(limits = c(0, .30))  +
  labs(x = "Age", y = "Annual Probability of (Dis)Belief in God w/ 95 CI ") +
  theme_bw() + labs(title = "Hidden Markov Model: (Dis)Afilliation Pooled Imputation Model",
                    subtitle= "N = 21936, years 2016-2021") +
  #scale_colour_manual(values=c( "#E69F00", "#56B4E9")) +
  scale_color_npg() +
  theme(legend.position = "bottom")

plot_5_g_30_pooled
save_here(plot_5_g_30_pooled, "plot_5_g_30_pooled")


plot_5_g_30_pooled <-
  ggplot(
    aes(
      x = age,
      y = pooled_mean,
      ymin = pooled_mean - margin_error,
      ymax = pooled_mean + margin_error,
      colour = Group
    ),
    data = pooled_5_g
  ) +
  geom_point(alpha = 1, stroke = 2) +
  geom_linerange(alpha = 1) +
  geom_smooth(se = F) +
  scale_y_continuous(limits = c(0, .30))  +
  labs(x = "Age", y = "Annual Probability of (Dis)Belief in God w/ 95 CI ") +
  theme_bw() + labs(title = "Hidden Markov Model: (Dis)Belief in God Pooled Imputation Model",
                    subtitle= "N = 21936, years 2016-2021") +
  #scale_colour_manual(values=c( "#E69F00", "#56B4E9")) +
  scale_color_npg() +
  theme(legend.position = "bottom")

plot_5_g_30_pooled
save_here(plot_5_g_30_pooled, "plot_5_g_30_pooled")


plot_5_s_30_pooled <-
  ggplot(
    aes(
      x = age,
      y = pooled_mean,
      ymin = pooled_mean - margin_error,
      ymax = pooled_mean + margin_error,
      colour = Group
    ),
    data = pooled_5_s
  ) +
  geom_point(alpha = 1, stroke = 2) +
  geom_linerange(alpha = 1) +
  geom_smooth(se = F) +
  scale_y_continuous(limits = c(0, .30))  +
  labs(x = "Age", y = "Annual Probability of (Dis)Belief in Spirit w/ 95 CI ") +
  theme_bw() + labs(title = "Hidden Markov Model: (Dis)Belief in Spirit Pooled Imputation Model",
                    subtitle= "N = 21936, years 2016-2021") +
  #scale_colour_manual(values=c( "#E69F00", "#56B4E9")) +
  scale_color_okabe_ito() +
  theme(legend.position = "bottom")


plot_5_s_30_pooled
save_here(plot_5_s_30_pooled, "plot_5_s_30_pooled")


