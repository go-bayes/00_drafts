# Scratchpad for checks on confounders
# for simulation see: https://rstudio-pubs-static.s3.amazonaws.com/570370_e1f26ce9e165496cb67cd40bd2e575d1.html

library(dagitty)
library(ggdag)


# Some methods for checking for confounders
g1 <- dagitty( "dag {
    A -> D
    A -> Y
    L-> Y
}")

g2 <- dagitty( "dag {
    Y <- X <- Z1 <- V -> Z2 -> Y
    Z1 <- W1 <-> W2 -> Z2
    X <- W1 -> Y
    X <- W2 -> Y
}")

plot(graphLayout(g1))

print( impliedConditionalIndependencies( g1 ) )


print( adjustmentSets( g1, "A", "Y") )


for( n in names(g1) ){
  for( m in setdiff( descendants( g1, n ), n ) ){
    a <- adjustmentSets( g1, n, m )
    if( length(a) > 0 ){
      cat("The total effect of ",n," on ",m,
          " is identifiable controlling for:\n",sep="")
      print( a, prefix=" * " )
    }
  }
}


# selection bias

#2

coords <- tibble::tribble(~ name, ~ x,  ~ y,
                          "L", 1,   2,
                          "D", 2,   3,
                          "A", 3,   2,
                          "Y", 4,   1)

dagify(Y ~ L,
       D ~ A,
       Y ~ L, coords = coords
) %>%
  ggdag_dseparated("Y", "A",
                   controlling_for = "D",
                   collider_lines = TRUE
  ) + theme_dag_grey()




# some daffner dags


# variation  of two using measurement errors



coords <- tibble::tribble(~ name, ~ x,  ~ y,
                          "S", 1,   3,
                          "A", 3,   2,
                          "Y", 4,   0,
                          "X", 1,   0)

d <- dagify(Y ~ A + S,
       coords = coords,
       exposure = "X",
       outcome = "Y",
        controlling_for = c("S"),
)

d%>%
  ggdag_dseparated("Y", "X",
                   controlling_for = c("S"),
                   collider_lines = TRUE
  ) + theme_dag_grey()




d |> ggdag_adjust(collider_lines = TRUE) + theme_dag_blank()


# variation  of two using measurement errors



coords <- tibble::tribble(~ name, ~ x,  ~ y,
                          "L", 1,   2,
                          "Ystar", 4,   3,
                          "A", 3,   2,
                          "Y", 4,   1)

dagify(Y ~ L,
       Ystar ~ Y + A + L,
       A ~ L,
       coords = coords,
       exposure = "A",
       outcome = "Y",
) %>%
  ggdag_dseparated("Y", "A",
                   controlling_for = c("Ystar","L", "A"),
                   collider_lines = TRUE
  ) + theme_dag_grey()




#3
coords <- tibble::tribble(~ name, ~ x,  ~ y,
                          "L", 1,   2,
                          "D", 2,   3,
                          "A", 3,   2,
                          "Y", 4,   1)





dag <- dagify(Y ~ L,
              D ~ L + A,
              exposure = "A",
              outcome = "Y",
              coords = coords
) %>%
  control_for(c("D","L"))
# ggdag_dseparated("Y", "A",
#                  controlling_for = "D",
#                  collider_lines = TRUE
# ) + theme_dag_grey()

ggdag_adjust(dag,collider_lines = TRUE) + theme_dag_blank()




ggdag_adjust(dag,collider_lines = FALSE) + theme_dag_blank()

ggdag_dseparated(dag, "Y", "A",
                 controlling_for = "D",
                 collider_lines = TRUE
) + theme_dag_grey()




#4 Selection bias as collider
coords_col <- tibble::tribble(~ name, ~ x,  ~ y,
                          "L", 0,   2,
                          "A", 2,   2,
                          "Yeta", 3,   0,
                          "Y_S", 4, 1)

dagify(Yeta ~ L,
       Yeta ~ A ,
       A ~ L,
       Y_S~  A   + Yeta,  coords = coords_col
) %>%
  ggdag_dseparated("Yeta", "A",
                   controlling_for = c("L", "Y_S"),
                   collider_lines = T
  ) + theme_dag_grey()


#4  cconsider previous graph aas an instance of measurment error
coords <- tibble::tribble(~ name, ~ x,  ~ y,
                          "L", 1,   2,
                          "Y", 2,   3,
                          "A", 3,   2,
                          "Yeta", 4, 2) 
dagify(
       Y ~ A + L, 
       Y ~ Yeta,  coords = coords
) %>%
  ggdag_dseparated("Y", "A",
                   controlling_for = c("Y","L"),
                   collider_lines = T
  ) + theme_dag_grey()








#5
coords_q <- tibble::tribble(~ name, ~ x,  ~ y,
                          "Q", 0, 0,
                          "Z", 0, 1,
                          "L", 1,   2,
                          "D", 2,   3,
                          "A", 3,   2,
                          "Y", 4,   1)

dagify(Y ~ Z,
       A ~ Q,
       L ~ Q + Z,
       D ~ A + L,  coords = coords_q
) %>%
  ggdag_dseparated("Y", "A",
                   controlling_for = c("D"),
                   collider_lines = T,
  ) + theme_dag_grey()



### Complex DAT WITH MEASUREMENT ERROR




coords <- tibble::tribble(~ name, ~ x,  ~ y,
                          "L", 1,   2,
                          "Ystar", 4,   3,
                          "A", 3,   2,
                          "Y", 4,   1)

dagify(Y ~ L,
       Ystar ~ Y + A + L,
       A ~ L,
       coords = coords
) %>%
  ggdag_dseparated("Y", "A",
                   controlling_for = c("Ystar","L", "A"),
                   collider_lines = TRUE
  ) + theme_dag_grey()



# \node [rectangle, draw=black] (L0) at (0, 0) {L};
# \node [rectangle, draw=white] (K1) at (2, 0) {$\eta$=K};
# \node [rectangle, draw=white] (X1) at (5, 0) {$(X_1, X_2, \dots X_n)$};
# \node [rectangle, draw=black] (A1) at (8, 0) {A};
# \node [rectangle, draw=white] (Y2) at (10, 0) {Y$_k$};
# 
# \draw [-latex, draw=black] (L0) to (K1);
# \draw [-latex, bend right, draw=black] (L0) to (Y2);
# \draw [-latex, draw=black] (K1) to (X1);
# \draw [-latex, draw=black] (X1) to (A1);
# \draw [-latex, draw=black, bend left] (K1) to (Y2);


## measurement error specific 

coords_ms1 <- tibble::tribble(~ name, ~ x,  ~ y,
                         "L0", 0, 0,
                         "K1", 2, 0,
                         "K", 3, 0,
                         "X1", 4, 3,
                         "A1", 3, 2,
                         "Y_k", 4, 1)

dagify(Y_k ~ L0,
       K1 ~ L0,
       X1 ~ K1,
      # Y_k~ K1,
       A1 ~ X1,
       coords = coords_ms1
) %>%
  ggdag_dseparated("Y_k", "A1",
                   controlling_for = c("L0"),
                   collider_lines = TRUE
  ) + theme_dag_grey()





# When confounders are measured without much error

coords_ms1b <- tibble::tribble(~ name, ~ x,  ~ y,
                              "UL", 0, 3,
                              "UA", 6, 5,
                              "UY", 9, 5,
                              "L0", 3, 2,
                              "A1", 7, 4,
                              "Y2", 11, 4,
                              "Leta0", 3,0,
                              "Aeta1", 7,2,
                              "Yeta2", 11,2)

dagify(L0 ~ UL,
       A1 ~ UA,
       Y2 ~ UY,
     # Yeta2 ~ Aeta1,
       L0 ~ Leta0, 
       A1 ~ Aeta1, 
       Y2 ~ Yeta2,
       Aeta1 ~ L0, 
       A1 ~ Aeta1,
#       UY ~ UA,# could introduce confounding
       Yeta2 ~ Leta0,
       coords = coords_ms2
) %>%
  ggdag_dseparated("Y2", "A1",
                   controlling_for = c("L0"),
                   collider_lines = T
  ) + theme_dag_grey()




dagify( 
        L ~ A + Y
) %>%
  ggdag_dseparated("Y", "A",
                   controlling_for = c("L"),
                   collider_lines = T
  ) + theme_dag_grey()




## when confounders are measured with error


dagify(L0 ~ UL,
       A1 ~ UA,
       Y2 ~ UY,
       # Yeta2 ~ Aeta1,
       L0 ~ Leta0, 
       A1 ~ Aeta1, 
       Y2 ~ Yeta2,
       Aeta1 ~ Leta0, 
       A1 ~ Aeta1,
       Yeta2 ~ Leta0,
       coords = coords_ms2
) %>%
  ggdag_dseparated("Y2", "A1",
                   controlling_for = c("L0"),
                   collider_lines = T
  ) + theme_dag_grey()



# standard graph 
dagify( 
  L ~ A + Y
) %>%
  ggdag_dseparated("Y", "A",
                   controlling_for = c("L"),
                   collider_lines = T
  ) + theme_dag_grey()



## measurement error generic 3 wave 

# \usetikzlibrary{positioning}
# \usetikzlibrary{shapes.geometric}
# \usetikzlibrary{arrows}
# \usetikzlibrary{decorations}
# \tikzstyle{Arrow} = [->, thin, preaction = {decorate}]
# \tikzset{>=latex}
# 
# % Define a simple decoration
# \tikzstyle{cor} = [-, dotted, preaction = {decorate}]
# 
# 
# \begin{tikzpicture}[{every node/.append style}=draw]
# 
# \node [rectangle, draw=white] (UL) at (0, 1) {U$_L$};
# \node [rectangle, draw=white] (UA) at (6, 2) {U$_A$};
# \node [rectangle, draw=white] (UY) at (9, 3) {U$_Y$};
# 
# \node [rectangle, draw=black] (L0) at (3, 1) {L$_{f(X_1\dots X_n)}^{t0}$};
# \node [rectangle, draw=black] (A1) at (7, 1) {A$_{f(X_1\dots X_n)}^{t1}$};
# \node [rectangle, draw=black] (Y2) at (11, 1) {Y$_{f(X_1\dots X_n)}^{t2}$};
# 
# \node [rectangle, draw=white] (Leta0) at (3, 0) {L$^{t0}_\eta$};
# \node [rectangle, draw=white] (Aeta1) at (7, 0) {A$^{t1}_\eta$};
# \node [rectangle, draw=white] (Yeta2) at (11, 0) {Y$^{t2}_\eta$};
# 
# 
# \draw [-latex, draw=black] (UL) to (L0);
# \draw [-latex, draw=black,bend left=20] (UA) to (A1);
# \draw [-latex, draw=black,bend left=30] (UY) to (Y2);
# \draw [-latex, draw=black] (Leta0) to (L0);
# \draw [-latex, draw=black] (Leta0) to (Aeta1);
# \draw [-latex, draw=black, bend right=30] (Leta0) to (Yeta2);
# \draw [-latex, draw=black] (Aeta1) to (A1);
# \draw [-latex, draw=black] (Yeta2) to (Y2);
# 
# 
# \draw [cor, draw=black, dashed,bend right=80] (UL) to (Leta0);
# \draw [cor, draw=black, dashed, bend right = 80] (UA) to (Aeta1);
# \draw [cor, draw=black, dashed, bend right = 80] (UY) to (Yeta2);
# 
# 
# 
# \end{tikzpicture}




coords_ms1b <- tibble::tribble(~ name, ~ x,  ~ y,
                               "UL", 0, 3,
                               "UA", 6, 5,
                               "UY", 9, 5,
                               "L0", 3, 2,
                               "A1", 7, 4,
                               "Y2", 11, 4,
                               "Leta0", 3,0,
                               "Aeta1", 7,2,
                               "Yeta2", 11,2)

dagify(L0 ~ UL,
       A1 ~ UA,
       Y2 ~ UY,
       #  Aeta1 ~ Leta0,
       Yeta2 ~ Leta0,
       L0 ~ Leta0, 
       A1 ~ Aeta2, 
       Y2 ~ Yeta2,
       #   Leta0 ~ UL,  # if L0 is measured without error
       Aeta1 ~ L0, 
       A1 ~ Aeta1,
       UY~ Aeta1,
       coords = coords_ms2
) %>%
  ggdag_dseparated("Y2", "A1",
                   controlling_for = c("L0"),
                   collider_lines = T
  ) + theme_dag_grey()



## Generic three wave 


coords_ms1b <- tibble::tribble(~ name, ~ x,  ~ y,
                               "ULAY", 0, 8,
                               "L0", 0, 2,
                               "Leta0", 0, 0,
                               "UA", 2, 6,
                               "A1", 2, 4,
                               "Aeta1",2,2,
                               "UY", 4, 8,
                               "Y2", 4, 6,
                               "Yeta2", 4,2)

dagify(L0 ~ ULAY + Leta0,
       A1 ~ Aeta1 + UA, 
       Y2 ~ Yeta2 + UY,
       Aeta1 ~ L0,  # good measurement of confounders
       Yeta2 ~ Leta0,
       UA ~ ULAY, 
       UY ~ ULAY,
       coords = coords_ms1b
) %>%
  ggdag_dseparated("Y2", "A1",
                   controlling_for = c("L0"),
                   collider_lines = F
  ) + theme_dag_grey()


# no orthogonal confounding


coords_ms1b <- tibble::tribble(~ name, ~ x,  ~ y,
                               "ULAY", 0, 8,
                               "L0", 0, 2,
                               "Leta0", 0, 0,
                               "UA", 2, 6,
                               "A1", 2, 4,
                               "Aeta1",2,2,
                               "UY", 4, 8,
                               "Y2", 4, 6,
                               "Yeta2", 4,2)

dagify(L0 ~ ULAY + Leta0,
       A1 ~ Aeta1 + UA, 
       Y2 ~ Yeta2 + UY,
       Aeta1 ~ L0,  # good measurement of confounders
       Yeta2 ~ Leta0,
     #  UA ~ ULAY, 
     #  UY ~ ULAY,
       coords = coords_ms1b
) %>%
  ggdag_dseparated("Y2", "A1",
                   controlling_for = c("L0"),
                   collider_lines = F
  ) + theme_dag_grey()



# orthogonal confounding


coords_ms1b <- tibble::tribble(~ name, ~ x,  ~ y,
                               "ULAY", 0, 4,
                               "L0", 0, 1,
                               "Leta0", 0, 0,
                               "UA", 1, 3,
                               "A1", 1, 2,
                               "Aeta1",1,1,
                               "UY", 2, 4,
                               "Y2", 2, 2,
                               "Yeta2", 2,1)

dagify(L0 ~ ULAY + Leta0,
       A1 ~ Aeta1 + UA, 
       Y2 ~ Yeta2 + UY,
       Aeta1 ~ L0,  # good measurement of confounders
       Yeta2 ~ Leta0,
       UY ~ ULAY + Aeta1,
       coords = coords_ms1b
) %>%
  ggdag_dseparated("Y2", "A1",
                   controlling_for = c("L0"),
                   collider_lines = F
  ) + theme_dag_grey()



# no orthogonal confounding but path opened from Aeta1 to UY



# no orthogonal confounding


coords_ms1b <- tibble::tribble(~ name, ~ x,  ~ y,
                               "ULAY", 0, 4,
                               "L0", 0, 1,
                               "Leta0", 0, 0,
                               "UA", 2, 4,
                               "A1", 2, 2,
                               "Aeta1",2,1,
                               "UY", 4, 4,
                               "Y2", 4, 3,
                               "Yeta2", 4,2)

dag <- dagify(L0 ~ ULAY + Leta0,
       A1 ~ Aeta1 + UA, 
       Y2 ~ Yeta2 + UY,
       Aeta1 ~ Leta0,  # good measurement of confounders
       Yeta2 ~ L0,
      # UY ~ A1, # path from A1 to UY
      # UA ~ ULAY,  no orthogonal conofunders
      # UY ~ ULAY,  no orthogonal conofunders,
      latent = c("UA","UY", "ULAY", "Leta0", "Aeta1", "Yeta2"),
      exposure = "A1",
      outcome = "Y2",
      coords = coords_ms1b
) 



# Adjustment set
ggdag_adjustment_set(dag, #text = FALSE, use_labels = "label", 
                     shadow = TRUE)+ theme_dag_grey()



 dag%>%
  ggdag_dseparated("Y2", "A1",
                   controlling_for = c("L0"),
                   collider_lines = F
  ) + theme_dag_grey()


 
 
### Independent non-differential error -- NO BIAS UNDER THE NULL


 
 
 
 coords_m_indep_nondiff <- tibble::tribble(~ name, ~ x,  ~ y,
                                "UA", 0, 4,
                                "A", 0, 2,
                                "Aeta",0,0,
                                "UY", 2, 4,
                                "Y", 2, 2,
                                "Yeta", 2,0)
 
 dag_m_indep_nondiff <- dagify(A ~ UA + Aeta,
               Y ~ UY + Yeta,
            #   Y ~ A,
               latent = c("UA","UY","Aeta","Yeta" ),
               exposure = "A",
               outcome = "Y",
               coords = coords_m_indep_nondiff
 ) 
 
 
 
 # Adjustment set
 ggdag_adjustment_set(dag_m_indep_nondiff, #text = FALSE, use_labels = "label", 
                      shadow = TRUE)+ theme_dag_grey()
 
 
 
 dag_m_indep_nondiff%>%
   ggdag_dseparated("Y", "A",
                  #  controlling_for = c("L0"),
                    collider_lines = T
   ) + theme_dag_grey()

 

# Dependent non differential ----------------------------------------------

 
 
 coords_m_dep_nondiff <- tibble::tribble(~ name, ~ x,  ~ y,
                                           "UAY", 1,6,
                                           "UA", 0, 4,
                                           "A", 0, 2,
                                           "Aeta",0,0,
                                           "UY", 2, 4,
                                           "Y", 2, 2,
                                           "Yeta", 2,0)
 
 dag_m_dep_nondiff <- dagify(A ~ UA + Aeta,
                               Y ~ UY + Yeta,
                              # Y ~ A,
                               UA ~ UAY,
                               UY ~ UAY,
                               latent = c("UA","UY", "UAY", "Aeta","Yeta" ),
                               exposure = "A",
                               outcome = "Y",
                               coords = coords_m_dep_nondiff
 ) 
 
 
 
 # Adjustment set
 ggdag_adjustment_set(dag_m_dep_nondiff, #text = FALSE, use_labels = "label", 
                      shadow = TRUE)+ theme_dag_grey()
 
 
 
 dag_m_dep_nondiff%>%
   ggdag_dseparated("Y", "A",
                    #  controlling_for = c("L0"),
                    collider_lines = TRUE
   ) + theme_dag_grey()
 
 
 
 
 ## independent differential
 
 coords_m_indep_diff <- tibble::tribble(~ name, ~ x,  ~ y,
                                           "UA", 0, 4,
                                           "A", 0, 2,
                                           "Aeta",0,0,
                                           "UY", 2, 4,
                                           "Y", 2, 2,
                                           "Yeta", 2,0)
 
 dag_m_indep_diff <- dagify(A ~ UA + Aeta,
                               Y ~ UY + Yeta,
                               UY ~ Aeta,
                               latent = c("UA","UY", "Aeta","Yeta" ),
                               exposure = "A",
                               outcome = "Y",
                               coords = coords_m_indep_diff
 ) 
 
 
 
 # Adjustment set
 ggdag_adjustment_set(dag_m_indep_diff, #text = FALSE, use_labels = "label", 
                      shadow = TRUE)+ theme_dag_grey()
 
 
 
 dag_m_indep_diff%>%
   ggdag_dseparated("Y", "A",
                    #  controlling_for = c("L0"),
                    collider_lines = T
   ) + theme_dag_grey()
 
 
 
 
 

# Dependent differential measurement error --------------------------------

 
 
 coords_m_dep_diff <- tibble::tribble(~ name, ~ x,  ~ y,
                                         "UAY", 1,6,
                                         "UA", 0, 4,
                                         "A", 0, 2,
                                         "Aeta",0,0,
                                         "UY", 2, 4,
                                         "Y", 2, 2,
                                         "Yeta", 2,0)
 
 dag_m_dep_diff <- dagify(A ~ UA + Aeta,
                             Y ~ UY + Yeta,
                             # Y ~ A,
                             UA ~ UAY,
                             UY ~ UAY + Aeta,
                             latent = c("UA","UY", "UAY", "Aeta","Yeta" ),
                             exposure = "A",
                             outcome = "Y",
                             coords = coords_m_dep_diff
 ) 
 
 
 
 # Adjustment set
 ggdag_adjustment_set(dag_m_dep_diff, #text = FALSE, use_labels = "label", 
                      shadow = TRUE)+ theme_dag_grey()
 
 
 
 dag_m_dep_diff%>%
   ggdag_dseparated("Y", "A",
                    #  controlling_for = c("L0"),
                    collider_lines = TRUE
   ) + theme_dag_grey()
 
 
 

# Surrogate confounding ---------------------------------------------------

 
 coords_surrogate_confounder<- tibble::tribble(~ name, ~ x,  ~ y,
                                                #  "UL", 0,0,
                                                  "Leta", 0, 2,
                                                  "L", 0,1,
                                                  "A", 1,2,
                                                  "Y", 2,3)
 
 
 
 dag_surrogate_confounder  <- dagify(Y ~ Leta,
                                       L ~ Leta,
                                       A ~ Leta,
                                      #latent = c(),
                                       exposure = "A",
                                       outcome = "Y",
                                       coords = coords_surrogate_confounder_2
 ) 
 
 
 
 # Adjustment set
 ggdag_adjustment_set(dag_surrogate_confounder, #text = FALSE, use_labels = "label", 
                      shadow = TRUE)+ theme_dag_grey()
 
 
 
 
 dag_m_dep_diff%>%
   ggdag_dseparated("Y", "A",
                    #  controlling_for = c("L0"),
                    collider_lines = TRUE
   ) + theme_dag_grey()
 
 
 
 ## Equivalent to 

# surrogate confounding as measurement error ------------------------------

 
 
 coords_surrogate_confounder_2 <- tibble::tribble(~ name, ~ x,  ~ y,
                                                "UL", 0,0,
                                                "Leta", 0, 2,
                                                "L", 0,1,
                                                "A", 1,2,
                                                "Y", 2,3)
 
 
 dag_surrogate_confounder_2  <- dagify(Y ~ Leta,
                                       L ~ Leta + UL,
                                       A ~ Leta,
                                       UA ~ UAY,
                                       UY ~ UAY + Aeta,
                                  #     latent = c("Leta", "UL"),
                                     exposure = "A",
                                     outcome = "Y",
                                     coords = coords_surrogate_confounder_2
 ) 
 
 
 
# Adjustment set
 ggdag_adjustment_set(dag_surrogate_confounder_2, #text = FALSE, use_labels = "label", 
                      shadow = TRUE)+ theme_dag_grey()
 
 
 
 
 dag_m_dep_diff%>%
   ggdag_dseparated("Y", "A",
                    #  controlling_for = c("L0"),
                    collider_lines = TRUE
   ) + theme_dag_grey()
 
 
 

# more complex structures  ------------------------------------------------

 
 coords_surrogate_confounder_3 <- tibble::tribble(~ name, ~ x,  ~ y,
                                                  "UL", 0,1,
                                                  "Leta", 0, 3,
                                                  "L", 0,2,
                                                  "UA", 2, 1,
                                                  "Aeta", 2,3,
                                                  "A", 2, 2,
                                                  "UY",4,1,
                                                  "Yeta", 4,3,
                                                  "Y", 4,2,
                                                  "ULA",1, 0
                                                  )
 
 
 dag_surrogate_confounder_3 <- dagify(UL ~ ULA,
                                       L ~ Leta + UL,
                                       Aeta ~ Leta,
                                       A ~ Aeta + UA,
                                       UA ~ ULA,
                                       Y ~ UY + Yeta,
                                      # Yeta ~ Aeta,
                                       exposure = "A",
                                       outcome = "Y",
                                    #   latent = c("Aeta", "Yeta", "Leta", "UL", "UA", "UY"),
                                       coords = coords_surrogate_confounder_3
 ) 
 
 
 
 # Adjustment set
 ggdag_adjustment_set(dag_surrogate_confounder_3, #text = FALSE, use_labels = "label", 
                      shadow = TRUE)+ theme_dag_grey()
 
 
 
 
 dag_surrogate_confounder_3%>%
   ggdag_dseparated("Y", "A",
                    #  controlling_for = c("L0"),
                    collider_lines = TRUE
   ) + theme_dag_grey()
 
 
 
 
 # more complex structures again  ------------------------------------------------
 
 
 coords_surrogate_confounder_4 <- tibble::tribble(~ name, ~ x,  ~ y,
                                                  "UL", 0,1,
                                                  "Leta", 0, 3,
                                                  "L", 0,2,
                                                  "UA", 2, 1,
                                                  "Aeta", 2,3,
                                                  "A", 2, 2,
                                                  "UY",4,1,
                                                  "Yeta", 4,3,
                                                  "Y", 4,2,
                                                  "ULY",2, 0
 )
 
 
 dag_surrogate_confounder_4 <- dagify(UL ~ ULY,
                                      L ~ Leta + UL,
                                      Aeta ~ Leta,
                                      A ~ Aeta + UA + L,
                                      UL ~ ULY,
                                      UY ~ ULY,
                                      Y ~ UY + Yeta,
                                     # Yeta ~ Aeta,
                                      exposure = "A",
                                      outcome = "Y",
                                   latent = c("Aeta", "Yeta", "Leta", "UL", "UA", "UY", "ULY"),
                                      coords = coords_surrogate_confounder_4
 ) 
 
 
 
 # Adjustment set
 ggdag_adjustment_set(dag_surrogate_confounder_4, #text = FALSE, use_labels = "label", 
                      shadow = TRUE)+ theme_dag_grey()
 
 
 
 
 dag_surrogate_confounder_4%>%
   ggdag_dseparated("Y", "A",
                    controlling_for = c("L"),
                    collider_lines = TRUE
   ) + theme_dag_grey()

 
 ## 
 