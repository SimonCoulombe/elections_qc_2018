install.packages("here")
library(tidyverse)

## sondages 2014 et 2018 ----

# https://newsinteractives.cbc.ca/qcvotes/poll-tracker/

sondage2018 <- data.frame(
  firme = c("Forum",
            "Research",
            "Ipsos",
            "Mainstreet",
            "Léger",
            "Ipsos",
            "Research",
            "Léger",
            "CROP",
            "Forum"   
  ),
  plq = c(28,
          30,
          31,
          29,
          30,
          30,
          30,
          30,
          37,
          22),
  pq = c(20,
         18,
         18,
         20,
         19,
         20,
         19,
         21,
         16,
         24
  ),
  caq = c(
    33,
    33,
    32,
    32,
    32,
    30,
    32,
    31,
    30,
    32  ),
  qs = c(17,
         16,
         16,
         16,
         17,
         16,
         16,
         14,
         14,
         19
  ),
  autres = c(2,
            3,
            3,
            4,
            2,
            4,
            3,
            4,
            3,
            2
  ),
  sample = c(1716,
             625,
             125,
             176,
             1502,
             125,
             601,
             3017,
             1000,
             1274
  )
)
final2018 <- sondage2018[c(1,5,9),] %>%
  summarise_at(vars(plq, pq, caq, qs, autres),
               funs(weighted.mean(.,sample))) %>%
  gather(key=parti, value =sond2018 )

# https://www.cbc.ca/elections/quebecvotes2014/features/view/poll-tracker

sondage2014 <- data.frame(
  firme = c("Léger",
            "Léger",
            "Forum Research",
            "CROP",
            "Léger",
            "CROP",
            "Léger"),
  plq = c(38,
          40,
          45,
          39,
          37,
          36,
          35
  ),
  pq = c(29,
         33,
         34,
         36,
         37,
         36,
         37
  ),
  caq = c(23,
          15,
          13,
          13,
          14,
          17,
          15
  ),
  qs = c(9,
         9,
         7,
         10,
         9,
         8,
         8
  ),
  sample = c(1220,
             3397,
             1650,
             1400,
             1205,
             1400,
             1502
  )
)  %>% 
  mutate(autres = 100 - plq - pq - caq -qs) %>% 
  select(firme, plq,pq, caq, qs, autres, sample)

final2014 <- sondage2014[c(1,3,4),] %>%
  summarise_at(vars(plq, pq, caq, qs, autres),
               funs(weighted.mean(.,sample)))%>%
  gather(key=parti, value =sond2014 )

## résultats 2014 ----

download.file("https://www.electionsquebec.qc.ca/documents/zip/resultats-section-vote/2014-04-07.zip",
              destfile = "2014-04-07.zip")
# this break files with accent, just unzip in terminal instead
#utils::unzip(zipfile = "2014-04-07.zip", exdir = here::here("data/")) 

results <- list.files(path= here::here("data"), 
                   pattern="*.csv",
                   full.names = T) %>% 
  map_df(~read_csv2(., locale = locale(encoding = "ISO-8859-1")) %>%
           rename(CO_CEP = Code,
                  NO_SV = S.V., 
                  municipalite = `Nom des Municipalités`,
                  etendue = `Étendue`,
                  date = `Date scrutin`,
                  ei = `É.I.`,
                  circonscription = Circonscription,
                  secteur = Secteur,
                  regroupement = Regroupement,
                  bv = B.V.,
                  br = B.R.) %>%
  mutate(PLQ = rowSums(.[grep("P.L.Q.", names(.))], na.rm = TRUE),
         PQ = rowSums(.[grep("P.Q.", names(.))], na.rm = TRUE),
         CAQ = rowSums(.[grep("C.A.Q.", names(.))], na.rm = TRUE),
         QS = rowSums(.[grep("Q.S.", names(.))], na.rm = TRUE),
         autres =  bv - PLQ - PQ - CAQ - QS,
         NO_SV =  str_replace(NO_SV, "[A-Z]", ""), #certains sont numériques, d'autres caractères.
         # aussi, parfois on a NO_SV = 65A ou 65B au lieu de 65 ( exemple dans gouin co_cep 381). 
         #solution: on eneleve la lettre et on somme le tout avant de continuer)
         CO_CEP = as.character(CO_CEP)) %>%
  filter(municipalite == "Total de la circonscription") %>% 
  mutate(
    pct_plq = ifelse(bv> 0, round(100* PLQ / bv,1), 0),
    pct_pq = ifelse(bv> 0, round(100* PQ / bv,1), 0),
    pct_caq = ifelse(bv> 0, round(100* CAQ / bv,1), 0),
    pct_qs = ifelse(bv> 0, round( 100* QS / bv,1), 0)) %>% 
  select(circonscription,ei,bv, br, PLQ, PQ, CAQ, QS, autres, pct_plq, pct_pq, pct_caq, pct_qs)) 

summary_results2014 <- results %>% 
  summarise_at(vars(bv, PLQ, PQ, CAQ, QS, autres),
               sum)   %>% 
  mutate(
    plq = ifelse(bv> 0, round(100* PLQ / bv,1), 0),
    pq = ifelse(bv> 0, round(100* PQ / bv,1), 0),
    caq = ifelse(bv> 0, round(100* CAQ / bv,1), 0),
    qs = ifelse(bv> 0, round( 100* QS / bv,1), 0),
    autres =  ifelse(bv> 0, round( 100* autres / bv,1), 0))   %>%
  select(plq, pq, caq, qs, autres) %>%
  gather(key=parti, value =elect2014 )

summary_results2014 %>%
  left_join(final2014) %>% 
  left_join(final2018)

