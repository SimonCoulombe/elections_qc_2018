# librairies ----
library(tidyverse)
library(sf)
library(ggplot2)
library(rgdal) #reads in shapefiles
library(scales) # tells ggplot what the proper scale should be
library(ggmap) #  comes with a nice theme_nothing() function
library(dplyr)  #I only use the left_join() function but this is a very useful package to add to your toolbox if you haven’t already
library(Cairo) #creates high quality vector and bitmap images
library(broom)  
library(rmapshaper) #pour simplifier les polygones
library(leaflet)
library(readr)
library(stringr)
library(htmlwidgets)

# downloads results and shapefile ----
##  résultats 2018 
# download, manually unzip to /data folder
download.file("https://www.electionsquebec.qc.ca/documents/zip/resultats-section-vote/2018-10-01.zip",
              destfile = "2018-10-01.zip")

## shapefile
download.file("https://www.electionsquebec.qc.ca/documents/zip/sections_vote_08_08_2018_shapefile.zip",
              destfile = "sections_vote_08_08_2018_shapefile.zip")
# manually unzip to /shp folder

# wrangle data ----

mydf <- list.files(path= "./data", 
                  pattern="DGE*",
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
                  br = B.R.)%>%
           mutate(PLQ = rowSums(.[grep("P.L.Q.", names(.))], na.rm = TRUE),
                  PQ = rowSums(.[grep("P.Q.", names(.))], na.rm = TRUE),
                  CAQ = rowSums(.[grep("C.A.Q.", names(.))], na.rm = TRUE),
                  QS = rowSums(.[grep("Q.S.", names(.))], na.rm = TRUE),
                  autres =  bv - PLQ - PQ - CAQ - QS,
                  NO_SV =  str_replace(NO_SV, "[A-Z]", ""), #certains sont numériques, d'autres caractères.
                  # aussi, parfois on a NO_SV = 65A ou 65B au lieu de 65 ( exemple dans gouin co_cep 381). 
                  #solution: on eneleve la lettre et on somme le tout avant de continuer)
                  CO_CEP = as.character(CO_CEP)) %>%
           filter(!(municipalite == "Total de la circonscription"),
                  !(str_sub(municipalite,1,8) == "Majorité")) %>% 
           group_by(CO_CEP, NO_SV, circonscription,
                    date, etendue, municipalite, secteur ) %>% # pas la variable regroupement ici a cause d'une circonstription dans
           #jean lesage qui a  CO_CEP et NO_SV identique, mais regroupement différent..
           summarise_at(vars(ei,bv, br, PLQ, PQ, CAQ, QS, autres),
                        funs(sum))%>%
           ungroup() %>%
           mutate(
             pct_plq = ifelse(bv> 0, round(100* PLQ / bv,1), 0),
             pct_pq = ifelse(bv> 0, round(100* PQ / bv,1), 0),
             pct_caq = ifelse(bv> 0, round(100* CAQ / bv,1), 0),
             pct_qs = ifelse(bv> 0, round( 100* QS / bv,1), 0)))

circnames <- mydf %>% distinct(CO_CEP, circonscription)



#shp secteur de vote
sv_spdf <- st_read(
  "./shp/Sections_vote_08_08_2018_shapefile/Section_de_vote.shp",
  stringsAsFactors = FALSE) %>%
  mutate(CO_CEP = as.character(CO_CEP_NC), NO_SV = as.character(NO_SV_NC))  %>% #pour merge avec resultats
  select(CO_CEP, NO_SV, geometry) %>% 
  arrange(CO_CEP, NO_SV)

sv_spdf1 <- sv_spdf %>% sf::st_transform( crs = 4326) 
rm(sv_spdf)
sv_spdf2 <- sv_spdf1 %>% rmapshaper::ms_simplify()
sv_spdf_simple <- ms_simplify(sv_spdf)
box <- bbox(sv_spdf_simple)


#shp contour de circonscriptions
circ_spdf <- st_read(
  "./data/shp/circonscriptions/Circonscriptions électorales 2011.shp",
  stringsAsFactors = FALSE) %>%
  mutate(CO_CEP = as.character(CO_CEP)) %>% #pour merge avec resultats %>% 
  as(., 'Spatial') %>%
  spTransform(., CRS("+proj=longlat +ellps=WGS84"))

circ_spdf_simple <-    ms_simplify(circ_spdf) 
bbox(circ_spdf_simple)


Sections_vote_08_08_2018_shapefile/Section_de_vote.shp
