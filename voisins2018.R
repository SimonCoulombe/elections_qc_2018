# librairies ----
library(tidyverse)
library(sf) # wrangle spatial data
library(rmapshaper) # simplify polygons
library(leaflet) #  create slippy map
library(htmlwidgets) # save to html

# couleurs
partiPalette <- c("#00A8E7", "#ED1C2E", "#004185", "#FF5505") # CAQ , PLQ, PQ, QS

# downloads results and shapefile ----
##  résultats 2018 
# download, manually unzip to /data folder due to weird encoding issues
download.file("https://www.electionsquebec.qc.ca/documents/zip/resultats-section-vote/2018-10-01.zip",
              destfile = "2018-10-01.zip")

## shapefile
download.file("https://www.electionsquebec.qc.ca/documents/zip/sections_vote_08_08_2018_shapefile.zip",
              destfile = "sections_vote_08_08_2018_shapefile.zip")

download.file("https://www.electionsquebec.qc.ca/documents/zip/circonscriptions_electorales_2017_shapefile.zip",
              destfile = "circonscriptions_electorales_2017_shapefile.zip")
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
  arrange(CO_CEP, NO_SV) %>%
  sf::st_transform( crs = 4326)  %>%
  rmapshaper::ms_simplify(keep =0.01)


#shp contour de circonscriptions
circ_spdf <- st_read(
  "./shp/Circonscriptions_Вlectorales_2017_shapefile.shp",
  stringsAsFactors = FALSE) %>%
  mutate(CO_CEP = as.character(CO_CEP)) %>% #pour merge avec resultats %>% 
   sf::st_transform( crs = 4326)  %>%
  ms_simplify(.) 


################################################################################
## check pour doublons
################################################################################
#pas de doublon dans mydf :
mydf %>% group_by(CO_CEP, NO_SV, municipalite) %>%  count() %>% filter(n > 2)

# pas de doublon dans sv_spdf  quand on droppe les 9999
sv_spdf %>% filter(NO_SV != "999") %>% group_by(CO_CEP, NO_SV) %>%  count() %>% filter(n > 2)


################################################################################
# préparer données et popup pour leafet
################################################################################
# il faut que mes résultats (mydata) et popups (mydatapopup) aient le même
# nombre de lignes que mon shapefile (sv_spdf_simple ) et qu'elles soient
# dans le même ordre
################################################################################

mydata <- sv_spdf %>% filter(NO_SV != "999") %>% 
  left_join(circnames, by ="CO_CEP") %>% 
  left_join(mydf %>% select(-circonscription), by=c("CO_CEP", "NO_SV")) %>%
  mutate( pct_max = pmax(pct_plq, pct_pq, pct_caq, pct_qs),
          gagnant = as.factor(case_when(
            pct_plq == pct_max ~ "PLQ",
            pct_pq == pct_max ~ "PQ",
            pct_caq == pct_max ~ "CAQ",
            pct_qs == pct_max ~ "QS"
          )))


mydatapopup <- paste0("<strong>Circonscription: </strong>",
                      mydata$circonscription, " (",  mydata$CO_CEP, ")",
                      " <br><strong>Secteur de vote: </strong>",
                      mydata$NO_SV,
                      " <br><strong>Municipalité: </strong>",
                      mydata$municipalite,
                      " <br><strong>Bulletins valides / Électeurs inscrits: </strong>",
                      mydata$bv,   " / " ,mydata$ei,
                      " <br><strong>% PLQ: </strong>",
                      mydata$pct_plq,                                        
                      " <br><strong>% PQ: </strong>",
                      mydata$pct_pq,                                        
                      " <br><strong>% CAQ: </strong>",
                      mydata$pct_caq,                                        
                      " <br><strong>% QS: </strong>",
                      mydata$pct_qs)


################################################################################
# création de la palette et du leaflet
################################################################################


mypal_bin <- colorFactor(palette = partiPalette, 
                      domain = mydata$gagnant)



mymap <- leaflet(mydata)%>%  
  addProviderTiles(providers$Stamen.TonerLite)  %>%
  addPolygons( fillColor = ~ mypal_bin(mydata$gagnant),   # couleur du gagnant
               fillOpacity = ~ pct_max/100,         ## opacity proportionelle au pourcentage de votes du gagnant
               color = "white",       ## color of borders between voting sections 
               weight = 0.5,
               popup = mydatapopup) %>% 
  addPolygons( data = circ_spdf,
               color = "black",
               fill = F,
               weight = 1.5) %>%
  addLegend(position = 'topright',
            pal = mypal_bin,
            values = mydata$gagnant) %>%
  setView( lng= -71.25, lat= 46.78, zoom=11 , options = list())

mymap

saveWidget( mymap , "elections_qc_2018.html", selfcontained = F)
