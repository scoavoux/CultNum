library(tidyverse)
library(readxl)
library(haven)
library(here)

panel <- read_sas(here("01_data", "panel7318.sas7bdat"))
pc97 <- read_sas(here("01_data", "pc97_15fev2021.sas7bdat"))
pc08 <- read_sas(here("01_data", "pc08_15fev2021.sas7bdat"))

# Créer labels pc08
library(yaml)
labs_pc08 <- read_yaml(here("01_data", "pc08_dic.yaml"))

lmod <- map(labs_pc08, function(x){
  if(!is.null(x$L)) {
    tibble(lab = x$V, L = names(x$L), modlabel = unlist(x$L))
  } else {
    tibble(lab = x$V, L = NA, modlabel = NA)
  }
}) %>% bind_rows()
lvar <- map(labs_pc08, function(x){
  if(!is.null(x$O)) {
    tibble(lab = x$V, O = names(x$O), qcmlabel = unlist(x$O))
  } else {
    tibble(lab = x$V, O = NA, qcmlabel = NA)
  }
}) %>% bind_rows()

# extraire les noms de variables, etc.
# faire un dico des codes propres.

lvar <- separate(lvar, col = lab, into = c("var", "type"), sep = "\\s", remove=FALSE) %>% 
  mutate(
    var = str_remove(var, "\\[.*?\\]"),
    var = ifelse(is.na(O), 
                 var,
                 paste0(var, "_O", O)),
    lab = str_remove(lab, "^.*?\""),
    lab = str_remove_all(lab, "\""),
    lab = str_remove_all(lab, "-{2,}"),
    lab = ifelse(is.na(O), 
                 lab,
                 paste0(var, " : ", qcmlabel))) %>% 
  select(variable = var, varlabel = lab, type)

lmod <- separate(lmod, col = lab, into = c("variable", "type"), sep = "\\s", remove=FALSE) %>% 
  select(-lab) %>% 
  left_join(lvar)

lvar <- filter(lmod, str_detect(type, "M")) %>% 
  mutate(variable = paste0(variable, "_M", L),
         varlabel = paste0(variable, " : ", modlabel)) %>% 
  select(variable, varlabel, type) %>% 
  bind_rows(lvar, .)

lvar <- mutate(lvar, varlabel = str_wrap(varlabel, width = 50))

labs_pc08 <- lvar

labs_panel <- tribble(~variable, ~varlabel, 
                      "id", "Identifiant",
                      "annee", "Annee du millésime d enquête",
                      "poids", "Poids normé",
                      "poidsgen", "Poids non normé pour les calculs par génération",
                      "agen", "Age",
                      "age4", "Age en 4 postes",
                      "age5", "Age en 5 postes",
                      "age8", "Age en 8 postes",
                      "cs_regroupée", "Catégorie socioprofessionnelle du répondant",
                      "dip", "Niveau de diplôme en 3 postes",
                      "dipp", "Niveau de diplôme en 4 postes",
                      "habn", "Tranche d'unité urbaine en 6 postes ",
                      "sexn", "Sexe",
                      "nb_cine", "Nombre de sorties au cinéma dans les 12 derniers mois",
                      "cine3", "Intensité de fréquentation des cinémas dans les 12 derniers mois en 4 modalités",
                      "cine3_1", "Fréquentation faible des cinémas (cine3=1)",
                      "cine3_2", "Fréquentation moyenne des cinémas (cine3=2)",
                      "cine3_3", "Fréquentation forte des cinémas (cine3=3)",
                      "nb_livres", "Nombre de livres lus dans les 12 derniers mois",
                      "nb_bd", "Nombre de BD lues dans les 12 derniers mois",
                      "livres3", "Intensité de lecture dans les 12 derniers mois en 4 modalités",
                      "livres3_1", "Faible lecteur (livres3=1)",
                      "livres3_2", "Moyen lecteur (livres3=2)",
                      "livres3_3", "Grand lecteur (livres3=3)",
                      "médiathèquesinsc_biblio", "Inscrit dans une bibliothèque/médiathèque",
                      "freq_biblio", "Fréquente une bibliothèque/médiathèque",
                      "inscfreq_biblio", "Inscrit et fréquente une bibliothèque/médiathèque",
                      "prat_tv", "prat_tv",
                      "tljp_tv", "tljp_tv",
                      "moyh_tv", "moyh_tv",
                      "prat_radio", "prat_radio",
                      "tljp_radio", "tljp_radio",
                      "moyh_radio", "moyh_radio",
                      "prat_musique", "prat_musique",
                      "tljp_musique", "tljp_musique",
                      "prat_jv", "prat_jv",
                      "tljp_jv", "tljp_jv",
                      "prat_internet", "prat_internet",
                      "tljp_internet", "tljp_internet",
                      "prat_reseaux", "prat_reseaux",
                      "tljp_reseaux", "tljp_reseaux",
                      "prat_presse", "prat_presse",
                      "tljp_presse", "tljp_presse",
                      "prat_livres_seul", "prat_livres_seul",
                      "prat_bd", "prat_bd",
                      "prat_cine", "prat_cine",
                      "prat_danse", "prat_danse",
                      "prat_theatre", "prat_theatre",
                      "prat_concertclas", "prat_concertclas",
                      "prat_concertrock", "prat_concertrock",
                      "prat_concertjazz", "prat_concertjazz",
                      "prat_concertrocjaz", "prat_concertrocjaz",
                      "prat_varietes", "prat_varietes",
                      "prat_cirque", "prat_cirque",
                      "prat_spectaclam", "prat_spectaclam",
                      "prat_festival", "prat_festival",
                      "prat_musee", "prat_musee",
                      "prat_expo", "prat_expo",
                      "prat_musexpo", "prat_musexpo",
                      "prat_monum", "prat_monum",
                      "pratam_musicale", "pratam_musicale",
                      "pratam_ecriture", "pratam_ecriture",
                      "pratam_peinture", "pratam_peinture",
                      "pratam_poterie", "pratam_poterie",
                      "pratam_dessin", "pratam_dessin",
                      "pratam_theatre", "pratam_theatre",
                      "pratam_danse", "pratam_danse",
                      "pratam_photo", "pratam_photo",
                      "pratam_nonmusicale", "pratam_nonmusicale",
                      "prat_sortiesoir", "prat_sortiesoir",
                      "prat_fetefo", "prat_fetefo",
                      "prat_jardinage", "prat_jardinage",
                      "prat_tricot", "prat_tricot",
                      "prat_sport", "prat_sport",
                      "prat_zoo", "prat_zoo",
                      "prat_cartes", "prat_cartes",
                      "jamais_cine", "jamais_cine",
                      "jamais_danse", "jamais_danse",
                      "jamais_theatre", "jamais_theatre",
                      "jamais_concertclas", "jamais_concertclas",
                      "jamais_concertrock", "jamais_concertrock",
                      "jamais_concertjazz", "jamais_concertjazz",
                      "jamais_concertrocjaz", "jamais_concertrocjaz",
                      "jamais_varietes", "jamais_varietes",
                      "jamais_cirque", "jamais_cirque",
                      "jamais_spectaclam", "jamais_spectaclam",
                      "jamais_musee", "jamais_musee",
                      "jamais_expo", "jamais_expo",
                      "jamais_musexpo", "jamais_musexpo",
                      "jamais_monum", "jamais_monum")

save(panel, pc97, pc08, labs_pc08, labs_panel, file = here("01_data", "pc97-08-panel.RData"))
