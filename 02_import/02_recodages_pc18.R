library(tidyverse)
library(forcats)

# Netoyer les labels
labs <- mutate(labs, 
               varlabel = ifelse(is.na(varlabel_frombase), varlabel_fromdic, varlabel_frombase),
               varlabel = str_wrap(varlabel, width = 40))

# Collapse diplômes
d <- mutate(d, dipl = fct_collapse(DIPLOME,
                    `Aucun diplôme à BEPC` = c("'Jamais allé à l'école ou a quitté l'école avant la primaire'", 
                                              "'Aucun diplôme et scolarité interrompue à la fin du primaire ou avant la fin du collège'", 
                                              "'Aucun diplôme et scolarité jusqu''à la fin du collège ou au-delà'", 
                                              "'CEP (certificat d''études primaires)'", 
                                              "'BEPC, brevet élémentaire, brevet des collèges, DNB'"),
                    CAP_BEP = "'CAP, BEP ou diplôme de niveau équivalent'", 
                    Bac = c("'Baccalauréat général ou technologique, brevet supérieur'", 
                            "'Capacité en droit, DAEU, ESEU'", 
                            "'Baccalauréat professionnel, brevet professionnel, de technicien ou d''enseignement, diplôme équivalent'"), 
                    `Bac+2` = "'BTS, DUT, Deug, Deust, diplôme de la santé ou du social de niveau bac+2, diplôme équivalent'", 
                    `Bac+3` = "'Licence, licence pro, maîtrise, diplôme équivalent de niveau bac+3 ou bac+4'", 
                    `Deuxieme cycle` = c("'Master, DEA, DESS, diplôme grande école niveau bac+5, doctorat de santé'", 
                                         "'Doctorat de recherche (hors santé)'"),
                    `NA` = c("'(NSP)'", "'(REF)'")))

labs <- add_case(labs, variable = "dipl", category = "recodage", varlabel = "Diplôme recodé")

# Collapse PCS
d <- mutate(d, 
            pcs1 = str_trunc(CSTOT, 1, ellipsis = ""),
            pcs1 = factor(pcs1, levels = c(1:6, 9), 
                          labels = c("Agric. expl.", "Art., comm., ce", 
                                     "CPIS", "Prof. inter.",
                                     "Employés", "Ouvriers",
                                     "Inactifs")))

d <- mutate(d, 
            support_musique_digital = factor(paste0(E83, E84, E85),
                                             levels = c(
                                               "'No''No''No'",
                                               "'Yes''No''No'",
                                               "'Yes''Yes''No'",
                                               "'Yes''No''Yes'",
                                               "'No''No''Yes'",
                                               "'No''Yes''No'",
                                               "'No''Yes''Yes'",
                                               "'Yes''Yes''Yes'"),
                                             labels = c(
                                               "N'écoute pas de musique en numérique",
                                               "Plateforme spécialisée",
                                               "Plateforme spécialisée + non spécialisée",
                                               "Plateforme spécialisée + Téléchargement",
                                               "Téléchargement",
                                               "Plateforme non spécialisée",
                                               "Plateforme non spécialisée + téléchargement",
                                               "Plateforme spécialisée + non spécialisée + téléchargement")
            ))

labs <- add_case(labs, variable = "pcs1", category = "recodage", varlabel = "PCS ego niveau 1")
labs <- add_case(labs, variable = "support_musique_digital", category = "recodage", varlabel = "Support écoute de musique numérique")

# Changer label de sexe
d <- mutate(d, SEXE = fct_recode(SEXE, Hommes = "'Masculin'", Femmes = "'Féminin'"))

# (FG) Recodage AGE en classes pour demandes PI_TV
range(d$AGE)
d$age_c <- cut(d$AGE, c(15, 24, 39, 54, 64, 97), include.lowest = TRUE, labels = c("15-24", "25-39", "40-54", "55-64","65+"))

# (FG) Creation d'une variable "tpsTVjour" estimant le tps moyen par jour passé devant la tv, pour demandes PI_TV
# Recodage à vérifier car résultat faux
d$C11 <- as.character(d$C11)
d$x1 <- case_when(
  d$C11 == "'Par jour'" ~ d$C10_C_1 * 5,
  d$C11 == "'Par semaine'" ~ d$C10_C_1,
  d$C11 == "'Par mois'" ~ d$C10_C_1 / 4
)

d$C13 <- as.character(d$C13)
d$x2<- case_when(
  d$C13 == "'Par jour'" ~ d$C12_C_1*2,
  d$C13 == "'Par week-end'" ~ d$C12_C_1
)

d$tpsTVjour <- (d$x1 + d$x2)/7

d <- d %>% select(-x1,-x2) 

# (FG) Nombre de postes de télévision dans le foyer
d$nbtv <- case_when(
  d$I102 == "'No'" ~ "Foyer sans TV",
  d$I2 == "'Un'" ~ "Un",
  d$I2 == "'Deux'" ~ "Deux",
  d$I2 == "'Trois'" | d$I2 == "'Quatre'" | d$I2 == "'Cinq ou plus'"~ "Trois ou +",
  d$I2 == "'(NSP)'" | d$I2 == "'(REF)'" ~ "NSP/REF")

d$nbtv <- as.factor(d$nbtv)
d$nbtv<- fct_relevel(
  d$nbtv,
  "Foyer sans TV",
  "Un",
  "Deux",
  "Trois ou +",
  "NSP/REF"
)

# (FG) Equipement en ordinateur (fixe ou portable) dans le foyer
d$Iordi <- case_when(
  d$I108 == "'Yes'" ~ "'Yes'",  
  d$I109 == "'Yes'" ~ "'Yes'",
  TRUE ~ "'No'")

# (FG) Equipement en console de jeux (fixe ou portable) dans le foyer
d$Iconsole <- case_when(
  d$I105 == "'Yes'" ~ "'Yes'",  
  d$I106 == "'Yes'" ~ "'Yes'",
  TRUE ~ "'No'")

# (FG) Groupe PCS_menage : renommage des modalités
d$G_PCS_MENAGE_r <- case_when(
  d$G_PCS_MENAGE_ == "I"  ~  "I. à dominante cadre", 
  d$G_PCS_MENAGE_ == "II" ~  "II. à dominante intermédiaire",
  d$G_PCS_MENAGE_ == "III" ~ "III. à dominante employée",
  d$G_PCS_MENAGE_ == "IV" ~  "IV. à dominante indépendante",
  d$G_PCS_MENAGE_ == "V"  ~  "V. à dominante ouvrière",
  d$G_PCS_MENAGE_ == "VI" ~  "VI. d’un employé ou ouvrier",
  d$G_PCS_MENAGE_ == "VII" ~ "VII. inactifs (hors retraités)"
)

# (FG) Visionnage série en mobilité : recodage NA
d$C31 <- as.character(d$C31)
d$C35 <- as.character(d$C35)
d$C35_r <- case_when(
  d$C31 == "'Jamais ou pratiquement jamais'" | d$C31 == "'(NSP)'" | d$C31 == "'(REF)'"   ~  "Ne regarde pas de séries",
  TRUE ~ d$C35
)

# (FG) équipements utilisés pour regarder la télé
d$C6_r <- case_when(
  d$C61 == "'Yes'" & d$C62 == "'No'" & d$C63 == "'No'" & d$C64 == "'No'" & d$C65 == "'No'" ~ "Ecran de TV uniquement",  
  d$C61 == "'Yes'" & (d$C62 == "'Yes'" | d$C63 == "'Yes'" | d$C64 == "'Yes'" | d$C65 == "'Yes'") ~ "Ecran de TV + autre", 
  d$C61 == "'No'" & (d$C62 == "'Yes'" | d$C63 == "'Yes'" | d$C64 == "'Yes'" | d$C65 == "'Yes'") ~ "Autre uniquement",
  is.na(d$C61) == TRUE ~ "Ne regarde pas la TV",
  TRUE ~ "nsp"
)

# Unité urbaine

d <- mutate(d, TUU2016 = factor(TUU2016, 
                           levels = as.character(0:8),
                           labels = c("Commune hors unité urbaine",
                                      "Commune appartenant à une unité urbaine de 2 000 à 4 999 habitants",
                                      "Commune appartenant à une unité urbaine de 5 000 à 9 999 habitants",
                                      "Commune appartenant à une unité urbaine de 10 000 à 19 999 habitants",
                                      "Commune appartenant à une unité urbaine de 20 000 à 49 999 habitants",
                                      "Commune appartenant à une unité urbaine de 50 000 à 99 999 habitants",
                                      "Commune appartenant à une unité urbaine de 100 000 à 199 999 habitants",
                                      "Commune appartenant à une unité urbaine de 200 000 à 1 999 999 habitants",
                                      "Commune appartenant à l'unité urbaine de Paris")))
