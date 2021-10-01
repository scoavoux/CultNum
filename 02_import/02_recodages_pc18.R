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

