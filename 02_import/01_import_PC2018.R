library(tidyverse)
library(readxl)
library(haven)
library(here)

# Import SAS database
d <- read_sas(here("01_data", "pc18_12fev2021.sas7bdat"))

# Agréger tous les dictionnaires des codes: 
# répertorier les variables dans la base
# et repérer les levels/labels pour chacune
# Test
dic <- map(2:14, ~read_excel(here("01_data", "Dictionnaire des codes EPC 2018 (22 janvier 2021).xlsx"), sheet = .x))
cat <- map(dic, ~names(.x)[1]) %>% unlist()
for(i in 1:length(dic)){
  dic[[i]] <- mutate(dic[[i]], category = cat[i])
  names(dic[[i]])[1] <- "variable"
}
rm(cat, i)
dic <- bind_rows(dic)

# Clean up dictionary
dic <- filter(dic, !is.na(variable))
dic <- mutate(dic, 
              code     = ifelse(str_detect(variable, "="), variable, NA),
              variable = ifelse(str_detect(variable, "="), NA, variable)) %>% 
  fill(variable) %>% 
  rename(varlabel_fromdic = "...2")
dic <- separate(dic, code, into = c("level", "modlabel"), sep = "=")
dic <- mutate(dic, across(.cols = c("level", "modlabel"), str_trim))
dic <- filter(dic, !(variable %in% c("Variables techniques", 
                                   "(chaîne de caractères alphanumériques)", 
                                   "Ménage", 
                                   "(nombre)", 
                                   "Individu répondant", 
                                   "(âge numérique)", 
                                   "(deux digits)", 
                                   "Santé de l'individu (voir Bloc K p.50 dans le questionnaire)", 
                                   "Champ texte libre")))

# We make some manual correction due to errors in the 
# excel file (variables appearing in several tabs)
dic <- filter(dic, 
              !(variable == "RECODE_SITUA" & category == "Loisirs et vacances"),
              !(variable == "RECODE_SITUA" & category == "Jeux-vidéo"),
              !(variable == "CS"           & category == "Loisirs et vacances"),
              !(variable == "CS"           & category == "Jeux-vidéo"),
              !(variable == "A15"          & category == "Jeux-vidéo"),
              variable != "A6")
dic <- bind_rows(dic, 
                 tibble(variable = "A6", 
                        varlabel_fromdic = NA, 
                        category = "Loisirs et vacances", 
                        level = c(1, 2, 3, 4, 5, 6, 7) %>% as.character(),
                        modlabel = c("1 à 2 fois", "3 à 5 fois", "6 à 10 fois", "11 à 20 fois", "Plus de 20 fois", "(NSP)", "(REF)")))

# Plus some corrections of inconsistancies
dic <- mutate(dic, modlabel = ifelse(modlabel == "Masculin" & variable == "SEXE", "M", modlabel))

# No of variables in the dictionary
length(unique(dic$variable))

# Check correspondance between dictionary and database
# In dictionary, no in database
unique(dic$variable)[!(unique(dic$variable) %in% names(d))]
filter(dic, variable %in% unique(dic$variable)[!(unique(dic$variable) %in% names(d))]) %>% 
  distinct(variable)

## Seem to be only technical variables.

# In the database, not in the dictionary
names(d)[!(names(d) %in% unique(dic$variable))]


###### Variable labels ######
## Extract variable labels from SAS database/data frame
## In the "label" attribute
## then turn it to a data.frame
v <- sapply(d, attr, "label") %>% unlist()
labs <- tibble(variable = names(v),
               varlabel_frombase = v)
labs <- mutate(labs, indatabase = "yes")
dic <- mutate(dic, indic = "yes")
dic <- full_join(dic, labs, by = "variable")
var <- distinct(dic, variable, .keep_all = TRUE) %>% 
  select(-level, -modlabel)
  
###### Modalities variables ######
mod <- filter(dic, !is.na(level),
              variable %in% names(d))

d <- mutate(d, across(.cols = unique(mod$variable), as.character))

# For some reason, probably a very good one, the following, ugly
# loop does not work on tiblle. Hope Hadley never sees this code.
d <- as.data.frame(d)

for(v in unique(mod$variable)){
  print(v)
  m <- filter(mod, variable == v)
  d[,v] <- factor(d[,v], levels = m$level, labels = m$modlabel)
}
# Annnnnd we're back
d <- as_tibble(d)

###### Produce code dictionary ######
labs <- distinct(dic, variable, .keep_all = TRUE) %>% 
  select(-level, -modlabel)

###### Saves ######
save(d, file = here("01_data", "pc2018.RData"))
save(dic, labs, file = here("01_data", "dictionary.RData"))
write_excel_csv(dic, file = here("01_data", "modalite.csv"))
write_excel_csv(labs, file = here("01_data", "variable_labels.csv"))