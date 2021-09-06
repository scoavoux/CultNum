library(tidyverse)
library(readxl)
library(haven)
library(here)

panel <- read_sas(here("01_data", "panel7318.sas7bdat"))
pc97 <- read_sas(here("01_data", "pc97_15fev2021.sas7bdat"))
pc08 <- read_sas(here("01_data", "pc08_15fev2021.sas7bdat"))

save(panel, pc97, pc08, file = here("01_data", "pc97-08-panel.RData"))
