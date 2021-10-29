library(rlang)

donnat_table <- function(.dep, .value = "'Yes'", .data=d, .labs=labs, .caption = "", .panel_wave = FALSE){
  indep <- c("SEXE", "pcs1", "dipl", "age_c")
  
  if(.panel_wave){
    total <- select(.data, {{ .dep }}, POND_INIT, annee) %>% 
      pivot_longer({{ .dep }}, names_to = "dep") %>% 
      count(annee, dep, value, wt = POND_INIT) %>% 
      filter(!is.na(value)) %>% 
      group_by(annee, dep) %>% 
      mutate(f = round(n / sum(n) * 100, 1) %>% paste0(., "%")) %>% 
      ungroup() %>% 
      filter(value == .value) %>% 
      left_join(select(.labs, dep = "variable", lab = "varlabel")) %>% 
      mutate(indep = "Total",
             lab = paste(annee, lab, sep = "_")) %>% 
      select(indep, lab, f) %>% 
      pivot_wider(names_from = lab, values_from = f)
    
    
    select(.data, {{ .dep }}, !! enquo(indep), POND_INIT, annee) %>%
      pivot_longer({{ .dep }}, names_to = "dep", values_to = "depmod") %>% 
      pivot_longer(!!enquo(indep), names_to = "indep", values_to = "indepmod") %>% 
      count(annee, indep, indepmod, dep, depmod, wt = POND_INIT) %>% 
      filter(!is.na(depmod), !is.na(indepmod)) %>% 
      group_by(annee, indep, indepmod, dep) %>% 
      mutate(f = round(n / sum(n) * 100, 1) %>% paste0(., "%")) %>% 
      ungroup() %>% 
      filter(depmod == .value) %>%
      left_join(select(.labs, dep = "variable", lab = "varlabel")) %>% 
      mutate(lab = paste(annee, lab, sep = "_")) %>% 
      select(indep, indepmod, lab, f) %>% 
      pivot_wider(names_from = lab, values_from = f) %>% 
      mutate(indep = ifelse(indep == dplyr::lag(indep) & row_number() != 1, "NA", indep)) %>% 
      add_case(total, .before = 1L) %>% 
      kable(caption = .caption)
  } else {
    total <- select(.data, {{ .dep }}, POND_INIT) %>% 
      pivot_longer({{ .dep }}, names_to = "dep") %>% 
      count(dep, value, wt = POND_INIT) %>% 
      filter(!is.na(value)) %>% 
      group_by(dep) %>% 
      mutate(f = round(n / sum(n) * 100, 1) %>% paste0(., "%")) %>% 
      ungroup() %>% 
      filter(value == .value) %>% 
      left_join(select(.labs, dep = "variable", lab = "varlabel")) %>% 
      mutate(indep = "Total") %>% 
      select(indep, lab, f) %>% 
      pivot_wider(names_from = lab, values_from = f)
    
    
    select(.data, {{ .dep }}, !! enquo(indep), POND_INIT) %>%
      pivot_longer({{ .dep }}, names_to = "dep", values_to = "depmod") %>% 
      pivot_longer(!!enquo(indep), names_to = "indep", values_to = "indepmod") %>% 
      count(indep, indepmod, dep, depmod, wt = POND_INIT) %>% 
      filter(!is.na(depmod), !is.na(indepmod)) %>% 
      group_by(indep, indepmod, dep) %>% 
      mutate(f = round(n / sum(n) * 100, 1) %>% paste0(., "%")) %>% 
      ungroup() %>% 
      filter(depmod == .value) %>%
      left_join(select(.labs, dep = "variable", lab = "varlabel")) %>% 
      select(indep, indepmod, lab, f) %>% 
      pivot_wider(names_from = lab, values_from = f) %>% 
      mutate(indep = ifelse(indep == dplyr::lag(indep) & row_number() != 1, "NA", indep)) %>% 
      add_case(total, .before = 1L) %>% 
      kable(caption = .caption)
  }
  

}

graph_qcm <- function(.vars, .value = "'Yes'", .data=d, .labs=labs){
  select(.data, {{ .vars }}, POND_INIT) %>% 
    pivot_longer(-POND_INIT) %>% 
    count(name, value, wt = POND_INIT) %>% 
    filter(!is.na(value)) %>%
    group_by(name) %>% 
    mutate(f = n / sum(n)) %>% 
    filter(value == .value) %>% 
    left_join(select(.labs, name = "variable", lab = "varlabel")) %>% 
    mutate(lab = factor(lab, levels = unique(lab))) %>% 
    ggplot(aes(lab, f, label = paste0(round(f, 2)*100, "%"))) +
      geom_col() +
      geom_label() +
      coord_flip() +
      labs(x = "Modalités", y = "Fréquence")
}

graph_qcm_indep <- function(.dep, .indep, .value = "'Yes'", .data=d, .labs=labs){
  
  x <- select(.data, {{ .dep }}, {{ .indep }}, POND_INIT) %>%
    pivot_longer({{ .dep }}) %>% 
    count(!! enquo(.indep), name, value, wt = POND_INIT) %>%
    filter(!is.na(value)) %>% 
    group_by(!! enquo(.indep), name) %>% 
    mutate(f = n / sum(n)) %>% 
    ungroup() %>% 
    arrange(desc(n)) %>%
    filter(value == .value) %>%
    left_join(select(.labs, name = "variable", lab = "varlabel"))
    
  if(!is.numeric(pull(.data,{{ .indep }}))){
    ggplot(x, aes(!! enquo(.indep), f, label = paste0(round(f, 2)*100, "%"))) +
      geom_col() +
      geom_label() +
      coord_flip() +
      facet_wrap(~lab) +
      labs(x = "Modalités", y = "Fréquence")
  } else {
    cpt <- filter(.labs, variable == as_name(enquo(.indep))) %>% pull(varlabel)
    
    ggplot(x, aes(!! enquo(.indep), f)) +
      geom_line() +
      geom_smooth() +
      facet_wrap(~lab) +
      labs(y = "Fréquence", title = cpt)
    
  }
}

graph_qcm_all <- function(.dep, .value = "'Yes'", .data=d, .labs=labs){
  graph_qcm(.vars = {{ .dep }}, .value = .value, .data = .data, .labs = .labs) %>% print()
  
  graph_qcm_indep(.dep = {{ .dep }}, .indep = SEXE, .value = .value, .data=.data, .labs=.labs) %>% print()
  
  graph_qcm_indep(.dep = {{ .dep }}, .indep = dipl, .value = .value, .data=.data, .labs=.labs) %>% print()

  graph_qcm_indep(.dep = {{ .dep }}, .indep = pcs1, .value = .value, .data=.data, .labs=.labs) %>% print()
  
  graph_qcm_indep(.dep = {{ .dep }}, .indep = AGE, .value = .value, .data=.data, .labs=.labs) %>% print()
  
  graph_qcm_indep(.dep = {{ .dep }}, .indep = age_c, .value = .value, .data=.data, .labs=.labs) %>% print()
}

table_univar <- function(.var, .labs = labs, .data = d){
  cpt <- filter(.labs, variable == as_name(enquo(.var))) %>% pull(varlabel)

  count(.data, !! enquo(.var), wt = POND_INIT) %>%
    filter(!is.na(!! enquo(.var)),
           !! enquo(.var) != "'(NSP)'",
           !! enquo(.var) != "'(REF)'") %>% 
    mutate(f = round(n / sum(n) * 100),
           f = paste0(f, "%")) %>%
    rename(Eff. = "n", Freq. = "f") %>%
    kable(caption = ifelse(!is.null(cpt) & !is.na(cpt), cpt, " "))
}

graph_univar_indep <- function(.dep, .indep, .data = d, .labs = labs) {
  
  cpt <- filter(.labs, variable == as_name(enquo(.dep))) %>% pull(varlabel)
  xlab <- filter(.labs, variable == as_name(enquo(.indep))) %>% pull(varlabel)
  
  x <- select(.data, {{ .dep }}, {{ .indep }}, POND_INIT) %>% 
    count(!! enquo(.dep), !! enquo(.indep)) %>% 
    filter(!is.na(!! enquo(.dep)),
           !! enquo(.dep) != "'(NSP)'",
           !! enquo(.dep) != "'(REF)'") %>% 
    group_by(!! enquo(.indep)) %>% 
    mutate(f = n / sum(n))
  
  if(!is.numeric(pull(.data,{{ .indep }}))){
    ggplot(x, aes(!! enquo(.indep), f, group = !! enquo(.dep))) +
      geom_col(position = "dodge", aes(fill = !! enquo(.dep))) +
      geom_label(aes(label = paste0(round(f*100), "%")), position = position_dodge(1)) +
      coord_flip() +
      labs(x = "Modalites", y = "Frequence", title = cpt)
  } else {
    ggplot(x, aes(!! enquo(.indep), f, color = !! enquo(.dep))) +
      geom_line() +
      geom_smooth() +
      labs(x = xlab, y = "Frequence", title = cpt)
  }
}

graph_univar_all <- function(.dep, .data = d, .labs = labs){
  table_univar({{ .dep }}, .labs = .labs, .data=.data) %>% print()
  
  graph_univar_indep({{ .dep }}, SEXE, .labs = .labs, .data=.data) %>% print()

  graph_univar_indep({{ .dep }}, dipl, .labs = .labs, .data=.data) %>% print()

  graph_univar_indep({{ .dep }}, pcs1, .labs = .labs, .data=.data) %>% print()

  graph_univar_indep({{ .dep }}, AGE, .labs = .labs, .data=.data) %>% print()
  
  graph_univar_indep({{ .dep }}, age_c, .labs = .labs, .data=.data) %>% print()
}

## Une fonction pour recoder les NA dans les questions filtrées
## séparer NA parce que choix pas sélectionné de NA parce que question
## non posée.
## arguments:
# .var: la variable cible
# .filtre: la variable filtre
# .filtre_val: la valeur de la variable filtre qui fait que la question
# var est posée
# .nonselection_val: la valeur que doit prendre le fait de ne pas avoir
# coché la case quand on a posé la question
recode_nonsel_na <- function(.var, .filtre, .filtre_val, .nonselection_val){
  lvl <- c(levels(.var), .nonselection_val)
  x <- as.character(.var)
  ifelse(is.na(x) & {{ .filtre }} == .filtre_val, .nonselection_val, x) %>% 
    factor(levels = lvl)
}
