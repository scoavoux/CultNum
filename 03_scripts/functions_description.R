library(rlang)

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
  
  graph_qcm_indep(.dep = {{ .dep }}, .indep = TYPMEN, .value = .value, .data=.data, .labs=.labs) %>% print()
  
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
}
