########################################################################
# PANELS
#######################################################################

# panels <- load_one_obj("nomenclature/panels.rdata", "panels")
panels <- fromJSON("nomenclature/panels.json")

#liste des panels dans les bases
# panel_base <- proposal %>% 
#   select(CODE_PANEL) %>% 
#   dplyr::filter(!is.na(CODE_PANEL)) %>% 
#   unique()
# 
# pan_comp <- panel_base %>% 
#   anti_join(panels, by = "CODE_PANEL") %>% 
#   dplyr::filter(str_detect(CODE_PANEL, "^[0-9]+", negate=TRUE))
# if (nrow(pan_comp) == 0) {
#   print("tout va bien")
# } else{
#   print("il manque un/plusieurs code_panel dans la table nomenclature panels")
#   pan_comp <- pan_comp %>% 
#     left_join(proposal[,c("CODE_PANEL", "programme_code", "CODE_ACT")], by = "CODE_PANEL") %>% 
#     unique()
# }


# proposal <- proposal %>% 
#   mutate(CODE_PANEL = ifelse(is.na(CODE_PANEL) & CODE_ACT == "ERC-POC", "PC1", CODE_PANEL),
#          CODE_PANEL = ifelse(str_detect(CODE_PANEL, "^[0-9]+$") & CODE_ACT == "ERC-POC", "PC1", CODE_PANEL),
#          CODE_PANEL = ifelse(is.na(CODE_PANEL) & programme_abbr == "ERC", "SP1", CODE_PANEL),
#          CODE_PANEL = ifelse(is.na(CODE_PANEL) & programme_abbr == "MSCA", "SAP", CODE_PANEL)) 
                                
#verification que tous les ERC et MSCA ont un panel
# panel_verif <- proposal %>% 
#   dplyr::filter(programme_code %in% c("ERC", "MSCA") & is.na(CODE_PANEL))

#création des variables "panel"
proposal <- proposal %>% 
        left_join(panels, by = c("CODE_PANEL"='panel_code')) %>% 
  dplyr::rename(panel_code=CODE_PANEL)


#ajout des panels aux contrats

project <- proposal %>% 
  select(NUM_PROJET, panel_code, panel_name, panel_regroupement_code, panel_regroupement_name, panel_description) %>%  
  unique() %>% 
  right_join(project, by = "NUM_PROJET")
  
