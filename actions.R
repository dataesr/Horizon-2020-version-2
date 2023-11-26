################################################################
# ACTIONS
###############################################################

typeOfActions <- fromJSON(paste0(chemin_bd,"typeOfActions.json")) %>% 
  select(-lastUpdateDate, -framework)

actions_base <- typeOfActions %>% 
  select(typeOfActionSimplifiedCode, typeOfActionSimplifiedDescription) %>% 
  unique()

typeOfActions <- typeOfActions %>% 
  select(-typeOfActionSimplifiedCode, -typeOfActionSimplifiedDescription, -typeOfActionlvl4Code) %>% 
  unique()


# si regroupement de types d'action, modifier dans proposal et project
proposal <- group_action(proposal, "CSA-LSP", "CSA-LS")
project <- group_action(project, "CSA-LSP", "CSA-LS")

# MSCA modif des night csa
proposal <- proposal %>% dplyr::mutate(CODE_ACT=if_else(str_detect(APPEL, "NIGHT"), "MSCA-NIGHT", CODE_ACT))
project <- project %>% dplyr::mutate(CODE_ACT=if_else(str_detect(APPEL, "NIGHT"), "MSCA-NIGHT", CODE_ACT))


x <- c('MSCA-NIGHT',"European Researchers' Night (NIGHT)",'MSCA','MSCA-NIGHT',NA_character_)
typeOfActions[nrow(typeOfActions) + 1,] <- x

code_act <- bind_rows(proposal %>% select(CODE_ACT), project %>% select(CODE_ACT)) %>% 
  unique() %>% 
  left_join(typeOfActions, by=c('CODE_ACT'='typeOfActionCode')) %>% 
  dplyr::rename(action_detail_id=typeOfActionlvl1Code, action_name = typeOfActionDescription) %>% 
  select(CODE_ACT, action_name, action_detail_id) %>% 
  mutate(action_detail_id=ifelse(str_detect(CODE_ACT, '^COFUND'), 'COFUND', action_detail_id),
         action_detail_id=ifelse(str_detect(CODE_ACT, '^CSA'), 'CSA', action_detail_id),
         action_detail_id=ifelse(str_detect(CODE_ACT, '^IA'), 'IA', action_detail_id),
         action_detail_id=ifelse(str_detect(CODE_ACT, '^RIA'), 'RIA', action_detail_id),
         action_detail_id=ifelse(str_detect(CODE_ACT, '^SGA'), 'SGA', action_detail_id),
         action_detail_id=ifelse(CODE_ACT=='H2020-EEN-SGA', 'SGA', action_detail_id)
         ) %>% 
  left_join(actions_base, by=c('action_detail_id'='typeOfActionSimplifiedCode')) %>% 
  dplyr::rename(action_detail_name=typeOfActionSimplifiedDescription) %>% 
  mutate(action_detail_name=ifelse(action_detail_id=='SGA', 'Specific Grant agreement', action_detail_name),
         action_detail_name=ifelse(action_detail_id=='COFUND', 'COFUND', action_detail_name))


proposal <- proposal %>% left_join(code_act, by="CODE_ACT") %>% 
  dplyr::rename(action_id=CODE_ACT)

project <- project %>% left_join(code_act, by="CODE_ACT") %>% 
  dplyr::rename(action_id=CODE_ACT)
