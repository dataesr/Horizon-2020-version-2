########################################################################
# THEMES
#######################################################################


#charger nomenclatures themes avec harmonisation des codes à utiliser
themes <- load_one_obj("nomenclature/theme.rdata", "themes")

cd_topic <- bind_rows(proposal %>% select(topicCode) %>% unique(),
                   project %>% select(topicCode) %>% unique()) %>% 
  unique()

comp <- fromJSON(paste0(chemin_bd,"topicLbDivisions.json")) %>% 
  dplyr::filter(isPrincipal==TRUE) %>% 
  dplyr::rename(CD_THEMA=divisionAbbreviation) %>% 
  dplyr::mutate(pilier=ifelse(is.na(lvl3Description), lvl2Description, lvl3Description),
                programme_name_en=lvl4Description,
                prog_thema=lvl4Code,
                programme_name_en=ifelse(lvl4Code=='EU.2.1.', lvl5Description, programme_name_en),
                prog_thema=ifelse(lvl4Code=='EU.2.1.', lvl5Code, prog_thema),
                thema_name_en=lvl5Description, area_thema=lvl5Code,
                thema_name_en=ifelse(lvl4Code=='EU.2.1.', lvl6Description, thema_name_en),
                area_thema=ifelse(lvl4Code=='EU.2.1.', lvl6Code, area_thema)) %>% 
  select(-isPrincipal, -framework, -lastUpdateDate, -starts_with('lvl')) %>%
  unique()

comp <- comp %>% 
  left_join(select(themes, c(CD_THEMA, abbr)), by=c('prog_thema'='CD_THEMA')) %>% 
  dplyr::rename(programme_code=abbr) %>% 
  left_join(select(themes, c(CD_THEMA, abbr)), by=c('area_thema'='CD_THEMA')) %>% 
  dplyr::rename(thema_code=abbr) %>% 
  select(-ends_with('thema'), -divisionDescription)
  
          

proposal <- left_join(proposal, comp, by='topicCode')
project <- left_join(project, comp, by='topicCode')

controle_NA(proposal$pilier)
controle_NA(project$pilier)

#contrôle des deux tables thema referentes des bases H2020
# comparaison_df(THEMA_PROJ,THEMA_PROP)
# themes_ref <- dplyr::union(THEMA_PROP,THEMA_PROJ) %>% 
#               select(CD_THEMA = CD_DIVNAME)


unique(proposal$CODE_PANEL)
unique(project$CODE_PANEL)
#verification qu'il n'y a pas de themes dans les panels
pan <- proposal %>% 
  dplyr::filter(!(is.na(CODE_PANEL) | CODE_PANEL== "null") & !(pilier=="Excellent Science")) %>% 
  select(topicCode, CODE_PANEL) %>% unique()
#ajouter la liste à la table THEMES si n'existe pas


# transforme certains panels en sous-themes
# temp <- themes %>% select(code_temp = CODE_THEME, theme) 
# temp <- left_join(pan, temp, by=c("CODE_PANEL"="theme")) %>% 
#   dplyr::filter(!is.na(code_temp))
# 
# proposal <- proposal %>% 
#   mutate(CODE_PANEL = ifelse(CODE_PANEL == "null", NA_character_, CODE_PANEL)) %>% 
#   left_join(temp, by = c("CD_THEMA", "CODE_PANEL")) %>%
#   # left_join(temp, by = c("CODE_PANEL" = "theme")) %>%
#   mutate(CODE_THEME = ifelse(!is.na(code_temp), code_temp, CODE_THEME))

# temp <- proposal %>% 
#   dplyr::filter(!is.na(code_temp)) %>% 
#   select(NUM_PROJET, code_temp) %>% 
#   unique()
# 
# project <- project %>%  
#   left_join(temp, by = "NUM_PROJET") %>%
#   mutate(CODE_THEME = ifelse(!is.na(code_temp), code_temp, CODE_THEME)) %>%
#   select(-code_temp)


#creation des generations themes
#max_level_new : les différents niveaux de la nomenclature themes
#programme qui ajoute à la table demandée tous les niveaux des themes
# mln <- unique(theme$LEVEL_NEW) #niveau le plus élevé
# mln <- mln[!is.na(mln)]
# proposal <- generation_theme(proposal, mln)
# project <- generation_theme(project, mln)
# 
# proposal <- proposal %>% 
#   select(-CD_THEMA, -CODE_THEME, -code_temp) %>% 
#   mutate(programme = ifelse(programme_abbr == toupper(programme_lib), programme_lib, paste0(programme_abbr," - ",programme_lib)), 
#    area = ifelse(area_abbr == toupper(area_lib), area_lib,paste0(area_abbr," - ", area_lib)))
# 
# project <- project %>% 
#   select(-CD_THEMA, -CODE_THEME)  %>% 
#   mutate(programme = paste0(programme_abbr," - ",programme_lib), 
#    area = paste0(area_abbr," - ", area_lib))

#pour l'instant supprimer les code_panels non ERC et MSCA; à voire si on les transforme en libellé thémes
proposal <- proposal %>% 
  mutate(CODE_PANEL = ifelse(!is.na(CODE_PANEL) & !(pilier == 'Excellent Science'), NA_character_, CODE_PANEL))

controle_NA(proposal$thema_name_en)
controle_NA(proposal$thema_code)

controle_NA(proposal$programme_name_en)
controle_NA(proposal$programme_code)


