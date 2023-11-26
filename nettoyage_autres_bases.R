# ARTICLE 185 projet

art_proj <- art_proj %>% 
  mutate(CODE_THEME = ifelse(APPEL=="EMPIR", "ABBB",
                             ifelse(APPEL=="Eurostars 2", "ABE", 
                                    ifelse(APPEL %in% c("AAL 2","EDCTP2"), "ACA", 
                                           NA_character_))),
         destination=ifelse(APPEL=="EMPIR", 'European Metrology Research Programme',
                            ifelse(APPEL=="Eurostars 2",'Eurostars',
                                   ifelse(APPEL=="AAL 2","Active and Assisted Living",
                                          ifelse(APPEL=="EDCTP2","European and Developing Countries Clinical Trials Partnership 2",
                                                 NA_character_))))) %>% 
  dplyr::filter(!is.na(CODE_THEME), CD_PROJ_ID != "7a3b6a48b3794a7c8d70d18f2a91afbc") %>% 
  mutate(CD_TOPIC = "P2P", 
         DATE_DEBUT = format(dmy(DATE_DEBUT), "%d-%m-%Y"), DATE_FIN = format(dmy(DATE_FIN), "%d-%m-%Y"),
         ANNEE = str_extract_all(LB_CALL_TITLE, '(\\d{4})', simplify = TRUE)[,1],
         ANNEE = ifelse(ANNEE=="" & APPEL=="Eurostars 2" & str_detect(LB_CALL_TITLE, "[1-3]{1}$"), '2014',
                        ifelse(ANNEE=="" & APPEL=="Eurostars 2" & str_detect(LB_CALL_TITLE, "[4-5]{1}$"), '2015',
                               ifelse(ANNEE=="" & APPEL=="Eurostars 2" & str_detect(LB_CALL_TITLE, "[6-7]{1}$"), '2016',
                                      ifelse(ANNEE=="" & APPEL=="Eurostars 2" & str_detect(LB_CALL_TITLE, "[8-9]{1}$"), '2017',
                                             ANNEE)))),
         ANNEE = ifelse(ANNEE=="", substring(DATE_DEBUT,7, 10), ANNEE),
         ANNEE = ifelse(is.na(ANNEE), "2020", ANNEE))

role <- art_part %>% select(ROLE_PART) %>% unique()

sum(art_part$AM_TOTAL_REQ_GRANT, na.rm=TRUE) # 1 165 181 533

art_part <- art_part %>% 
  mutate(CODE_ORG=as.character(CODE_ORG)) %>% 
  group_by(CD_PROJ_ID) %>% 
  mutate(ligne= cur_group_id(),
         NUM_PROJET = paste0("art_", ligne), 
         role = ifelse(str_detect(str_to_upper(ROLE_PART), '^CO'), "coordinator", "participant")) %>% 
  relocate(NUM_PROJET, role) %>% 
  arrange(NUM_PROJET, role) %>% 
  ungroup()

art <- art_proj %>%
  inner_join(art_part, by = "CD_PROJ_ID") %>%
  select(-LAT,-LNG,-ligne, -ROLE_PART)
sum(art$AM_TOTAL_REQ_GRANT, na.rm=TRUE) # 455 916 305



###########################################################################################
sum(eit_participant$AM_CONTRIB_EIT, na.rm=TRUE) # 1 655 221 651
sum(eit_participant$AM_CONTRIB_KIC_PARTNER , na.rm=TRUE) # 339 010 400

kic <- eit_activities %>% select(LB_KIC_NAME) %>% unique()
eit_activities <- eit_activities %>% 
  mutate(LB_KIC_NAME=ifelse(LB_KIC_NAME=='Raw Materials', 'RawMaterials', LB_KIC_NAME)) %>% 
  unique()

verif <- eit_activities %>% select(CD_ACTIVITY_ID, LB_KIC_NAME) %>% unique() %>% 
  group_by(CD_ACTIVITY_ID) %>% mutate(n=n())


eit <- full_join(eit_participant, 
                 eit_activities %>% select(CD_ACTIVITY_ID, LB_KIC_NAME) %>% unique(), 
                 by = "CD_ACTIVITY_ID") %>% 
  unique() %>% 
  group_by(CD_ACTIVITY_ID) %>% 
  mutate(ligne= cur_group_id(),
         NUM_PROJET = paste0("eit_", ligne)) %>% 
  ungroup() %>% 
  unique() %>% 
  select(ANNEE=KAVA_YEAR, destination=LB_KIC_NAME, NUM_PROJET, CODE_PAYS, isSme, TYPE_ORG, role, CODE_ORG, PIC_ID2, 
         NOM_LONG, NOM_COURT, ADDRESS, POSTAL_CODE, CITY, NUTS_CODE,
         AM_COST_TOTAL, AM_CONTRIB_EIT, AM_CONTRIB_KIC_PARTNER, AM_CONTRIB_EU_NON_EIT,
         AM_CONTRIB_NAT_REG, AM_CONTRIB_PRIVATE, AM_CONTRIB_OTHER, AM_CONTRIB_TOTAL,LAST_UPDATE_DATE) %>% 
  unique() %>% 
  mutate(APPEL = "EIT", CODE_THEME = "AG", CD_TOPIC = "EIT", CODE_ORG=as.character(CODE_ORG), 
         ANNEE=as.character(ANNEE), isSme = ifelse(is.na(isSme)| isSme == "NO", FALSE, TRUE),
         role = ifelse(str_detect(tolower(role), "head|lead|coordinat") | is.na(role), 'coordinator', 'participant'))

sum(eit$AM_CONTRIB_EIT, na.rm=TRUE) # 1 655 221 651
sum(eit$AM_CONTRIB_KIC_PARTNER , na.rm=TRUE) # 339 010 400  

no_eit <- eit %>% dplyr::filter(is.na(LAST_UPDATE_DATE)) %>% group_by(destination) %>% 
  mutate(n=n_distinct(NUM_PROJET)) %>% select(destination, n) %>% unique()

art_eit <- as.data.frame(bind_rows(art,eit %>% dplyr::filter(!is.na(LAST_UPDATE_DATE)))) %>% 
  mutate(CODE_ORG=ifelse(is.na(CODE_ORG), PIC_ID2, CODE_ORG)) %>% 
  select(-LAST_UPDATE_DATE, -CD_PROJ_ID) %>% 
  unique()

sum(art_eit$AM_CONTRIB_EIT, na.rm=TRUE) # 1 655 221 651
sum(art_eit$AM_TOTAL_REQ_GRANT, na.rm=TRUE) # 455 838 003

ref_eit = bind_rows(read_excel(paste0(chemin, "datas_reference/participant_id_eit_p2p.xlsx"), 
                               sheet = 'fr', guess_max = 2000, col_types = "text", na=""),
                    read_excel(paste0(chemin, "datas_reference/participant_id_eit_p2p.xlsx"), 
                               sheet = 'foreign', guess_max = 2000, col_types = "text", na=""))%>% 
  mutate(id=ifelse(!is.na(id_new), id_new, id), id=ifelse(str_detect(id, "^0{1}([a-zA-Z0-9]{6})[0-9]{2}$"), paste0("R",id), id)) %>% 
  select(destination=theme, CODE_ORG, NOM_LONG, CODE_PAYS=country_code, ZONAGE, id) %>% 
  unique() 


art_eit <- left_join(art_eit, ref_eit, by=c('CODE_ORG', 'NOM_LONG', 'CODE_PAYS', 'destination')) %>% 
  unique() %>% 
  mutate(CODE_PAYS=if_else(CODE_PAYS=="GB", "UK", if_else(CODE_PAYS=="GR", "EL", CODE_PAYS))) 

art_eit <- left_join(art_eit, ref_source %>% select(generalPic,countryCode,id,ZONAGE), 
                     by=c('CODE_ORG'='generalPic', "CODE_PAYS"="countryCode"), suffix=c('', '_ref')) %>% 
  mutate(id=ifelse(is.na(id), id_ref, id), ZONAGE=ifelse(is.na(ZONAGE), ZONAGE_ref, ZONAGE)) %>% 
  select(-id_ref, -ZONAGE_ref)

# test <- art_eit %>% dplyr::filter(is.na(id)) %>% 
#   inner_join(ref %>% select(generalPic,countryCode,legalName,id,ZONAGE) %>% dplyr::filter(!is.na(id)), 
#              by=c('CODE_ORG'='generalPic'),suffix=c('', '_ref')) %>% 
#   select(CODE_ORG,CODE_PAYS,countryCode,NOM_LONG,legalName,id_ref,ZONAGE_ref)

themes <- load_one_obj("nomenclature/theme.rdata", "themes")
theme <- art_eit %>% left_join(themes, by = "CODE_THEME") %>% 
  select(CODE_THEME, LEVEL_NEW) %>% unique()
mln <- unique(theme$LEVEL_NEW)

art_eit <- generation_theme(art_eit, mln)

art_eit <- left_join(art_eit, 
                     country %>% select(countryCode, country_code_mapping, country_code), 
                     by=c('CODE_PAYS'='countryCode')) %>%
  mutate(SUBV=ifelse(CD_TOPIC=='P2P', AM_TOTAL_REQ_GRANT, AM_CONTRIB_EIT),
         country_code=ifelse(!is.na(ZONAGE), ZONAGE, country_code),
         stage='project', status_code='SIGNED', PIC_ID2=as.character(PIC_ID2)) %>% 
  select(call_id=APPEL, action_id=CD_TOPIC, acronym=ACRONYM_PROJET, title=TITRE_PROJET, 
         project_startDate=DATE_DEBUT, project_endDate=DATE_FIN, destination_name_en=destination, 
         call_year=ANNEE, project_id=NUM_PROJET, 
         role, generalPic=CODE_ORG, participant_pic=PIC_ID2, legalName=NOM_LONG, shortName=NOM_COURT,
         address=ADDRESS, postal_code=POSTAL_CODE, nuts_code=NUTS_CODE, city=CITY, countryCode=CODE_PAYS, 
         legalEntityTypeCode=TYPE_ORG, isSme, 
         id, programme_code=programme_abbr, programme_name_en=programme_lib, 
         thema_code=area_abbr, thema_name_en=area_lib, stage, status_code,
         pilier, country_code_mapping, country_code, fund=SUBV) %>% 
  left_join(country%>% select(country_code,country_code, country_name_en, country_association_code, 
                              country_association_name_en, country_group_association_code, 
                              country_group_association_name_en, country_group_association_name_fr, 
                              country_name_fr, article1, article2) %>% unique(), 
            by='country_code')

  


sum(art_eit$fund, na.rm=TRUE) # 2 111 059 654

save("eit", "art", "art_eit",  'ref_eit', file = str_c(chemin_bd,  "autres_prog.RData"))
save("eit", "art", "art_eit",  'ref_eit', file = "C:/Users/zfriant/Documents/OneDrive/PCRI/bilan H2020/autres_prog.RData")
