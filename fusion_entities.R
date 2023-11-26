#################################################"

gen_state <- c('VALIDATED', 'DECLARED', 'DEPRECATED', 'SLEEPING', 'SUSPENDED', 'BLOCKED', 'Undefined')
entities <- load_one_obj(paste0(chemin_bd, "bases.RData"), "legalEntities") %>% 
  select(CODE_ORG=generalPic,
         pic,
         legalName, generalState, webPage, city, countryCode, isSme, isInternationalOrganisation, legalEntityTypeCode, legalType, vat, legalRegNumber) %>% 
  group_by(CODE_ORG, countryCode) %>% 
  arrange(factor(generalState, levels = gen_state), .by_group=TRUE) %>% 
  dplyr::filter(row_number() == 1) %>% 
  select(-generalState) %>% 
  mutate_at(.vars=c('CODE_ORG','pic'), as.character)


x=entities %>% group_by(CODE_ORG, pic) %>% mutate(n=n()) %>% dplyr::filter(n>1)


test <- subv_p %>% inner_join(entities, by = c('CODE_ORG', 'PIC_ID2'='pic', 'CODE_PAYS'='countryCode'))
test2 <- anti_join(subv_p, test, 
                   by=c('NUM_PROJET', 'CODE_ORG','ORDRE_PART', 'PIC_ID2', 'ORDRE_ALL', "stage",
                        'ROLE_PART', 'TYPE_PART', 'CODE_PAYS')) %>% 
  inner_join(entities, by = c('CODE_ORG',  'CODE_PAYS'='countryCode'))

# test <- bind_rows(test, test2)
# test2 <- anti_join(subv_p, test,
#                    by=c('NUM_PROJET', 'CODE_ORG','ORDRE_PART', 'PROPOSAL_ORDRE_PART',
#                         'PROPOSAL_PIC_ID2', 'PROPOSAL_CODE_ORG', 'calculated_pic', 
#                         'projNlien', 'propNlien', 'PROPOSAL_ROLE_PART','inProject',
#                         'inProposal', 'PROPOSAL_TYPE_PART', 'role', 'participates_as', 'countryCode')) %>% 
#   inner_join(entities, by = c('PROPOSAL_CODE_ORG'='CODE_ORG', 'countryCode')) %>% 
#   select(-pic) %>% unique()
# 
# test <- bind_rows(test, test2)
# test2 <- anti_join(subv_p, test,
#                    by=c('NUM_PROJET', 'CODE_ORG','ORDRE_PART', 'PROPOSAL_ORDRE_PART',
#                         'PROPOSAL_PIC_ID2', 'PROPOSAL_CODE_ORG', 'calculated_pic', 
#                         'projNlien', 'propNlien', 'PROPOSAL_ROLE_PART','inProject',
#                         'inProposal','PROPOSAL_TYPE_PART', 'role', 'participates_as', 'countryCode')) %>% 
#   inner_join(entities, by = c('CODE_ORG','countryCode'))

test <- bind_rows(test, test2) %>% select(-pic)

test2 <- anti_join(subv_p, test,
                   by=c('NUM_PROJET', 'CODE_ORG','ORDRE_PART', 'PIC_ID2', 'ORDRE_ALL', "stage",
                        'ROLE_PART', 'TYPE_PART', 'CODE_PAYS'))%>% 
  inner_join(entities %>% dplyr::rename(countryEnt=countryCode) %>% select(-pic), 
             by = c('CODE_ORG')) %>% 
  dplyr::filter(!(CODE_ORG=='899858913'&countryEnt=='NL'))

test <- bind_rows(test, test2)


test2 <- anti_join(subv_p, test,
                   by=c('NUM_PROJET', 'CODE_ORG','ORDRE_PART', 'PIC_ID2', 'ORDRE_ALL', "stage",
                        'ROLE_PART', 'TYPE_PART', 'CODE_PAYS'))
#   inner_join(participant %>% 
#                select(NUM_PROJET, CODE_ORG, PIC_ID2, ORDRE_PART, NOM_LONG, CODE_PAYS, TYPE_ORG,PME),
#              by=c('NUM_PROJET', 'CODE_ORG', 'PIC_ID2', 'ORDRE_PART', 'CODE_PAYS'))

# test <- bind_rows(test, test2) %>% 
#   mutate(legalEntityTypeCode=ifelse(is.na(legalEntityTypeCode), TYPE_ORG, legalEntityTypeCode),
#          isSme=ifelse(is.na(isSme), PME, isSme),
#          # countryCode=ifelse(is.na(countryCode), CODE_PAYS, countryCode),
#          legalName=ifelse(is.na(legalName), NOM_LONG, legalName)) %>% 
#   select(-TYPE_ORG, -NOM_LONG, -PME)

# test2 <- anti_join(subv_p, test,
#                    by=c('NUM_PROJET', 'CODE_ORG','ORDRE_PART', 'PROPOSAL_ORDRE_PART',
#                         'PROPOSAL_PIC_ID2', 'PROPOSAL_CODE_ORG', 'calculated_pic', 
#                         'projNlien', 'propNlien', 'PROPOSAL_ROLE_PART','inProject',
#                         'inProposal','PROPOSAL_TYPE_PART', 'role', 'participates_as','countryCode')) %>%
#   inner_join(applicant %>% 
#                select(NUM_PROJET, CODE_ORG, PIC_ID2, ORDRE_PART, NOM_LONG, CODE_PAYS, TYPE_ORG, PME, URL_PART),
#              by=c('NUM_PROJET', 'PROPOSAL_CODE_ORG'='CODE_ORG', 'PROPOSAL_PIC_ID2'='PIC_ID2', 'PROPOSAL_ORDRE_PART'='ORDRE_PART', 'countryCode'='CODE_PAYS'))

part <- bind_rows(test, test2) %>% 
  mutate(legalEntityTypeCode=ifelse(is.na(legalEntityTypeCode), TYPE_ORG, legalEntityTypeCode),
         isSme=ifelse(is.na(isSme), PME, isSme),
         # countryCode=ifelse(is.na(countryCode), CODE_PAYS, countryCode),
         legalName=ifelse(is.na(legalName), NOM_LONG, legalName),
         webPage=ifelse(is.na(webPage), URL_PART, webPage)) %>% 
  select(-TYPE_ORG, -NOM_LONG, -PME, -URL_PART) %>% 
  mutate(legalName=str_squish(str_replace_all(legalName, "\\n|\\t|\\r|\\s+", " ")))

sum(part$SUBV_NET, na.rm=TRUE)#68 777 704 935
sum(part[part$stage=='proposal',]$SUBV, na.rm=TRUE)#687 415 105 573

name <- part %>% dplyr::filter(is.na(legalName)) %>% 
  select(CODE_ORG, CODE_PAYS) %>% unique() %>% 
  left_join(applicant %>% select(CODE_ORG, NOM_LONG, CODE_PAYS)) %>% 
  unique() %>% 
  select(CODE_ORG, NOM_LONG)

if (nrow(name)>0){
part <- part %>% 
  mutate(legalName=ifelse(is.na(legalName)&CODE_ORG==name$CODE_ORG, name$NOM_LONG, legalName),
         countryEnt=ifelse(inProject==TRUE & !is.na(countryEnt), countryEnt, CODE_PAYS))
}


# x=part %>% select('NUM_PROJET', 'CODE_ORG','ORDRE_PART', 'PROPOSAL_ORDRE_PART',
#                 'PROPOSAL_PIC_ID2', 'PROPOSAL_CODE_ORG', 'calculated_pic', 
#                 'projNlien', 'propNlien', 'PROPOSAL_ROLE_PART','inProject',
#                 'inProposal','PROPOSAL_TYPE_PART', 'role', 'participates_as') %>% 
#   group_by(NUM_PROJET, CODE_ORG,ORDRE_PART, PROPOSAL_ORDRE_PART,
#            PROPOSAL_PIC_ID2, PROPOSAL_CODE_ORG, calculated_pic, 
#            projNlien, propNlien, PROPOSAL_ROLE_PART,inProject,
#            inProposal,PROPOSAL_TYPE_PART, role, participates_as) %>% 
#   mutate(n=n()) %>% dplyr::filter(n>1)

############## 
# verif diff entre lien et part
# nrow(part %>% select(NUM_PROJET, CODE_ORG, calculated_pic, PROPOSAL_PIC_ID2, PROPOSAL_CODE_ORG) %>% unique())
# #1 139 228
# nrow(lien %>% select(NUM_PROJET, CODE_ORG, calculated_pic, PROPOSAL_PIC_ID2, PROPOSAL_CODE_ORG) %>% unique())
# # 1 139 228
# 
# x = anti_join(lien %>% select(NUM_PROJET, CODE_ORG, calculated_pic, PROPOSAL_PIC_ID2, PROPOSAL_CODE_ORG) %>% unique(),
#               part %>% select(NUM_PROJET, CODE_ORG, calculated_pic, PROPOSAL_PIC_ID2, PROPOSAL_CODE_ORG) %>% unique())


######################

# nrow(unique(part %>% dplyr::filter(is.na(CODE_PAYS)) %>% select(CODE_ORG)))
# 
# verif_na <- part[apply(part, 2, function(x) any(is.na(x)))]
# 
# # test unicité code_org name country
# x=part %>% select(CODE_ORG, CODE_PAYS, legalName) %>% unique() %>% 
#   group_by(CODE_ORG, CODE_PAYS, legalName) %>% 
#   mutate(n=n()) %>% dplyr::filter(n>1)
# 
# nrow(part %>% group_by(CODE_ORG,legalName, CODE_PAYS) %>% unique()) #1 140 339

##### table pour compléter ref_source si pic manquant
# H20_entities <- part %>% 
#   select(CODE_ORG, legalName, webPage, city, countryCode, isInternationalOrganisation) %>% 
  # group_by(CODE_ORG, legalName, webPage, city, countryCode, isInternationalOrganisation) %>% 
  # mutate(applicant_subv=sum(applicant_subv, na.rm=TRUE), participant_subv=sum(participant_subv, na.rm=TRUE)) %>% 
  # # select(-applicant_subv, -participant_subv) %>%
  # unique()

###########################################################################

part <- part %>% 
  left_join(ref_source ,  by=c('CODE_ORG'='generalPic', 'CODE_PAYS'='countryCode')) %>%  
  dplyr::rename(countryCode=CODE_PAYS) %>% 
  left_join(country %>% select(countryCode, country_code_mapping, country_code), by='countryCode') %>% 
  mutate(country_code=ifelse(!is.na(ZONAGE), ZONAGE, country_code)) %>% 
  select(-ZONAGE) %>% 
  left_join(country %>% select(country_code,country_code, country_name_en, country_association_code, 
                               country_association_name_en, country_group_association_code, 
                               country_group_association_name_en, country_group_association_name_fr, 
                               country_name_fr, article1, article2) %>% unique(), 
            by='country_code')

sum(part$SUBV_NET, na.rm=TRUE)#68 777 704 935
sum(part[part$stage=='proposal',]$SUBV, na.rm=TRUE)#687 415 105 573
 