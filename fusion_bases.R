# unicité

test <- applicant %>% group_by(NUM_PROJET, CODE_ORG, PIC_ID2) %>% 
  mutate(n=n()) %>% dplyr::filter(n>1)
test1 <- participant %>% group_by(NUM_PROJET, CODE_ORG, PIC_ID2) %>% 
  mutate(n=n()) %>% dplyr::filter(n>1)

part2 <- participant %>% 
  mutate(n_part=1, ORDRE_PART=as.character(ORDRE_PART)) %>% 
  select(NUM_PROJET, CODE_ORG, PIC_ID2, ORDRE_PART, n_part) %>% 
  unique()

app2 = applicant %>% mutate(n_app=1) %>% 
  select(NUM_PROJET, CODE_ORG, PIC_ID2, ORDRE_PART, n_app) %>% 
  unique()


cols_part = colnames(part2)
cols_app = colnames(app2)


# proposal uniquement
lien1 <- app2 %>% dplyr::filter(!(NUM_PROJET %in% as.vector(unique(part2$NUM_PROJET)))) %>% 
  mutate(base_only='prop_only')
# lien1 = 949 889

app2 <- anti_join(app2, lien1, by='NUM_PROJET')
#app2 = 163 403

# jointure parfaite
lien2 = inner_join(part2, app2)

part3 = anti_join(part2, lien2[cols_part])
app3 = anti_join(app2, lien2[cols_app])  

# '''jointure 'CODE_ORG', 'PIC_ID2'
lien3 = inner_join(part3,app3, by=c('NUM_PROJET', 'CODE_ORG', 'PIC_ID2'), suffix=c('', '_p'))

part4 <- anti_join(part3, lien3[cols_part])
app4 <- anti_join(app3,
                  lien3 %>% select(NUM_PROJET, CODE_ORG, PIC_ID2, ORDRE_PART=ORDRE_PART_p))


# jointure  'CODE_ORG', 'ORDRE_PART'
lien4 <- inner_join(part4,app4, by=c('NUM_PROJET', 'CODE_ORG', 'ORDRE_PART'), suffix=c('', '_p'))

part5 <- anti_join(part4, lien4[cols_part])
app5 <- anti_join(app4,
                  lien4 %>% select(NUM_PROJET, CODE_ORG, PIC_ID2=PIC_ID2_p, ORDRE_PART))

# jointure juste generalPic'''
lien5 <- inner_join(part5,app5, by=c('NUM_PROJET', 'CODE_ORG'), suffix=c('', '_p'))

part6 <- anti_join(part5, lien5[cols_part]) 
app6 <- anti_join(app5,
                  lien5 %>% select(NUM_PROJET, CODE_ORG, PIC_ID2=PIC_ID2_p, ORDRE_PART=ORDRE_PART_p))


# jointure participant_pic + ORDRE_PART'''
lien6 <- inner_join(part6,app6, by=c('NUM_PROJET', 'PIC_ID2', 'ORDRE_PART'), suffix=c('', '_p'))


part7 <- anti_join(part6, lien6[cols_part])          
app7 <- anti_join(app6,
                  lien6 %>% select(NUM_PROJET, CODE_ORG=CODE_ORG_p, PIC_ID2, ORDRE_PART))

# jointure participant_pic'''
lien7 <- inner_join(part7,app7, by=c('NUM_PROJET', 'PIC_ID2'), suffix=c('', '_p'))


part8 <- anti_join(part7, lien7[cols_part])          
app8 <- anti_join(app7,
                  lien7 %>% select(NUM_PROJET, CODE_ORG=CODE_ORG_p, PIC_ID2, ORDRE_PART=ORDRE_PART_p))



lien <- bind_rows(lien1, lien2, lien3, lien4, lien5,lien6,lien7, app8, part8) %>% unique()

lien <- lien %>% 
  mutate(inProposal = ifelse(!is.na(n_part)&is.na(n_app), FALSE, TRUE),
         inProject = ifelse(is.na(n_part)&!is.na(n_app), FALSE, TRUE),
         ORDRE_PART_p = ifelse(is.na(ORDRE_PART_p)&inProposal==TRUE, ORDRE_PART, ORDRE_PART_p),
         ORDRE_PART = ifelse(inProject==FALSE, NA_character_, ORDRE_PART),
         PIC_ID2_p = ifelse(is.na(PIC_ID2_p)&inProposal==TRUE, PIC_ID2, PIC_ID2_p),
         PIC_ID2 = ifelse(inProject==FALSE, NA_character_, PIC_ID2),) %>% 
  rename_with(.fn = ~str_c("PROPOSAL_", .) %>% str_replace("_p", ""), .cols = ends_with(c('_p'))) %>% 
  mutate(calculated_pic = ifelse(!is.na(PIC_ID2), PIC_ID2, PROPOSAL_PIC_ID2),
         PROPOSAL_CODE_ORG = ifelse(is.na(PROPOSAL_CODE_ORG)&inProposal==TRUE, CODE_ORG, PROPOSAL_CODE_ORG))


lien <- lien %>% 
  group_by(NUM_PROJET, PROPOSAL_ORDRE_PART, CODE_ORG, calculated_pic) %>% 
  mutate(projNlien=n_distinct(ORDRE_PART)) %>% 
  ungroup() %>% 
  group_by(NUM_PROJET, ORDRE_PART, CODE_ORG, calculated_pic) %>% 
  mutate(propNlien=n_distinct(PROPOSAL_ORDRE_PART)) %>% 
  ungroup() 


nrow(lien %>% dplyr::filter(is.na(lien$calculated_pic)))


temp=anti_join(part2 %>% mutate(PIC_ID2=as.character(PIC_ID2)), lien, by=c('NUM_PROJET',"ORDRE_PART", 'CODE_ORG', 'PIC_ID2'))
temp=anti_join(app2 , lien, by=c('NUM_PROJET',"ORDRE_PART"="PROPOSAL_ORDRE_PART", 'CODE_ORG'='PROPOSAL_CODE_ORG', 'PIC_ID2'='PROPOSAL_PIC_ID2'))

subv_p <- lien %>% 
  left_join(participant %>% mutate(PIC_ID2=as.character(PIC_ID2), ORDRE_PART=as.character(ORDRE_PART)) %>% 
              select(NUM_PROJET, CODE_ORG, PIC_ID2, ORDRE_PART, ROLE_PART, TYPE_PART, SUBV_NET, CODE_PAYS), 
            by = c('NUM_PROJET', 'CODE_ORG', 'PIC_ID2', 'ORDRE_PART')) %>% 
  mutate(participant_subv = SUBV_NET/propNlien)
sum(subv_p$participant_subv, na.rm=TRUE) # 68 777 704 935



subv_p <- subv_p %>% 
  left_join(applicant %>% select(NUM_PROJET, CODE_ORG, PIC_ID2, ORDRE_PART, PROPOSAL_ROLE_PART=ROLE_PART, PROPOSAL_TYPE_PART=TYPE_PART, SUBV, countryCode=CODE_PAYS), 
            by = c('NUM_PROJET', 'PROPOSAL_CODE_ORG'='CODE_ORG', 'PROPOSAL_PIC_ID2'='PIC_ID2', 'PROPOSAL_ORDRE_PART'='ORDRE_PART')) %>% 
  mutate(applicant_subv = SUBV/projNlien)
sum(subv_p$applicant_subv, na.rm=TRUE) # 687 415 105 573 - 687 415 276 761 =-171 188


subv_p <- subv_p %>% 
  mutate(role=ifelse(is.na(ROLE_PART),PROPOSAL_ROLE_PART, ROLE_PART),
         participates_as=ifelse(is.na(TYPE_PART), PROPOSAL_TYPE_PART, TYPE_PART),
         countryCode=ifelse(!is.na(CODE_PAYS), CODE_PAYS, countryCode)) %>% 
  select(-TYPE_PART, -ROLE_PART, -n_app, -n_part, -base_only, -SUBV_NET, -SUBV, -PIC_ID2, -CODE_PAYS)


rm(list=ls(pattern = "^(lien[1-9]|part[1-9]|app[1-9])"))
gc()
