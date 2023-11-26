
subv_p <- bind_rows(applicant, participant)

temp_prop <- subv_p %>% dplyr::filter(CODE_PAYS %in% c("ZZ", NA_character_)) %>% 
  select(CODE_ORG) %>% 
  left_join(applicant, by = c("CODE_ORG")) %>%
  select(CODE_ORG, pays_temp = CODE_PAYS) %>%
  dplyr::filter(!is.na(pays_temp)) %>% 
  group_by(CODE_ORG, pays_temp) %>% mutate(n=n()) %>% 
  group_by(CODE_ORG) %>% 
  dplyr::filter(n==max(n)) %>% 
  ungroup() %>% unique()
subv_p <- 
  subv_p %>% left_join(temp_prop, by = c("CODE_ORG")) %>% 
  mutate(CODE_PAYS = if_else(!is.na(pays_temp), pays_temp, CODE_PAYS)) %>% 
  select(-pays_temp, -n) %>% unique() 
temp_prop <- subv_p %>% dplyr::filter(CODE_PAYS %in% c("ZZ", NA_character_))

sum(subv_p$SUBV_NET, na.rm=TRUE)#68 777 704 935
sum(subv_p[subv_p$stage=='proposal',]$SUBV, na.rm=TRUE)#687 415 105 573
####################################################

# traitement entities

- fusion_entities -> part + H20_entities


####################################################


#############################################################################################
# no_ref <- anti_join(H20_entities, ref, by=c('CODE_ORG'='generalPic', 'countryCode')) %>% 
#   unique() %>% 
#   group_by(CODE_ORG, countryCode) %>% 
#   dplyr::mutate(n=n(), FP='H20') %>% 
#   dplyr::rename(generalPic=CODE_ORG)
# 
# 
# test <- bind_rows(ref, no_ref) %>% select(-n) %>%
#   mutate(FP=ifelse(is.na(FP)|str_detect(FP, 'H20', negate=TRUE), paste(FP,'H20', sep=' '), FP)) %>%
#   separate_rows(FP, sep=" ") %>%
#   unique() %>%
#   group_by(generalPic) %>%
#   arrange(desc(FP), .by_group=TRUE) %>%
#   mutate(FP=str_c(na.omit(unique(FP)), collapse = " ")) %>%
#   ungroup() %>%
#   unique()
# 
# write.csv2(test, file=paste0(chemin, 'datas_work/ref_H20.csv'), na='', fileEncoding = 'UTF-8', row.names = FALSE)
# 
# 
# controle_NA(applicant$CODE_PAYS)
# controle_NA(participant$CODE_PAYS)
# # 
# # 
# temp <- applicant %>% select(CODE_ORG) %>% unique() %>% 
#   rbind(participant %>% select(CODE_ORG) %>% unique()) %>% 
#   unique() %>% 
#   write.csv2('C:/Users/zfriant/Documents/OneDrive/PCRI/eCorda_datas/datas_reference/pic_H2020.csv', row.names=FALSE)


