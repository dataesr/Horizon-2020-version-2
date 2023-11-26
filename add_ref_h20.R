country <- read.csv2(paste0(chemin_bd, 'country_current.csv'), na='') 

filename='_id_pic_entities.xlsx'
ref_source = read_excel(paste0(chemin,"datas_reference/", filename), 
                        sheet = '_id_pic_2023_11', guess_max = 2000, col_types = "text", na="")

ref <- ref_source %>% 
  select(generalPic, countryCode, FP, ZONAGE, id, last_control, legalName, webPage, city, comments ) %>% 
  group_by(generalPic, countryCode) %>% 
  dplyr::mutate(n=n(), across(where(is.character), ~str_c(unique(na.omit(.)), collapse=" | ") %>%  na_if(""))) %>% 
  ungroup() %>% 
  unique()


ref_H20=read_excel(paste0(chemin,"datas_reference/", filename), sheet = 'H20', guess_max = 2000, col_types = "text", na="") %>% 
  group_by(generalPic, countryCode) %>% 
  dplyr::mutate(across(where(is.character)|where(is.logical), ~str_c(unique(na.omit(.)), collapse=" | ") %>%  na_if(""))) %>% 
  ungroup() %>% 
  mutate(last_control=ifelse(!is.na(id)& is.na(last_control), '2023_11', last_control)) %>% 
  unique()

###########################
country_fr <- as.vector(unique(country$countryCode[country$country_code=='FRA']))
fr <- ref_H20 %>% dplyr::filter(countryCode %in% country_fr, is.na(id), !is.na(vat)|!is.na(legalRegNumber)) %>% 
  select(generalPic, countryCode, legalName, vat, legalRegNumber) %>% 
  unique() %>% 
  mutate(vat=toupper(vat), legalRegNumber=toupper(legalRegNumber),
         vat_net=ifelse(str_detect(vat, '^FR'), str_sub(vat, 5, 13), NA_character_),
         vat_net=ifelse(str_detect(vat_net, '^0+$'), NA_character_, vat_net)) %>% 
  tidyr::separate(legalRegNumber, into=c("id1", "id2"), sep="/") %>%
  mutate(id1 = str_replace_all(id1, '^\\D+$',''), id2 = str_replace_all(id2, '^\\D+$',''),
         reg=ifelse(str_detect(id1, '^\\d+$'), id1, NA_character_),
         reg=ifelse(str_detect(id1, '^FR'), str_sub(str_replace_all(id1, " ", ""), 5, 13), reg),
         reg=ifelse(is.na(reg)&str_detect(id1, 'W'), str_extract(id1, 'W\\d{8,9}'), reg),
         reg=ifelse(is.na(reg), str_replace_all(id1, '\\D+', ''), reg),
         reg=str_squish(reg), reg=ifelse(!is.na(reg)&str_detect(reg, '^W', negate=TRUE), str_sub(reg, 1, 9), reg),
         id_a_verif=ifelse(!is.na(vat_net)&reg==str_sub(vat_net,1,9), reg, str_sub(vat_net,1,9)),
         id_a_verif=ifelse(is.na(id_a_verif), str_sub(vat_net,1,9), id_a_verif),
         id_a_verif=ifelse(is.na(id_a_verif)&str_detect(reg,'^W'), reg, id_a_verif),
         id_a_verif=ifelse(is.na(id_a_verif)&str_detect(reg,'^\\d+$'), str_sub(reg,1,9), id_a_verif),
         id_a_verif=ifelse(nchar(id_a_verif)==9|str_detect(id_a_verif, '^W'), id_a_verif, NA_character_)) %>% 
  select(generalPic, countryCode, id_a_verif) %>% dplyr::filter(!is.na(id_a_verif)) %>% unique()
########################

ref_H20 <- left_join(ref_H20, fr) %>% 
  mutate(id=ifelse(is.na(id), id_a_verif, id)) %>% 
  select(-id_a_verif)

write.csv2(ref_H20 %>% select(id) %>% unique(), file=paste0(chemin,'datas_work/id_h20.csv'), row.names=FALSE)

tmp <- anti_join(ref, ref_H20, by=c('generalPic', 'countryCode')) %>% 
  bind_rows(ref_H20)

tmp <- tmp %>% 
  separate_rows(FP, sep=" ") %>% 
  unique() %>% 
  group_by(generalPic) %>% 
  arrange(desc(FP), .by_group=TRUE) %>% 
  mutate(FP=str_c(na.omit(unique(FP)), collapse = " "), FP=na_if(FP, '')) %>% 
  ungroup() %>% 
  unique()

verif_na <- tmp[apply(tmp, 2, function(x) any(is.na(x)))]

non_diffusible <- c('529830432', '527701080','152000014', '151000023')

retour <- read.csv2(file=paste0(chemin,'datas_work/control_ID_2023-11-23T15-14.csv')) %>% 
  mutate(id=ifelse(source_id=='ror', paste0('R',id), id), 
         code=ifelse(str_sub(id,1,9) %in% non_diffusible, 401, code),
         source_id=ifelse(source_id=='identifiantAssociationUniteLegale', 'rna', source_id))


tmp <- left_join(tmp, retour, by='id') %>% 
  mutate(id=ifelse(code==404&source_id!='rna', NA_character_, id), 
         comments=ifelse(code==401, 'non diffusible', comments)) %>% 
  select(-n)

tmp <- tmp %>% 
  left_join(country %>% select(countryCode, country_code_mapping, country_name_mapping, country_code),
            by='countryCode')

write.csv2(tmp, file=paste0(chemin,'datas_work/ref.csv'), row.names=FALSE, na="")
