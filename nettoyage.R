##############################################################
# STATUT
##############################################################

n_distinct(project$NUM_PROJET) # 35 931
unique(project$STADE_PROJET)

project <- project %>% mutate(stage='project') %>% unique()

project %>% select(STADE_PROJET, NUM_PROJET) %>% unique() %>% 
  group_by(STADE_PROJET) %>% mutate(n_distinct(as.character(NUM_PROJET))) %>% 
  select(-NUM_PROJET) %>% unique()


#1-jointure avec PROJECT pour mettre à jour les propositions de PROPOSAL en "MAINLIST" et "ELIGIBLE"
#2-correction de "ELIGIBLE" pour les propositions en "MAINLIST"
#3-si "ELIGIBLE" est vide alors ineligible
#4-suppression des propositions ineligibles

n_distinct(proposal$NUM_PROJET) # 305 460
unique(proposal$STATUT_EVAL)

proj_act <- as.vector(unique(project$NUM_PROJET))

proposal <- proposal %>% 
  unique() %>% 
  dplyr::mutate(
    stage='proposal',
    STADE_PROJET = ifelse(NUM_PROJET %in% proj_act, "ELIGIBLE", NA_character_),
    STATUT_EVAL = ifelse(NUM_PROJET %in% proj_act, "MAIN", STATUT_EVAL),
    STADE_PROJET = ifelse(STATUT_EVAL == "MAIN", "ELIGIBLE", STADE_PROJET),
    STADE_PROJET = ifelse(is.na(STADE_PROJET), "ELIGIBLE", STADE_PROJET)) %>% 
  dplyr::filter(!STATUT_EVAL %in% c("INADMISSIBLE","DUPLICATE"))

unique(proposal$STATUT_EVAL)

######################################################################################


prop_act <- as.vector(unique(proposal$NUM_PROJET))

diff <- setdiff(proj_act,prop_act)

###########################################################
# traitement des listes

proposal$free_keywords <- map_chr(proposal$KEY_WORDS, paste, collapse=",")

proposal$eic_panels <-  map_chr(proposal$eicPanels, paste, collapse=",")

project$free_keywords <- map_chr(project$KEY_WORDS, paste, collapse=",")


proposal <- proposal %>% 
  mutate_at(.vars=c('TITRE_PROJET','ABSTRACT','ACRONYM_PROJET'), .funs=~str_replace(., '\\n|\\t|\\r|\\s+', ' ') %>% 
              str_squish(.)) %>% 
  select(-KEY_WORDS, -eicPanels)

project <- project %>% 
  mutate_at(.vars=c('TITRE_PROJET','ABSTRACT','ACRONYM_PROJET'), .funs=~str_replace(., '\\n|\\t|\\r|\\s+', ' ') %>% 
              str_squish(.)) %>% 
  select(-KEY_WORDS)  

##################################################################
# ANNEE D'EXERCICE
##################################################################

#1-extraction de l'année dans le libellé de l'appel à projets
#3-creation de "DATE" sans les champs à années multiples qui sont remplacées par 'null'
#4-creation de "DATE_SUBM" pour l'extraction de l'année de la var "DT_SUBMISSION_DATE"
#5-creation de "ANNEE", si "DATE" = 'null' alors on prend "DATE_SUBM"
#6-si call continu, utilisation de l'année de "DATE_SUBM"
#7-changer 2013 en 2014
#8-ramener les années ultérieures à maintenant

proposal <- proposal %>% 
  dplyr::mutate(ANNEE = str_extract_all(str_replace(APPEL, '^H2020', ''),"[0-9]{4}"), ANNEE = ifelse(nchar(ANNEE)!=4, '', ANNEE)) %>% 
  unnest(cols = c(ANNEE)) %>% 
  dplyr::mutate(ANNEE = ifelse(ANNEE=='' & str_detect(APPEL, 'ERC-VPRES'), str_c('20', str_extract(APPEL,'[0-9]{2}$')), ANNEE),
                ANNEE = ifelse(ANNEE=='', format(ymd(DATE_CALL), "%Y"), ANNEE),
                ANNEE = ifelse(APPEL %in% call_continu, format(ymd(DT_SUBMISSION_DATE), "%Y"), ANNEE),
                ANNEE = ifelse(ANNEE == "2013", "2014", ifelse(ANNEE >= "2021", "2020", ANNEE)),
                ANNEE = as.character(ANNEE))

verif_annee_proposal <- proposal %>% select(NUM_PROJET, ANNEE) %>% 
  unique(.) %>% dplyr::filter(is.na(ANNEE))
if (length(is.na(verif_annee_proposal$ANNEE)) > 0) {
  print ("Manque une année d'exercice, voir table verif_annee_proposal")
} else{
  print("tout va bien")
}

#4-creation de "DATE2" pour extraction de l'année de la "DATE_SIGNE"
#5-creation de "ANNEE", si "DATE" = 'null' alors on prend "DATE_START"
#6-si call continu, utilisation de l'année de "DATE2"
#7-si stade projet est "UNDER_PREPARATION" et sans "DATE2" alors prendre l'année du projet dans PROPOSAL
temp <- proposal %>% select(NUM_PROJET, ANNEE, DT_SUBMISSION_DATE, eic_panels) %>% unique()
project <- project %>% left_join(temp, by='NUM_PROJET')


verif_annee_project <- project %>% 
  select(NUM_PROJET, ANNEE, STADE_PROJET) %>% 
  unique(.) %>% 
  dplyr::filter(is.na(ANNEE), STADE_PROJET != "REJECTED")
if (length(is.na(verif_annee_project$ANNEE)) > 0) {
  print ("Manque année d'exercice, voir table verif_annee_project")
} else {
  print("tout va bien")  
}


################################################################
# ROLE
################################################################

#verification que les vars ROLE_PART et PME ne sont pas manquantes
controle_NA(applicant$ROLE_PART)
unique(applicant$ROLE_PART)
controle_NA(applicant$PME)
unique(applicant$PME)



#transformation de "Partner" en "participant" , ROLE_PART en minuscule 
#si PME est vide mettre non = "N"

sum(applicant %>% select(SUBV), na.rm=T)#689 877 176 491

# 2021 -> 15 912 693 459
# 2020 -> 89 267 459 723 = 105 180 153 182


applicant <- 
  applicant %>% 
  mutate(
    ROLE_PART = tolower(ROLE_PART),
    TYPE_PART = ifelse(ROLE_PART %in% c("partner", "coordinator"), "beneficiary", ROLE_PART),
    TYPE_PART = ifelse(TYPE_PART == "associatedpartner", "associated partner", TYPE_PART),
    TYPE_PART = ifelse(TYPE_PART == "host", "host", TYPE_PART),
    ROLE_PART = ifelse(ROLE_PART != "coordinator", "partner", ROLE_PART)) %>% 
  unique()

unique(applicant$ROLE_PART)
unique(applicant$TYPE_PART)

#verification que toutes les propositions ont un coordinateur
verif_coord <- role_coordinator(applicant) %>% 
  left_join(proposal, by = "NUM_PROJET") %>% 
  select(NUM_PROJET, Freq)

if (nrow(verif_coord) > 0) {
  verif_coord <- verif_coord %>% 
    left_join(applicant, by = "NUM_PROJET") %>% 
    mutate(prem = ifelse(ORDRE_PART == 1, 1, 0)) %>% 
    group_by(NUM_PROJET, prem) %>% 
    dplyr::summarise(nb = n()) %>% 
    ungroup()
} else {
  print("tout va bien")  
}

#si le participant est unique, le "coordoniser"
# temp <- applicant %>% dplyr::filter(ORDRE_PART == 1) %>% inner_join(verif_coord, by = "NUM_PROJET")
temp <- verif_coord %>% 
  dplyr::filter(nb == 1)
applicant <- applicant %>% 
  mutate(ROLE_PART = ifelse(NUM_PROJET %in% temp$NUM_PROJET, "coordinator", ROLE_PART))

temp <- verif_coord %>% 
  inner_join(applicant, by = "NUM_PROJET") %>% 
  dplyr::filter(ORDRE_PART == 1)


#verification que les vars "ROLE_PART" et "PME" ne sont pas manquantes
unique(participant$ROLE_PART)
unique(participant$TYPE_PART)
unique(participant$PME)

#ROLE_PART en minuscule 
#si PME est vide mettre non = "N"
participant <- participant %>% 
  dplyr::filter(NUM_PROJET %in%proj_act) %>% 
  mutate(ROLE_PART = tolower(ROLE_PART),
         ROLE_PART = ifelse(ROLE_PART=="participant", "partner", ROLE_PART),
         TYPE_PART = tolower(TYPE_PART),
         TYPE_PART = ifelse(TYPE_PART == "international_partner", "international partner", TYPE_PART),
         TYPE_PART = ifelse(TYPE_PART == "partnerorganization", "organization partner", TYPE_PART),
         ROLE_PART = ifelse(TYPE_PART != "beneficiary", "partner", ROLE_PART),
         PME = ifelse(is.na(PME), FALSE, PME)) %>% 
  unique()

controle_NA(participant$ROLE_PART)
controle_NA(participant$PME)


coord_trop <- role_coordinator(participant) 
if(nrow(coord_trop)>0){
  write.table(coord_trop, file=paste0(chemin, livraison, "/projets_+coord.csv"))
}

if(nrow(coord_trop)>0){
  coord_trop <- coord_trop %>% 
    left_join(participant, by=c("NUM_PROJET", "ROLE_PART")) %>% 
    group_by(NUM_PROJET) %>% 
    mutate(ROLE_PART=ifelse(SUBV>0, 'coordinator', 'partner'), nb=ifelse(ROLE_PART=='coordinator', 1, 0))
  
  verif_coord <- coord_trop %>% 
    group_by(NUM_PROJET) %>% 
    mutate(nb=sum(nb)) %>% 
    select(NUM_PROJET, nb) %>% 
    unique() %>% 
    dplyr::filter(nb>1)
  
    if (nrow(verif_coord)>0){
      print('attention verifier encore les coordinations soit doublon ou manquant')
    } else{print('ok')}
}


################################################################
# ORDRE_PART et type de participation
################################################################

# selection des type_part non bénéficiaires et dont le numero d'ordre > 100 car composé du num du bénéficiaire
# et d'un num commençant par "0" ex : 101 -> beneficiaire = 1 et ordre = 01
applicant <- applicant %>%
  mutate(ORDRE_PART=ifelse(TYPE_PART=='associated partner', str_sub(as.character(ORDRE_PART), 1, str_length(as.character(ORDRE_PART))-2), ORDRE_PART)) %>% 
  ordre_type_part(.)

participant <- ordre_type_part(participant)
