livraison = "2023_10"
date_pub = "16 octobre 2023"

chemin = paste0("C:/Users/zfriant/Documents/OneDrive/PCRI/eCorda_datas/");
chemin_bd = "C:/Users/zfriant/Documents/OneDrive/PCRI/eCorda_datas/datas_load/H2020/"

source(file = "fonction.R")


# Import des tables brutes et sauvegarde; lancer le programme renommage pour réimporter des tables R
importation_data <- function(path){
  
  # DOSSIER = "proposal/"
  INPUT_FILE = c("proposals", "proposals_applicants", "projects", "projects_participants", 'legalEntities', 'countries', 'topics', 'typeOfActionsAttributes', 'topicLbDivisions', 'typeOfActions', 'calls')
  for (i in INPUT_FILE){
    assign(i, fromJSON(paste0(path, i, ".json")))
  }
  save(list = c("proposals", "proposals_applicants","projects", "projects_participants", "legalEntities", 'countries', 'topics', 'typeOfActionsAttributes', 'topicLbDivisions', 'typeOfActions', 'calls'),
       file = paste0(path, "bases.RData"))
}

importation_data(chemin_bd)


#chargement des tables RData après importation des tables sources et renommage des variables

- renommage


#maj les call continu : surtout pour le traitement de l'année de call
call_continu_detect()
call_continu = c("H2020-Adhoc-2014-20", "EURATOM-Adhoc-2014-20")


##################Traitements particuliers :
## -année d'exercice
## -statut : verifier qu'il n'y a pas une nouvelle modalité à prendre en compte
## -role : vérifie que tous les projets ont un coordinateur, à développer si dans prochaine actualisation
#ce n'est pas le cas (voire le programme SAS qui règle le problème)
## -suppression :
# des projets COST
# des contributions pour ESA (999644559), CERN (999988133), GEANT LIMITED (999740589)

- nettoyage 


rm(verif_annee_project,verif_annee_proposal)
gc()

save( "applicant", "proposal", "participant", "project", file=paste0(chemin_bd, "bases_tempory.Rdata"))
#############################################################
# applicant <- load_one_obj(rdata=paste0(chemin_bd, "bases_tempory.Rdata"), "applicant")
# project <- load_one_obj(rdata=paste0(chemin_bd, "bases_tempory.Rdata"), "project")
# load(file=paste0(chemin_bd, "bases_tempory.Rdata"))
sum(applicant$SUBV, na.rm=TRUE) # 689 808 490 246
sum(participant$SUBV_NET, na.rm=FALSE)#68 777 704 935


applicant <- applicant %>% 
  dplyr::filter(CODE_ORG!=-1, NUM_PROJET %in% prop_act) %>% 
  mutate(CODE_ORG=ifelse(CODE_ORG=='0', paste(NUM_PROJET,ORDRE_PART,sep='-'), CODE_ORG),
         stage='proposal')


verif <- proposal %>% 
  dplyr::filter(is.na(CODE_ACT) | is.na(topicCode) | is.na(APPEL))
proposal <-proposal %>% 
  mutate(CODE_ACT=ifelse(NUM_PROJET==742498, 'ERC-ADG', CODE_ACT),
         topicCode=ifelse(NUM_PROJET==742498, APPEL, topicCode),
         CODE_ACT=ifelse(NUM_PROJET==645070, 'CSA', CODE_ACT))

sum(applicant$SUBV, na.rm=TRUE) # 687 415 105 573


participant <-participant %>% 
  dplyr::filter(NUM_PROJET %in% prop_act) %>% 
  mutate(stage='project')
sum(participant$SUBV_NET, na.rm=FALSE)#68 777 704 935

############################################################################################

# -fusion_bases -> lien + subv_p
country <- read.csv2(paste0(chemin_bd, 'country_current.csv'), na='')

filename='_id_pic_entities.xlsx'
ref_source = read_excel(paste0("C:/Users/zfriant/Documents/OneDrive/PCRI/eCorda_datas/datas_reference/", filename), 
                        sheet = 'ref', guess_max = 2000, col_types = "text", na="") %>% 
  select(generalPic, countryCode, ZONAGE, id) %>% 
  dplyr::filter(!is.na(id)|!is.na(ZONAGE)) %>% 
  unique()

- pays -> part
rm(list=ls(pattern = "^(pays|COUNTRY|temp|new|tmp|verif|x|test|typ|cj|cat)"))
rm('app','id','zonage','ror','sir','ent')
gc()

legalEntityType <- fromJSON("nomenclature/legalEntityType.json") %>% 
  mutate(legalEntityTypeCode=ifelse(legalEntityTypeCode=='NA', NA_character_, legalEntityTypeCode))
part <- left_join(part, legalEntityType, by='legalEntityTypeCode')

# topic
source(file = "topic.R")
rm(list=ls(pattern = "^(TOPIC|comp)"))
gc()

# theme
source(file = "themes.R")
rm(list=ls(pattern = "^(THEM|mln)"))
gc()

# panel
source(file = "panels.R")
rm(list=ls(pattern = "^(pan|verif|temp)"))
gc()


# actions
source(file = "actions.R")
rm(list=ls(pattern = "^(act|ACT|typeOf|comp)"))
gc()


- nettoyage_autres_bases

art_eit <- load_one_obj(rdata=paste0(chemin_bd,"autres_prog.RData"), 'art_eit') %>% 
  left_join(legalEntityType, by='legalEntityTypeCode')

verif_na <- proposal[apply(proposal, 2, function(x) any(is.na(x)))]
verif_na <- project[apply(project, 2, function(x) any(is.na(x)))]


##################################################################################
p <- merge(part %>% dplyr::filter(stage=='project', TYPE_PART=='beneficiary') %>% 
             group_by(NUM_PROJET) %>% mutate(n_p=n()),
           part %>% dplyr::filter(stage=='proposal', TYPE_PART=='beneficiary') %>% 
             group_by(NUM_PROJET) %>% mutate(n_a=n()), by='NUM_PROJET') %>% 
  select(NUM_PROJET, n_a, n_p) %>% 
  unique() %>% 
  mutate(rapport=(n_p-n_a)/n_a) %>% 
  dplyr::filter(rapport!=0.0) %>% 
  left_join(project %>% select(NUM_PROJET, ACRONYM_PROJET, pilier, programme_code, action_id), by='NUM_PROJET')
write.csv2(p, file="C:/Users/zfriant/Documents/OneDrive/PCRI/bilan H2020/rapport des présences entre proposals et projects.csv", row.names = FALSE)

##################################################################################

projects <- bind_rows(proposal, project) 

calls <- fromJSON(paste0(chemin_bd, "calls.json")) %>% 
  select(callId, callDeadlineDate, proposal_expected_number=expectedNbrProposals, call_budget=callFunding) %>% 
  unique() 

projects <- left_join(projects, calls, by=c('APPEL'='callId', 'DATE_CALL'='callDeadlineDate')) %>% 
  unique()

projects <- inner_join(projects, part, by=c('NUM_PROJET', "stage")) %>% 
  mutate(fund=ifelse(stage=='project', SUBV_NET, SUBV)) %>% 
  dplyr::rename(
    project_id=NUM_PROJET,
    acronym=ACRONYM_PROJET,
    title=TITRE_PROJET,
    abstract=ABSTRACT,
    call_year=ANNEE,
    call_id=APPEL, 
    call_deadline=DATE_CALL,
    status_eval=STATUT_EVAL,
    status_code=STADE_PROJET,
    totalCost=COUT_PROJET,
    proposal_eic_panels=eic_panels,
    project_startDate=DATE_START,
    project_endDate=DATE_END,
    project_ecSignatureDate=DATE_SIGNE,
    project_duration=DURATION,
    proposal_submissionDate=DT_SUBMISSION_DATE,
    topic_id=topicCode,
    topic_lib=topicDescription,
    totalCost_part=COUT_PART,
    project_projectWebpage=url_project,
    generalPic=CODE_ORG,
    participant_pic=PIC_ID2,
    shortName=NOM_COURT,
    role=ROLE_PART,
    participates_as=TYPE_PART,
    partWebpage=webPage,
    subv_benef=SUBV
    ) %>% 
  select(-ELIGIBLE, -CODE_HIERAR, -CODE_DG, -COUT_TOT_PART, -SUBV_NET)
  

sum(projects[projects$stage=='proposal',]$fund, na.rm=TRUE) # 687 415 105 573

sum(projects[projects$stage=='project',]$fund, na.rm=TRUE) # 68 777 704 935



h2020 <- bind_rows(projects, art_eit) %>% 
  mutate(across(where(is.character), ~na_if(.,''))) %>% 
  mutate(framework='Horizon 2020', ecorda_date='2023-10-16')

#sauvagerde des deux tables nettoyées et détaillées
save("art_eit", "projects", file = paste0(chemin_bd,"datas.RData"))
save("h2020", file = paste0(chemin_bd,"H2020_datas.RData"))
