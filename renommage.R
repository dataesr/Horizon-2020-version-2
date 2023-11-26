#renommage des datas pour traitements classiques

#PROPOSAL
proposal <- load_one_obj(paste0(chemin_bd, "bases.RData"), "proposals")  %>%
  select( NUM_PROJET = proposalNbr, 
          ACRONYM_PROJET = acronym, 
          TITRE_PROJET = title, 
          ABSTRACT = abstract,
          APPEL = callId,
          DATE_CALL = callDeadlineDate,
          CODE_ACT = typeOfActionCode,
          CODE_PANEL = scientificPanel,
          CODE_DG = ecHiearchyResp,
          STATUT_EVAL = stageExitStatus,
          ELIGIBLE = isEligibile,
          COUT_PROJET = budget,
          COUT_PART = requestedGrant,
          NB_PART = numberOfApplicants,
          DURATION = duration,
          KEY_WORDS = freeKeywords,
          DT_SUBMISSION_DATE=submissionDate, eicPanels, topicCode) %>% 
  mutate(NUM_PROJET=as.character(NUM_PROJET))

#PROPOSAL_APPLICANTS   
applicant <- load_one_obj(paste0(chemin_bd, "bases.RData"), "proposals_applicants") %>% 
  select(NUM_PROJET = proposalNbr,
         ORDRE_PART = orderNumber,
         CODE_ORG = generalPic,
         PIC_ID2 = applicantPic,
         NOM_LONG = applicantPicLegalName, 
         NOM_COURT = shortName,
         ROLE_PART = role,
         CODE_PAYS = countryCode,
         PME = isSme,
         SUBV = requestedGrant,
         COUT_TOT_PART = budget,
         URL_PART = url,
         TYPE_ORG = legalEntityTypeCode) %>% 
  mutate_at(.vars = c('NUM_PROJET', 'ORDRE_PART', 'CODE_ORG', 'PIC_ID2'), as.character)


#PROJECT 
project <- load_one_obj(paste0(chemin_bd, "bases.RData"), "projects") %>% 
  select (NUM_PROJET = projectNbr,
          APPEL = callId,
          DATE_CALL = callDeadlineDate,
          CODE_HIERAR = ecHiearchyResp,
          CODE_ACT = typeOfActionCode,
          ACRONYM_PROJET = acronym,
          TITRE_PROJET = title,
          ABSTRACT = abstract,
          STADE_PROJET = projectStatus,
          DATE_START = startDate,
          DATE_END = endDate,
          DATE_SIGNE = ecSignatureDate,
          COUT_PROJET = totalCost,
          COUT_PART = euContribution,
          NB_PART = numberOfParticipants,
          DURATION = duration,
          KEY_WORDS = freeKeywords,
          url_project=url, 
          topicCode) %>% 
  mutate(NUM_PROJET=as.character(NUM_PROJET))



#PROJECT_PARTICIPANT
participant <- load_one_obj(paste0(chemin_bd, "bases.RData"), "projects_participants") %>% 
  select(NUM_PROJET = projectNbr,
         ORDRE_PART = orderNumber,
         CODE_ORG = generalPic,
         PIC_ID2 = participantPic,
         TYPE_PART = partnerType,
         ROLE_PART  = partnerRole,
         NOM_LONG = participantLegalName,
         COUT_TOT_PART = totalCosts,
         SUBV = euContribution,
         SUBV_NET = netEuContribution,
         STATUT_PART = partnerRemovalStatus,
         CODE_PAYS = countryCode,
         TYPE_ORG = legalEntityTypeCode,
         PME = isSme)%>% 
  mutate_at(.vars = c('NUM_PROJET', 'ORDRE_PART', 'CODE_ORG', 'PIC_ID2'), as.character)

# ARTICLE 185 projet
art_proj <-  load_one_obj(paste0(chemin_bd, "bases_2021_12.RData"), "p2p_projects") %>%
  select (CD_PROJ_ID,
          APPEL = LB_NETWORK,
          LB_CALL_TITLE,
          ACRONYM_PROJET = LB_ACRONYM,
          TITRE_PROJET = LB_TITLE,
          DATE_DEBUT = DT_START_DATE,
          DATE_FIN = DT_END_DATE)

art_proj[art_proj == ""]<- NA_character_

art_part <- load_one_obj(paste0(chemin_bd, "bases_2021_12.RData"), "p2p_participants") %>%
  select(CD_PROJ_ID,
         CODE_ORG = CD_PART_PIC,
         PIC_ID2 = CD_APPL_PIC,
         ROLE_PART  = CD_ROLE,
         NOM_LONG = LB_TITLE,
         AM_TOTAL_REQ_GRANT,
         AM_TOTAL_COSTS,
         AM_TOTAL_REQ_OTHER,
         ADDRESS=LB_ADDRESS,
         POSTAL_CODE=CD_POSTAL_CODE,
         NUTS_CODE=CD_NUTS_CODE,
         CITY=LB_CITY,
         CODE_PAYS = CD_COUNTRY,
         LAT=CD_LATITUDE,
         LNG=CD_LONGITUDE,
         TYPE_ORG = CD_ORG_TYPE)

art_part[art_part == ""]<- NA_character_


# #import EIT_KAVA
eit_activities <- load_one_obj(paste0(chemin_bd, "bases_2021_12.RData"), "eit_activities")
eit_activities[eit_activities == ""]<- NA_character_

eit_participant <- load_one_obj(paste0(chemin_bd, "bases_2021_12.RData"), "eit_kava_participants") %>%
  dplyr::rename(CODE_ORG = CD_PART_PIC,
                PIC_ID2 = CD_APPL_PIC,
                role  = CD_ROLE,
                NOM_LONG = LB_PART_LEG_NAM,
                NOM_COURT = LB_PART_SHT_NAM,
                TYPE_ORG = CD_ORG_TYPE,
                CODE_PAYS = CD_PART_COUNTRY,
                ADDRESS=LB_PART_ADRS,
                POSTAL_CODE=CD_PART_POST,
                CITY=LB_PART_CITY,
                NUTS_CODE=CD_PART_NUTS,
                LAT=LATITUDE,
                LNG=LONGITUDE,
                isSme=FL_SME)

eit_participant[eit_participant == ""]<- NA_character_


