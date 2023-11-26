# modif encoding
fix_encoding <- function(x) {
  Encoding(x) <- "utf-8"
  return(x)
}

#######################################################
creation_dossier <- function() {
  list_doss = c("bd", "open_data", "donnees", "tableau", "bd/project", "bd/proposal")
  for (elem in list_doss) {
    if (length(dir(path=paste0(chemin,livraison, "/", elem)))==0) {
      dir.create(path=paste0(chemin,livraison, "/", elem))
    }
  }
}


call_continu_detect <- function() {
  
 # x <- levels(as.factor(grep("Adhoc", proposal$APPEL, value = TRUE)))
  x <- unique(str_subset(proposal$APPEL, "Adhoc"))
   return(x)
}


#1-extraction de l'année dans le libellé de l'appel à projets
#2-suppression du tiret devant l'année de la variable "year"
#3-sans les champs à années multiples qui sont remplacées par 'null'
# annee_call <- function(var) {
#   return str_extract_all(var, "[^H2020](?<=-)[0-9]{4}")
#   # year = str_extract_all(year, "[0-9]{4}")
#   return(year)
# }

#verification des variables avec des valuers manquantes
controle_NA <- function(var1) {
  
  length(which(is.na(var1)))
}

#traitement des coordinations de projet
role_coordinator <- function(tab1) {
  
  role <- as.data.frame(xtabs(~NUM_PROJET+ROLE_PART, data = tab1))
  xxx <- role %>% 
    dplyr::filter(ROLE_PART == "coordinator" & (Freq < 1 | Freq > 1))

     if (nrow(xxx) > 0) {
     print("pas de coordinateur ou trop")
     return(xxx)
   } else {print('tout va bien')}
}

#traitement du numero d'ordre des non bénéficiaires directs
# ordre_type_part <- function(tab1){
#   
#   tab1 %>% 
#   mutate(temp = ifelse(TYPE_PART != "beneficiary" & ORDRE_PART > 100, trunc(ORDRE_PART/100,0), ORDRE_PART)) %>% 
#   arrange(NUM_PROJET, ORDRE_PART, TYPE_PART, CODE_ORG) %>% 
#   group_by(NUM_PROJET, ORDRE_PART) %>% 
#   mutate(ORDRE_ALL = paste0(temp, 0, seq_len(n())-1)) %>% 
#   ungroup()  
# }
ordre_type_part <- function(tab1){
  
  tab1 %>% arrange(NUM_PROJET, ORDRE_PART, TYPE_PART, PIC_ID2, CODE_ORG) %>% 
    group_by(NUM_PROJET, ORDRE_PART) %>% 
    mutate(ORDRE_ALL = paste0(ORDRE_PART, "-", seq_len(n())-1)) %>% 
    ungroup()  
}

comparaison_df <- function(tab1, tab2) {
  
  comp1 <- setequal(tab1, tab2)
  comp2 <- setequal(tab2, tab1)
  
  if (comp1 == FALSE | comp2 == FALSE){
        comp1 <- anti_join(tab1, tab2)
        comp2 <- anti_join(tab2, tab1)
        comp <- bind_rows(comp1,comp2)
         return(comp)
  } else {
    print("tout va bien")
      }
}

#######################################################################################

#detecte si des participants sont non localisés : vérifier si on ne peut pas corriger
detect_pays_zz <- function(tab1) {
  
  tab_zz <- 
    tab1 %>% 
    dplyr::filter(CODE_PAYS %in% c("ZZ", NA_character_)) %>% 
    select(CODE_ORG, PIC_ID2, NOM_LONG) %>% 
    unique()
    
  
  if (nrow(tab_zz) > 0) {
    
  print("ZZ ou vide détecté, chercher le pays si possible")  
  return(tab_zz) 
  } 
}

#mise à jour des tables PROPOSAL et PROJECT avec les codes regroupement des pays
maj_pays <- function(tab1){
  tab1 %>% 
  left_join(pays, by = "CODE_PAYS") %>%
  mutate(CODE_PAYS = CODE_PAYS_RGP) %>% 
  select(-NOM_STATUT, -NOM_PAYS, -CODE_PAYS_RGP, -CODE_STATUT) %>% 
  left_join(pays, by = "CODE_PAYS") %>%  
  dplyr::rename(pays_lib = NOM_PAYS)
}

#ajout des statuts pays aux deux tables
statut_pays <- function(tab1){
  
  tab1 %>% dplyr::rename(pays_statut_2 = NOM_STATUT) %>% 
    left_join(pays_statut_gen, by = "CODE_STATUT") %>% 
    left_join(pays_statut, by = c("gen_3_pays" = "CODE_STATUT")) %>%
    dplyr::rename(pays_statut_1 = NOM_STATUT) %>% 
    select(-starts_with("gen"), -CODE_STATUT)
}
#############################################################################################

#fonction qui decoupe la var code_theme en generation à partir du 1er caractère
decoupe_variable <- function(var1, fin) {
  str_sub(var1, start = 1, end = fin) 
}

#ajoute aux tables les variables nomenclatures pilier, programme, area
generation_theme <- function(tab1, mln) {

#dans la boucle, le prog va créer un niveau à la fois et lui attribuer les libellés correspondants
  for (i in mln){
   tab1 <- tab1 %>%  mutate( tt = decoupe_variable(CODE_THEME, i)) %>% 
      left_join(themes[,c("CODE_THEME", "abbr", "theme")], by = c("tt" = "CODE_THEME"))
     
    
    if (i == 2) {
      names(tab1)[match("abbr",names(tab1))] <- "pilier_abbr"
      names(tab1)[match("theme",names(tab1))] <- "pilier"
      names(tab1)[match("tt",names(tab1))] <- "pilier_tri"
    }

    if (i == 3){
        names(tab1)[match("abbr",names(tab1))] <- "programme_abbr"
        names(tab1)[match("theme",names(tab1))] <- "programme_lib"
        names(tab1)[match("tt",names(tab1))] <- "programme_tri"
      }

    if (i == 4){
        names(tab1)[match("abbr",names(tab1))] <- "area_abbr"
        names(tab1)[match("theme",names(tab1))] <- "area_lib"
        names(tab1)[match("tt",names(tab1))] <- "area_tri"
      }
  }
  
  return(tab1)
} 

#############################################################################################
group_action <- function(tab, old_var, new_var){
  
  tab %>% mutate(CODE_ACT=if_else(CODE_ACT==old_var, new_var, CODE_ACT))
  
}





###############################################################################################
#transforation de certaines variables avant export
net_var <- function(tab1) {
   tab1 %>% mutate(NUM_PROJET = as.character(as.integer(NUM_PROJET))) 
}

###################################################################

sauve_fin <- function(tab,nom) {
  save(tab, file = str_c(chemin_donnees, nom, ".RData"))
  write.csv2(tab, file = str_c(chemin_tab, nom, ".csv"), na="", fileEncoding = "UTF-8", row.names = F) 
  # save(tab, file = str_c(chemin_donnees, nom, ".RData"))
}

###################################################################################################
resave <- function(tables, file) { 
  previous  <- load(file)
  if(!is.vector(tables)) stop("argument 'tables' has to be a vector, use c('table_1', 'table_2', ...)") 
  var.names <- tables
  for (var in var.names) assign(var, get(var, envir = parent.frame())) 
  save(list = unique(c(previous, var.names)), file = file)
} 

#########################################################################################################
#doublement des varibales SUBVENTIONS pour résoudre le problème des NAN dans tableau
no_nan <- function(tab1) {
        tab1 %>% mutate(subv2 = subv, subv = ifelse(is.na(NUM_PROJET), 0, subv),
                        subv_t2 = subv_t, subv_t = ifelse(!is.na(NUM_PROJET), 0, subv_t),
                        subv_net2 = subv_net, subv_net = ifelse(is.na(NUM_PROJET), 0, subv_net),
                        subv_net_t2 = subv_net_t, subv_net_t = ifelse(!is.na(NUM_PROJET), 0, subv_net_t))

}

###################################################################################################"
REG_ID <- function(){

# traitement sur le siren_corda à voir comment gérer
retour <- participant %>% 
  dplyr::filter(CODE_PAYS=="FR") %>% 
  select(PIC_ID, PIC_ID2, name_source, CODE_PAYS, CD_REG_ID, CD_REG_VAT_ID) %>% 
  unique() %>%
  mutate(id0 = str_replace_all(CD_REG_ID, "N/A|n/a", NA_character_),
         id0 = str_replace_all(id0, "^[[:punct:]]+$", NA_character_)) %>% 
  tidyr::separate(id0, into=c("id1", "id2"), sep="/") %>% 
  mutate(id3 = if_else(str_detect(id1, "^W"), id1, str_replace_all(id1, "\\D+", "")),
         id3 = str_replace_all(id3, " +", ""),
         id4 = if_else(nchar(id3) < 9, NA_character_, if_else(nchar(id3) > 9, substr(id3,1,9), id3)),
         CD_REG_VAT_ID = str_replace_all(CD_REG_VAT_ID, " +", ""),
         id4 = if_else(is.na(id4) & str_detect(str_sub(CD_REG_VAT_ID,-9), "\\d+") & nchar(CD_REG_VAT_ID) == 13, 
                       str_sub(CD_REG_VAT_ID, -9), 
                       if_else(is.na(id4) & str_detect(str_sub(CD_REG_VAT_ID,-9), "\\d+") & nchar(CD_REG_VAT_ID) == 16,
                               str_sub(CD_REG_VAT_ID,3,11), 
                                if_else(is.na(id4) & str_detect(str_sub(CD_REG_VAT_ID,1,2), "^FR") & nchar(CD_REG_VAT_ID) == 14,
                                       str_sub(CD_REG_VAT_ID,-9),
                                       if_else(is.na(id4) & str_detect(str_sub(CD_REG_VAT_ID), "^\\d+$") & nchar(CD_REG_VAT_ID) == 14,
                                               str_sub(CD_REG_VAT_ID,1,9),                       
                                               id4))))) %>% 
 select(-id1, -id3)

 return(retour)
}

## fonction LOAD avec choix d'importer un objet specifique
load_one_obj <- function(rdata, nom_objet = NULL){
  
  if (!file.exists(rdata)) {
    stop(paste0("Le fichier \"", rdata, "\" n'existe pas."), call. = FALSE)
  }
  
  env = new.env()
  load(file = rdata, envir = env)
  
  if (!is.null(nom_objet)) {
    charger_rdata <- env[[nom_objet]]
  } else {
    charger_rdata <- as.list(env)
  }
  
  return(charger_rdata)
}