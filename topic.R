##############################################################
# TOPICS
#############################################################


topic <- fromJSON(paste0(chemin_bd,"topics.json")) %>% 
  select(topicCode, topicDescription) %>% unique()

proposal <- left_join(proposal, topic, by = "topicCode")
project <- left_join(project, topic, by = "topicCode")

# controle_NA(proposal$CD_TOPIC)
# controle_NA(project$CD_TOPIC)
# 
# controle_NA(proposal$LIB_TOPIC)
# controle_NA(project$LIB_TOPIC)
