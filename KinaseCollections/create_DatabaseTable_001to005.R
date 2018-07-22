#this file creates database table for kinase identifiers

#source('http://depot.sagebase.org/CRAN.R')
#pkgInstall("synapseClient")
#install.packages("rstudioapi")

library(synapseClient)
library(dplyr)
library(tidyr)
library(ggplot2)
`%nin%`<-Negate(`%in%`)

##################################################################################################################T
# Get user input ------------
##################################################################################################################T
#User input
Synapse_UID<-"myusername"
Synapse_password<-"mypassword"
dir_project<-"local project directory"
synapse_ids<-c("syn12176085","syn12074611",
               "syn12074612","syn12617103","syn12617102",
               "syn12617104","syn12617106","syn12617169",
               "syn12617171")#c("synID_1","synID_2",...,"synID_n") ## requires user input
variable_names<-NA#optional: change to c("myname_1","myname_2",...,"myname_n") ## optional user input

dir_databasetables<-"local directory datatable output"

##################################################################################################################T
# make connection to synapse, set directories ------------
##################################################################################################################T
synapseLogin(Synapse_UID, Synapse_password) ## requires user input
c.time<-Sys.time()%>%as.character()%>%gsub("-","",.)%>%gsub(" ","_",.)%>%gsub(":","",.)
paste0(dir_project,paste0(rstudioapi::getActiveDocumentContext()$path%>%dirname()%>%basename()))
dir_c_script<-paste0(dir_project,paste0(rstudioapi::getActiveDocumentContext()$path%>%dirname()%>%basename()))
dir_log<-paste0(dir_project,paste0(rstudioapi::getActiveDocumentContext()$path%>%dirname()%>%basename()),"_SynapseLog")
tempdir<-paste0(dir_project,"temp_local_synapse_files",Sys.time()%>%as.character()%>%gsub("-","",.)%>%gsub(" ","_",.)%>%gsub(":","",.))
dir.create(tempdir, showWarnings = TRUE, recursive = TRUE)
##################################################################################################################T
# get info of files copied in for user to check ------------
##################################################################################################################T
file_info.l<-list()
i=0
for(syn_id in synapse_ids){
  i=i+1
  syn_file<-synGet(syn_id, downloadFile =F, downloadLocation = tempdir, ifcollision = "overwrite.local")
  c.info<-list()
  c.info$file_name<-syn_file[[1]]$name
  c.info$R_variable_name<-ifelse(is.na(variable_names)==T,tools::file_path_sans_ext(c.info$file_name),variable_names[i])
  c.info$id<-syn_file[[1]]$id%>%as.character(.)
  c.info$descreption<-syn_file[[1]]$description
  c.info$url<-syn_file@synapseWebUrl
  c.info$version_number<-syn_file[[1]]$versionNumber
  c.info$versions_label<-syn_file[[1]]$versionLabel
  c.info$DateTime_modified<-syn_file[[1]]$modifiedOn
  file_info.l[[i]]<-c.info%>%as.data.frame(.,stringsAsFactors=F)
}
file_info<-file_info.l%>%bind_rows(.)

writeLines(
  paste0("to be loaded:\n ",
         file_info$file_name%>%toString()%>%gsub(",","\n",.),
         "\n===========================================================
         \nplease check 'file_info' before continuing loading process"))

##################################################################################################################T
# download files from Synapse ------------
##################################################################################################################T

for(syn_id in synapse_ids){
  syn_file<-synGet(syn_id, downloadFile =T, downloadLocation = tempdir, ifcollision = "overwrite.local")
  c.info<-list()
  c.info$file_name<-syn_file[[1]]$name
  c.info$R_variable_name<-ifelse(is.na(variable_names)==T,tools::file_path_sans_ext(c.info$file_name),variable_names[i])
  c.info$id<-syn_file[[1]]$id
  c.info$descreption<-syn_file[[1]]$description
  c.info$url<-syn_file@synapseWebUrl
  c.info$version_number<-syn_file[[1]]$versionNumber
  c.info$versions_label<-syn_file[[1]]$versionLabel
  c.info$DateTime_modified<-syn_file[[1]]$modifiedOn
  file_info.l[[i]]<-c.info%>%as.data.frame(.,stringsAsFactors=F)
}
file_info<-file_info.l%>%bind_rows(.)
synapseLogout()

##################################################################################################################T
# write logfile input ------------
##################################################################################################################T
setwd(dir_c_script)
paste0(dir_log)%nin%list.files()
if(dir_log%nin%list.files()){
  dir.create(dir_log, showWarnings = TRUE, recursive = TRUE)}

currentscript_insert<-paste0("_",
                             rstudioapi::getActiveDocumentContext()$path%>%basename()%>%gsub(".R","",.),
                             "_")
logfile_name<-Sys.time()%>%as.character()%>%gsub("-","",.)%>%
  gsub(" ",currentscript_insert,.)%>%gsub(":","",.)%>%paste0(.,"GMT")%>%
  paste0("FilesUsed_",.,".csv")

#Sys.time()%>%as.character()%>%gsub("-","",.)%>%
#  gsub(" ",currentscript_insert,.)%>%gsub(":","",.)%>%paste0(.,"GMT")%>%
#  paste0("FilesUsed_",.,".csv")

setwd(dir_log)
write.csv(file_info,file = logfile_name,row.names = F)

##################################################################################################################T
# load data into R ------------
##################################################################################################################T
##!!modify to allow more datatypes
setwd(tempdir)
file_info<-file_info%>%arrange(file_name)
files<-ifelse(list.files()%>%sort()==file_info$file_name%>%as.character(),
              list.files(),"something went wrong")
i=0
for(c.file in files){
  i=i+1
  c.varname<-file_info$R_variable_name[i]%>%as.character()
  c.file<-read.csv(c.file, stringsAsFactors = F) #!!modify to allow more datatypes
  assign(c.varname,c.file)
}

##################################################################################################################T
# remove excess variables ------------
##################################################################################################################T
 vars_to_keep<-c(c("%nin%","file_info",file_info$R_variable_name),
                "Synapse_UID","Synapse_password", "dir_project","tempdir",
                "dir_log", "currentscript_insert")
rm(list = ls()[ls()%nin%vars_to_keep])
##################################################################################################################T
# clean tables ------------
##################################################################################################################T
IDG_Kinaselist<-IDG_KinaseList_v20180320%>%
  filter(Keep.Add %in% c("Add",""))%>%
  select(Approved.name, NIH.name, Tier,Pharos.designation,Binds.ATP,X2017.PubMed.Citations,Notes,
         Justification)
IDG_Kinaselist[IDG_Kinaselist==""]<-NA
names(IDG_Kinaselist)<-c("gene_symbol", "name_nih","tier","pharos_designation",
                         "atp_binder","pubmed_citation_2017","comment","justification")
head(IDG_Kinaselist)

kinases_ManningTable<-kinases_ManningTable_v20180424%>%
  select(Entrez_GeneID, Entrez_Symbol, Group, Family, Subfamily)
kinases_ManningTable[kinases_ManningTable==""]<-NA
names(kinases_ManningTable)<-c("gene_id","gene_symbol","group_manning","family_manning","subfamily_manning")
head(kinases_ManningTable)

Kinmap<-Kinmap_v20180623%>%
  select(UniprotID, Group, Family, SubFamily)
names(Kinmap)<-c("uniprot_id","group_kinmap","family_kinmap","subfamily_kinmap")

NCBI_gene_info<-NCBI_gene_info_homosapiens_v20180425%>%
  select(GeneID, Symbol, Full_name_from_nomenclature_authority)
names(NCBI_gene_info)<-c("gene_id","gene_symbol","name")
NCBI_gene_info%>%head()

map_unprotID2geneID_for_kinasedomains_uniprot%>%filter(gene_id=="unmapped")
map_unprotID2geneID_for_kinasedomains_uniprot[map_unprotID2geneID_for_kinasedomains_uniprot$uniprot_id==
                                                "Q6A1A2",]$gene_id<-653650
map_unprotID2geneID_for_kinasedomains_uniprot[map_unprotID2geneID_for_kinasedomains_uniprot$uniprot_id==
                                                "Q96Q04",]$gene_id<-114783
map_unprotID2geneID_for_kinasedomains_uniprot[map_unprotID2geneID_for_kinasedomains_uniprot$uniprot_id==
                                                "A4QPH2",]$gene_id<-375133
map_unprotID2geneID_for_kinasedomains_uniprot[map_unprotID2geneID_for_kinasedomains_uniprot$uniprot_id==
                                                "O43930",]$gene_id<-5616
map_unprotID2geneID_for_kinasedomains_uniprot[map_unprotID2geneID_for_kinasedomains_uniprot$uniprot_id==
                                                "P57078",]$gene_id<-54101
map_unprotID2geneID_for_kinasedomains_uniprot[map_unprotID2geneID_for_kinasedomains_uniprot$uniprot_id==
                                                "Q96LW2",]$gene_id<-124923

map_unprotID2geneID_for_Kinmap%>%filter(gene_id=="unmapped")
map_unprotID2geneID_for_Kinmap[map_unprotID2geneID_for_Kinmap$uniprot_id==
                                                "Q6IBK5",]$gene_id<-2962
map_unprotID2geneID_for_Kinmap[map_unprotID2geneID_for_Kinmap$uniprot_id==
                                                "Q96Q04",]$gene_id<-114783#
map_unprotID2geneID_for_Kinmap[map_unprotID2geneID_for_Kinmap$uniprot_id==
                                                "B5MCJ9",]$gene_id<-9866
map_unprotID2geneID_for_Kinmap[map_unprotID2geneID_for_Kinmap$uniprot_id==
                                                "O43930",]$gene_id<-5616#
map_unprotID2geneID_for_Kinmap[map_unprotID2geneID_for_Kinmap$uniprot_id==
                                                "P57078",]$gene_id<-54101#
map_unprotID2geneID_for_Kinmap[map_unprotID2geneID_for_Kinmap$uniprot_id==
                                                "Q96LW2",]$gene_id<-124923#

##################################################################################################################T
# Add gene_id & symbol columns to all tables --> save as database tables  ------------
##################################################################################################################T
IDG_Kinaselist_c<-IDG_Kinaselist%>%
  merge(NCBI_gene_info, by="gene_symbol",all.x = T)%>%
  select(gene_id, gene_symbol, name_nih, name, atp_binder,pharos_designation,pubmed_citation_2017,
         comment, tier,justification)
dim(IDG_Kinaselist[IDG_Kinaselist$gene_symbol %nin% IDG_Kinaselist_c$gene_symbol,])[1]==0
IDG_Kinaselist_c%>%filter(gene_symbol==""|is.na(gene_symbol)==T)
#setwd(dir_databasetables)
#write.csv(IDG_Kinaselist_c,file="IDG_kinaselist_database.csv",row.names = F)

kinase_domains_uniprot_c<-kinase_domains_uniprot%>%
  merge(map_unprotID2geneID_for_kinasedomains_uniprot, by="uniprot_id", all.x = T)%>%
  merge(NCBI_gene_info, by="gene_id", all.x = T)%>%
  select(gene_id,gene_symbol,uniprot_id,name,uniprot_name)
dim(kinase_domains_uniprot[kinase_domains_uniprot$uniprot_id %nin%
                             kinase_domains_uniprot_c$uniprot_id,])[1]==0
kinase_domains_uniprot_c%>%filter(gene_symbol==""|is.na(gene_symbol)==T)
kinase_domains_uniprot_c%>%filter(gene_id==""|is.na(gene_symbol)==T)
#setwd(dir_databasetables)
#write.csv(kinase_domains_uniprot_c,file="kinasedomainse_uniprot_database.csv",row.names = F)

kinases_ManningTable_c<-kinases_ManningTable%>%
  filter(gene_symbol!=""|is.na(gene_symbol)==F)%>%
  filter(gene_id!=""|is.na(gene_id)==F)
kinases_ManningTable_c%>%filter(gene_symbol==""|is.na(gene_symbol)==T)
kinases_ManningTable_c%>%filter(gene_id==""|is.na(gene_id)==T)
#setwd(dir_databasetables)
#write.csv(kinases_ManningTable_c,file="kinases_ManningTable_database.csv",row.names = F)

Kinmap_c<-Kinmap%>%
  merge(map_unprotID2geneID_for_Kinmap, by="uniprot_id", all.x = T)%>%
  merge(NCBI_gene_info, by="gene_id", all.x = T)%>%
  select(gene_id,gene_symbol, uniprot_id, name, group_kinmap, family_kinmap,subfamily_kinmap)
dim(Kinmap[Kinmap$gene_symbol %nin% Kinmap_c$gene_symbol,])[1]==0
Kinmap_c%>%filter(gene_symbol==""|is.na(gene_symbol)==T)
#setwd(dir_databasetables)
#write.csv(Kinmap_c,file="kinases_Kinmap_database.csv",row.names = F)

##################################################################################################################T
# create final database table  ------------
##################################################################################################################T
all_geneIDs_kinase<-c(IDG_Kinaselist_c$gene_id,
                      kinase_domains_uniprot_c$gene_id,
                      kinases_ManningTable_c$gene_id,
                      Kinmap_c$gene_id)%>%
  unique()

all_kinases_info<-NCBI_gene_info%>%filter(gene_id %in% all_geneIDs_kinase)

all_kinases_c<-all_kinases_info%>%
  mutate(in_manning=ifelse(gene_id %in% kinases_ManningTable_c$gene_id,T,F),
         in_kinmap=ifelse(gene_id %in% Kinmap_c$gene_id,T,F),
         in_uniprot_kinasedomain=ifelse(gene_id %in% kinase_domains_uniprot_c$gene_id,T,F),
         in_IDG_darkkinases=ifelse(gene_id %in% IDG_Kinaselist_c$gene_id,T,F),
         n_pubmed_citations_2013to2018=NA,
         pharos_designation=NA)

#setwd(dir_databasetables)
#write.csv(all_kinases_c,file="all_kinases_table_database.csv",row.names = F)

##################################################################################################################T
# user input for upload  ------------
##################################################################################################################T
file_info_upload<-
  data.frame(Rvariable_name=c("all_kinases_c", 
                                           "kinases_ManningTable_c",
                                           "kinase_domains_uniprot_c",
                                           "Kinmap_c",
                                           "IDG_Kinaselist_c"),
             filenames=c("Table_001_all_kinases.csv",
                                         "Table_002_Manning_kinases.csv",
                                         "Table_003_Uniprot_kinasedomains.csv",
                                         "Table_004_Kinmap_kinases.csv",
                                         "Table_005_IDG_dark_kinome.csv"), 
             file_description=c("Table with kinases from sources Manning, Kinmap,uniprot and IDG dark kinome.",
                                "kinases from Manning (kinase.com) retrieved 20180424",
                                "kinase domains uniprot retrieved [ask Changchang]",
                                "kinases kinmap (kinhub.org) retrieved 20180623",
                                "IDG dark kinome version 20180320"),
             synapse_folder_id=c("syn12176087"),
             synapse_folder_url=c("https://www.synapse.org/Portal.html#!Synapse:syn12176087"),
             date_time=c(Sys.time()%>%as.character()%>%gsub("-","",.)%>%paste0(.,"GMT")),
             Rscript=c(currentscript_insert),
             local_copy_name=c("all_kinases_table_database.csv",
                               "kinases_ManningTable_database.csv",
                               "kinasedomainse_uniprot_database.csv",
                               "kinases_Kinmap_database.csv",
                               "IDG_kinaselist_database.csv"
                               ),
             stringsAsFactors = F)
#synapse_folder<-"syn12176087"

##################################################################################################################T
# create logfile input  ------------
##################################################################################################################T
logfile_name_upload<-Sys.time()%>%as.character()%>%gsub("-","",.)%>%
  gsub(" ",currentscript_insert,.)%>%gsub(":","",.)%>%paste0(.,"GMT")%>%
  paste0("FilesUploaded_",.,".csv")
setwd(dir_log)
write.csv(file_info_upload,logfile_name_upload,row.names = F)

##################################################################################################################T
# upload to synapse  ------------
##################################################################################################################T
synapseLogin(Synapse_UID, Synapse_password)
setwd(tempdir)
for(i in 1:dim(file_info_upload)[1]){
  write.csv(get(file_info_upload$Rvariable_name[i]),file=file_info_upload$filenames[i],row.names = F)
  c.file<-File(paste0(tempdir,"/",file_info_upload$filenames[i]),
             parentId=file_info_upload$synapse_folder_id[i], synapseStore = T)
  synSetAnnotations(c.file)<-list(description = file_info_upload$file_description[i])
  file<-synStore(c.file)
  print(paste0(i,"-",dim(file_info_upload)[1]))
}
synapseLogout()
##################################################################################################################T
# remove tempdir  ------------
##################################################################################################################T
setwd(dir_project)
unlink(tempdir,recursive =T)




