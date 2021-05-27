#script to clean and format data from metadataset

#########################################################################
#this script is for cleaning data on invasive species from metadataset##
# it does xx main things:
# 1 - removes irrelevant data
# 2 - reformats data to make it more user-friendly
# 3 - adds columns
# 4 - calculate effect sizes


.libPaths("C:/R/Library")
.Library<-("C:/R/Library")

rm(list = ls())


#load packages
library(data.table)
library(ggplot2)
library(stringr)
library(stringi)
library(maps)
library(cowplot)
library(tidyverse)
library(metafor)
library(mice)
library(ggbeeswarm)
library(scales)
library(plyr)



#load dataset
inv_data<-read.csv("data/full_dataset.csv")


##############################################################
#1 - reformat data to make it more user-friendly##############
##############################################################


#extract species name from string with web address of page relating to study
inv_data$species<-str_match(inv_data$publication_id, "subject/\\s*(.*?)\\s*/publication")[,2]
inv_data$species<-mapvalues(inv_data$species,unique(inv_data$species),to=c("Spartina","Himalayan balsam",
                                                         "Giant hogweed",
                                                         "Nuttall's waterweed",
                                                         "Japanese knotweed",
                                                         "Curly waterweed",
                                                         "Parrot's feather",
                                                         "Floating pennywort"))

###################################################################################
#3 - remove irrelevant data########################################################
###################################################################################

#remove unneeded columns
inv_data_sub<-inv_data[,-c(1,3,4,9,10,11,12,36:59,86,87,89:96,99,100,102,22:29)]

##############################################################
#2 - reformat data to make it more user-friendly##############
##############################################################

#make more concise column names
names(inv_data_sub)[c(23:44)]<-
  c("confound","temp_spatial","exp_correl","field_lab","rep_dist",
    "treat_dist","sample_unit_area","plot_homo","unclear_n","ecosystem",
    "inv_time","int_area","int_duration","int_freq","int_time","herbicide_type",
    "herb_rate_foliar","herb_rate_subsurface","introduced_animal","int_animal_density",
    "costs_included","native_non_native")
#shorten citation field
inv_data_sub$study<-gsub("\\[.*?\\]", "", inv_data_sub$citation)


#add columns for each design element
inv_data_sub$ba<-grepl("Before-and-after",inv_data_sub$Design,fixed=TRUE)
inv_data_sub$blocked<-grepl("Blocked",inv_data_sub$Design,fixed=TRUE)
inv_data_sub$controlled<-grepl("Controlled",inv_data_sub$Design,fixed=TRUE)
inv_data_sub$correlative<-grepl("Correlated",inv_data_sub$Design,fixed=TRUE)
inv_data_sub$ma<-grepl("Meta-analysis",inv_data_sub$Design,fixed=TRUE)
inv_data_sub$paired<-grepl("Paired",inv_data_sub$Design,fixed=TRUE)
inv_data_sub$randomised<-grepl("Randomised",inv_data_sub$Design,fixed=TRUE)
inv_data_sub$replicated<-grepl("Replicated",inv_data_sub$Design,fixed=TRUE)

#alter levels for all variables that are strings
#this removes all numbers and white space around the string
cat_vars<-inv_data_sub[,c(23:44)] %>% select_if(is.character) %>% names()

for (i in 1:length(cat_vars)){
  col_vals<-inv_data_sub %>% pull(cat_vars[i])
  new_col_vals<-str_trim(gsub('[0-9]+', '',
                str_replace_all(col_vals,
                "[^[:alnum:]]", " ")))
  inv_data_sub
  col_index<-which(names(inv_data_sub)==cat_vars[i])
  inv_data_sub[,col_index]<-new_col_vals
}

##########################
#alter intervention names#
##########################

#rename interventions so that they are shorter
inv_data_sub$new_int<-mapvalues(inv_data_sub$intervention, from = sort(unique(inv_data_sub$intervention)), 
                           to = c("Biological control","Burning","Burning","Burning","Burning","Covering",
                                  "Cutting and covering","Cutting and flooding","Cutting and replanting",
                                  "Cutting and herbicide","Cutting","Decontamination","Digging and replanting",
                                  "Digging and herbicide","Draining","Excluding grazers","Soil amendment","Flooding",
                                  "Grazing","Grazing","Invasive management","Mowing","Plowing","Habitat restoration",
                                  "Uprooting","Herbicide","Different physical control","Herbicide","Grazing"))



#high-level interventions
inv_data_sub$hli<-mapvalues(inv_data_sub$new_int, from = sort(unique(inv_data_sub$new_int)), 
                       to = c("Biological control","Physical control","Physical control","Physical control",
                              "Physical control","Integrated control","Integrated control","Integrated control",
                              "Decontamination","Physical control","Integrated control","Integrated control",
                              "Habitat management","Habitat management","Habitat management","Habitat management",
                              "Habitat management","Chemical control","Other","Habitat management",
                              "Habitat management","Habitat management","Physical control"))

####################################
#alter outcome names################
####################################
#rename outcomes so that they are shorter

inv_data_sub$new_out<-mapvalues(inv_data_sub$outcome,from=sort(unique(inv_data_sub$outcome)), 
           to = c("Cation exchange capacity","Electrical conductivity","Fish abundance","Fish size",
                  "Fungi abundance","Fungi diversity","Invertebrate abundance","Invertebrate diversity",
                  "Invertebrate abundance","Soil nitrogen","Soil nutrients","Water oxygen","Soil pH",
                  "SOil phosphorus","Plant biomass","Invasive biomass","Invasive condition","Plant cover",
                  "Invasive cover","Invasive damage","Plant density","Invasive density","Plant diversity","Plant evenness",
                  "Plant fecundity","Invasive fecundity","Plant mortality","Invasive mortality","Invasive occurence",
                  "Plant size","Invasive size","Plant survival","Invasive survival","Soil potassium","Soil bulk density",
                  "Soil elements","Soil enzymes","Soil enzymes","Soil formation","Soil microbial biomass",
                  "Soil micronutrients","Soil mineralisation","Soil organic carbon","Soil organic matter",
                  "Soil organic matter","Soil redox potential","Soil pH","Soil respiration","Soil salinity",
                  "Soil temperature","Soil texture","Soil water content","Water chemistry","Water quality",
                  "Water velocity"))
                  

#produce high-level outcomes 
inv_data_sub$hlo<-mapvalues(inv_data_sub$new_out,
                  from=unique(sort(inv_data_sub$new_out)),
                  to=c("Cation exchange capacity","Electrical conductivity","Animal abundance","Animal condition",
                 "Fungi abundance","Fungi diversity","Invasive abundance","Invasive condition",
                 "Invasive abundance","Invasive condition","Invasive abundance","Invasive fecundity","Invasive condition",
                 "Invasive abundance","Invasive condition","Invasive condition","Animal abundance","Animal diversity",
                 "Plant abundance","Plant abundance","Plant abundance","Plant diversity","Plant diversity","Plant fecundity",
                 "Plant condition","Plant condition","Plant condition","Soil bulk density","Soil elements",
                 "Soil enzymes","Soil formation","Soil microbial biomass","Soil micronutrients","Soil mineralisation",
                 "Soil nitrogen","Soil nutrients","Soil organic carbon","Soil organic matter","Soil pH","Soil phosphorus",
                 "Soil potassium","Soil redox","Soil respiration","Soil salinity","Soil temperature","Soil texture",
                 "SOil water content","Water chemistry","Water oxygen","Water quality","Water velocity"))
                 


inv_data_sub$hlo<-ifelse(inv_data_sub$new_out=="invasive biomass"&inv_data_sub$biomass_eq_abundance==FALSE,
                         "invasive condition",inv_data_sub$hlo)
inv_data_sub$hlo<-ifelse(inv_data_sub$new_out=="plant biomass"&inv_data_sub$biomass_eq_abundance==FALSE,
                         "plant condition",inv_data_sub$hlo)


#save data
write.csv(inv_data_sub,"data/cleaned_data.csv")
