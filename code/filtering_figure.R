#script to draw figure showing filtering process for dynamic meta-analyis

.libPaths("C:/R/Library")
.Library<-("C:/R/Library")

rm(list = ls())

#load packages
library(tidyverse)
library(cowplot)
library(metafor)
library(data.table)
library(grid)
library(gridExtra)

#load data
inv_data<-read.csv("data/cleaned_data.csv")

########################
#first split - outcomes#
########################
inv_data%>%dplyr::group_by(population)%>%dplyr::summarise(n_studies=length(unique(citation)),k=length(log_response_ratio))%>%print(n=Inf)
#only include outcomes relating to invasive species abundance, condition, etc
inv_data$population<-ifelse(inv_data$population=="Pathogens, pests, weeds, and invasive species","Invasive plants",inv_data$population)

inv_data%>%dplyr::group_by(population)%>%
          dplyr::summarise(n_studies=length(unique(citation)),k=length(log_response_ratio))%>%print(n=Inf)
inv_data%>%dplyr::group_by(hlo)%>%
  dplyr::summarise(n_studies=length(unique(citation)),k=length(log_response_ratio))%>%print(n=Inf)

#subset into different groups for different populations
#1 - Invasive plants, 2 - native plants, 3. animals and 4 - carbon
inv_sub<-subset(inv_data,population=="Invasive plants")
plant_sub<-subset(inv_data,hlo=="Plant abundance"|hlo=="Plant condition"|hlo=="Plant diversity"|hlo=="Plant fecundity")
animal_sub<-subset(inv_data,hlo=="Animal abundance"|hlo=="Animal condition"|hlo=="Animal diversity")
carbon_sub<-subset(inv_data,hlo=="SOil organic carbon"|hlo=="Soil organic matter"|hlo=="Soil microbial biomass")

#run different meta-analysese for each of these subsets
inv_out_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
       control=list(maxiter=1000),data=inv_sub)
plant_out_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
                   control=list(maxiter=1000),data=plant_sub)
animal_out_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
                   control=list(maxiter=1000),data=animal_sub)
carbon_out_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
                   control=list(maxiter=1000),data=carbon_sub)
#put outputs into a dataframe along with details on numbers of studies and comparisons
outcome_results<-data.frame(split=rep("Different\noutcomes",4),
                            outcome=c("Invasive plants","Native plants","Native animals","Carbon"),
                            n=c(165,34,14,11),
                            k=c(4298,863,219,90),
           estimate=c(inv_out_m1$beta,plant_out_m1$beta,animal_out_m1$beta,carbon_out_m1$beta),
           se=c(inv_out_m1$se,plant_out_m1$se,animal_out_m1$se,carbon_out_m1$se))
#convert effect sizes to percentages
outcome_results$perc<-(exp(outcome_results$estimate)-1)*100
outcome_results$lci<-(exp(outcome_results$estimate-(1.96*outcome_results$se))-1)*100
outcome_results$uci<-(exp(outcome_results$estimate+(1.96*outcome_results$se))-1)*100

########################
#second split - species#
########################
inv_sub$species_split<-as.factor(ifelse(inv_sub$species=="Spartina","Spartina","Other species"))
inv_sub%>%dplyr::group_by(species)%>%dplyr::summarise(n_studies=length(unique(citation)),k=length(log_response_ratio))%>%print(n=Inf)
#subset into different groups for different invasive species
#1 - Spartina, 2 - Parrot's feather, 3. Japanese knotweed, and 4 - Giant hogweed
spar_sub<-subset(inv_sub,species=="Spartina")
pf_sub<-subset(inv_sub,species=="Parrot's feather")
jk_sub<-subset(inv_sub,species=="Japanese knotweed")
gh_sub<-subset(inv_sub,species=="Giant hogweed")
#run meta-analyses for each group
spar_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
                   control=list(maxiter=1000),data=spar_sub)
jk_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
                     control=list(maxiter=1000),data=jk_sub)
gh_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
              control=list(maxiter=1000),data=gh_sub)
pf_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
              control=list(maxiter=1000),data=pf_sub)
#put outputs into a dataframe along with details on numbers of studies and comparisons
species_results<-data.frame(split=rep("Different\ninvasives",4),
                            outcome=c("Spartina","Japanese knotweed",
                                      "Parrot's feather","Giant hogweed"),
                            k=c(2386,484,508,199),
                            n=c(89,30,17,9),
                            estimate=c(spar_m1$beta,jk_m1$beta,gh_m1$beta,pf_m1$beta),
                            se=c(spar_m1$se,jk_m1$se,gh_m1$se,pf_m1$se))
species_results$perc<-(exp(species_results$estimate)-1)*100
species_results$lci<-(exp(species_results$estimate-(1.96*species_results$se))-1)*100
species_results$uci<-(exp(species_results$estimate+(1.96*species_results$se))-1)*100

#############################################
#third split - different broad interventions#
#############################################

spar_sub%>%dplyr::group_by(hli)%>%dplyr::summarise(n_studies=length(unique(citation)))%>%print(n=Inf)
#subset into different groups for different types of intervention
#1 - Physical control, 2 - habtiat management, 3. Chemical control, and 4 - Biological control
phys_sub<-subset(spar_sub,hli=="Physical control")
hab_sub<-subset(spar_sub,hli=="Habitat management")
chem_sub<-subset(spar_sub,hli=="Chemical control")
biol_sub<-subset(spar_sub,hli=="Biological control")
#run meta-analyses for each group
phys_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
                control=list(maxiter=1000),data=phys_sub)
hab_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
                    control=list(maxiter=1000),data=hab_sub)
chem_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
               control=list(maxiter=1000),data=chem_sub)
biol_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
               control=list(maxiter=1000),data=biol_sub)
#put outputs into a dataframe along with details on numbers of studies and comparisons
int_results<-data.frame(split=rep("Different\ninterventions",4),
                          outcome=c("Physical interventions","Habitat management",
                         "Chemical control","Biological control"),
                          k=c(1353,578,259,33),
                          n=c(46,30,17,7),
                          estimate=c(phys_m1$beta,hab_m1$beta,chem_m1$beta,biol_m1$beta),
                          se=c(phys_m1$se,hab_m1$se,chem_m1$se,biol_m1$se))
int_results$perc<-(exp(int_results$estimate)-1)*100
int_results$lci<-(exp(int_results$estimate-(1.96*int_results$se))-1)*100
int_results$uci<-(exp(int_results$estimate+(1.96*int_results$se))-1)*100

##############################################
#fourth split - different types of herbicide##
##############################################
chem_sub%>%dplyr::group_by(herbicide_type)%>%
  dplyr::summarise(n_studies=length(unique(citation)),k=length(log_response_ratio))%>%print(n=Inf)
#change name of glyphosate from "Glyphosate   Roundup" to "Glyphosate"
chem_sub$herbicide_type<-ifelse(chem_sub$herbicide_type=="Glyphosate   Roundup","Glyphosate",chem_sub$herbicide_type)
#run meta-analysis for each different type of herbicide and save parameter estimates
herb_results<-NULL
un_herb<-unique(chem_sub$herbicide_type)
for (i in 1:length(un_herb)){
  sub_temp<-subset(chem_sub,herbicide_type==un_herb[i]&!is.na(selected_v)&!is.na(log_response_ratio))
  temp_n<-length(unique(sub_temp$study))
  temp_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
                  control=list(maxiter=1000),data=sub_temp)
  temp_res<-data.frame(split=("Different\nherbicides"),
            outcome=un_herb[i],
            estimate=c(temp_m1$beta),
            se=c(temp_m1$se),
            k=temp_m1$k,n=temp_n)
  temp_res$perc<-(exp(temp_res$estimate)-1)*100
  temp_res$lci<-(exp(temp_res$estimate-(1.96*temp_res$se))-1)*100
  temp_res$uci<-(exp(temp_res$estimate+(1.96*temp_res$se))-1)*100
  herb_results<-rbind(herb_results,temp_res)
}

######################################
#figure###############################
######################################
#combine results for different meta-analyes into one dataframe
combined_results<-rbind(outcome_results,species_results,int_results,herb_results)
#plot results
comb_plot<-combined_results%>%group_by(split)%>%slice_max(k,n=4)%>%
  mutate(outcome=as.factor(outcome),split=as.factor(split))%>%
  mutate(outcome=fct_relevel(outcome,(c("Invasive plants","Native plants","Native animals","Carbon",
                                        "Spartina","Japanese knotweed","Parrot's feather","Giant hogweed",
                                        "Physical interventions","Habitat management",
                                        "Chemical control","Biological control"))),
        split=fct_relevel(split,c("Different\noutcomes","Different\ninvasives",
                          "Different\ninterventions","Different\nherbicides")))%>%
  ggplot(aes(x=perc,xmin=lci,xmax=uci,y=outcome,colour=k,size=k))+
  geom_point()+geom_errorbar(size=0.5)+geom_point(shape=1,colour="black",stroke=1)+
  theme_cowplot()+geom_vline(xintercept=0,lty=2)+
  scale_y_discrete(limits=rev,position = "right")+
  facet_wrap(~split,scales = "free_y",ncol=1)+
  xlab("Percentage change in outcome")+
  scale_x_continuous(breaks = c(-75,-50,-25,0,25))+
  scale_size_continuous(range=c(2, 7), breaks=seq(0, 5000, by=1000),limits=c(1,5000),trans="sqrt")+
  scale_colour_continuous(low="grey90",high="blue3",breaks=seq(0, 5000, by=1000),limits=c(1,5000),trans="sqrt")+
  guides(color= guide_legend(title="no. of comparisons",title.position="top",title.hjust = 0.5), 
         size=guide_legend(title="no. of comparisons",title.position="top",title.hjust = 0.5))+
  theme(legend.position = "right",
        legend.margin=margin(c(5,5,5,5)),
        legend.box.margin=margin(-50,-10,-30,-30))+
  theme(axis.line=element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 4), "cm"),
        strip.background = element_rect(colour = "black",size = 1,fill="#9494F6"),
        panel.border = element_rect(colour = "black",size = 1),
        text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))
ggsave("figures/filter_plot.png",comb_plot,width = 20,height = 30,units = "cm",dpi = 320)#save plot

############################
#tree part of figure########
############################

col2hex("blue2")

library(ggparty)

#adapted example of this for metadataset
unique(inv_data$new_pop)

inv_data$new_pop<-ifelse((grepl("Invasive",inv_data$hlo)),"Invasive plants",NA)
inv_data$new_pop<-ifelse((grepl("Plant",inv_data$hlo)),"Native plants",inv_data$new_pop)
inv_data$new_pop<-ifelse((grepl("Animal",inv_data$hlo)),"Native animals",inv_data$new_pop)
inv_data$new_pop<-ifelse(is.na(inv_data$new_pop),inv_data$population,inv_data$new_pop)

inv_data_split<-inv_data%>%group_by(new_pop,species,hli)%>%dplyr::summarise(k=length(log_response_ratio))%>%print(n=Inf)
inv_data_split$new_pop<-as.factor(inv_data_split$new_pop)
inv_data_split$species<-as.factor(inv_data_split$species)
inv_data_split$hli<-as.factor(inv_data_split$hli)
inv_data_split%>%print(n=Inf)

#different outcomes/populations
sp_pop<-partysplit(1L, index = 1:5)
#different invasive species for invasive plant populations
sp_species_inv<-partysplit(2L, index = 1:8)
#different invasive species for native plants
sp_species_plants<-partysplit(2L, index = 1:7)
#different invasive species for native plants
sp_species_animals<-partysplit(2L, index = 1:3)
#different invasive species for soil
sp_species_soil<-partysplit(2L, index = 1:2)
#different invasive species for water
sp_species_water<-partysplit(2L, index = 1:3)
#different interventions for curly waterweed
sp_species_cw<-partysplit(2L, index = 1:4)
#different interventions for floating pennywort
sp_species_fp<-partysplit(2L, index = 1:2)
#different interventions for giant hogweed
sp_species_gh<-partysplit(2L, index = 1:5)
#different interventions for giant hogweed
sp_species_hb<-partysplit(2L, index = 1:3)
#different interventions for Japanese knotweed
sp_species_jk<-partysplit(2L, index = 1:7)
#different interventions for Nutall's waterweed
sp_species_nw<-partysplit(2L, index = 1:4)
#different interventions for Parrot's feather
sp_species_pf<-partysplit(2L, index = 1:4)
#different interventions for Spartina
sp_species_sp<-partysplit(2L, index = 1:5)
#different interventions for curly waterweed - native species
sp_species_cw_native<-partysplit(2L, index = 1:2)
sp_species_gh_native<-partysplit(2L, index = 1:2)
sp_species_hb_native<-partysplit(2L, index = 1:2)
sp_species_jk_native<-partysplit(2L, index = 1:4)
sp_species_nw_native<-partysplit(2L, index = 1:2)
sp_species_pf_native<-partysplit(2L, index = 1:2)
sp_species_sp_native<-partysplit(2L, index = 1:4)
#different interventions for different invasives - native animals
sp_species_cw_animals<-partysplit(2L, index = 1:2)
sp_species_hb_animals<-partysplit(2L, index = 1:2)
sp_species_sp_animals<-partysplit(2L, index = 1:4)
#different interventions for different invasives - soil
sp_species_hb_soil<-partysplit(2L, index = 1:2)
sp_species_sp_soil<-partysplit(2L, index = 1:4)


inv_data%>%group_by(new_pop)%>%dplyr::summarise(k=length(log_response_ratio))%>%print(n=Inf)
inv_data%>%group_by(new_pop,species)%>%dplyr::summarise(k=length(log_response_ratio))%>%print(n=Inf)
inv_data%>%group_by(new_pop,species,hli)%>%dplyr::summarise(k=length(log_response_ratio))%>%print(n=Inf)

#different high-level interventions
sp_int<-partysplit(3L, index = 1:7)

#plot
pn <- partynode(1L, split = sp_pop,kids=list(#split for different populations
        partynode(2L,split=sp_species_inv,info = 5027,kids=list(#split for different invasive species
        partynode(3L,split=sp_species_cw,info = 172,kids=list(#split for curly waterweed
          partynode(4L,info = 117,),
          partynode(5L, info = 10),
          partynode(6L, info = 20),
          partynode(6L, info = 25))),
        partynode(2L,split=sp_species_fp,info = 28,kids=list(#split for floating pennywort
         partynode(4L, info = 28),
         partynode(5L, info = 1))),                                   
        partynode(2L,split=sp_species_gh ,info = 228,kids=list(#split for giant hogweed
          partynode(4L, info = 6),
          partynode(5L, info = 76),
          partynode(6L, info = 8),
          partynode(6L, info = 84),
          partynode(6L, info = 54))),
        partynode(2L,split=sp_species_hb ,info = 153,kids=list(#split for himlayan balsam
          partynode(7L, info = 24),
          partynode(8L, info = 6),
          partynode(9L, info = 123))),
        partynode(2L, split=sp_species_jk,info = 584,kids=list(#split for japanese knotweed
          partynode(3L, info = 27),
          partynode(3L, info = 310),
          partynode(3L, info = 40),
          partynode(3L, info = 104),
          partynode(3L, info = 29),
          partynode(3L, info = 2),
          partynode(3L, info = 72))),
        partynode(8L, split=sp_species_nw,info = 420,kids=list(#split for Nuttall's waterweed
          partynode(3L, info = 38),
          partynode(3L, info = 140),
          partynode(3L, info = 108),
          partynode(3L, info = 134))),
        partynode(9L,split=sp_species_pf, info = 731,kids=list(#split for PArrot's feather
          partynode(3L, info = 8),
          partynode(3L, info = 654),
          partynode(3L, info = 16),
          partynode(3L, info = 53))),
        partynode(10L, split=sp_species_sp,info = 2711,kids=list(#Split for Spartina
          partynode(3L, info = 33),
          partynode(3L, info = 285),
          partynode(3L, info = 599),
          partynode(3L, info = 275),
          partynode(3L, info = 1519))))),
      partynode(2L,split=sp_species_plants,info = 993,kids=list(#split for native plant species
        partynode(3L, split=sp_species_cw_native,info = 72,kids=list(#split for curly waterweed
          partynode(4L, info = 30),
          partynode(4L, info = 42))),
        partynode(3L, split=sp_species_gh_native,info = 171,kids=list(#split for greater hogweed
          partynode(4L, info = 80),
          partynode(4L, info = 91))),
        partynode(3L, split=sp_species_hb_native,info = 363,kids=list(#split for himalayan balsam
          partynode(4L, info = 363),
          partynode(4L, info = 0))),
        partynode(3L, split=sp_species_jk_native,info = 84,kids=list(#split for japanese knotweed
          partynode(4L, info = 2),
          partynode(4L, info = 18),
          partynode(4L, info = 14),
          partynode(4L, info = 50))),
        partynode(3L, split=sp_species_nw_native,info = 70,kids=list(#split for nuttall's waterweed
          partynode(4L, info = 70),
          partynode(4L, info = 1))),
        partynode(3L, split=sp_species_pf_native,info = 115,kids=list(#split for parrot's feather
          partynode(4L, info = 9),
          partynode(4L, info = 106))),
        partynode(3L, split=sp_species_sp_native,info = 118,kids=list(#split for spartina
          partynode(4L, info = 26),
          partynode(4L, info = 53),
          partynode(4L, info = 4),
          partynode(4L, info = 35))))),
      partynode(2L,split=sp_species_animals,info = 228,kids=list(#split for animal biodiversity
          partynode(3L,split=sp_species_cw_animals, info = 9,kids=list(#split for 
            partynode(4L, info = 9),
            partynode(4L, info = 0))),
          partynode(3L, split=sp_species_hb_animals,info = 40,kids=list(#split for himalayan balsam
            partynode(4L, info = 40),
            partynode(4L, info = 0))),
          partynode(3L, split=sp_species_sp_animals,info = 179,kids=list(#split for spartina
            partynode(4L, info = 59),
            partynode(4L, info = 74),
            partynode(4L, info = 19),
            partynode(4L, info = 27))))),
      partynode(2L,split=sp_species_soil,info = 761,kids=list(#split for soil
        partynode(3L,split=sp_species_hb_soil, info = 314,kids=list(#split for Himalayan balsam
          partynode(4L, info = 314),
          partynode(4L, info = 1))),
        partynode(3L, split=sp_species_sp_soil,info = 447,kids=list(#split for spartina
          partynode(4L, info = 27),
          partynode(4L, info = 120),
          partynode(4L, info = 199),
          partynode(4L, info = 101))))),
  partynode(2L,split=sp_species_water,info = 39,kids=list(#split for water
    partynode(3L, info = 2),
    partynode(4L, info = 27),
    partynode(4L, info = 10)))
))

#plot
pn <- partynode(1L, split = sp_pop,kids=list(#split for different populations
  partynode(2L,split=sp_species_inv,info = 5027,kids=list(#split for different invasive species
    partynode(3L,split=sp_species_cw,info = 172,kids=list(#split for curly waterweed
      partynode(4L,info = 117),
      partynode(5L, info = 10),
      partynode(6L, info = 20),
      partynode(6L, info = 25))),
    partynode(2L,split=sp_species_fp,info = 28,kids=list(#split for floating pennywort
      partynode(4L, info = 28),
      partynode(5L, info = 1))),                                   
    partynode(2L,split=sp_species_gh ,info = 228,kids=list(#split for giant hogweed
      partynode(4L, info = 6),
      partynode(5L, info = 76),
      partynode(6L, info = 8),
      partynode(6L, info = 84),
      partynode(6L, info = 54))),
    partynode(2L,split=sp_species_hb ,info = 153,kids=list(#split for himlayan balsam
      partynode(7L, info = 24),
      partynode(8L, info = 6),
      partynode(9L, info = 123))),
    partynode(2L, split=sp_species_jk,info = 584,kids=list(#split for japanese knotweed
      partynode(3L, info = 27),
      partynode(3L, info = 310),
      partynode(3L, info = 40),
      partynode(3L, info = 104),
      partynode(3L, info = 29),
      partynode(3L, info = 2),
      partynode(3L, info = 72))),
    partynode(8L, split=sp_species_nw,info = 420,kids=list(#split for Nuttall's waterweed
      partynode(3L, info = 38),
      partynode(3L, info = 140),
      partynode(3L, info = 108),
      partynode(3L, info = 134))),
    partynode(9L,split=sp_species_pf, info = 731,kids=list(
      partynode(3L, info = 8),
      partynode(3L, info = 654),
      partynode(3L, info = 16),
      partynode(3L, info = 53))),
    partynode(10L, split=sp_species_sp,info = 2711,kids=list(
      partynode(3L, info = 33),
      partynode(3L, info = 285),
      partynode(3L, info = 599),
      partynode(3L, info = 275),
      partynode(3L, info = 1519))))),
  partynode(2L,split=sp_species_plants,info = 993,kids=list(#split for native plant species
    partynode(3L, split=sp_species_cw_native,info = 72,kids=list(#split for curly waterweed
      partynode(4L, info = 30),
      partynode(4L, info = 42))),
    partynode(3L, split=sp_species_gh_native,info = 171,kids=list(#split for greater hogweed
      partynode(4L, info = 80),
      partynode(4L, info = 91))),
    partynode(3L, split=sp_species_hb_native,info = 363,kids=list(#split for himalayan balsam
      partynode(4L, info = 363),
      partynode(4L, info = 0))),
    partynode(3L, split=sp_species_jk_native,info = 84,kids=list(#split for japanese knotweed
      partynode(4L, info = 2),
      partynode(4L, info = 18),
      partynode(4L, info = 14),
      partynode(4L, info = 50))),
    partynode(3L, split=sp_species_nw_native,info = 70,kids=list(#split for nuttall's waterweed
      partynode(4L, info = 70),
      partynode(4L, info = 1))),
    partynode(3L, split=sp_species_pf_native,info = 115,kids=list(#split for parrot's feather
      partynode(4L, info = 9),
      partynode(4L, info = 106))),
    partynode(3L, split=sp_species_sp_native,info = 118,kids=list(#split for spartina
      partynode(4L, info = 26),
      partynode(4L, info = 53),
      partynode(4L, info = 4),
      partynode(4L, info = 35))))),
  partynode(2L,split=sp_species_animals,info = 228,kids=list(#split for animal biodiversity
    partynode(3L,split=sp_species_cw_animals, info = 9,kids=list(#split for 
      partynode(4L, info = 9),
      partynode(4L, info = 0))),
    partynode(3L, split=sp_species_hb_animals,info = 40,kids=list(#split for himalayan balsam
      partynode(4L, info = 40),
      partynode(4L, info = 0))),
    partynode(3L, split=sp_species_sp_animals,info = 179,kids=list(#split for spartina
      partynode(4L, info = 59),
      partynode(4L, info = 74),
      partynode(4L, info = 19),
      partynode(4L, info = 27))))),
  partynode(2L,split=sp_species_soil,info = 761,kids=list(#split for soil
    partynode(3L,split=sp_species_hb_soil, info = 314,kids=list(#split for Himalayan balsam
      partynode(4L, info = 314),
      partynode(4L, info = 1))),
    partynode(3L, split=sp_species_sp_soil,info = 447,kids=list(#split for spartina
      partynode(4L, info = 27),
      partynode(4L, info = 120),
      partynode(4L, info = 199),
      partynode(4L, info = 101))))),
  partynode(2L,split=sp_species_water,info = 39,kids=list(#split for water
    partynode(3L, info = 2),
    partynode(4L, info = 27),
    partynode(4L, info = 10)))
))

split_names<-c("No","Yes",rep("No",36),"Yes",rep("No",4),"Yes","Yes2",rep("No",6),"Yes2","Yes2",rep("No",42))
py <- party(pn, inv_data_split,names=split_names)

meta_tree<-ggparty(py) +
  geom_edge(aes(size=info,colour=split_names))+
  scale_size(range = c(0.25,3))+
  theme(legend.position = "none")+
  geom_node_label(aes(label = "  "),ids="inner",label.r=unit(0, "lines"))+
  scale_color_manual(values=c("grey50","#9494F6","#EFBD92"))+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 2), "cm"))
ggsave("figures/decision_tree.png",width = 30,height=20,dpi=300,units="cm")


tree_grid<-plot_grid(meta_tree,NULL,ncol = 1,rel_heights = c(15,1))
plot_grid(meta_tree,comb_plot,labels=c("(a)","(b)"),rel_widths = c(1.5,2),nrow=1)
ggsave("figures/combined_figure.png",width = 30,height=20,dpi=300,units="cm")

