#script to draw decision-tree style diagram

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

#first split - outcomes
inv_data%>%dplyr::group_by(population)%>%dplyr::summarise(n_studies=length(unique(citation)),k=length(log_response_ratio))%>%print(n=Inf)
#only include outcomes relating to invasive species abundance, condition, etc
inv_data$population<-ifelse(inv_data$population=="Pathogens, pests, weeds, and invasive species","Invasive plants",inv_data$population)

inv_data%>%dplyr::group_by(population)%>%
          dplyr::summarise(n_studies=length(unique(citation)),k=length(log_response_ratio))%>%print(n=Inf)
inv_data%>%dplyr::group_by(hlo)%>%
  dplyr::summarise(n_studies=length(unique(citation)),k=length(log_response_ratio))%>%print(n=Inf)

#subset into different groups for different populations
inv_sub<-subset(inv_data,population=="Invasive plants")

plant_sub<-subset(inv_data,hlo=="Plant abundance"|hlo=="Plant condition"|hlo=="Plant diversity"|hlo=="Plant fecundity")
animal_sub<-subset(inv_data,hlo=="Animal abundance"|hlo=="Animal condition"|hlo=="Animal diversity")
carbon_sub<-subset(inv_data,hlo=="SOil organic carbon"|hlo=="Soil organic matter"|hlo=="Soil microbial biomass")
soil_sub<-subset(inv_data,population=="Soil")

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
                            outcome=c("Invasive plants","Native plants",
                                      "Native animals","Carbon"),
                            n=c(165,34,14,11),
                            k=c(4298,863,219,90),
           estimate=c(inv_out_m1$beta,plant_out_m1$beta,animal_out_m1$beta,carbon_out_m1$beta),
           se=c(inv_out_m1$se,plant_out_m1$se,animal_out_m1$se,carbon_out_m1$se))
#convert effect sizes to percentages
outcome_results$perc<-(exp(outcome_results$estimate)-1)*100
outcome_results$lci<-(exp(outcome_results$estimate-(1.96*outcome_results$se))-1)*100
outcome_results$uci<-(exp(outcome_results$estimate+(1.96*outcome_results$se))-1)*100


#second split - species
inv_sub$species_split<-as.factor(ifelse(inv_sub$species=="Spartina","Spartina","Other species"))
inv_sub%>%dplyr::group_by(species)%>%dplyr::summarise(n_studies=length(unique(citation)),k=length(log_response_ratio))%>%print(n=Inf)

spar_sub<-subset(inv_sub,species=="Spartina")
pf_sub<-subset(inv_sub,species=="Parrot's feather")
jk_sub<-subset(inv_sub,species=="Japanese knotweed")
gh_sub<-subset(inv_sub,species=="Giant hogweed")

spar_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
                   control=list(maxiter=1000),data=spar_sub)
jk_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
                     control=list(maxiter=1000),data=jk_sub)
gh_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
              control=list(maxiter=1000),data=gh_sub)
pf_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
              control=list(maxiter=1000),data=pf_sub)


summary(spar_m1)
summary(non_spar_m1)

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


#third split
spar_sub%>%dplyr::group_by(hli)%>%dplyr::summarise(n_studies=length(unique(citation)))%>%print(n=Inf)


phys_sub<-subset(spar_sub,hli=="Physical control")
hab_sub<-subset(spar_sub,hli=="Habitat management")
chem_sub<-subset(spar_sub,hli=="Chemical control")
biol_sub<-subset(spar_sub,hli=="Biological control")


phys_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
                control=list(maxiter=1000),data=phys_sub)
hab_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
                    control=list(maxiter=1000),data=hab_sub)
chem_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
               control=list(maxiter=1000),data=chem_sub)
biol_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
               control=list(maxiter=1000),data=biol_sub)


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

int_plot<-int_results%>%mutate(outcome=fct_relevel(outcome))%>%
  ggplot(aes(x=perc,xmin=lci,xmax=uci,y=outcome,size=n,colour=k))+
  geom_point()+geom_errorbar(size=0.5)+theme_cowplot()+geom_vline(xintercept=0,lty=2)+
  theme(axis.line=element_blank(),
        axis.title.x=element_blank(),axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())+
        theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        plot.background = element_rect(colour = "black",size = 1))+
        facet_wrap(~split)+scale_colour_viridis_b(option = "viridis",limits=c(0,4298))+
        scale_size(range = c(1,6),limits = c(0,165))


plot_grid(outcome_plot,species_plot,int_plot,align = "v",ncol=1)


#fourth split - different types of herbicide
chem_sub%>%dplyr::group_by(herbicide_type)%>%
  dplyr::summarise(n_studies=length(unique(citation)),k=length(log_response_ratio))%>%print(n=Inf)

chem_sub$herbicide_type<-ifelse(chem_sub$herbicide_type=="Glyphosate   Roundup","Glyphosate",chem_sub$herbicide_type)

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



combined_results<-rbind(outcome_results,species_results,int_results,herb_results)


comb_plot<-combined_results%>%group_by(split)%>%slice_max(k,n=4)%>%
  mutate(outcome=as.factor(outcome),split=as.factor(split))%>%
  mutate(outcome=fct_relevel(outcome,(c("Invasive plants","Native plants","Native animals","Carbon",
                                        "Spartina","Japanese knotweed","Parrot's feather","Giant hogweed",
                                        "Physical interventions","Habitat management",
                                        "Chemical control","Biological control"))),
        split=fct_relevel(split,c("Different\noutcomes","Different\ninvasives",
                          "Different\ninterventions","Different\nherbicides")))%>%
  ggplot(aes(x=perc,xmin=lci,xmax=uci,y=outcome,colour=n,size=n))+
  geom_point()+geom_errorbar(size=0.5)+geom_point(shape=1,colour="black",stroke=1)+theme_cowplot()+geom_vline(xintercept=0,lty=2)+
  theme(axis.line=element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        strip.background = element_rect(colour = "black",size = 1),
        panel.border = element_rect(colour = "black",size = 1))+
  scale_y_discrete(limits=rev)+
  facet_wrap(~split,scales = "free_y",ncol=1)+
  xlab("Percentage change in outcome")+
  scale_x_continuous(breaks = c(-75,-50,-25,0,25))+
  scale_size_continuous(range=c(2, 7), breaks=seq(10, 160, by=50),limits=c(1,165),trans="sqrt")+
  scale_colour_continuous(low="grey90",high="blue3",breaks=seq(10, 160, by=50),limits=c(1,165),trans="sqrt")+
  guides(color= guide_legend(title="no. of studies",title.position="top",title.hjust = 0.5), 
         size=guide_legend(title="no. of studies",title.position="top",title.hjust = 0.5))+
  theme(legend.position = "bottom")


ggsave("figures/filter_plot.png",comb_plot,width = 13,height = 30,units = "cm",dpi = 320)


