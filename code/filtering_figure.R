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

#learning how to use ggparty and party packages


inv_data<-read.csv("data/cleaned_data.csv")

#work out first split
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
                            outcome=c("Invasive plants \n (n=165, k=4298)","Native plants \n(n=34, k=863)",
                                      "Native animals\n (n=14, k=219)","Carbon \n (n=11, k=90)"),
           estimate=c(inv_out_m1$beta,plant_out_m1$beta,animal_out_m1$beta,carbon_out_m1$beta),
           se=c(inv_out_m1$se,plant_out_m1$se,animal_out_m1$se,carbon_out_m1$se))
#convert effect sizes to percentages
outcome_results$perc<-(exp(outcome_results$estimate)-1)*100
outcome_results$lci<-(exp(outcome_results$estimate-(1.96*outcome_results$se))-1)*100
outcome_results$uci<-(exp(outcome_results$estimate+(1.96*outcome_results$se))-1)*100

#plot result
outcome_plot<-outcome_results%>%mutate(outcome=fct_relevel(outcome,"Carbon \n (n=11, k=90)",
              "Native animals\n (n=14, k=219)","Native plants \n(n=34, k=863)","Invasive plants \n (n=165, k=4298)"))%>%
              ggplot(aes(x=perc,xmin=lci,xmax=uci,y=outcome,colour=outcome))+
              geom_point(size=5)+geom_errorbar()+theme_cowplot()+geom_vline(xintercept=0,lty=2)+
              theme(axis.line=element_blank(),
              axis.title.x=element_blank(),axis.ticks = element_blank(),
              axis.title.y=element_blank(),legend.position="none",
              panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),plot.background=element_blank())+
              theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
              plot.background = element_rect(colour = "black",size = 1))+
              facet_wrap(~split)+scale_colour_manual(values=c("grey20","grey20","grey20","#f8fac8"))+
              theme(plot.background = element_rect(fill = "grey80"))


#second split - species

unique(inv_sub$species)

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
                            outcome=c("Spartina \n (n=89, k=2386)","Japanese knotweed \n(n=30, k=484)",
                                      "Parrot's feather\n(n=17, k=508)","Giant hogweed\n(n=9, k=199)"),
                            estimate=c(spar_m1$beta,jk_m1$beta,gh_m1$beta,pf_m1$beta),
                            se=c(spar_m1$se,jk_m1$se,gh_m1$se,pf_m1$se))
species_results$perc<-(exp(species_results$estimate)-1)*100
species_results$lci<-(exp(species_results$estimate-(1.96*species_results$se))-1)*100
species_results$uci<-(exp(species_results$estimate+(1.96*species_results$se))-1)*100

species_plot<-species_results%>%mutate(outcome=fct_relevel(outcome,
              rev(c("Spartina \n (n=89, k=2386)","Japanese knotweed \n(n=30, k=484)",
              "Parrot's feather\n(n=17, k=508)","Giant hogweed\n(n=9, k=199)"))))%>%
              ggplot(aes(x=perc,xmin=lci,xmax=uci,y=outcome,colour=outcome))+
              geom_point(size=5)+geom_errorbar()+theme_cowplot()+geom_vline(xintercept=0,lty=2)+
              theme(axis.line=element_blank(), axis.title.x=element_blank(),axis.ticks = element_blank(),
              axis.title.y=element_blank(),legend.position="none",
              panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),plot.background=element_blank())+
              theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
              plot.background = element_rect(colour = "black",size = 1))+
              facet_wrap(~split)+scale_colour_manual(values=c("grey20","grey20","grey20","#dbc8fa"))+
              theme(plot.background = element_rect(fill = "#f8fac8"))

plot_grid(outcome_plot,species_plot,align = "v")

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
summary(phys_m1)
summary(hab_m1)
summary(chem_m1)
summary(biol_m1)


int_results<-data.frame(split=rep("Different\ninterventions",4),
                          outcome=c("Physical interventions \n (n=46, k=1353)","Habitat management \n(n=30, 578)",
                         "Chemical control\n (n=17, k=259)","Biological control\n (n=7, k=33)"),
                          estimate=c(phys_m1$beta,hab_m1$beta,chem_m1$beta,biol_m1$beta),
                          se=c(phys_m1$se,hab_m1$se,chem_m1$se,biol_m1$se))
int_results$perc<-(exp(int_results$estimate)-1)*100
int_results$lci<-(exp(int_results$estimate-(1.96*int_results$se))-1)*100
int_results$uci<-(exp(int_results$estimate+(1.96*int_results$se))-1)*100

int_plot<-int_results%>%mutate(outcome=fct_relevel(outcome))%>%
  ggplot(aes(x=perc,xmin=lci,xmax=uci,y=outcome,colour=outcome))+
  geom_point(size=5)+geom_errorbar()+theme_cowplot()+geom_vline(xintercept=0,lty=2)+
  theme(axis.line=element_blank(),
        axis.title.x=element_blank(),axis.ticks = element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())+
        theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        plot.background = element_rect(colour = "black",size = 1))+
        facet_wrap(~split)+scale_colour_manual(values=c("grey20","grey20","grey20","#c8facb"))+
        theme(plot.background = element_rect(fill = "#dbc8fa"))


plot_grid(outcome_plot,species_plot,int_plot,align = "v",nrow = 2)


#fourth split - different types of herbicide
chem_sub%>%dplyr::group_by(herbicide_type)%>%
  dplyr::summarise(n_studies=length(unique(citation)),k=length(log_response_ratio))%>%print(n=Inf)
#only include outcomes relating to cutting

chem_sub$herbicide_type<-ifelse(chem_sub$herbicide_type=="Glyphosate   Roundup","Glyphosate",chem_sub$herbicide_type)

herb_results<-NULL
un_herb<-unique(chem_sub$herbicide_type)
for (i in 1:length(un_herb)){
  sub_temp<-subset(chem_sub,herbicide_type==un_herb[i]&!is.na(selected_v)&!is.na(log_response_ratio))
  temp_n<-length(unique(sub_temp$study))
  temp_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
                  control=list(maxiter=1000),data=sub_temp)
  temp_res<-data.frame(split=("Different\nherbicides"),
            outcome=paste(un_herb[i],"\n(n=",temp_n,", k=",temp_m1$k,")",sep = ""),
            estimate=c(temp_m1$beta),
            se=c(temp_m1$se),
            k=temp_m1$k,n=temp_n)
  temp_res$perc<-(exp(temp_res$estimate)-1)*100
  temp_res$lci<-(exp(temp_res$estimate-(1.96*temp_res$se))-1)*100
  temp_res$uci<-(exp(temp_res$estimate+(1.96*temp_res$se))-1)*100
  herb_results<-rbind(herb_results,temp_res)
}




herb_plot<-herb_results%>%slice_max(k,n=4)%>%
  ggplot(aes(x=perc,xmin=lci,xmax=uci,y=outcome,colour=outcome))+
  geom_point(size=5)+geom_errorbar()+theme_cowplot()+geom_vline(xintercept=0,lty=2)+
  theme(axis.line=element_blank(),
        axis.title.x=element_blank(),axis.ticks = element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        plot.background = element_rect(colour = "black",size = 1))+
  scale_y_discrete(limits=rev)+
  facet_wrap(~split)+scale_colour_manual(values=c("grey20","grey20","grey20","grey20"))+
  theme(plot.background = element_rect(fill = "#c8facb"))


#combine all plots
comb_plot<-plot_grid(outcome_plot,species_plot,int_plot,herb_plot,align = "v",nrow = 1)

ggsave("figures/filter_plot_new1.png",comb_plot,width = 60,height = 10,units = "cm",dpi = 320)

comb_plot2<-plot_grid(outcome_plot,species_plot,herb_plot,int_plot,align = "hv",nrow = 2)

ggsave("figures/filter_plot_new2.png",comb_plot2,width = 30,height = 20,units = "cm",dpi = 320)

### dummy_data does not exist here yet...
arrow_plot<-ggplot(dummy_data)+
  geom_segment(aes(
  x=0,y=1,
  xend=1.8,yend=1),
  lineend = "round",
  linejoin = "round",
  size=1,
  arrow = arrow(length = unit(0.3, "inches")))+
  geom_text(x=0.8,y=1.3,label="Increasing filtering",size=8)+
  scale_x_continuous(limits=c(0,2))+
  scale_y_continuous(limits=c(0.5,1.5))+theme_void()+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

comb_plot2<-plot_grid(NULL,arrow_plot,ncol=2,rel_widths = c(0.2,1))

comb_plot3<-plot_grid(comb_plot2,comb_plot,ncol = 1,rel_heights = c(0.3,1),rel_widths = c(1,1),align = "hv")


x.grob <- textGrob("Percentage change in outcome", 
                   gp=gpar(col="black", fontsize=14))


comb_plot4<-grid.arrange(arrangeGrob(comb_plot3, bottom = x.grob))

ggsave("figures/filter_plot.png",comb_plot4,width = 60,height = 12,units = "cm",dpi = 320)
