#script to draw decision-tree style diagram


.libPaths("C:/R/Library")
.Library<-("C:/R/Library")

rm(list = ls())


#load packages
library(tidyverse)
library(cowplot)
library(metafor)
library(ggparty)
library(party)
library(data.table)
library(grid)
library(gridExtra)

#learning how to use ggparty and party packages


inv_data<-read.csv("data/cleaned_data.csv")

#work out first split
#first split - outcomes
inv_data%>%dplyr::group_by(hlo)%>%dplyr::summarise(n_studies=length(unique(citation)))%>%print(n=Inf)
#only include outcomes relating to invasive species abundance, condition, etc
inv_data$out_split<-as.factor(ifelse(inv_data$hlo=="Invasive abundance"|inv_data$hlo=="Invasive condition"|
                                       inv_data$hlo=="Invasive fecundity","Invasive outcomes","Other outcomes"))
inv_data%>%dplyr::group_by(out_split)%>%
          dplyr::summarise(n_studies=length(unique(citation)),k=length(log_response_ratio))%>%print(n=Inf)

#subset to remove outcomes other than invasive species
inv_sub<-subset(inv_data,out_split=="Invasive outcomes")
other_out_sub<-subset(inv_data,out_split!="Invasive outcomes")


inv_out_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
       control=list(maxiter=1000),data=inv_sub)
other_out_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
                   control=list(maxiter=1000),data=other_out_sub)

summary(inv_out_m1)
summary(other_out_m1)

outcome_results<-data.frame(split=c("Outcome","Outcome"),
                            outcome=c("Invasive outcomes \n (n=220, k=5027)","Other outcomes (n=82, k=2021)"),
           estimate=c(inv_out_m1$beta,other_out_m1$beta),se=c(inv_out_m1$se,other_out_m1$se))

outcome_results$perc<-(exp(outcome_results$estimate)-1)*100
outcome_results$lci<-(exp(outcome_results$estimate-(1.96*outcome_results$se))-1)*100
outcome_results$uci<-(exp(outcome_results$estimate+(1.96*outcome_results$se))-1)*100


outcome_plot<-outcome_results%>%mutate(outcome=fct_relevel(outcome,rev))%>%
ggplot(aes(x=perc,xmin=lci,xmax=uci,y=outcome))+
  geom_point(size=3)+geom_errorbar()+theme_cowplot()+geom_vline(xintercept=0,lty=2)+
   theme(axis.line=element_blank(),
          axis.title.x=element_blank(),axis.ticks = element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

#second split - species
inv_sub$species_split<-as.factor(ifelse(inv_sub$species=="Spartina","Spartina","Other species"))
inv_sub%>%dplyr::group_by(out_split,species_split)%>%dplyr::summarise(n_studies=length(unique(citation)),k=length(log_response_ratio))%>%print(n=Inf)

spar_sub<-subset(inv_sub,species_split=="Spartina")
non_spar_sub<-subset(inv_sub,species_split!="Spartina")

spar_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
                   control=list(maxiter=1000),data=spar_sub)
non_spar_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
                     control=list(maxiter=1000),data=non_spar_sub)

summary(spar_m1)
summary(non_spar_m1)

species_results<-data.frame(split=c("Species","Species"),outcome=c("Spartina \n (n=122, k=2711)","Other species \n(n=98, 2316)"),
                            estimate=c(spar_m1$beta,non_spar_m1$beta),se=c(spar_m1$se,non_spar_m1$se))
species_results$perc<-(exp(species_results$estimate)-1)*100
species_results$lci<-(exp(species_results$estimate-(1.96*species_results$se))-1)*100
species_results$uci<-(exp(species_results$estimate+(1.96*species_results$se))-1)*100

species_plot<-species_results%>%mutate(outcome=fct_relevel(outcome))%>%
  ggplot(aes(x=perc,xmin=lci,xmax=uci,y=outcome))+
  geom_point(size=3)+geom_errorbar()+theme_cowplot()+geom_vline(xintercept=0,lty=2)+
  theme(axis.line=element_blank(),
        axis.title.x=element_blank(),axis.ticks = element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))


plot_grid(outcome_plot,species_plot,align = "v")

#third split
spar_sub%>%dplyr::group_by(species_split,out_split,hli)%>%dplyr::summarise(n_studies=length(unique(citation)))%>%print(n=Inf)

spar_sub$phys_split<-as.factor(ifelse(spar_sub$hli=="Physical control","Physical control","Other interventions"))

spar_sub%>%dplyr::group_by(species_split,out_split,phys_split)%>%
  dplyr::summarise(n_studies=length(unique(citation)),k=length(log_response_ratio))%>%print(n=Inf)


phys_sub<-subset(spar_sub,phys_split=="Physical control")
non_phys_sub<-subset(spar_sub,phys_split!="Physical control")


phys_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
                control=list(maxiter=1000),data=phys_sub)
non_phys_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
                    control=list(maxiter=1000),data=non_phys_sub)
summary(phys_m1)
summary(non_phys_m1)

phys_results<-data.frame(split=c("physical","physical"),
                         outcome=c("Physical interventions \n (n=54, k=1519)","Other interventions \n(n=68, 1982)"),
                            estimate=c(phys_m1$beta,non_phys_m1$beta),se=c(phys_m1$se,non_phys_m1$se))
phys_results$perc<-(exp(phys_results$estimate)-1)*100
phys_results$lci<-(exp(phys_results$estimate-(1.96*phys_results$se))-1)*100
phys_results$uci<-(exp(phys_results$estimate+(1.96*phys_results$se))-1)*100

phys_plot<-phys_results%>%mutate(outcome=fct_relevel(outcome))%>%
  ggplot(aes(x=perc,xmin=lci,xmax=uci,y=outcome))+
  geom_point(size=3)+geom_errorbar()+theme_cowplot()+geom_vline(xintercept=0,lty=2)+
  theme(axis.line=element_blank(),
        axis.title.x=element_blank(),axis.ticks = element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))


plot_grid(outcome_plot,species_plot,phys_plot,align = "v",nrow = 1)


#fourth split - different types of physical intervention
phys_sub%>%dplyr::group_by(species_split,out_split,phys_split,new_int)%>%
  dplyr::summarise(n_studies=length(unique(citation)),k=length(log_response_ratio))%>%print(n=Inf)
#only include outcomes relating to cutting
phys_sub$cut_split<-as.factor(ifelse(phys_sub$new_int=="Cutting"|phys_sub$new_int=="CUtting and covering",
                           "Cutting","Other interventions"))

phys_sub%>%dplyr::group_by(species_split,out_split,phys_split,cut_split)%>%
  dplyr::summarise(n_studies=length(unique(citation)),k=length(log_response_ratio))%>%print(n=Inf)


cut_sub<-subset(phys_sub,cut_split=="Cutting")
non_cut_sub<-subset(phys_sub,cut_split!="Cutting")

cut_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
                control=list(maxiter=1000),data=cut_sub)
non_cut_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
                    control=list(maxiter=1000),data=non_cut_sub)
summary(cut_m1)
summary(non_cut_m1)


cut_results<-data.frame(split=c("cutting","cutting"),
                         outcome=c("Cutting \n (n=34, k=1289)","Other interventions \n(n=20, 230)"),
                         estimate=c(cut_m1$beta,non_cut_m1$beta),se=c(cut_m1$se,non_cut_m1$se))
cut_results$perc<-(exp(cut_results$estimate)-1)*100
cut_results$lci<-(exp(cut_results$estimate-(1.96*cut_results$se))-1)*100
cut_results$uci<-(exp(cut_results$estimate+(1.96*cut_results$se))-1)*100

cut_plot<-cut_results%>%mutate(outcome=fct_relevel(outcome,rev))%>%
  ggplot(aes(x=perc,xmin=lci,xmax=uci,y=outcome))+
  geom_point(size=3)+geom_errorbar()+theme_cowplot()+geom_vline(xintercept=0,lty=2)+
  theme(axis.line=element_blank(),
        axis.title.x=element_blank(),axis.ticks = element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))


plot_grid(outcome_plot,species_plot,phys_plot,cut_plot,align = "v",nrow = 1)

#fifth split - cutting frequency
cut_sub$freq_split<-as.factor(ifelse(cut_sub$int_freq<=1,"<1 per year",">1 per year"))

cut_sub%>%dplyr::group_by(species_split,out_split,phys_split,cut_split,freq_split)%>%
  dplyr::summarise(n_studies=length(unique(citation)),k=length(log_response_ratio))%>%print(n=Inf)

more_one_cut<-subset(cut_sub,freq_split==">1 per year")
less_one_cut<-subset(cut_sub,freq_split=="<1 per year")

more_cut_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
               control=list(maxiter=1000),data=more_one_cut)
less_cut_m1<-rma.mv(log_response_ratio,selected_v,random=list(~1|study),
                   control=list(maxiter=1000),data=less_one_cut)
summary(more_cut_m1)
summary(less_cut_m1)


freq_results<-data.frame(split=c("frequency","frequency"),
                        outcome=c(">1 per year \n (n=12, k=523)","<1 per year \n(n=22, 625)"),
                        estimate=c(more_cut_m1$beta,non_cut_m1$beta),se=c(less_cut_m1$se,non_cut_m1$se))
freq_results$perc<-(exp(freq_results$estimate)-1)*100
freq_results$lci<-(exp(freq_results$estimate-(1.96*freq_results$se))-1)*100
freq_results$uci<-(exp(freq_results$estimate+(1.96*freq_results$se))-1)*100

freq_plot<-freq_results%>%mutate(outcome=fct_relevel(outcome,rev))%>%
  ggplot(aes(x=perc,xmin=lci,xmax=uci,y=outcome))+
  geom_point(size=3)+geom_errorbar()+theme_cowplot()+geom_vline(xintercept=0,lty=2)+
  theme(axis.line=element_blank(),
        axis.title.x=element_blank(),axis.ticks = element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))



comb_plot<-plot_grid(outcome_plot,species_plot,phys_plot,cut_plot,freq_plot,align = "v",nrow = 1)


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
