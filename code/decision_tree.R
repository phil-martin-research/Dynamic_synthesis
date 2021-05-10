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

#learning how to use ggparty and party packages

WeatherPlay


inv_data<-read.csv("data/cleaned_data.csv")

names(inv_data)

#work out first split

inv_data%>%dplyr::group_by(species)%>%dplyr::summarise(n_studies=length(unique(citation)))

inv_data$species_split<-as.factor(ifelse(inv_data$species=="Spartina","Spartina","Other species"))


#second split - outcomes
inv_data%>%dplyr::group_by(species_split,hlo)%>%dplyr::summarise(n_studies=length(unique(citation)))%>%print(n=Inf)
#only include outcomes relating to invasive species abundance, condition, etc
inv_data$out_split<-as.factor(ifelse(inv_data$hlo=="Invasive abundance"|inv_data$hlo=="Invasive condition"|
                             inv_data$hlo=="Invasive fecundity","Invasive outcomes","Other outcomes"))


#third split
inv_data%>%dplyr::group_by(species_split,out_split,hli)%>%dplyr::summarise(n_studies=length(unique(citation)))%>%print(n=Inf)

inv_data$phys_split<-as.factor(ifelse(inv_data$hli=="Physical control","Physical control","Other interventions"))

#fourth split - different types of physical intervention
inv_data%>%dplyr::group_by(species_split,out_split,phys_split,new_int)%>%
  dplyr::summarise(n_studies=length(unique(citation)))%>%print(n=Inf)
#only include outcomes relating to cutting
inv_data$cut_split<-as.factor(ifelse(inv_data$new_int=="Cutting"|inv_data$new_int=="CUtting and covering",
                           "Cutting","Other interventions"))

names(inv_data)

#fifth split - cutting frequency
inv_data$freq_split<-as.factor(ifelse(inv_data$int_freq<=1,"<=1 per year",">1 per year"))

inv_data%>%dplyr::group_by(species_split,out_split,phys_split,cut_split,freq_split)%>%
  dplyr::summarise(n_studies=length(unique(citation)))%>%print(n=Inf)

names(inv_data)

#there are 138 studies on Spartina
sp_split <- partysplit(69L,index=2:1)
out_split<-partysplit(70L,index=1:2)
phys_split <-partysplit(71L,index=1:2)
cut_split <-partysplit(72L,index=1:2)
freq_split <-partysplit(73L,index=1:2)




pn <- partynode(1L, split = sp_split,kids=list(
  partynode(2L,split=out_split,kids=list(
    partynode(3L,split=phys_split,kids=list(
      partynode(4L,info = "Yes"),
      partynode(5L,info = "No"))),
    partynode(6L,info="k=30"))),
  partynode(6L,info="k=20")))

pn <- partynode(1L, split = sp_split,kids=list(
  partynode(2L,split=out_split,kids=list(
    partynode(3L,split=phys_split,kids=list(
      partynode(4L,split=cut_split,kids=list(
        partynode(4L,split=freq_split,kids=list(
          partynode(4L,info = "Yes"),
          partynode(5L,info = "No"))),
        partynode(5L,info = "No"))),
      partynode(5L,info = "No"))),
    partynode(6L,info="k=30"))),
        partynode(6L,info="k=20")))


plot(py)


#plot this in ggplot

ggparty(py)+
  geom_edge()+
  geom_edge_label()+
  geom_node_label(aes(label = splitvar), ids = "inner")+
  geom_node_label(aes(label = info), ids = "terminal")

#probably best to patchwork for this in the end
library(patchwork)

#first subset data to only give results for invasive outcomes

#only include outcomes relating to invasive species abundance, condition, etc

invasive_sub<-inv_data%>%dplyr::filter(hlo=="Invasive abundance"|hlo=="Invasive condition"|hlo=="Invasive fecundity")
#run meta-analysis for this
invasive_model<-rma.mv(log_response_ratio,selected_v,data=invasive_sub,random = list(~1|study))
summary(invasive_model)

outcome_result<-data.frame(outcome="Invasive species",b=invasive_model$beta,se=invasive_model$se)
outcome_result$perc<-(exp(outcome_result$b)-1)*100
outcome_result$uci<-(exp(outcome_result$b+(outcome_result$se*1.96))-1)*100
outcome_result$lci<-(exp(outcome_result$b-(outcome_result$se*1.96))-1)*100


outcome_plot<-ggplot(outcome_result,aes(y=outcome,x=perc,xmax=uci,xmin=lci))+
              geom_errorbarh()+geom_point(size=4)+theme_cowplot()

outcome_plot2<-ggplot(outcome_result,aes(y=outcome,x=perc,xmax=uci,xmin=lci))+
  geom_errorbarh()+geom_point(size=4)+theme_cowplot()


row_1<-wrap_plots(plot_spacer(),outcome_plot,plot_spacer(),
                  outcome_plot,plot_spacer(),plot_spacer(),
                  ncol = 3)
row_2<-wrap_plots(outcome_plot,plot_spacer(),plot_spacer(),plot_spacer(),plot_spacer(),ncol=5)
row_1+row_2


plot_spacer()+plot_spacer()+outcome_plot/plot_spacer()+plot_spacer()+outcome_plot+
  plot_layout(ncol=3,widths = c(1,1,1),heights = c(1,1))

####below here is useful code from the ggparty vignette

data("WeatherPlay", package = "partykit")
sp_o <- partysplit(1L, index = 1:3)
sp_h <- partysplit(3L, breaks = 75)
sp_w <- partysplit(4L, index = 1:2)
pn <- partynode(1L, split = sp_o, kids = list(
  partynode(2L, split = sp_h, kids = list(
    partynode(3L, info = "yes"),
    partynode(4L, info = "no"))),
  partynode(5L, info = "yes"),
  partynode(6L, split = sp_w, kids = list(
    partynode(7L, info = "yes"),
    partynode(8L, info = "no")))))
py <- party(pn, WeatherPlay)


n1 <- partynode(id = 1L, split = sp_o, kids = lapply(2L:4L, partynode))
t2 <- party(n1,
            data = WeatherPlay,
            fitted = data.frame(
              "(fitted)" = fitted_node(n1, data = WeatherPlay),
              "(response)" = WeatherPlay$play,
              check.names = FALSE),
            terms = terms(play ~ ., data = WeatherPlay)
)
t2 <- as.constparty(t2)
ggplot(t2[2]$data) +
  geom_bar(aes(x = "", fill = play),
           position = position_fill()) +
  xlab("play")



ggparty(t2) +
  geom_edge() +
  geom_edge_label() +
  geom_node_splitvar() +
  # pass list to gglist containing all ggplot components we want to plot for each
  # (default: terminal) node
  geom_node_plot(gglist = list(geom_bar(aes(x = "", fill = play),
                                        position = position_fill()),
                               xlab("play")))
#try this with my own data

n1 <- partynode(id = 1L, split = sp_split, kids = lapply(2L:3L, partynode))


t2 <- party(n1,
            data = inv_data,
            fitted = data.frame(
              "(fitted)" = fitted_node(n1, data = inv_data),
              "(response)" = inv_data$log_response_ratio,
              check.names = FALSE),
            terms = terms(log_response_ratio ~ ., data = inv_data)
)


t2 <- as.constparty(t2)
ggplot(t2[2]$data) +
  geom_histogram(aes(x = log_response_ratio)) +
  xlab("effect size")

ggparty(t2) +
  geom_edge() +
  geom_edge_label() +
  geom_node_splitvar() +
  # pass list to gglist containing all ggplot components we want to plot for each
  # (default: terminal) node
  geom_node_plot(gglist = list(geom_histogram(aes(x = log_response_ratio)) +
                                 xlab("effect size")))




#another approach
install.packages("DiagrammeR")
library(DiagrammeR)

# A minimal plot
DiagrammeR::grViz("digraph {
  
graph[layout = dot]

 node[shape = box, 
       fontname = Helvetica]

a [label='All data \n k=20']
b [label='Invasive \n abundance & condition']
c [label='Other outcomes']
d [label='Spartina']
e [label='Other species']
f [label='Physical interventions']
g [label='Other interventions']
h [label='Cutting']
i [label='Other physical interventions']

a -> b a->c b->d b->e d->f d->g f->h f->i
}")
