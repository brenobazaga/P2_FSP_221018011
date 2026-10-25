.libPaths(c("C:/Users/Breno/Rlib",.libPaths()))
suppressMessages({library(bibliometrix);library(ggplot2);library(ggrepel)})
setwd(file.path(Sys.getenv("S_DIR"),"data")); M<-readRDS("M_bib.rds"); out<-"C:/Users/Breno/bibout"
th<-theme_minimal(base_size=11)+theme(panel.grid.minor=element_blank())
tm<-thematicMap(M,field="DE",n=250,minfreq=8,stemming=FALSE,size=0.5,n.labels=3,repel=TRUE)
cl<-as.data.frame(tm$clusters);cl$rot<-gsub("\n"," | ",cl$name_full);print(cl[,c("groups","name","freq","centrality","density")])
mc<-median(cl$centrality);md<-median(cl$density)
cl$lab<-sapply(strsplit(cl$name_full,"\n"),function(v)paste(head(v,3),collapse="\n"))
g<-ggplot(cl,aes(centrality,density))+geom_vline(xintercept=mc,linetype=2,color="grey50")+geom_hline(yintercept=md,linetype=2,color="grey50")+geom_point(aes(size=freq,color=factor(groups)),alpha=.55)+geom_label_repel(aes(label=lab),size=3,box.padding=.6,min.segment.length=0)+scale_size(range=c(8,26),guide="none")+scale_color_manual(values=c("#1F4E79","#C55A11","#548235","#7030A0"),guide="none")+labs(x="Centralidade (relevância)",y="Densidade (desenvolvimento)")+th+annotate("text",x=Inf,y=Inf,label="Temas motores",hjust=1.05,vjust=1.5,color="grey40")+annotate("text",x=-Inf,y=Inf,label="Temas de nicho",hjust=-.05,vjust=1.5,color="grey40")+annotate("text",x=Inf,y=-Inf,label="Temas básicos",hjust=1.05,vjust=-.7,color="grey40")+annotate("text",x=-Inf,y=-Inf,label="Emergentes/em declínio",hjust=-.05,vjust=-.7,color="grey40")+coord_cartesian(clip="off")
ggsave(file.path(out,"fig5_mapa_tematico.png"),g,width=7.5,height=5.6,dpi=200)
tt<-fieldByYear(M,field="DE",timespan=c(2000,2024),min.freq=25,n.items=2,graph=FALSE);d<-as.data.frame(tt$df_graph)
q<-c("fiscal federalism","flypaper effect","fiscal illusion","intergovernmental transfers");d<-d[!tolower(d$item)%in%q,];d<-d[order(d$year_med),]
d$item<-factor(d$item,levels=unique(d$item));print(d)
g<-ggplot(d,aes(y=item))+geom_segment(aes(x=year_q1,xend=year_q3,yend=item),color="#9DB5D1",linewidth=1.4)+geom_point(aes(x=year_med,size=freq),color="#1F4E79")+scale_size(range=c(2,6),name="Frequência")+labs(x="Ano (1º quartil, mediana e 3º quartil)",y=NULL)+th
ggsave(file.path(out,"fig6_trend_topics.png"),g,width=7.5,height=4.8,dpi=200);write.csv(d,file.path(out,"trend_topics.csv"),row.names=FALSE)
