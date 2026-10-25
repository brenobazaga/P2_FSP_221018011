.libPaths(c("C:/Users/Breno/Rlib",.libPaths()))
suppressMessages({library(bibliometrix);library(ggplot2);library(jsonlite)})
setwd(file.path(Sys.getenv("S_DIR"),"data")); M<-readRDS("M_bib.rds")
out<-"C:/Users/Breno/bibout";dir.create(out,showWarnings=FALSE)
th<-theme_minimal(base_size=11)+theme(plot.title=element_text(face="bold"),panel.grid.minor=element_blank())
col1<-"#1F4E79";col2<-"#C55A11"
res<-biblioAnalysis(M,sep=";")
# 1 producao anual
pa<-as.data.frame(table(M$PY));names(pa)<-c("ano","n");pa$ano<-as.integer(as.character(pa$ano));write.csv(pa,file.path(out,"producao_anual.csv"),row.names=FALSE)
g<-ggplot(pa,aes(ano,n))+geom_col(fill=col1,width=.8)+geom_smooth(method="loess",se=FALSE,color=col2,linewidth=.9,formula=y~x)+labs(title=NULL,x="Ano de publicação",y="Artigos")+th
ggsave(file.path(out,"fig1_producao_anual.png"),g,width=7,height=3.6,dpi=200)
# 2 fontes
src<-as.data.frame(sort(table(M$SO),decreasing=TRUE)[1:15]);names(src)<-c("fonte","n");src$fonte<-factor(src$fonte,levels=rev(src$fonte));write.csv(src,file.path(out,"top_fontes.csv"),row.names=FALSE)
g<-ggplot(src,aes(fonte,n))+geom_col(fill=col1)+coord_flip()+labs(x=NULL,y="Artigos")+th
ggsave(file.path(out,"fig2_fontes.png"),g,width=7,height=4.4,dpi=200)
# 3 paises (autor correspondente)
ct<-res$CountryCollaboration;ct<-ct[ct$Country!="NA",];ct<-head(ct[order(-(ct$SCP+ct$MCP)),],12);write.csv(ct,file.path(out,"top_paises.csv"),row.names=FALSE)
l<-rbind(data.frame(pais=ct$Country,tipo="Mesmo país (SCP)",n=ct$SCP),data.frame(pais=ct$Country,tipo="Multi-países (MCP)",n=ct$MCP));l$pais<-factor(l$pais,levels=rev(ct$Country))
g<-ggplot(l,aes(pais,n,fill=tipo))+geom_col()+coord_flip()+scale_fill_manual(values=c(col1,col2))+labs(x=NULL,y="Artigos",fill=NULL)+th+theme(legend.position="bottom")
ggsave(file.path(out,"fig3_paises.png"),g,width=7,height=4.4,dpi=200)
# 4 citacoes globais top docs
top<-M[order(-M$TC),c("AU","TI","SO","PY","TC","DI")][1:20,];write.csv(top,file.path(out,"top_citados.csv"),row.names=FALSE)
# 5 keywords
kw<-as.data.frame(sort(table(trimws(unlist(strsplit(M$DE,";")))),decreasing=TRUE)[1:40]);names(kw)<-c("kw","n");write.csv(kw,file.path(out,"top_keywords.csv"),row.names=FALSE)
png(file.path(out,"fig4_keywords_rede.png"),width=1700,height=1300,res=200)
NetMatrix<-biblioNetwork(M,analysis="co-occurrences",network="author_keywords",sep=";")
net<-networkPlot(NetMatrix,normalize="association",n=45,Title=NULL,type="fruchterman",size=TRUE,remove.multiple=FALSE,labelsize=0.9,edgesize=3,cluster="louvain",verbose=FALSE)
dev.off()
saveRDS(net$cluster_res,file.path(out,"kw_clusters.rds")); write.csv(net$cluster_res,file.path(out,"kw_clusters.csv"),row.names=FALSE)
print(head(net$cluster_res,60))
