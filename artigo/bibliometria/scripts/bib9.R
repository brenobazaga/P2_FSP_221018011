.libPaths(c("C:/Users/Breno/Rlib",.libPaths()))
suppressMessages({library(bibliometrix);library(ggplot2);library(ggrepel);library(igraph)})
setwd(file.path(Sys.getenv("S_DIR"),"data")); M<-readRDS("M_bib.rds"); out<-"C:/Users/Breno/bibout"
pal<-c("#1F4E79","#C55A11","#548235","#B8860B","#7030A0","#C00000")
ggnet<-function(A,labs,file,k=2,qthr=.85,seed=3,niter=3000,w=7.5,h=6){
 d<-diag(A);S<-A/outer(d,d);diag(S)<-0;S[is.na(S)]<-0
 g<-graph_from_adjacency_matrix(S,mode="undirected",weighted=TRUE,diag=FALSE);ew<-E(g)$weight;keep<-ew>=quantile(ew,qthr)
 for(v in V(g)){ie<-incident(g,v);o<-ie[order(-E(g)$weight[ie])][1:min(k,length(ie))];keep[as.integer(o)]<-TRUE}
 g<-delete_edges(g,E(g)[!keep]);cl<-membership(cluster_louvain(g));set.seed(seed);lay<-layout_with_fr(g,niter=niter)
 nd<-data.frame(no=V(g)$name,x=lay[,1],y=lay[,2],cluster=factor(cl[V(g)$name]),freq=d[V(g)$name],lab=labs[V(g)$name],stringsAsFactors=FALSE)
 ed<-as_data_frame(g,"edges");ed$x<-nd$x[match(ed$from,nd$no)];ed$y<-nd$y[match(ed$from,nd$no)];ed$xend<-nd$x[match(ed$to,nd$no)];ed$yend<-nd$y[match(ed$to,nd$no)]
 p<-ggplot()+geom_segment(data=ed,aes(x,y,xend=xend,yend=yend,linewidth=weight),color="grey60",alpha=.5)+scale_linewidth(range=c(.2,1.6),guide="none")+geom_point(data=nd,aes(x,y,size=freq,color=cluster),alpha=.9)+scale_size(range=c(2.5,12),guide="none")+scale_color_manual(values=pal,guide="none")+geom_text_repel(data=nd,aes(x,y,label=lab),size=2.9,max.overlaps=Inf,box.padding=.35,point.padding=.2,segment.color="grey70",segment.size=.2,min.segment.length=0.1,seed=1)+theme_void()+theme(plot.margin=margin(4,4,4,4))
 ggsave(file,p,width=w,height=h,dpi=200);data.frame(no=nd$no,rotulo=nd$lab,cluster=as.integer(nd$cluster),freq=nd$freq)}
NM<-biblioNetwork(M,analysis="co-occurrences",network="author_keywords",sep=";");A<-as.matrix(NM);top<-names(sort(diag(A),decreasing=TRUE))[1:36];A<-A[top,top]
labs<-setNames(tolower(top),top);kc<-ggnet(A,labs,file.path(out,"fig4_keywords_rede.png"),k=2,qthr=.85);write.csv(kc,file.path(out,"kw_clusters_final.csv"),row.names=FALSE)
x<-readRDS(file.path(out,"cocit_raw.rds"));B<-x$B;meta<-read.csv(file.path(out,"cocitacao_top40_meta.csv"),stringsAsFactors=FALSE)
sobren<-function(a){if(is.na(a))return("NA");p<-strsplit(a," ")[[1]];p[length(p)]}
lab<-paste0(sapply(meta$autor1,sobren)," ",meta$ano);names(lab)<-meta$id
man<-c(W1969701836="Brennan & Buchanan 1980 (a)",W4242087282="Brennan & Buchanan 1980 (b)",W1994719822="Musgrave 1959",W1538299216="Rodden et al. 2003",W3121321214="Besley & Coate 2003",W2100154814="Bradford & Oates 1971 (a)",W1490523263="Bradford & Oates 1971 (b)",W2032099933="Weingast 2009",W2095862456="Oates 2005",W1987261148="Gramlich 1973",W2000638340="Wagner 1977",W4252060949="Wagner 1976",W2095172060="Fisher 1982",W2122621376="Weingast 1995",W2076316673="Weingast et al. 1981",W2124222250="Prud'homme 1995",W1580866068="Bailey 1998",W2135904738="Becker 1996",W2018783730="Pommerehne & Schneider 1978",W2043285762="Niskanen 1971")
lab[names(man)]<-man;ids<-meta$id;A2<-B[ids,ids];cc<-ggnet(A2,lab,file.path(out,"fig7_cocitacao.png"),k=2,qthr=.85,seed=5);write.csv(cc,file.path(out,"cocit_clusters.csv"),row.names=FALSE)
meta$rotulo<-lab[meta$id];write.csv(meta,file.path(out,"cocitacao_top40_meta.csv"),row.names=FALSE);print(cc[order(cc$cluster,-cc$freq),])
