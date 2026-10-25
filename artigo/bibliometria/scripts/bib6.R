.libPaths(c("C:/Users/Breno/Rlib",.libPaths()))
suppressMessages({library(igraph);library(jsonlite)})
setwd(file.path(Sys.getenv("S_DIR"),"data")); out<-"C:/Users/Breno/bibout"
x<-readRDS(file.path(out,"cocit_raw.rds"));B<-x$B;top<-x$top
ids<-paste(top,collapse="|")
o<-system2("curl",c("-s","-m","60","-G","https://api.openalex.org/works","--data-urlencode",shQuote(paste0("filter=openalex_id:",ids)),"-d","per-page=100","-d",shQuote("select=id,display_name,publication_year,authorships,primary_location,doi,cited_by_count,type")),stdout=TRUE)
j<-fromJSON(paste(o,collapse=""),simplifyVector=FALSE)$results
meta<-do.call(rbind,lapply(j,function(r){au<-r$authorships;a1<-if(length(au))au[[1]]$author$display_name else NA;n<-length(au);data.frame(id=sub("https://openalex.org/","",r$id),autor1=a1,nautores=n,ano=r$publication_year,titulo=r$display_name,revista=if(!is.null(r$primary_location$source$display_name))r$primary_location$source$display_name else NA,doi=if(!is.null(r$doi))r$doi else NA,tipo=r$type,cit_global=r$cited_by_count)}))
meta$freq_cocit<-diag(B)[match(meta$id,rownames(B))];meta<-meta[order(-meta$freq_cocit),];write.csv(meta,file.path(out,"cocitacao_top40_meta.csv"),row.names=FALSE);options(width=250);print(meta[,c("id","autor1","ano","titulo","freq_cocit")])
sobren<-function(a){p<-strsplit(a," ")[[1]];paste(p[length(p)])}
meta$lab<-paste0(sapply(meta$autor1,sobren)," ",meta$ano)
meta$lab<-ifelse(duplicated(meta$lab)|duplicated(meta$lab,fromLast=TRUE),paste0(meta$lab,letters[ave(seq_along(meta$lab),meta$lab,FUN=seq_along)]),meta$lab)
man<-c(W1969701836="Brennan & Buchanan 1980 (a)",W4242087282="Brennan & Buchanan 1980 (b)",W1994719822="Musgrave 1959",W1538299216="Rodden et al. 2003",W3121321214="Besley & Coate 2003",W2100154814="Bradford & Oates 1971 (a)",W1490523263="Bradford & Oates 1971 (b)",W2032099933="Weingast 2009",W2095862456="Oates 2005",W1987261148="Gramlich 1973",W2000638340="Wagner 1977",W4252060949="Wagner 1976",W2095172060="Fisher 1982",W2122621376="Weingast 1995",W1580866068="Bailey 1998",W2135904738="Becker 1996",W2076316673="Weingast et al. 1981",W2124222250="Prud'homme 1995");meta$lab[match(names(man),meta$id)]<-man[!is.na(match(names(man),meta$id))&TRUE];lab<-setNames(meta$lab,meta$id)
pal<-c("#1F4E79","#C55A11","#548235","#7F6000","#7030A0","#C00000","#2E75B6","#808080")
ok<-meta$id;A<-B[ok,ok];d<-diag(A);S<-A/outer(d,d);diag(S)<-0;S[is.na(S)]<-0
g<-graph_from_adjacency_matrix(S,mode="undirected",weighted=TRUE,diag=FALSE);ew<-E(g)$weight;keep<-ew>=quantile(ew,.85)
for(v in V(g)){ie<-incident(g,v);o2<-ie[order(-E(g)$weight[ie])][1:min(2,length(ie))];keep[as.integer(o2)]<-TRUE}
g<-delete_edges(g,E(g)[!keep]);V(g)$name<-lab[V(g)$name];cl<-membership(cluster_louvain(g));set.seed(5);lay<-layout_with_fr(g,niter=2000)
V(g)$size<-5+13*(sqrt(d)-min(sqrt(d)))/(max(sqrt(d))-min(sqrt(d)))
png(file.path(out,"fig7_cocitacao.png"),width=1700,height=1300,res=200);par(mar=c(.2,.2,.2,.2))
plot(g,layout=lay,vertex.color=adjustcolor(pal[(cl-1)%%8+1],.9),vertex.frame.color="white",vertex.label.cex=.8,vertex.label.color="black",edge.width=0.5+4*E(g)$weight/max(E(g)$weight),edge.color=adjustcolor("grey45",.5));dev.off()
cc<-data.frame(ref=V(g)$name,cluster=as.integer(cl[V(g)$name]));write.csv(cc,file.path(out,"cocit_clusters.csv"),row.names=FALSE);print(cc[order(cc$cluster),])
