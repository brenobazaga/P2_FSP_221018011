.libPaths(c("C:/Users/Breno/Rlib",.libPaths()))
suppressMessages({library(openalexR);library(bibliometrix)})
setwd(file.path(Sys.getenv("S_DIR"),"data"))
q<-'"fiscal federalism" OR "flypaper effect" OR "fiscal illusion" OR "intergovernmental transfers"'
works<-oa_fetch(entity="works",title_and_abstract.search=q,type="article",from_publication_date="1970-01-01",to_publication_date="2024-12-31",options=list(sort="cited_by_count:desc",per_page=200),pages=1:19,output="list",verbose=FALSE)
cat("lista:",length(works),"\n"); save(works,file="C:/Users/Breno/oa_works_list.RData")
M<-convert2df("C:/Users/Breno/oa_works_list.RData",dbsource="openalex_api",format="api")
cat("n=",nrow(M),"\n");cat(paste(names(M),collapse=","),"\n"); saveRDS(M,"M_bib.rds")
print(head(M[,c("AU","TI","SO","PY","TC","DT")],3)); print(head(M$DE,3)); print(head(M$ID,2)); print(head(M$CR,1)); print(head(M$AU_CO,3))
