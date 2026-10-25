suppressWarnings(library(jsonlite)); setwd(file.path(Sys.getenv("S_DIR"),"data"))
wb<-function(i){x<-fromJSON(sprintf("wb_%s.json",i))[[2]];setNames(as.numeric(x$value),x$date)}
y<-as.character(2015:2024)
gdp<-wb("NY.GDP.MKTP.CN")[y]/1e6;pop<-wb("SP.POP.TOTL")[y];rev<-wb("GC.REV.XGRT.CN")[y]/1e6;tax<-wb("GC.TAX.TOTL.CN")[y]/1e6;xpn<-wb("GC.XPN.TOTL.CN")[y]/1e6
inf<-wb("FP.CPI.TOTL.ZG")[as.character(2014:2024)]
idx<-cumprod(1+inf/100);names(idx)<-2014:2024;defl<-idx["2024"]/idx[y]
nta<-c(389.86,428.62,486.89,522.75,575.52,648.92,695.49,959.04,820.27,871.38)*1000  # milhoes PHP
src<-c("DBM/LMP (fonte secundaria)","Derivado de DBM LBM 2017 (486,885-58,266)","DBM LBM 2017","DBM LBM 75 (2018)","SEPO/CPBRD citando DBM","SEPO/PAGBA citando DBM","DBM (SEPO/PAGBA)","DBM (SEPO/PAGBA): NTA","DBM FY2023 briefing: NTA","DBM LBM 87 (2023): NTA")
o<-data.frame(pais="Filipinas",ano=2015:2024,populacao_bm=pop,pib_nominal_milhoes_php=round(gdp),deflator_cpi_base2024=round(defl,4),
 receita_governo_central_milhoes_php=round(rev),receita_tributaria_gc_milhoes_php=round(tax),despesa_governo_central_milhoes_php=round(xpn),
 ira_nta_milhoes_php=nta,ira_nta_fonte=src)
o$ira_nta_pct_receita_gc<-round(o$ira_nta_milhoes_php/o$receita_governo_central_milhoes_php*100,2)
o$ira_nta_pct_pib<-round(o$ira_nta_milhoes_php/o$pib_nominal_milhoes_php*100,3)
o$receita_gc_pct_pib<-round(o$receita_governo_central_milhoes_php/o$pib_nominal_milhoes_php*100,2)
o$tributaria_gc_pct_pib<-round(o$receita_tributaria_gc_milhoes_php/o$pib_nominal_milhoes_php*100,2)
o$despesa_gc_pct_pib<-round(o$despesa_governo_central_milhoes_php/o$pib_nominal_milhoes_php*100,2)
o$resultado_gc_milhoes_php<-o$receita_governo_central_milhoes_php-o$despesa_governo_central_milhoes_php
for(m in c("pib_nominal_milhoes_php","receita_governo_central_milhoes_php","ira_nta_milhoes_php"))o[[sub("milhoes_php","real2024_milhoes_php",m)]]<-round(o[[m]]*o$deflator_cpi_base2024)
o$ira_nta_pc_real2024_php<-round(o$ira_nta_real2024_milhoes_php*1e6/o$populacao_bm,0)
o$pib_pc_real2024_php<-round(o$pib_nominal_real2024_milhoes_php*1e6/o$populacao_bm,0)
write.csv(o,"filipinas_corrigido.csv",row.names=FALSE,fileEncoding="UTF-8")
options(width=250,scipen=99);print(o[,c("ano","populacao_bm","pib_nominal_milhoes_php","deflator_cpi_base2024","receita_governo_central_milhoes_php","ira_nta_milhoes_php","ira_nta_pct_receita_gc","ira_nta_pct_pib","receita_gc_pct_pib","despesa_gc_pct_pib")])
