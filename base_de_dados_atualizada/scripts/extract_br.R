suppressWarnings({library(jsonlite)}); setwd(file.path(Sys.getenv("S_DIR"),"data"))
rd<-function(f)fromJSON(f)$items
ints<-function(cod)lapply(strsplit(sub("^[A-Z]+","",cod),".",fixed=TRUE),function(v)suppressWarnings(as.integer(v)))
norm<-function(cod)sapply(ints(cod),function(v){v<-v[!is.na(v)];if(!length(v))return(NA_character_);while(length(v)>1&&v[length(v)]==0)v<-v[-length(v)];paste(v,collapse=".")})
B<-"Receitas Brutas Realizadas";L<-"Despesas Liquidadas"
val<-function(d,nk,col){i<-which(d$nk==nk&d$coluna==col);if(length(i))sum(d$valor[i]) else NA_real_}
fpm_sum<-function(d){i<-which(d$coluna==B&grepl("^RO",d$cod_conta)&grepl("Cota-Parte do Fundo de Participa",d$conta))
 if(!length(i))return(NA_real_);nk<-d$nk[i];keep<-sapply(seq_along(i),function(a)!any(sapply(seq_along(i),function(b)b!=a&&nk[b]!=nk[a]&&startsWith(nk[a],paste0(nk[b],".")))));sum(d$valor[i][keep])}
txtmax<-function(d,pat){i<-which(d$coluna==B&grepl("^RO",d$cod_conta)&grepl(pat,d$conta));if(length(i))max(d$valor[i]) else NA_real_}
firstok<-function(a,b)if(!is.na(a))a else b
out<-list();chk<-list()
for(e in c("Alagoinhas","Juazeiro")) for(y in 2015:2024){
 c1<-rd(sprintf("dca/%s_%d_I-C.json",e,y)); c1$nk<-norm(c1$cod_conta)
 r<-c1[c1$cod_conta=="TotalReceitas",]
 rb<-sum(c1$valor[c1$cod_conta=="TotalReceitas"&c1$coluna==B])  # total geral (inclui intra)
 re<-c1[c1$cod_conta=="ReceitasExcetoIntraOrcamentarias",]
 if(nrow(re)){rec_ex<-re$valor[re$coluna==B][1];df<-sum(re$valor[grepl("FUNDEB",re$coluna)]);do<-sum(re$valor[grepl("Outras Dedu",re$coluna)])}else{rec_ex<-NA;df<-NA;do<-NA}
 rc<-sum(c1$valor[which(c1$nk=="1"&c1$coluna==B)]); rcap<-sum(c1$valor[which(c1$nk=="2"&c1$coluna==B)])
 if(is.na(rec_ex)) rec_ex<-rc+rcap   # fallback: correntes + capital (exceto intra) 
 if(is.na(df)){ # anos sem linha agregada: soma de colunas de dedução na linha 'Total'
   t<-c1[c1$cod_conta=="TotalReceitas",];df<-sum(t$valor[grepl("FUNDEB",t$coluna)]);do<-sum(t$valor[grepl("Outras Dedu",t$coluna)])}
 dd<-rd(sprintf("dca/%s_%d_I-D.json",e,y)); dd$nk<-norm(dd$cod_conta)
 ee<-rd(sprintf("dca/%s_%d_I-E.json",e,y))
 one<-function(x)if(length(x))x[1] else NA_real_
 tot<-one(ee$valor[which(ee$coluna==L&grepl("^Despesas.*Exceto Intra",ee$conta))])
 out[[length(out)+1]]<-data.frame(ente=e,ano=y,rec_ex_intra_bruta=rec_ex,ded_fundeb=df,ded_outras=do,
  rec_correntes=rc,rec_capital=rcap,rec_tributaria=val(c1,"1.1",B),transf_correntes=val(c1,"1.7",B),transf_capital=val(c1,"2.4",B),
  fpm=fpm_sum(c1),icms=txtmax(c1,"Cota-Parte do ICMS"),ipva=txtmax(c1,"Cota-Parte do IPVA"),iptu=txtmax(c1,"Propriedade Predial e Territorial Urbana"),itbi=txtmax(c1,"Transmiss.*Inter Vivos"),iss=firstok(txtmax(c1,"Servi.os de Qualquer Natureza"),txtmax(c1,"Impostos sobre a Produ..o, Circula..o de Mercadorias e Servi.os")),
  desp_total_liq=tot,desp_corr_liq=val(dd,"3",L),desp_pessoal_liq=val(dd,"3.1",L),desp_capital_liq=val(dd,"4",L),desp_invest_liq=val(dd,"4.4",L),
  desp_saude_liq=one(ee$valor[which(ee$coluna==L&ee$conta=="10 - Saúde")]),desp_educ_liq=one(ee$valor[which(ee$coluna==L&ee$conta=="12 - Educação")]))
}
o<-do.call(rbind,out); write.csv(o,"br_siconfi_raw.csv",row.names=FALSE)
options(width=250,scipen=99)
print(cbind(o[,1:2],round(o[,c("rec_tributaria","iptu","iss","itbi")]/1e6,2)))
