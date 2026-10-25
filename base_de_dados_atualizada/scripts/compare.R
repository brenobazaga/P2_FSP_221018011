.libPaths(c("C:/Users/Breno/Rlib",.libPaths()));setwd(file.path(Sys.getenv("S_DIR"),"data"));options(width=240,scipen=99)
o<-read.csv("C:/Users/Breno/OneDrive - Grupo NTSec/Estudos/UnB/2026-2/Finanças do Setor Público/P2_FSP_221018011/base_de_dados_atualizada/CSV/Base_de_Dados_Consolidada_2015_2024.csv",fileEncoding="UTF-8-BOM")
names(o)[1]<-"ente";n<-read.csv("C:/Users/Breno/fsp_out/municipios_com_derivadas.csv");ph<-read.csv("filipinas_corrigido.csv")
m<-merge(o,n,by=c("ente","ano"),suffixes=c("",".n"))
pairs<-list(c("populacao","populacao.n","População"),c("pib_nominal_mil_reais","pib_nominal_mil_reais.n","PIB nominal"),c("deflator_ipca_base2024","deflator_ipca_base2024.n","Deflator IPCA (2024=1)"),c("receita_total_corrente_mil","rec_ex_intra_bruta_nominal_mil","Receita total (bruta, exceto intra)"),c("receita_propria_corrente_mil","rec_tributaria_nominal_mil","Receita tributária própria"),c("transf_intergov_corrente_mil","transf_total_nominal_mil","Transferências intergovernamentais"),c("despesa_total_corrente_mil","desp_total_liq_nominal_mil","Despesa total liquidada"),c("despesa_pessoal_corrente_mil","desp_pessoal_liq_nominal_mil","Despesa com pessoal"),c("despesa_investimento_corrente_mil","desp_invest_liq_nominal_mil","Despesa com investimentos"),c("grau_dependencia_pct","grau_dependencia_pct.n","Grau de dependência (%)"),c("esforco_arrecadatorio_pct","esforco_arrecadatorio_pct_pib","Esforço arrecadatório (% PIB)"))
rows<-list();full<-list()
for(e in c("Alagoinhas","Juazeiro"))for(p in pairs){x<-m[m$ente==e,];a<-x[[p[1]]];b<-x[[p[2]]];ok<-!is.na(b);err<-abs(a[ok]/b[ok]-1)*100
 rows[[length(rows)+1]]<-c(e,p[3],sum(ok),sum(err>1),round(mean(err),1),round(max(err),1));full[[length(full)+1]]<-data.frame(ente=e,variavel=p[3],ano=x$ano[ok],original=a[ok],verificado=b[ok],dif_pct=round((a[ok]/b[ok]-1)*100,2))}
r<-do.call(rbind,rows);colnames(r)<-c("ente","variavel","n","n_dif_gt1pct","erro_medio_pct","erro_max_pct");print(r)
f<-do.call(rbind,full);write.csv(f,"C:/Users/Breno/fsp_out/comparacao_original_vs_verificado_municipios.csv",row.names=FALSE,fileEncoding="UTF-8")
# tabela do artigo
tb<-file.path(Sys.getenv("S_DIR"),"tabs");fmv<-function(x)formatC(as.numeric(x),format="f",digits=1,decimal.mark=",")
rr<-list(c("Variável","Obs. com divergência > 1% (Alagoinhas)","Erro médio % (Alag.)","Obs. com divergência > 1% (Juazeiro)","Erro médio % (Jua.)"))
for(p in pairs){a<-r[r[,1]=="Alagoinhas"&r[,2]==p[3],];j<-r[r[,1]=="Juazeiro"&r[,2]==p[3],];rr[[length(rr)+1]]<-c(p[3],paste0(a[4]," de ",a[3]),fmv(a[5]),paste0(j[4]," de ",j[3]),fmv(j[5]))}
writeLines(sapply(rr,function(x)paste0("|",paste(x,collapse="|"))),file.path(tb,"t_verif.txt"),useBytes=TRUE)
# Filipinas
q<-merge(o[o$ente=="Filipinas",],ph,by="ano",suffixes=c("",".n"))
pf<-list(c("populacao","populacao_bm","População"),c("pib_nominal_mil_reais","pib_nominal_milhoes_php","PIB nominal (milhões PHP)"),c("receita_total_corrente_mil","receita_governo_central_milhoes_php","Receita total"),c("transf_intergov_corrente_mil","ira_nta_milhoes_php","IRA/NTA"),c("despesa_total_corrente_mil","despesa_governo_central_milhoes_php","Despesa total"))
for(p in pf){ok<-!is.na(q[[p[2]]]);err<-abs(q[[p[1]]][ok]/q[[p[2]]][ok]-1)*100;cat(p[3],": n div>1% =",sum(err>1)," de ",sum(ok)," erro medio",round(mean(err),1)," max",round(max(err),1),"\n")}
print(q[,c("ano","transf_intergov_corrente_mil","ira_nta_milhoes_php","populacao","populacao_bm","pib_nominal_mil_reais","pib_nominal_milhoes_php")])
write.csv(q[,c("ano","populacao","populacao_bm","pib_nominal_mil_reais","pib_nominal_milhoes_php","receita_total_corrente_mil","receita_governo_central_milhoes_php","transf_intergov_corrente_mil","ira_nta_milhoes_php","despesa_total_corrente_mil","despesa_governo_central_milhoes_php")],"C:/Users/Breno/fsp_out/comparacao_original_vs_verificado_filipinas.csv",row.names=FALSE)
