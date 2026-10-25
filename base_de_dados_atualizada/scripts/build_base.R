suppressWarnings(library(jsonlite)); setwd(file.path(Sys.getenv("S_DIR"),"data"))
rd<-function(f){x<-fromJSON(f);x<-x[-1,];data.frame(mun=sub(" .BA.","",x$D1N),ano=as.integer(x$D3N),v=as.numeric(x$V))}
pop<-rbind(rd("pop6579.json"),rd("pop4714.json")); pib<-rd("pib.json")
# IPCA (media anual do numero-indice, base 2024=1)
i<-fromJSON("ipca_idx.json");i<-i[-1,];idx<-data.frame(m=i$D3C,v=as.numeric(i$V),y=as.integer(substr(i$D3C,1,4)))
am<-tapply(idx$v,idx$y,mean); defl_br<-setNames(am["2024"]/am[as.character(2015:2024)],2015:2024)
br<-read.csv("br_siconfi_raw.csv",stringsAsFactors=FALSE)
br$populacao<-NA_real_;br$pop_fonte<-NA_character_;br$pib_mil<-NA_real_
for(r in seq_len(nrow(br))){p<-pop$v[pop$mun==br$ente[r]&pop$ano==br$ano[r]]; if(length(p)){br$populacao[r]<-p[1];br$pop_fonte[r]<-if(br$ano[r]==2022)"IBGE Censo 2022 (Tab. 4714)" else "IBGE estimativa (Tab. 6579)"}
 q<-pib$v[pib$mun==br$ente[r]&pib$ano==br$ano[r]];if(length(q))br$pib_mil[r]<-q[1]}
for(e in unique(br$ente)){ i22<-which(br$ente==e&br$ano==2022);i24<-which(br$ente==e&br$ano==2024);i23<-which(br$ente==e&br$ano==2023)
 br$populacao[i23]<-round(sqrt(br$populacao[i22]*br$populacao[i24]));br$pop_fonte[i23]<-"Interpolacao geometrica Censo 2022 - estimativa 2024 (IBGE nao divulgou estimativa para 2023)"}
br$deflator_ipca_base2024<-defl_br[as.character(br$ano)]
br$rec_liquida<-br$rec_ex_intra_bruta-br$ded_fundeb-br$ded_outras
br$transf_total<-br$transf_correntes+ifelse(is.na(br$transf_capital),0,br$transf_capital)
mon<-c("rec_ex_intra_bruta","ded_fundeb","ded_outras","rec_liquida","rec_correntes","rec_capital","rec_tributaria","transf_correntes","transf_capital","transf_total","fpm","icms","ipva","iptu","itbi","iss","desp_total_liq","desp_corr_liq","desp_pessoal_liq","desp_capital_liq","desp_invest_liq","desp_saude_liq","desp_educ_liq")
o<-data.frame(ente=br$ente,ano=br$ano,populacao=br$populacao,pop_fonte=br$pop_fonte,pib_nominal_mil_reais=br$pib_mil,deflator_ipca_base2024=round(br$deflator_ipca_base2024,4))
for(m in mon){o[[paste0(m,"_nominal_mil")]]<-round(br[[m]]/1000,1)}
for(m in mon){o[[paste0(m,"_real2024_mil")]]<-round(br[[m]]/1000*br$deflator_ipca_base2024,1)}
o$grau_dependencia_pct<-round(br$transf_total/br$rec_ex_intra_bruta*100,2)
o$autonomia_tributaria_pct<-round(br$rec_tributaria/br$rec_ex_intra_bruta*100,2)
o$esforco_arrecadatorio_pct_pib<-round(br$rec_tributaria/1000/br$pib_mil*100,3)
o$razao_pessoal_desp_total_pct<-round(br$desp_pessoal_liq/br$desp_total_liq*100,2)
o$razao_pessoal_rec_liquida_pct<-round(br$desp_pessoal_liq/br$rec_liquida*100,2)
o$taxa_investimento_pct<-round(br$desp_invest_liq/br$desp_total_liq*100,2)
o$resultado_orcamentario_real2024_mil<-round((br$rec_liquida-br$desp_total_liq)/1000*br$deflator_ipca_base2024,1)
o$receita_liquida_pc_real2024<-round(br$rec_liquida*br$deflator_ipca_base2024/br$populacao,2)
o$despesa_pc_real2024<-round(br$desp_total_liq*br$deflator_ipca_base2024/br$populacao,2)
o$transf_pc_real2024<-round(br$transf_total*br$deflator_ipca_base2024/br$populacao,2)
o$tributaria_pc_real2024<-round(br$rec_tributaria*br$deflator_ipca_base2024/br$populacao,2)
o$pib_pc_real2024<-round(br$pib_mil*1000*br$deflator_ipca_base2024/br$populacao,2)
write.csv(o,"municipios_corrigido.csv",row.names=FALSE,fileEncoding="UTF-8")
options(width=250,scipen=99)
print(o[,c("ente","ano","populacao","pib_nominal_mil_reais","deflator_ipca_base2024","rec_ex_intra_bruta_nominal_mil","rec_tributaria_nominal_mil","transf_total_nominal_mil","desp_total_liq_nominal_mil","grau_dependencia_pct","autonomia_tributaria_pct","esforco_arrecadatorio_pct_pib","razao_pessoal_rec_liquida_pct","resultado_orcamentario_real2024_mil")])
