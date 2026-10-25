.libPaths(c("C:/Users/Breno/Rlib",.libPaths()))
suppressMessages(library(ggplot2));setwd(file.path(Sys.getenv("S_DIR"),"data"));options(width=230,scipen=99)
d<-read.csv("municipios_corrigido.csv");ph<-read.csv("filipinas_corrigido.csv");out<-"C:/Users/Breno/fsp_out";dir.create(out,showWarnings=FALSE)
sh<-function(a,b)round(a/b*100,1)
d$fpm_pct<-sh(d$fpm_nominal_mil,d$rec_ex_intra_bruta_nominal_mil);d$icms_pct<-sh(d$icms_nominal_mil,d$rec_ex_intra_bruta_nominal_mil);d$ipva_pct<-sh(d$ipva_nominal_mil,d$rec_ex_intra_bruta_nominal_mil)
d$iptu_pct_trib<-sh(d$iptu_nominal_mil,d$rec_tributaria_nominal_mil);d$iss_pct_trib<-sh(d$iss_nominal_mil,d$rec_tributaria_nominal_mil)
d$saude_pct<-sh(d$desp_saude_liq_nominal_mil,d$desp_total_liq_nominal_mil);d$educ_pct<-sh(d$desp_educ_liq_nominal_mil,d$desp_total_liq_nominal_mil)
d$fundeb_pct<-sh(d$ded_fundeb_nominal_mil,d$rec_ex_intra_bruta_nominal_mil)
d$saude_pc<-round(d$desp_saude_liq_real2024_mil*1000/d$populacao);d$educ_pc<-round(d$desp_educ_liq_real2024_mil*1000/d$populacao)
print(d[,c("ente","ano","fpm_pct","icms_pct","ipva_pct","iptu_pct_trib","iss_pct_trib","saude_pct","educ_pct","fundeb_pct","taxa_investimento_pct","saude_pc","educ_pc")])
print(aggregate(cbind(fpm_pct,icms_pct,ipva_pct,iptu_pct_trib,iss_pct_trib,saude_pct,educ_pct,fundeb_pct)~ente,d,mean))
for(e in unique(d$ente)){x<-d[d$ente==e,];g<-function(v,a=2015,b=2024)round((x[[v]][x$ano==b]/x[[v]][x$ano==a]-1)*100,1);cat(e,"IPTU real var%",g("iptu_real2024_mil"),"ISS",g("iss_real2024_mil"),"FPM",g("fpm_real2024_mil"),"ICMS",g("icms_real2024_mil"),"invest",g("desp_invest_liq_real2024_mil"),"saude",g("desp_saude_liq_real2024_mil"),"educ",g("desp_educ_liq_real2024_mil"),"PIB real 15-23",round((x$pib_nominal_mil_reais[x$ano==2023]*x$deflator_ipca_base2024[x$ano==2023]/(x$pib_nominal_mil_reais[x$ano==2015]*x$deflator_ipca_base2024[x$ano==2015])-1)*100,1),"\n")}
# figuras
th<-theme_minimal(base_size=11)+theme(panel.grid.minor=element_blank(),legend.position="bottom");cols<-c(Alagoinhas="#1F4E79",Juazeiro="#C55A11")
g1<-ggplot(d,aes(ano,grau_dependencia_pct,color=ente))+geom_line(linewidth=1)+geom_point()+scale_color_manual(values=cols)+scale_x_continuous(breaks=2015:2024)+labs(x=NULL,y="Transferências / receita bruta (%)",color=NULL)+th
g2<-ggplot(d,aes(ano,autonomia_tributaria_pct,color=ente))+geom_line(linewidth=1)+geom_point()+scale_color_manual(values=cols)+scale_x_continuous(breaks=2015:2024)+labs(x=NULL,y="Receita tributária / receita bruta (%)",color=NULL)+th
ggsave(file.path(out,"fig8_dependencia.png"),g1,width=7,height=3.4,dpi=200);ggsave(file.path(out,"fig9_autonomia.png"),g2,width=7,height=3.4,dpi=200)
l<-rbind(data.frame(ente=d$ente,ano=d$ano,comp="Receita tributária própria",v=d$tributaria_pc_real2024),data.frame(ente=d$ente,ano=d$ano,comp="Transferências intergovernamentais",v=d$transf_pc_real2024),data.frame(ente=d$ente,ano=d$ano,comp="Outras receitas",v=pmax(0,d$rec_ex_intra_bruta_real2024_mil*1000/d$populacao-d$tributaria_pc_real2024-d$transf_pc_real2024)))
l$comp<-factor(l$comp,levels=c("Outras receitas","Receita tributária própria","Transferências intergovernamentais"))
g3<-ggplot(l,aes(ano,v,fill=comp))+geom_col(width=.75)+facet_wrap(~ente)+scale_fill_manual(values=c("#A6A6A6","#C55A11","#1F4E79"))+scale_x_continuous(breaks=c(2015,2017,2019,2021,2023))+scale_y_continuous(labels=function(x)format(x,big.mark=".",decimal.mark=","))+labs(x=NULL,y="R$ por habitante (preços de 2024)",fill=NULL)+th
ggsave(file.path(out,"fig10_composicao_receita.png"),g3,width=7,height=3.8,dpi=200)
e<-rbind(data.frame(ente=d$ente,ano=d$ano,comp="Pessoal e encargos",v=d$desp_pessoal_liq_real2024_mil),data.frame(ente=d$ente,ano=d$ano,comp="Investimentos",v=d$desp_invest_liq_real2024_mil),data.frame(ente=d$ente,ano=d$ano,comp="Demais despesas",v=d$desp_total_liq_real2024_mil-d$desp_pessoal_liq_real2024_mil-d$desp_invest_liq_real2024_mil))
e$comp<-factor(e$comp,levels=c("Demais despesas","Investimentos","Pessoal e encargos"));e$v<-e$v/1000
g4<-ggplot(e,aes(ano,v,fill=comp))+geom_col(width=.75)+facet_wrap(~ente)+scale_fill_manual(values=c("#A6A6A6","#548235","#1F4E79"))+scale_x_continuous(breaks=c(2015,2017,2019,2021,2023))+labs(x=NULL,y="R$ milhões (preços de 2024)",fill=NULL)+th
ggsave(file.path(out,"fig11_composicao_despesa.png"),g4,width=7,height=3.8,dpi=200)
# indices reais base 2015=100: tributaria, transferencias, despesa pessoal
ix<-do.call(rbind,lapply(split(d,d$ente),function(x){x<-x[order(x$ano),];rbind(data.frame(ente=x$ente[1],ano=x$ano,s="Receita tributária",v=x$rec_tributaria_real2024_mil/x$rec_tributaria_real2024_mil[1]*100),data.frame(ente=x$ente[1],ano=x$ano,s="Transferências",v=x$transf_total_real2024_mil/x$transf_total_real2024_mil[1]*100),data.frame(ente=x$ente[1],ano=x$ano,s="Despesa com pessoal",v=x$desp_pessoal_liq_real2024_mil/x$desp_pessoal_liq_real2024_mil[1]*100))}))
g5<-ggplot(ix,aes(ano,v,color=s))+geom_line(linewidth=1)+facet_wrap(~ente)+scale_color_manual(values=c("#C55A11","#1F4E79","#548235"))+scale_x_continuous(breaks=c(2015,2017,2019,2021,2023))+geom_hline(yintercept=100,linetype=3,color="grey50")+labs(x=NULL,y="Índice real (2015 = 100)",color=NULL)+th
ggsave(file.path(out,"fig12_indices_reais.png"),g5,width=7,height=3.6,dpi=200)
g6<-ggplot(d,aes(transf_pc_real2024,tributaria_pc_real2024,color=ente,label=ano))+geom_point(size=2)+geom_text(vjust=-.8,size=2.6,show.legend=FALSE)+scale_color_manual(values=cols)+labs(x="Transferências per capita (R$ de 2024)",y="Receita tributária per capita (R$ de 2024)",color=NULL)+th
ggsave(file.path(out,"fig13_dispersao.png"),g6,width=6.5,height=4,dpi=200)
# Filipinas
ph$nta_bi<-ph$ira_nta_milhoes_php/1000
g7<-ggplot(ph,aes(ano))+geom_col(aes(y=ira_nta_real2024_milhoes_php/1e6),fill="#1F4E79",width=.7)+geom_line(aes(y=ira_nta_pct_receita_gc/ 22*1.0),color="#C55A11",linewidth=1)+geom_point(aes(y=ira_nta_pct_receita_gc/22),color="#C55A11")+scale_y_continuous(name="IRA/NTA (PHP trilhões, preços de 2024)",sec.axis=sec_axis(~.*22,name="IRA/NTA como % da receita do governo central"))+scale_x_continuous(breaks=2015:2024)+labs(x=NULL)+geom_vline(xintercept=2021.5,linetype=2,color="grey50")+th
ggsave(file.path(out,"fig14_ira_nta.png"),g7,width=7,height=3.8,dpi=200)
write.csv(d,file.path(out,"municipios_com_derivadas.csv"),row.names=FALSE)
print(ph[,c("ano","pib_nominal_milhoes_php","receita_governo_central_milhoes_php","ira_nta_milhoes_php","ira_nta_real2024_milhoes_php","ira_nta_pct_receita_gc","ira_nta_pct_pib","receita_gc_pct_pib","tributaria_gc_pct_pib","despesa_gc_pct_pib","resultado_gc_milhoes_php","ira_nta_pc_real2024_php","pib_pc_real2024_php")])
x<-ph[ph$ano==2015|ph$ano==2024,];cat("var real IRA/NTA 2015-2024:",round((x$ira_nta_real2024_milhoes_php[2]/x$ira_nta_real2024_milhoes_php[1]-1)*100,1),"% ; PIB real:",round((x$pib_nominal_real2024_milhoes_php[2]/x$pib_nominal_real2024_milhoes_php[1]-1)*100,1),"\n")
cat("var real 2021->2022:",round((ph$ira_nta_real2024_milhoes_php[8]/ph$ira_nta_real2024_milhoes_php[7]-1)*100,1),"nominal",round((ph$ira_nta_milhoes_php[8]/ph$ira_nta_milhoes_php[7]-1)*100,1),"; 2022->2023 nominal",round((ph$ira_nta_milhoes_php[9]/ph$ira_nta_milhoes_php[8]-1)*100,1),"; 2023->2024",round((ph$ira_nta_milhoes_php[10]/ph$ira_nta_milhoes_php[9]-1)*100,1),"\n")
