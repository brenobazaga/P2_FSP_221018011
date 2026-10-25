.libPaths(c("C:/Users/Breno/Rlib",.libPaths()));setwd(file.path(Sys.getenv("S_DIR"),"data"));options(scipen=99)
tb<-file.path(Sys.getenv("S_DIR"),"tabs");dir.create(tb,showWarnings=FALSE)
fm<-function(x,d=1)ifelse(is.na(x),"n.d.",formatC(x,format="f",digits=d,big.mark=".",decimal.mark=","))
wr<-function(name,rows)writeLines(sapply(rows,function(r)paste0("|",paste(r,collapse="|"))),file.path(tb,paste0(name,".txt")),useBytes=TRUE)
d<-read.csv("C:/Users/Breno/fsp_out/municipios_com_derivadas.csv");ph<-read.csv("filipinas_corrigido.csv")
# T: medias
mm<-function(v,e,a=2015,b=2024)mean(d[[v]][d$ente==e&d$ano>=a&d$ano<=b])
rows<-list(c("Indicador (média 2015–2024, salvo indicação)","Alagoinhas","Juazeiro"))
add<-function(lab,v,dec=1,suf="",b=2024){rows[[length(rows)+1]]<<-list(lab,paste0(fm(mm(v,"Alagoinhas",2015,b),dec),suf),paste0(fm(mm(v,"Juazeiro",2015,b),dec),suf))[[1]]}
rows<-list(c("Indicador (média 2015–2024, salvo indicação)","Alagoinhas","Juazeiro"))
ad<-function(lab,v,dec=1,suf="",b=2024)rows[[length(rows)+1]]<<-c(lab,paste0(fm(mm(v,"Alagoinhas",2015,b),dec),suf),paste0(fm(mm(v,"Juazeiro",2015,b),dec),suf))
ad("População média","populacao",0);ad("Receita líquida per capita (R$ de 2024)","receita_liquida_pc_real2024",0);ad("Despesa total liquidada per capita (R$ de 2024)","despesa_pc_real2024",0);ad("Transferências per capita (R$ de 2024)","transf_pc_real2024",0);ad("Receita tributária per capita (R$ de 2024)","tributaria_pc_real2024",0)
ad("Autonomia tributária (receita tributária / receita bruta)","autonomia_tributaria_pct",1," %");ad("Dependência de transferências (transferências / receita bruta)","grau_dependencia_pct",1," %")
ad("Esforço arrecadatório (receita tributária / PIB, 2015–2023)","esforco_arrecadatorio_pct_pib",2," %",2023);ad("Despesa com pessoal / receita líquida","razao_pessoal_rec_liquida_pct",1," %");ad("Despesa com pessoal / despesa total","razao_pessoal_desp_total_pct",1," %");ad("Taxa de investimento (investimentos / despesa total)","taxa_investimento_pct",1," %")
ad("Cota-parte do FPM / receita bruta","fpm_pct",1," %");ad("Cota-parte do ICMS / receita bruta","icms_pct",1," %");ad("Dedução do Fundeb / receita bruta","fundeb_pct",1," %");ad("Despesa em saúde / despesa total","saude_pct",1," %");ad("Despesa em educação / despesa total","educ_pct",1," %")
wr("t_medias",rows)
# series anuais
for(e in c("Alagoinhas","Juazeiro")){x<-d[d$ente==e,];r<-list(c("Ano","População","PIB (R$ bi, preços de 2024)","Receita líquida","Receita tributária","Transferências","Despesa liquidada","Pessoal","Dep. (%)","Aut. (%)"))
 for(i in seq_len(nrow(x)))r[[length(r)+1]]<-c(x$ano[i],fm(x$populacao[i],0),ifelse(is.na(x$pib_nominal_mil_reais[i]),"n.d.",fm(x$pib_nominal_mil_reais[i]*x$deflator_ipca_base2024[i]/1e6,2)),fm(x$rec_liquida_real2024_mil[i]/1000,1),fm(x$rec_tributaria_real2024_mil[i]/1000,1),fm(x$transf_total_real2024_mil[i]/1000,1),fm(x$desp_total_liq_real2024_mil[i]/1000,1),fm(x$desp_pessoal_liq_real2024_mil[i]/1000,1),fm(x$grau_dependencia_pct[i],1),fm(x$autonomia_tributaria_pct[i],1))
 wr(paste0("t_serie_",tolower(e)),r)}
# tributos
r<-list(c("Ano","Alag.: IPTU","Alag.: ISS","Alag.: ITBI","Jua.: IPTU","Jua.: ISS","Jua.: ITBI"))
a<-d[d$ente=="Alagoinhas",];j<-d[d$ente=="Juazeiro",]
for(i in 1:10)r[[length(r)+1]]<-c(a$ano[i],fm(a$iptu_real2024_mil[i]/1000,1),fm(a$iss_real2024_mil[i]/1000,1),fm(a$itbi_real2024_mil[i]/1000,1),fm(j$iptu_real2024_mil[i]/1000,1),fm(j$iss_real2024_mil[i]/1000,1),fm(j$itbi_real2024_mil[i]/1000,1))
wr("t_tributos",r)
# regressao
hc<-function(fit){X<-model.matrix(fit);u<-resid(fit);h<-hatvalues(fit);XtXi<-solve(crossprod(X));meat<-crossprod(X*(u/(1-h)));XtXi%*%meat%*%XtXi}
star<-function(p)ifelse(p<.01,"***",ifelse(p<.05,"**",ifelse(p<.1,"*","")))
cellc<-function(fit,nm){V<-hc(fit);b<-coef(fit);se<-sqrt(diag(V));p<-2*pt(-abs(b/se),fit$df.residual);if(nm%in%names(b))paste0(fm(b[nm],3),star(p[nm])," (",fm(se[nm],3),")") else "–"}
tst<-function(fit){V<-hc(fit);b<-coef(fit);c<-rep(0,length(b));c[match("ltr",names(b))]<-1;c[match("ly",names(b))]<--1;d1<-sum(c*b);se1<-sqrt(as.numeric(t(c)%*%V%*%c));paste0(fm(d1,3)," (p=",fm(pt(-d1/se1,fit$df.residual),3),")")}
P<-d[d$ano<=2023,];P$lg<-log(P$desp_corr_liq_real2024_mil*1000/P$populacao);P$ltr<-log(P$transf_pc_real2024);P$ly<-log(P$pib_pc_real2024);P$lrec<-log(P$tributaria_pc_real2024)
mods<-list(lm(lg~ltr+ly,P[P$ente=="Alagoinhas",]),lm(lg~ltr+ly+lrec,P[P$ente=="Alagoinhas",]),lm(lg~ltr+ly,P[P$ente=="Juazeiro",]),lm(lg~ltr+ly+lrec,P[P$ente=="Juazeiro",]),lm(lg~ltr+ly+factor(ente),P),lm(lg~ltr+ly+lrec+factor(ente),P))
r<-list(c("Variável","(1) Alag.","(2) Alag.","(3) Jua.","(4) Jua.","(5) Painel EF","(6) Painel EF"))
r[[2]]<-c("ln TR (transferências pc)",sapply(mods,cellc,"ltr"));r[[3]]<-c("ln Y (PIB pc)",sapply(mods,cellc,"ly"));r[[4]]<-c("ln REC (receita tributária pc)",sapply(mods,cellc,"lrec"))
r[[5]]<-c("Observações",sapply(mods,function(m)nobs(m)));r[[6]]<-c("R²",sapply(mods,function(m)fm(summary(m)$r.squared,3)));r[[7]]<-c("β₁ − β₂ (p, unilateral)",sapply(mods,tst))
wr("t_reg",r)
fd<-function(e){x<-P[P$ente==e,];x<-x[order(x$ano),];dd<-data.frame(lg=diff(x$lg),ltr=diff(x$ltr),ly=diff(x$ly),lrec=diff(x$lrec));lm(lg~ltr+ly,dd)}
m1<-fd("Alagoinhas");m2<-fd("Juazeiro");r<-list(c("Variável","Alagoinhas","Juazeiro"));r[[2]]<-c("Δ ln TR",cellc(m1,"ltr"),cellc(m2,"ltr"));r[[3]]<-c("Δ ln Y",cellc(m1,"ly"),cellc(m2,"ly"));r[[4]]<-c("Observações",nobs(m1),nobs(m2));r[[5]]<-c("R²",fm(summary(m1)$r.squared,3),fm(summary(m2)$r.squared,3));r[[6]]<-c("β₁ − β₂ (p, unilateral)",tst(m1),tst(m2));wr("t_reg_fd",r)
# filipinas
r<-list(c("Ano","PIB (PHP tri)","Receita do gov. central (PHP tri)","IRA/NTA (PHP bi)","IRA/NTA, preços 2024 (PHP bi)","IRA/NTA / receita (%)","IRA/NTA / PIB (%)"))
for(i in 1:10)r[[length(r)+1]]<-c(ph$ano[i],fm(ph$pib_nominal_milhoes_php[i]/1e6,2),fm(ph$receita_governo_central_milhoes_php[i]/1e6,2),fm(ph$ira_nta_milhoes_php[i]/1e3,2),fm(ph$ira_nta_real2024_milhoes_php[i]/1e3,1),fm(ph$ira_nta_milhoes_php[i]/ph$receita_governo_central_milhoes_php[i]*100,1),fm(ph$ira_nta_milhoes_php[i]/ph$pib_nominal_milhoes_php[i]*100,2))
wr("t_ph_serie",r)
r<-list(c("Item (PHP bilhões)","2020","2021"),c("Renda operacional corrente total das LGUs (excl. barangays)","833,92","871,30"),c("IRA (renomeado NTA em 2022)","516,07","554,02"),c("Fontes locais (LSR)","252,57","256,21"),c("Fontes externas","581,35","615,08"),c("IRA / renda corrente total (%)","61,9","63,6"),c("LSR / renda corrente total (%)","30,3","29,4"));wr("t_ph_lgu",r)
r<-list(c("Nível de governo local (2021)","Renda corrente total (PHP bi)","IRA (PHP bi)","IRA / renda corrente (%)"),c("Cidades","351,94","159,51","45"),c("Municípios","314,35","235,10","75"),c("Províncias","205,01","159,01","78"));wr("t_ph_nivel",r)
cat("ok\n")
