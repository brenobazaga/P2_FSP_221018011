.libPaths(c("C:/Users/Breno/Rlib",.libPaths()))
setwd(file.path(Sys.getenv("S_DIR"),"data"))
cat(c("sandwich","lmtest","plm")%in%rownames(installed.packages()),"\n")
d<-read.csv("municipios_corrigido.csv");options(width=220,scipen=99)
d$g<-d$desp_corr_liq_real2024_mil*1000/d$populacao;d$tr<-d$transf_total_real2024_mil*1000/d$populacao;d$y<-d$pib_pc_real2024;d$rec<-d$rec_tributaria_real2024_mil*1000/d$populacao
# medias 2015-2024 e 2015-2023
m<-aggregate(cbind(populacao,grau_dependencia_pct,autonomia_tributaria_pct,razao_pessoal_rec_liquida_pct,razao_pessoal_desp_total_pct,taxa_investimento_pct,receita_liquida_pc_real2024,despesa_pc_real2024,transf_pc_real2024,tributaria_pc_real2024)~ente,d,mean);print(m)
m2<-aggregate(esforco_arrecadatorio_pct_pib~ente,d[d$ano<=2023,],mean);print(m2)
# crescimento real 2015->2024
for(e in unique(d$ente)){x<-d[d$ente==e,];f<-function(v)round((x[[v]][x$ano==2024]/x[[v]][x$ano==2015]-1)*100,1);cat(e,"var% real 2015-2024: rec liq",f("rec_liquida_real2024_mil"),"trib",f("rec_tributaria_real2024_mil"),"transf",f("transf_total_real2024_mil"),"desp",f("desp_total_liq_real2024_mil"),"pessoal",f("desp_pessoal_liq_real2024_mil"),"pc rec",f("receita_liquida_pc_real2024"),"\n")}
# regressao
hc<-function(fit){X<-model.matrix(fit);u<-resid(fit);n<-nrow(X);k<-ncol(X);h<-hatvalues(fit);XtXi<-solve(crossprod(X));meat<-crossprod(X*(u/(1-h)));XtXi%*%meat%*%XtXi}
rpt<-function(fit,nm){V<-hc(fit);b<-coef(fit);se<-sqrt(diag(V));df<-fit$df.residual;t<-b/se;p<-2*pt(-abs(t),df);r<-data.frame(coef=b,ep=se,t=t,p=p);cat("\n##",nm,"n=",nobs(fit),"R2=",round(summary(fit)$r.squared,3),"\n");print(round(r,4))
 if(all(c("ltr","ly")%in%names(b))){c<-c(0,1,-1,rep(0,length(b)-3));names(c)<-names(b);d1<-sum(c*b);se1<-sqrt(as.numeric(t(c)%*%V%*%c));cat("b1-b2=",round(d1,4)," ep=",round(se1,4)," p(unilateral b1>b2)=",round(pt(-d1/se1,df),4),"\n")}}
res<-list()
for(e in c("Alagoinhas","Juazeiro")){x<-d[d$ente==e&d$ano<=2023,];x$lg<-log(x$g);x$ltr<-log(x$tr);x$ly<-log(x$y);x$lrec<-log(x$rec)
 rpt(lm(lg~ltr+ly,x),paste(e,"niveis: lnG ~ lnTR + lnY"));rpt(lm(lg~ltr+ly+lrec,x),paste(e,"niveis: + lnREC"))
 x<-x[order(x$ano),];dx<-data.frame(dlg=diff(x$lg),dltr=diff(x$ltr),dly=diff(x$ly),dlrec=diff(x$lrec));names(dx)<-c("lg","ltr","ly","lrec");rpt(lm(lg~ltr+ly,dx),paste(e,"primeiras diferencas"))}
p<-d[d$ano<=2023,];p$lg<-log(p$g);p$ltr<-log(p$tr);p$ly<-log(p$y);p$lrec<-log(p$rec)
rpt(lm(lg~ltr+ly+lrec+factor(ente),p),"Painel 2 municipios, efeitos fixos: lnG ~ lnTR+lnY+lnREC")
rpt(lm(lg~ltr+ly+factor(ente),p),"Painel 2 municipios, efeitos fixos: lnG ~ lnTR+lnY")
# correlacoes
for(e in c("Alagoinhas","Juazeiro")){x<-d[d$ente==e,];cat(e,"cor(transf_pc,trib_pc)=",round(cor(x$tr,x$rec),3),"  cor(dlog transf, dlog trib)=",round(cor(diff(log(x$tr)),diff(log(x$rec))),3),"\n")}
write.csv(m,"tabela1_medias_municipios.csv",row.names=FALSE)
