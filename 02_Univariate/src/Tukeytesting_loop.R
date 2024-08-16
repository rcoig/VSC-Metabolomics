XYGXXG=c()
XYVXXV=c()
diff_XYGXXG=c()
diff_XYVXXV=c()
lwr_XYGXXG=c()
lwr_XYVXXV=c()
upr_XYGXXG=c()
upr_XYVXXV=c()
d_XYGXXG<-c()
d_XYVXXV<-c()
Tukeytesting<-function(df){
  for(i in mz){
    model<-lm(df[,i]~df$Group)
    av <- aov(model)
    tukey.test <- TukeyHSD(av)
    diff.tmp<-tukey.test$`df$Group`[1:6,1]
    lwr.tmp<-tukey.test$`df$Group`[1:6,2]
    upr.tmp<-tukey.test$`df$Group`[1:6,3]
    p.tmp<-tukey.test$`df$Group`[1:6, 4]
    XYGXXG<-c(XYGXXG,p.tmp[[3]])
    XYVXXV<-c(XYVXXV,p.tmp[[4]])
    diff_XYGXXG<-c(diff_XYGXXG,diff.tmp[[3]])
    diff_XYVXXV<-c(diff_XYVXXV,diff.tmp[[4]])
    upr_XYGXXG<-c(upr_XYGXXG,upr.tmp[[3]])
    upr_XYVXXV<-c(upr_XYVXXV,upr.tmp[[4]])
    lwr_XYGXXG<-c(lwr_XYGXXG,lwr.tmp[[3]])
    lwr_XYVXXV<-c(lwr_XYVXXV,lwr.tmp[[4]])
  }
  df=data.frame("Metabolite"=mz)
  df$p_3G<-XYGXXG
  df$p_VSC<-XYVXXV
  df$diff_3G<-diff_XYGXXG
  df$diff_VSC<-diff_XYVXXV
  df$upr_3G<-upr_XYGXXG
  df$upr_VSC<-upr_XYVXXV
  df$lwr_3G<-lwr_XYGXXG
  df$lwr_VSC<-lwr_XYVXXV
  df
}