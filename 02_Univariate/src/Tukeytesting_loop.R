XYwXXw=c()
XYdXXd=c()
ES_XYwXXw=c()
ES_XYdXXd=c()
Tukeytesting<-function(df){
  for(i in mz){
    model<-lm(df[,i]~df$Group)
    av <- aov(model)
    tukey.test <- TukeyHSD(av)
    ES.tmp<-round(tukey.test$`df$Group`[1:6,1],6)
    p.tmp<-tukey.test$`df$Group`[1:6, 4]
    XYwXXw<-c(XYwXXw,p.tmp[[3]])
    XYdXXd<-c(XYdXXd,p.tmp[[4]])
    ES_XYwXXw<-c(ES_XYwXXw,ES.tmp[[3]])
    ES_XYdXXd<-c(ES_XYdXXd,ES.tmp[[4]])
  }
  df=data.frame("Metabolite"=mz)
  df$p_w<-XYwXXw
  df$p_d<-XYdXXd
  df$ES_w<-ES_XYwXXw
  df$ES_d<-ES_XYdXXd
  df$absES_w <-abs(df$ES_w)
  df$absES_d <-abs(df$ES_d)
  df 
}