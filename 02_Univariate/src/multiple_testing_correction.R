multiple_testing<-function(df){
  df$FDR_w<-p.adjust(df$p_w,method="fdr")
  df$FDR_d<-p.adjust(df$p_d,method="fdr")
  df$sig_w[df$FDR_w<.05]<-"*"
  df$sig_w[df$FDR_w>=.05]<-"ns"
  df$sig_d[df$FDR_d<.05]<-"*"
  df$sig_d[df$FDR_d>=.05]<-"ns"
  df$sig_w2<-"ns"
  df$sig_w2[df$FDR_w < 0.05] <- "*"
  df$sig_w2[df$FDR_w < 0.01] <- "**"
  df$sig_w2[df$FDR_w < 0.001] <- "***"
  df$sig_w2[df$FDR_w < 0.0001] <- "****"
  df$sig_w2[df$FDR_w < 0.00001] <- "*****"
  df$sig_d2<-"ns"
  df$sig_d2[df$FDR_d < 0.05] <- "*"
  df$sig_d2[df$FDR_d < 0.01] <- "**"
  df$sig_d2[df$FDR_d < 0.001] <- "***"
  df$sig_d2[df$FDR_d < 0.0001] <- "****"
  df$sig_d2[df$FDR_d < 0.00001] <- "*****"
  df 
}