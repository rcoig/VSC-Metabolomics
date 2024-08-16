mergegroups<-function(df){
  df<-merge(df,key[,3:4],by="row.names",all=F)
  df$Group<-paste(df$Karyotype,df$V,sep="")
  df$Group <- factor(df$Group, levels = c("XXG", "XXV", "XYV", "XYG"))
  row.names(df)<-df$Row.names
  df<-df[,-c(1,93,94)]
  df 
}