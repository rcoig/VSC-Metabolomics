mergegroups<-function(df){
  df<-merge(df,key[,3:4],by="row.names",all=F)
  df$Group<-paste(df$Karyotype,df$SDGroup,sep="")
  df$Group <- factor(df$Group, levels = c("XXw", "XXd", "XYd", "XYw"))
  row.names(df)<-df$Row.names
  df<-df[,-c(1,93,94)]
  df 
}