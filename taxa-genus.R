abund_table<-read.csv("bacteria_genus.csv",row.names=1,check.names=FALSE)
#Transpose the data to have sample names on rows
abund_table<-t(abund_table)
#Apply proportion normalisation
x<-abund_table/rowSums(abund_table)
x<-x[,order(colSums(x),decreasing=TRUE)]
#Extract list of top N Taxa
#N<-11
#taxa_list<-colnames(x)[1:N]
#taxa_list
#remove "__Unknown__" and add it to others
#taxa_list<-taxa_list[!grepl("Unknown",taxa_list)]
#N<-length(taxa_list)
#Generate a new table with everything added to Others
#new_x<-data.frame(x[,colnames(x) %in% taxa_list],Others=rowSums(x[,!colnames(x) %in% taxa_list]))


df<-NULL
for (i in 1:dim(abund_table)[2]){
  tmp<-data.frame(row.names=NULL,Sample=rownames(abund_table),Taxa=rep(colnames(abund_table)[i],dim(abund_table)[1]),Value=abund_table[,i])
  if(i==1){df<-tmp} else {df<-rbind(df,tmp)}
}
colours <- c("#003380",  "#FFFF00","#CCEEFF","#FFDDDD",
             "#FFA8BB","#9DCC00", "gray",
          "#5EF1F2","#00998F", "#FFA405", "#F0A3FF")
library(ggplot2)
p<-ggplot(df,aes(Sample,Value,fill=Taxa))+geom_bar(stat="identity")
p<-p+scale_fill_manual(values=colours[1:(N+1)])
p<-p+theme_bw()+ylab("Relative abundance of bacteria at genus level")+xlab(" ") +labs(fill="Genus")+ theme(legend.text = element_text(face="italic",size=14))
p<-p+ scale_y_continuous(expand = c(0,0))+theme(strip.background = element_rect(fill="black"))+theme(panel.spacing = unit(0.3, "lines"))
p<-p+theme(axis.text.x=element_text(angle=0,hjust=0.5,vjust=0.5))
p<-p+theme(panel.grid.major =element_blank(), 
           panel.grid.minor = element_blank(),
           panel.background = element_blank(),
           axis.line = element_line(colour = "black"))
p<-p+theme(axis.title = element_text(face="plain",size=16)) +
  theme(axis.text.x = element_text(size=14, color = "black",face="plain")) +
  theme(axis.text.y = element_text(size=14, color = "black")) 
p
ggsave("genus-average.pdf",p,height=8.41,width=12.24,dpi=600)


pdf("genus-average.pdf",height=8.41,width=12.24)
print(p)
dev.off()
colours <- c("#003380", "#6600CC", "#FFFF00",
             "#CCEEFF","#FFDDDD","#FF0010","#808080",
             "#94FFB5","#8F7C00","#9DCC00","#C20088",
             "gray","#5EF1F2","#00998F","#426600",
             "#FFCC99","#FFA405","#FFA8BB","#740AFF",
             "#990000","#FFCC66","#F0A3FF")