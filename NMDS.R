library(vegan) 
library(ggplot2)
library(svglite)
library(ecodist)
require(ecodist)
##otu_table<-read.csv("nmds-bacteria.csv",row.names=1,check.names=FALSE)
otu_table<-read.csv("nmds-bacteria-rarc.csv",row.names=1,check.names=FALSE)
otu_table<-t(otu_table)
grouping_info<-data.frame(row.names=rownames(otu_table),t(as.data.frame(strsplit(rownames(otu_table),"_"))))
head(grouping_info)
sol<-metaMDS(otu_table,distance = "bray", k = 2, trymax = 50)
sol
NMDS=data.frame(x=sol$point[,1],y=sol$point[,2],group=as.factor(grouping_info[,1]),Sample=as.factor(grouping_info[,2]))
NMDS
plot.new()
ord<-ordiellipse(sol, as.factor(grouping_info[,1]) ,display = "sites", kind ="sd", conf = 0.95, label = T)
dev.off()
veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

df_ell <- data.frame()
for(g in levels(NMDS$group)){
  if(g!="" && (g %in% names(ord))){
    df_ell <- rbind(df_ell, cbind(as.data.frame(with(NMDS[NMDS$group==g,],
                                                     veganCovEllipse(ord[[g]]$cov,ord[[g]]$center,
                                                                     ord[[g]]$scale))),group=g))
  }
}

head(df_ell)
df_ell
NMDS.mean=aggregate(NMDS[,1:2],list(group=NMDS$group),mean)
NMDS.mean
##shape_values<-seq(1,5)
p<-ggplot(data=NMDS,aes(x,y,colour=group))
p<-p+annotate("text",x=NMDS.mean$x,y=NMDS.mean$y,label=NMDS.mean$group,size=0)
##p<-p+ geom_path(data=df_ell, aes(x=NMDS1, y=NMDS2), linewidth=1, linetype=2)
p<-p+geom_point(aes(shape=group),size=8)+
  ##   scale_color_brewer(type="seq",palette="Set2") +
   scale_color_manual(values = c("#00A08A","#88c4e8","#fb6a4a","#e5ce81","#d4b9da"))+ 
    scale_shape_manual(values = c(20,15,18,16,17))+theme_bw()


p<-p+theme(panel.grid.major =element_blank(), 
           panel.grid.minor = element_blank(),
           panel.background = element_blank(), 
           axis.line = element_line(colour = "black")
          )
p<-p+ xlab("NMDS1")+ylab("NMDS2")+theme(legend.title = element_text(colour = "black",size=14,face="plain"),
                                        legend.text = element_text(face="plain",size=14))
p
p<-p+ theme(axis.text.x = element_text(size=12, color = "black")) +
  theme(axis.text.y = element_text(size=12, color = "black"))+
  theme(axis.title=element_text(size=14, color="black", 
                                face="plain",vjust=3)) 
p
p1<-p+annotate('text',x=1.9,y=1.5,label='Stress= 0.052',colour = "black",size=5)
p1
ggsave("NMDS-bacteria-rarc.pdf",p1,height=5,width=5.97,dpi=600)
pdf("NMDS-bacteria.pdf")
print(p)
dev.off()

plot.new()
