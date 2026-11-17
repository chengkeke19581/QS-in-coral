library(ggplot2)
library(ggsignif)
library(ggsci)
library(Hmisc)
diversity<-read.csv("bacteria_diversity_rarc.csv",row.names=1,check.names=FALSE)


comp_list = list(c('Medium','BH52'),
                 c('vibM','BH52'),
                 c('vibS','BH52'),
                 c('vibA','BH52')) 

##??????
p_shannon<-ggplot(diversity, aes(group, Shannon_ln, color= group)) +
  geom_boxplot(width = 0.7,alpha=0.8,size=1) +
  geom_point(size = 3, alpha = 0.6) +
  geom_signif(comparisons = comp_list,
              test = 'wilcox.test',
              step_increase = 0.1,
              map_signif_level = T,
              vjust = 0.1,
              # y_position = c(7,7.5),
              # tip_length = c(0.1,0.25,0.7,0.1),
              linetype = 'dashed',color = 'black') +
  scale_x_discrete(limit = c('Medium','BH52','vibM','vibS','vibA')) +
  scale_color_manual(values = c("#88c4e8","#00A08A","#d4b9da","#fb6a4a","#e5ce81"))+
  xlab("group")+ylab("shannon")+
  theme_bw()+
  theme(panel.grid = element_blank(),legend.title = element_blank())
p_shannon
p_richness<-ggplot(diversity, aes(group,species_richness_S, color= group)) +
  geom_boxplot(width = 0.7,alpha=0.8,size=1) +
  geom_point(size = 3, alpha = 0.6) +
   geom_signif(comparisons = comp_list,
               test = 'wilcox.test',
              step_increase = 0.1,
               map_signif_level = T,
              vjust = 0.1,
  # y_position = c(7,7.5),
  # tip_length = c(0.1,0.25,0.7,0.1),
             linetype = 'dashed',color = 'black') +
  scale_x_discrete(limit = c('Medium','BH52','vibM','vibS','vibA')) +
  scale_color_manual(values = c("#88c4e8","#00A08A","#d4b9da","#fb6a4a","#e5ce81"))+
  xlab("group")+ylab("species richness")+
  theme_bw()+
  theme(panel.grid = element_blank(),legend.title = element_blank())
p_richness
ggsave("bacteria_species_richness_new.pdf",p_richness,height=4.68,width=5.97,dpi=600)
p_Simpson<-ggplot(diversity, aes(group, Simpson_D, color= group)) +
  geom_boxplot(width = 0.7,alpha=0.8,size=1) +
  geom_point(size = 3, alpha = 0.6) +
  geom_signif(comparisons = comp_list,
              test = 'wilcox.test',
              step_increase = 0.1,
              map_signif_level = T,
              vjust = 0.1,
              # y_position = c(7,7.5),
              # tip_length = c(0.1,0.25,0.7,0.1),
              linetype = 'dashed',color = 'black') +
  scale_x_discrete(limit = c('Medium','BH52','vibM','vibS','vibA')) +
  scale_color_manual(values = c("#88c4e8","#00A08A","#d4b9da","#fb6a4a","#e5ce81"))+
  xlab("group")+ylab("Simpson")+
  theme_bw()+
  theme(panel.grid = element_blank(),legend.title = element_blank())
p_Simpson

ggsave("all_species_richness.pdf",p,height=4.68,width=5.97,dpi=600)



