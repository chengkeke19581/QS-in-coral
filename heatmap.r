#install.packages("ComplexHeatmap") 
library(pheatmap) # 加载包
library(ggplot2) # 加载包
library(RColorBrewer)
data <- read.table("blast1.txt",  
                   header=T, # 数据集第一行为变量名
                   row.names=1, # 第一列为行名
                   sep="\t") # 指定分隔符号
annotation_row <- read.table("group.txt",  header=T, row.names=1, sep="\t")
row.names(annotation_row)<-rownames(data)

coul <- colorRampPalette(colors = c("#6baed6","white","#F46161"))(100)


p <- pheatmap(data,angle_col = 45,
              cellwidth = 40,cellheight = 30,
              color = coul,
              display_numbers=TRUE,
              border="white", # 设置边框为白色
              cluster_cols = F, # 去掉横向、纵向聚类
              cluster_rows = F,
              gaps_row = c(3,6,8),gaps_col = 5,
              annotation_row=annotation_row)
p <- pheatmap(data,angle_col = 45,
              cellwidth = 40,cellheight = 30,
              color = coul,
              display_numbers=TRUE,
              border="white", # 设置边框为白色
              cluster_cols = F, # 去掉横向、纵向聚类
              cluster_rows = F,
              gaps_row = c(3,6,8),gaps_col = 5,
              fontsize = 12, fontsize_row = 12, fontsize_col = 10,
              annotation_row=annotation_row)
              
ggsave("blast.pdf", plot = p, dpi = 600)
ggsave("blast1.pdf",  plot = p, width = 9, height =6, dpi = 600)
library(pheatmap)

