library(UpSetR)

# 1. 读取三组差异结果
de_luxM <- read.csv("luxMvsWT.csv",check.names=FALSE,header = T)
de_luxS <- read.csv("luxSvsWT.csv",check.names=FALSE,header = T)
de_cqsA <- read.csv("cqsAvsWT.csv",check.names=FALSE,header = T)

# 2. 按阈值筛选DEG：padj <0.05 且 |log2FC|>1
thresh_padj <- 0.05
thresh_lfc <- 1

de_list <- list(
  `ΔluxM vs WT` = de_luxM$gene_id[de_luxM$padj < thresh_padj & abs(de_luxM$log2FC) > thresh_lfc],
  `ΔluxS vs WT` = de_luxS$gene_id[de_luxS$padj < thresh_padj & abs(de_luxS$log2FC) > thresh_lfc],
  `ΔcqsA vs WT` = de_cqsA$gene_id[de_cqsA$padj < thresh_padj & abs(de_cqsA$log2FC) > thresh_lfc]
)

sapply(de_list, length)

# 3. 绘制UpSet图
upset(fromList(de_list),
      order.by = "freq",
      sets.bar.color = c("#E64B35","#4DBBD5","#3C5488"),
      mainbar.y.label = "Number of DEGs in intersection",
      sets.x.label = "Total DEGs per comparison",
      text.scale =2)

# 指定你想要的顺序
target_sets <- c( "ΔcqsA vs WT","ΔluxS vs WT","ΔluxM vs WT")

# # pdf("upset1.pdf", width = 7, height = 5)
cairo_pdf("upset.pdf", width =9, height=7)
upset(fromList(de_list),
      sets = target_sets,          # 定义展示集合+期望顺序
      keep.order = TRUE,           # 【关键】锁定sets顺序，不自动按集合大小排序
      order.by = "freq",           # 交集柱子依旧按交集基因数量排序
      sets.bar.color = c("#d4b9da","#e5ce81","#fb6a4a"),
      mainbar.y.label = "Number of DEGs in intersection",
      sets.x.label = "Total DEGs per comparison",
      text.scale = 2)
# 关闭设备，图片才会保存
dev.off()
