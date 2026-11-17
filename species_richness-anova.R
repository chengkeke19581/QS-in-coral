library(ggplot2)
library(ggsignif)
library(ggsci)
library(Hmisc)
###细菌
df <- data.frame(
  group = factor(c("Medium","Medium","Medium","BH52","BH52","BH52","vibM","vibM","vibM","vibS","vibS","vibS","vibA","vibA","vibA")),
  value = c(4925,10023,10023,
            40593,40593,53481,
            7281,9177,8918,
            42817,14319,21140,
            22868,51026,45772)
)

# 1. 拟合单因素ANOVA
fit_aov <- aov(value ~ group, data = df)
summary(fit_aov)
TukeyHSD(fit_aov)

# 2. Tukey HSD事后多重比较（平衡设计首选）
tukey_result <- TukeyHSD(fit_aov)
tukey_result
plot(tukey_result) # 可视化Tukey比较结果

# 3. 方差齐性检验（ANOVA前提：方差齐性）
library(car)
leveneTest(value ~ group, data = df)

# 4. 正态性检验（残差正态）
shapiro.test(resid(fit_aov))
par(mfrow=c(2,2))
plot(fit_aov) # 诊断图：残差正态、方差齐性
par(mfrow=c(1,1))
##用于论文结果输出（rstatix，方便提取 P 值、制表）
library(rstatix)
library(ggpubr)

# 常规ANOVA
anova_res <- anova_test(df, value ~ group)
anova_res

# Tukey事后检验，输出整洁表格
tukey_tab <- tukey_hsd(df, value ~ group)
tukey_tab

###总群落
df <- data.frame(
  group = factor(c("Medium","Medium","Medium","BH52","BH52","BH52","vibM","vibM","vibM","vibS","vibS","vibS","vibA","vibA","vibA")),
  value = c(12395,15410,15509,
            166024,131114,171686,
            15366,18010,18010,
            124559,58520,77340,
            82368,135683,151548)
)

# 1. 拟合单因素ANOVA
fit_aov <- aov(value ~ group, data = df)
summary(fit_aov)
TukeyHSD(fit_aov)

# 2. Tukey HSD事后多重比较（平衡设计首选）
tukey_result <- TukeyHSD(fit_aov)
tukey_result
plot(tukey_result) # 可视化Tukey比较结果

# 3. 方差齐性检验（ANOVA前提：方差齐性）
library(car)
leveneTest(value ~ group, data = df)

# 4. 正态性检验（残差正态）
shapiro.test(resid(fit_aov))
par(mfrow=c(2,2))
plot(fit_aov) # 诊断图：残差正态、方差齐性
par(mfrow=c(1,1))
##用于论文结果输出（rstatix，方便提取 P 值、制表）
library(rstatix)
library(ggpubr)

# 常规ANOVA
anova_res <- anova_test(df, value ~ group)
anova_res

# Tukey事后检验，输出整洁表格
tukey_tab <- tukey_hsd(df, value ~ group)
tukey_tab
