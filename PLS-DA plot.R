
# install Biomanager package
if (!require("BiocManager", quietly = TRUE))
install.packages("BiocManager")

# Load BiocManager
library(BiocManager)

BiocManager::install()
# install mixOmics package

BiocManager::install('mixOmics')

library(mixOmics)
library(readxl)
df <- read_excel("80_coherence/data after imputation and transformation-noadherence.xlsx",sheet = 2,col_names = TRUE)

#final PLSDA
X <- df[,3:794]
Y <- df$Group
final.plsda.df <- plsda(X,Y, ncomp = 2)
plotIndiv(final.plsda.df, ind.names = FALSE, legend=TRUE,
          ellipse = TRUE, 
          title =" PLS-DA",
          #X.label = 'component 1(13%)', Y.label = 'component 2(23%)',
          col = c("grey","orange","lightskyblue"),
          pch = 17
          )
#X.label = 'component 1(8%)', Y.label = 'component 2(22%)',
# col = c("grey","orange","lightskyblue")
##export!!!
png(
  filename = "80_coherence/results/PLS-DA_VO2_responder_NOcohe.png",
  type = "cairo", # 抗锯齿
  res = 300, # 300ppi 分辨率
  width = 2000, height = 1600,
  bg = "transparent" # 透明背景
)
plotIndiv(final.plsda.df, ind.names = FALSE, legend=TRUE,
          ellipse = TRUE, 
          title =" PLS-DA",
          X.label = 'component 1(27%)', Y.label = 'component 2(9%)',
          col = c("grey","orange","lightskyblue"),
          pch = 17
)
dev.off()

###add sex as different labels
# 假设性别在第3列
sex <- df$Sex
pch.shape <- ifelse(sex == "Men", 16, 1)
pch.levels <- rep(c("Women", "Men"), length.out = 101)
X <- df[,4:798]
Y <- df$Group
final.plsda.df <- plsda(X, Y, ncomp = 2)


# 绘制 PLS-DA 图
library(grid)
plotIndiv(final.plsda.df, ind.names = FALSE,
          ellipse = TRUE,
          legend = T,
          style = "ggplot2",
          title = "PLS-DA",
          X.label = "component 1(27%)", Y.label = "component 1(9%)",
          col = c("grey", "orange", "lightskyblue"),
          legend.title = "Group",
          legend.title.pch = "Sex",
          pch = pch.shape,
          pch.levels = pch.levels)

# 查看 PLS-DA 载荷（loadings）和样本（scores）
scores <- final.plsda.df$variates$X  # 获取样本得分（scores）
loadings <- final.plsda.df$loadings$X  # 获取变量载荷（loadings）

# 查看载荷，确认哪些变量在component 2 上的贡献
loadings[,2]  # Component 2 对每个变量的载荷

# 查看样本分布，观察性别在component 2 的差异
scores_df <- data.frame(scores)
scores_df$Sex <- df$Sex

# 绘制 component 2 的分布，检查性别差异
colnames(scores_df)
library(ggplot2)
# 绘制 component 2 与性别的关系
ggplot(scores_df, aes(x = comp1, color = Sex)) +
  geom_point(aes(y = comp1),size = 3) +  # 显式指定 y = comp2
  labs(title = "Component 1 vs Sex", x = "Component 1", y = "Score") +
  theme_minimal()

p1<- ggplot(scores_df, aes(x = comp1, color = Sex)) +
  geom_point(aes(y = comp1),size = 3)+
  scale_color_manual(values = c("Men" = "skyblue", "Women" = "orange")) +  # 手动设置颜色
  labs(title = "Component 1 vs Sex", x = "Component 1", y = "Score") +
  theme_minimal()
p1
p2<- ggplot(scores_df, aes(x = comp2, color = Sex)) +
  geom_point(aes(y = comp2),size = 3)+
  scale_color_manual(values = c("Men" = "skyblue", "Women" = "orange")) +  # 手动设置颜色
  labs(title = "Component 2 vs Sex", x = "Component 2", y = "Score") +
  theme_minimal()
p2

##export!!!
png(
  filename = "results/PLS-DA_sex_Component 1 vs Sex.png",
  type = "cairo", # 抗锯齿
  res = 300, # 300ppi 分辨率
  width = 1800, height = 1600,
  bg = "transparent" # 透明背景
)
print(p1)
dev.off()















# col = c("gray","gray37","palevioletred","red", "skyblue","blue","palegreen" ,"green")
#performance of PLS-DA
perf.final.plsda.df <- perf(final.plsda.df, validation = 'Mfold', 
                               folds = 3, 
                               progressBar = FALSE, # TRUE to track progress
                               nrepeat = 50)
perf.final.plsda.df$error.rate$BER[, 'max.dist']
perf.final.plsda.df$error.rate.class$max.dist




##### variable importance in the projection
options(max.print=999999)
vip <- vip(final.plsda.df)
# Select top 10 variables based on comp1 score
top_50_comp1 <- head(order(-vip[, "comp1"]), 50)

# Ensure indices are within the valid range
top_50_comp1 <- top_50_comp1[top_50_comp1 >= 1 & top_50_comp1 <= nrow(vip)]

# Subset the original VIP data to include only the top 50 variables-comp1
vip_top_50_comp1 <- vip[top_50_comp1, "comp1", drop = FALSE]

# Set larger margins for better visibility of labels

par(mar = c(5, 10, 4, 2) + 0.1, oma = c(0, 3, 0, 0))

# Create a bar plot for the top 30 comp1 values
library(RColorBrewer)
barplot(as.matrix(vip_top_50_comp1),
        beside = TRUE, col = c("lightblue", "mistyrose", "lightcyan"),
        #main = "Top 50 Variable Importance",
        xlim = c(0,3),
        font.main = 4,
        names.arg = rownames(vip_top_50_comp1),
        las = 2,
        horiz = TRUE,
        font = 1,
        cex.names = 0.7,
        xlab = "VIP scores"  # Add this line for x-axis label
        )


##export!!!
png(
  filename = "80_coherence/results/vip_NOcohe.png",
  type = "cairo", # 抗锯齿
  res = 300, # 300ppi 分辨率
  width = 3000, height = 3000,
  bg = "transparent" # 透明背景
)
par(mar = c(5, 10, 4, 2) + 0.1, oma = c(0, 3, 0, 0))
barplot(as.matrix(vip_top_50_comp1),
        beside = TRUE, col = c("lightblue", "mistyrose", "lightcyan"),
        #main = "Top 50 Variable Importance",
        xlim = c(0,3),
        font.main = 4,
        names.arg = rownames(vip_top_50_comp1),
        las = 2,
        horiz = TRUE,
        font = 1,
        cex.names = 0.95,
        xlab = "VIP scores"# Add this line for x-axis label)
)
dev.off()


