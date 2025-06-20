

#PACKAGE
library(openxlsx)
library(rstatix)
library(mixOmics)

#----------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------
##80adhe
  
#----------------------------------------------------------------------------------------------------------------------------
##PLSDA
df_80adhe <- read.xlsx("80_coherence/ACTIBATE_raw data+log2 - 80%coherence.xlsx",sheet = 9, colNames = TRUE, rowNames = F)
df_80adhe$Responder_VO2peak <- as.factor(df_80adhe$Responder_VO2peak)

###final PLSDA
X <- df_80adhe[,4:798]
Y <- df_80adhe$Responder_VO2peak
final.plsda.df_80adhe <- plsda(X,Y, ncomp = 3)
plotIndiv(final.plsda.df_80adhe, ind.names = FALSE, legend=TRUE,
          ellipse = TRUE, 
          title =" PLS-DA ",
          #X.label = 'component 1(17%)', Y.label = 'component 2(24%)',
          col = c("grey","lightskyblue"),
          pch = 17
)
#X.label = 'component 1(8%)', Y.label = 'component 2(22%)',
# col = c("grey","orange","lightskyblue")
###export!!!
png(
  filename = "80_coherence/results/PLS-DA_responder_VO2_PEAK.png",
  type = "cairo", # 抗锯齿
  res = 300, # 300ppi 分辨率
  width = 2200, height = 1500,
  bg = "transparent" # 透明背景
)
plotIndiv(final.plsda.df_80adhe, ind.names = FALSE, legend=TRUE,
          ellipse = TRUE, 
          title =" PLS-DA ",
          X.label = 'component 1(17%)', Y.label = 'component 2(24%)',
          col = c("grey","lightskyblue"),
          pch = 17
)
dev.off()

###performance of PLS-DA
perf.final.plsda.df_80adhe <- perf(final.plsda.df_80adhe, validation = 'Mfold', 
                            folds = 3, 
                            progressBar = FALSE, # TRUE to track progress
                            nrepeat = 50)
perf.final.plsda.df_80adhe$error.rate$BER[, 'max.dist']
perf.final.plsda.df_80adhe$error.rate.class$max.dist


#----------------------------------------------------------------------------------------------------------------------------
##VIP
##### variable importance in the projection
options(max.print=999999)
vip <- vip(final.plsda.df_80adhe)
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
P1 <- barplot(as.matrix(vip_top_50_comp1),
        beside = TRUE, col = c("lightblue", "mistyrose", "lightcyan"),
        #main = "Top 50 Variable Importance",
        xlim = c(0,3),
        font.main = 4,
        names.arg = rownames(vip_top_50_comp1),
        las = 2,
        horiz = TRUE,
        font = 1,
        cex.names = 0.47,
        xlab = "VIP scores"  # Add this line for x-axis label
)

P1
##export!!!
png(
  filename = "80_coherence/results/vip_vo2_PEAK.png",
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
        cex.names = 0.9,
        xlab = "VIP scores",# Add this line for x-axis label)
        cex.lab = 1.2         # Increase the size of the axis label
)
dev.off()



#----------------------------------------------------------------------------------------------------------------------------
##ANCOVA
     #ResponderM & non-Responder
###important anova doesnot support colnames with ()
df_80adhe_new <- df_80adhe
colnames(df_80adhe_new) <- gsub("\\(", "right", colnames(df_80adhe_new))
colnames(df_80adhe_new) <- gsub("\\)", "left", colnames(df_80adhe_new))
colnames(df_80adhe_new) <- gsub(":", "_", colnames(df_80adhe_new))
colnames(df_80adhe_new) <- gsub("/", "and", colnames(df_80adhe_new))
colnames(df_80adhe_new) <- gsub("-", "zhonghao", colnames(df_80adhe_new))
##ANCOVA-Loop
ancova_pvalue=data.frame(matrix(nrow=795,ncol=1))
rownames(ancova_pvalue)=colnames(df_80adhe_new[4:798])
colnames(ancova_pvalue)=c("Responder_VO2peak")

for (i in 4:798){
  var=names(df_80adhe_new)[i]
  res.aov <- anova_test(data = df_80adhe_new, as.formula(paste0(var,"~Group + Responder_VO2peak")), detailed = TRUE)
  ancova_pvalue[var,"Responder_VO2peak"]=res.aov[2,"p"]
}

##extration
significant_indices <- which(ancova_pvalue < 0.05)

adjusted_indices <- significant_indices + 3

###right name
rownames(ancova_pvalue) <- gsub("right", "(", rownames(ancova_pvalue))
rownames(ancova_pvalue) <- gsub("left", ")", rownames(ancova_pvalue))
rownames(ancova_pvalue) <- gsub("_", ":", rownames(ancova_pvalue))
rownames(ancova_pvalue) <- gsub("and", "/", rownames(ancova_pvalue))
rownames(ancova_pvalue) <- gsub("zhonghao", "-", rownames(ancova_pvalue))

rownames(ancova_pvalue)[58]

significant_samples <- df_80adhe[, c(1, 2, 3,  adjusted_indices)]
#
write.xlsx(significant_samples,file = "80_coherence/results/Significant_ANOVA_vo2.xlsx",colNames = TRUE, rowNames = TRUE)


##PLOT FOR MEAN
BiocManager::install("ComplexHeatmap")
library(RColorBrewer)
library(corrplot)
library(Hmisc)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(ComplexHeatmap)
library(circlize)



df_80adhe <- read.xlsx("80_coherence/results/Significant_ANOVA_vo2.xlsx",sheet = 2, colNames = TRUE, rowNames = TRUE)
df_80adhe <- as.matrix(df_80adhe)
col_fun = colorRamp2(c(-1, 0, 1), c("blue", "white", "red"))
col_fun(seq(-5, 5))
p1 <- pheatmap(df_80adhe[1:31,] ,
               cluster_rows = F,
               cluster_cols = F,
               scale = "none",
               border_color = "white",
               fontsize_col = 13,
               fontsize_row = 13,
               cellwidth = 20, 
               cellheight = 17,
               show_rownames = T,
               heatmap_legend_param = list(title = "log2FC", at = c(-1,-0.5,0,0.5,1)),
               legend = TRUE, # Add legend title
               border = T,
               row_names_side = "left",
               color = col_fun
)
p1
p2 <- pheatmap(df_80adhe[32:61,] ,
               cluster_rows = F,
               cluster_cols = F,
               scale = "none",
               border_color = "white",
               fontsize_col = 13,
               fontsize_row = 13,
               cellwidth = 20, 
               cellheight = 17,
               show_rownames = T,
               heatmap_legend_param = list(title = "log2FC", at = c(-1,-0.5,0,0.5,1)),
               legend = TRUE, # Add legend title
               border = T,
               row_names_side = "left",
               color = col_fun
)
p2

png(
  filename = "80_coherence/results/_ANCOVAheatmap_1.png",
  type = "cairo", # 抗锯齿
  res = 300, # 300ppi 分辨率
  width = 1000, height = 5000,
  bg = "transparent" # 透明背景
)
print(p1)
dev.off()
png(
  filename = "80_coherence/results/_ANCOVAheatmap_2.png",
  type = "cairo", # 抗锯齿
  res = 300, # 300ppi 分辨率
  width = 1000, height = 5000,
  bg = "transparent" # 透明背景
)
print(p2)
dev.off()



