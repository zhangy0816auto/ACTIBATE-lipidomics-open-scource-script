##load package
library(emmeans) #three new packages you might need to install them 
library(multcomp)
library(multcompView)
library(ggplot2) 
library(tidyr)
library(dplyr)
library(readr)
library(openxlsx)
library(RColorBrewer)
library(viridis)
library(ComplexHeatmap)
library(circlize)

#---------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------
#80%adherence 

#load data
df_80adhe <- read.xlsx("80_coherence/data after imputation and transformation-noadherence.xlsx",sheet = 2, colNames = TRUE, rowNames = F)
# convert 0 to 0.0001
df_80adhe[df_80adhe == 0] <- 0.0001
#check data
str(df_80adhe)
sum(is.na(df_80adhe))
#write.csv(df_80adhe,"80_coherence/results/LION-0-DATA.csv")
### Setting up a liner model
# Create a list to store results
final_results <- list()
col_names <- colnames(df_80adhe)[3:794]
for (col in col_names) {
  # extract data
  dbomics <- df_80adhe[[col]]
  fit <- lm(dbomics ~ Group, data = df_80adhe)
  # Store results in the list
  final_res <- anova(fit)
  # Store results in the list
  final_results[[col]] <-  final_res
  # Print or analyze results for the current column
  cat("Results for column:", col, "\n")
  print(final_res)
}
summary(final_results)

####posthoc$contrasts
for (col_name in names(final_results)) {
  pr_value <- final_results[[col_name]][["Pr(>F)"]]
  
  if (!is.null(pr_value)) {
    cat("Variable:", col_name, "p-value:", pr_value, "\n")
  }
}

significant_vars <- c()
for (col_name in names(final_results)) {
  res <- final_results[[col_name]]
  if ("Group" %in% rownames(res) && "Pr(>F)" %in% colnames(res)) {
    pr_value <- res["Group", "Pr(>F)"]
    if (!is.na(pr_value) && pr_value < 0.05) {
      cat("Variable:", col_name, "p-value:", pr_value, "\n")
      significant_vars <- c(significant_vars, col_name)
    }
  }
}

# tuky
tuky_final_results <- list()
for (col in significant_vars) {
  # extract data
  dbomics <- df_80adhe[[col]]
  
  # fit linear model
  lmR <- lm(dbomics ~ Group, data = df_80adhe)
  
  # perform Tukey's HSD test
  posthoc <- emmeans(lmR, "Group")
  adjusted_posthoc <- summary(pairs(posthoc, adjust = "bonferroni"))
  cat("Results for column:", col, "\n")
  print(adjusted_posthoc)
  # store results in the list
  tuky_final_results[[col]] <- adjusted_posthoc
}

###结果提取并且每组取平均值
#结果表格初始化
mean_table <- data.frame(Name = significant_vars, CON = NA, MOD_EX = NA, VIG_EX = NA)

# 遍历每个显著变量，计算各组均值
for (i in seq_along(significant_vars)) {
  var <- significant_vars[i]
  lipid_data <- df_80adhe[, c("Group", var)]
  
  # 计算分组均值
  group_means <- aggregate(lipid_data[[var]], by = list(Group = lipid_data$Group), FUN = mean)
  
  # 对应赋值
  mean_table$CON[i] <- group_means[group_means$Group == "CON", "x"]
  mean_table$MOD_EX[i] <- group_means[group_means$Group == "MOD-EX", "x"]
  mean_table$VIG_EX[i] <- group_means[group_means$Group == "VIG-EX", "x"]
}
write.xlsx(mean_table,"80_coherence/results/anova-filtered_LIPIDS_log2fc_NOadhe.xlsx",dec = ".",colNames = TRUE, rowNames = TRUE)
# 查看结果
print(mean_table)
# 提取 Group 列和 significant_vars 中的所有列
filtered_df <- df_80adhe[, c("Group", significant_vars)]
write.xlsx(filtered_df,"80_coherence/results/anova-filtered_log2fc_NOadhe_sex.xlsx",dec = ".",colNames = TRUE, rowNames = TRUE)
#####EXTRACT  P-values
# Extract p-values for each group comparison
variable_names <- character()
p_values <- matrix(nrow = 0, ncol = 3)

# Loop through the list
for (i in 1:length(tuky_final_results)) {
  # Extract the variable name
  variable_name <- names(tuky_final_results)[i]
  # Extract the p-values
  p_value <- tuky_final_results[[i]]$p.value
  
  # Append variable name to the list
  variable_names <- c(variable_names, variable_name)
  
  # Append p-values to the matrix
  p_values <- rbind(p_values, p_value)
}

# Create a data frame to store variable names and p-values
p_value_df <- data.frame(Variable = variable_names, 
                         Group1_vs_Group2 = p_values[,1], 
                         Group1_vs_Group3 = p_values[,2], 
                         Group2_vs_Group3 = p_values[,3])
write.xlsx(p_value_df,"80_coherence/results/anova-filtered_log2fc_NOadhe.xlsx",dec = ".",colNames = TRUE, rowNames = TRUE)



df_filter <- read.xlsx("80_coherence/results/anova-filtered_LIPIDS_log2fc_NOadhe.xlsx",sheet = 1, colNames = TRUE, rowNames = T)
df_pvalue <- read.xlsx("80_coherence/results/anova-filtered_log2fc_NOadhe.xlsx",sheet = 1, colNames = TRUE, rowNames = T)
###plot!!
col_fun = colorRamp2(c(-1, 0, 1), c("blue", "white", "red"))
col_fun(seq(-5, 5))

p2 <- Heatmap(df_filter[c(1:23),1:3],
              cluster_rows = F,
              cluster_columns = F,
              rect_gp = gpar(col = "white", lwd = 1),
              #border_gp = gpar(col = "black",lwd = 2),
              row_names_gp = gpar(fontsize = 9),
              width = unit(0.8, "in"),  # Adjust the width as needed
              height = unit(6, "in"),
              row_names_side = "left",
              heatmap_legend_param = list(title = "log2FC", at = c(-1,-0.5,0,0.5,1)),
              border = T,
              #column_names_rot = 0,
              col = col_fun
)
p2
breaks <- c(0, 0.01,0.02,0.03,0.04, 0.05)
labels <- c("<0.01", "0.01","0.02","0.03","0.04",">0.05")
p3 <-    Heatmap(df_pvalue[c(1:23),1:3],
                 cluster_rows = F,
                 cluster_columns = FALSE,
                 rect_gp = gpar(col = "grey90", lwd = 1),
                 row_names_gp = gpar(fontsize = 9),
                 width = unit(0.8, "in"),  # Adjust the width as needed
                 height = unit(6, "in"),
                 #row_names_side = "left",
                 show_row_names = FALSE,
                 border = T,
                 heatmap_legend_param = list(title = "p-value",at=breaks,labels=labels),
                 col = colorRamp2(c(0.01, 0.025, 0.051), c("#f3ff05","#f8f984", "white"))
)
p3
total_p = p2 + p3
draw(total_p)
#PE : 43:85
#PCs & Pis&SM :c(24:42,86:94)
#TG : 95:118
#others:1:23
####extract!!!!!
png(
  filename = "80_coherence/results/anova_Others_NOadhe.png",
  type = "cairo", # 抗锯齿
  res = 300, # 300ppi 分辨率
  width = 2000, height = 4000,
  bg = "transparent" # 透明背景
)
print(total_p)
dev.off()


