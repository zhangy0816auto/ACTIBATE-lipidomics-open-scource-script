install.packages("reshape")
install.packages("dplyr")
install.packages("stringr")
install.packages("tidyverse")
install.packages("tidyr")
install.packages("ggrepel")
install.packages("readxl")
#### Loading library ####
library(dplyr)
library(reshape)
library(stringr)
library(tidyverse)
library(tidyr)
library(ggrepel)
library(readxl)
##In the csv data file, replace left "(" with "_", remove right ")", replace ":" with "."
###Machine readable lipids names should be used in when building methods.

#### 01 Clean data, add lipid class info ####
# Import data frame
df <- read_excel("80_coherence/data after imputation and transformation-noadherence.xlsx",sheet = 1,col_names = TRUE)

df_identifier<- read.table("data/LIPID_PLATFORM_lipid identifier.csv", header = TRUE,sep = ",")

# numeric 
df <- df %>%
  mutate_at(vars(3:794), as.numeric)

#check NA
if (sum(is.na(df)) == 0) {
  cat("no NA。\n")
} else {
  cat("Still NA。\n")
}
# Wide-form to long-form
df <- as.data.frame(df) # ERROR:names do not match previous names, commonly occurs in package dplyr
class(df)
colnames(df)
any(duplicated(colnames(df)))
df_long <- melt(df, measure.vars = 3:794, variable_name = "Lipid", value_name = "Value")



colnames(df_long)[4] = "Value"


# Check how many NA row
table(is.na(df_long$Value))

# Perform log2
df_long$log2_value <- df_long$Value

# Filter group
df_Moderate_POS <- df_long %>% filter(Group == "Moderate_POS")

df_Moderate_PRE <- df_long %>% filter(Group == "Moderate_PRE")

df1 <- rbind(df_Moderate_POS, df_Moderate_PRE)

# omit NA in df1
df1 <- na.omit(df1)

# Change column class as factor
df1$Group <- as.factor(df1$Group)

# Change column class as numeric
df1$Value <- as.numeric(df1$Value)

# Perform Wilcox test and t test
df_result <- data.frame(
  "Lipid" = unique(df1$Lipid),
  "t.test.paired" = NA
)
for (idx in 1:nrow(df_result)) {
  val <- as.character(df_result$Lipid[idx])
  # 提取该 Lipid 对应的数据
  df_filter <- df1[df1$Lipid == val, ]
  # reshape 到宽表，Name 是配对键
  df_wide <- reshape(df_filter[, c("Name", "Group", "log2_value")],
                     timevar = "Group",
                     idvar = "Name",
                     direction = "wide")
  # 只有在两个组都有值的情况下才进行配对检验
  if (all(c("log2_value.Moderate_PRE", "log2_value.Moderate_POS") %in% colnames(df_wide))) {
    df_result$t.test.paired[idx] <- t.test(
      df_wide$log2_value.Moderate_PRE,
      df_wide$log2_value.Moderate_POS,
      paired = TRUE
    )$p.value
  } else {
    df_result$t.test.paired[idx] <- NA
  }
}
# p-adjust
df_result <- df_result %>%
  mutate(t_fdr = p.adjust(df_result$t.test, method = "fdr"))
#write.csv(df_result,"Volcano plot_ttests_MS human_Moderate_PRE_Moderate_POS.csv", row.names = TRUE)


#### 02 Volcano plot ####
##### 02-01 Calculate ####
# Calculate fold change of Moderate_POS/Moderate_PRE
df_Moderate_PRE <- df1 %>%
  filter(Group == "Moderate_PRE") %>%
  group_by(Lipid) %>%
  summarise(ave_Moderate_PRE = mean(log2_value))

df_Moderate_POS <- df1 %>%
  filter(Group == "Moderate_POS") %>%
  group_by(Lipid) %>%
  summarise(ave_Moderate_POS = mean(log2_value))


# Join
df_plot <- df_result %>%
  full_join(df_Moderate_PRE, by = "Lipid") %>%
  full_join(df_Moderate_POS, by = "Lipid") %>%
  mutate(log2FoldChange = ave_Moderate_POS - ave_Moderate_PRE)



# keep t_fdr test result
df_plot <- df_plot %>% select(c(Lipid, t.test.paired, log2FoldChange))
#df_plot <- df_plot %>% select(-c(ave_Moderate_PRE, ave_Moderate_POST, wilcox.test, ))
colnames(df_plot)[2] = "p.value"
colnames(df_plot)[1] = "Lipid"
df_plot$Lipid <- as.character(df_plot$Lipid)


### new version volcano plot
library(dplyr)
# Left join, add class info
df_plot2 <- df_plot %>%
  left_join(df_identifier, by = c("Lipid" = "Metabolite"))

df_plot2$Expression <- df_plot2$Class
df_plot2$Expression[df_plot2$p.value > 0.05] <- NA_character_   # 确保是字符型 NA
df_plot2$Expression[is.na(df_plot2$Expression)] <- "NA"         # 全部换成字符串 "NA"
df_plot2$Expression <- factor(df_plot2$Expression, levels = c(
  "TG", "PC", "CE", "PE", "LPE", "LPC", "LPI", "PI", "PG", "Cer", "GlcCer", "SM", "DAG","NA"
))

df_plot2$label <- df_plot2$Lipid
df_plot2$label[df_plot2$`p.value`>0.05]=NA

# 先选出显著的
# 只对非 NA 的显著类做分组筛选
sig_lipids <- df_plot2 %>%
  filter(Expression != "NA") %>%   # 排除灰色组（原本是 NA 的）   # 只对有 Expression 的显著点操作
  group_by(Expression) %>%
  slice_min(order_by = p.value, n = 3, with_ties = FALSE) %>%
  ungroup()

# 设置 label，只保留显著类中的前三个"green3"
df_plot2$label <- ifelse(df_plot2$Lipid %in% sig_lipids$Lipid, df_plot2$Lipid, NA)
df_plot2 <- df_plot2 %>% filter(!is.na(Expression))

g <- ggplot(data = df_plot2, aes(x = log2FoldChange, y = -log10(p.value), col = Expression, label = label)) +
  geom_point(size = 3) +
  theme_minimal() +
  scale_color_manual(values = c("NA" = "grey80",
                                "TG" = "hotpink",
                                "PC" = "green3",
                                "CE" = "lightcoral",
                                "PE" = "deepskyblue",
                                "LPE" = "forestgreen",
                                "LPC" = "orange",
                                "LPI" = "blue",
                                "PI" = "purple",
                                "PG" = "gold2",
                                "Cer" = "cornflowerblue",
                                "GlcCer" = "indianred4",
                                "SM" = "darkblue",
                                "DAG" = "plum4"
                                )) +
  geom_hline(yintercept = -log10(0.05), col = "red", size = 0.5, linetype = "dashed") +
  annotate("text", x = 1.2, y = 0.8, label = "p-value=0.05", colour = "darkblue", fontface = "italic") +
  geom_text_repel(min.segment.length = 0, seed = 42, box.padding = 0.1, max.overlaps = 120) +
  ggtitle("VIG-EX") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
  xlim(c(-1, 2)) +
  ylim(c(0, 6))
g
###export
png(
  filename = "80_coherence/results/volcano_VIG_paird_no_cohe.png",
  type = "cairo", # 抗锯齿
  res = 300, # 300ppi 分辨率
  width = 2600, height = 2000,
  bg = "transparent" # 透明背景
)
print(g)
dev.off()


##significant counts 
# 按 Expression 分组并统计每组中 p < 0.05 的数量
sig_summary <- df_plot2 %>%
  filter(p.value < 0.05, Expression != "NA") %>%   # 排除灰色组
  group_by(Expression) %>%
  summarise(Significant_Count = n()) %>%
  arrange(desc(Significant_Count))  # 按数量从多到少排序

# 查看结果表格
print(sig_summary)

##plot
sig_count <- df_plot2 %>%
  filter(p.value < 0.05, Expression != "NA") %>%
  group_by(Expression) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# 配色与火山图一致
color_map <- c("TG" = "hotpink",
               "PC" = "green3",
               "CE" = "lightcoral",
               "PE" = "deepskyblue",
               "LPE" = "forestgreen",
               "LPC" = "orange",
               "LPI" = "blue",
               "PI" = "purple",
               "PG" = "gold2",
               "Cer" = "cornflowerblue",
               "GlcCer" = "indianred4",
               "SM" = "darkblue")

# 创建 barplot
bar_plot <- ggplot(sig_count, aes(x = Count, y = reorder(Expression, Count))) +
  geom_bar(stat = "identity", aes(fill = Expression), width = 0.7) +
  geom_text(aes(label = Count), hjust = -0.2, size = 5) +
  scale_fill_manual(values = color_map) +
  theme_minimal() +
  labs(x = "", y = "", title = "Significant Lipids per subclass (p < 0.05)") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.text = element_text(face = "bold",size = 12),
    panel.grid = element_blank()  # <<< 这句移除所有网格线
  ) +
  xlim(0, max(sig_count$Count) * 1.1)  # 留出空间显示数字

# 显示图
bar_plot
###export
png(
  filename = "80_coherence/results/barplot_mod_paird_no_cohe.png",
  type = "cairo", # 抗锯齿
  res = 300, # 300ppi 分辨率
  width = 1200, height = 2500,
  bg = "transparent" # 透明背景
)
print(bar_plot)
dev.off()
