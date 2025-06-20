library(ggplot2)
library(tidyr)
library(openxlsx)
library(dplyr)
conflicts()
#load data

####only in moderate group
library(ggrepel)

data_mod <- read.xlsx("80_coherence/results/anova-filtered_log2fc_NOadhe_sex.xlsx", sheet=4, colNames = T)

modex_data <- data_mod %>% filter(Gender %in% c("Men", "Women"))

# 2. 分别提取男性和女性数据，并合并
modex_combined <- inner_join(
  modex_data %>% filter(Gender == "Men") %>% 
    select(Lipids, Label, Men_Value = MOD_EX),
  modex_data %>% filter(Gender == "Women") %>% 
    select(Lipids, Women_Value = MOD_EX),
  by = "Lipids"
)

#plot!!
# 1. 设定坐标范围
min_val <- floor(min(c(modex_combined$Men_Value, modex_combined$Women_Value), na.rm = TRUE))
max_val <- ceiling(max(c(modex_combined$Men_Value, modex_combined$Women_Value), na.rm = TRUE))

# 2. 构造 polygon 区域数据（上三角 & 下三角）
female_dominant_area <- data.frame(
  x = c(min_val, min_val, max_val),
  y = c(min_val, max_val, max_val)
)

male_dominant_area <- data.frame(
  x = c(min_val, max_val, max_val),
  y = c(min_val, min_val, max_val)
)

# 3. 画图
# 1. 选出每个 subclass 中差异最大的前三个脂质
top3_lipids <- modex_combined %>%
  mutate(diff = abs(Women_Value - Men_Value)) %>%
  group_by(Label) %>%
  slice_max(order_by = diff, n = 3, with_ties = FALSE) %>%
  ungroup()

# 2. 修改绘图代码，应用你给出的范围并只标注 top3
p <- ggplot(modex_combined, aes(x = Men_Value, y = Women_Value, color = Label)) +
  # 女性 dominant 阴影
  geom_polygon(data = female_dominant_area, aes(x = x, y = y),
               fill = "plum", alpha = 0.15, inherit.aes = FALSE) +
  # 男性 dominant 阴影
  geom_polygon(data = male_dominant_area, aes(x = x, y = y),
               fill = "lightblue", alpha = 0.15, inherit.aes = FALSE) +
  
  # 对角线
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "black", size = 0.8) +
  
  # 散点
  geom_point(size = 4) +
  
  # 只标注 top3 的脂质
  geom_text_repel(data = top3_lipids,
                  aes(label = Lipids),
                  size = 3, max.overlaps = 100, show.legend = FALSE) +
  
  # 参考线
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  
  # 标注区域说明
  annotate("text", x = -0.25, y = 1, label = "Women dominant", color = "mediumpurple2", size = 5, fontface = "bold") +
  annotate("text", x = 0.7, y = -0.5, label = "Men dominant", color = "skyblue", size = 5, fontface = "bold") +
  
  # 主题与调色
  coord_equal(xlim = c(-0.75, 1.25), ylim = c(-0.75, 1.25)) +
  theme_minimal() +
  scale_color_manual(values = c(
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
    "DG" = "plum4",
    "LPG" = "grey66"
  )) +
  labs(
    x = "Men fold change",
    y = "Women fold change",
    color = "subclass"
  ) +
  theme(legend.position = "right")
p

###export
png(
  filename = "results/scatter_mod_male vs female_noadhe.png",
  type = "cairo", # 抗锯齿
  res = 300, # 300ppi 分辨率
  width = 2000, height = 2300,
  bg = "transparent" # 透明背景
)
print(p)
dev.off()


#---------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------
#KNN 

#load data
data_mod <- read.xlsx("data/male fold change vs female.xlsx", sheet=6, colNames = T)

modex_data <- data_mod %>% filter(Gender %in% c("Men", "Women"))

# 2. 分别提取男性和女性数据，并合并
modex_combined <- inner_join(
  modex_data %>% filter(Gender == "Men") %>% 
    select(Lipids, Label, Men_Value = MOD_EX),
  modex_data %>% filter(Gender == "Women") %>% 
    select(Lipids, Women_Value = MOD_EX),
  by = "Lipids"
)

#plot!!
# 1. 设定坐标范围
min_val <- floor(min(c(modex_combined$Men_Value, modex_combined$Women_Value), na.rm = TRUE))
max_val <- ceiling(max(c(modex_combined$Men_Value, modex_combined$Women_Value), na.rm = TRUE))

# 2. 构造 polygon 区域数据（上三角 & 下三角）
female_dominant_area <- data.frame(
  x = c(min_val, min_val, max_val),
  y = c(min_val, max_val, max_val)
)

male_dominant_area <- data.frame(
  x = c(min_val, max_val, max_val),
  y = c(min_val, min_val, max_val)
)

# 3. 画图
# 1. 选出每个 subclass 中差异最大的前三个脂质
top3_lipids <- modex_combined %>%
  mutate(diff = abs(Women_Value - Men_Value)) %>%
  group_by(Label) %>%
  slice_max(order_by = diff, n = 3, with_ties = FALSE) %>%
  ungroup()

# 2. 修改绘图代码，应用你给出的范围并只标注 top3
p <- ggplot(modex_combined, aes(x = Men_Value, y = Women_Value, color = Label)) +
  # 女性 dominant 阴影
  geom_polygon(data = female_dominant_area, aes(x = x, y = y),
               fill = "plum", alpha = 0.15, inherit.aes = FALSE) +
  # 男性 dominant 阴影
  geom_polygon(data = male_dominant_area, aes(x = x, y = y),
               fill = "lightblue", alpha = 0.15, inherit.aes = FALSE) +
  
  # 对角线
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "black", size = 0.8) +
  
  # 散点
  geom_point(size = 4) +
  
  # 只标注 top3 的脂质
  geom_text_repel(data = top3_lipids,
                  aes(label = Lipids),
                  size = 3, max.overlaps = 100, show.legend = FALSE) +
  
  # 参考线
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  
  # 标注区域说明
  annotate("text", x = -0.25, y = 1, label = "Women dominant", color = "mediumpurple2", size = 5, fontface = "bold") +
  annotate("text", x = 0.7, y = -0.5, label = "Men dominant", color = "skyblue", size = 5, fontface = "bold") +
  
  # 主题与调色
  coord_equal(xlim = c(-0.75, 1.25), ylim = c(-0.75, 1.25)) +
  theme_minimal() +
  scale_color_manual(values = c(
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
    "DG" = "plum4",
    "LPG" = "grey66"
  )) +
  labs(
    x = "Men fold change",
    y = "Women fold change",
    color = "subclass"
  ) +
  theme(legend.position = "right")
p

###export
png(
  filename = "80_coherence/results/scatter_mod_male vs female.png",
  type = "cairo", # 抗锯齿
  res = 300, # 300ppi 分辨率
  width = 2000, height = 2300,
  bg = "transparent" # 透明背景
)
print(p)
dev.off()

#---------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------
#HALFMIN

#load data
data_mod <- read.xlsx("data/male fold change vs female.xlsx", sheet=7, colNames = T)

modex_data <- data_mod %>% filter(Gender %in% c("Men", "Women"))

# 2. 分别提取男性和女性数据，并合并
modex_combined <- inner_join(
  modex_data %>% filter(Gender == "Men") %>% 
    select(Lipids, Label, Men_Value = MOD_EX),
  modex_data %>% filter(Gender == "Women") %>% 
    select(Lipids, Women_Value = MOD_EX),
  by = "Lipids"
)

#plot!!
# 1. 设定坐标范围
min_val <- floor(min(c(modex_combined$Men_Value, modex_combined$Women_Value), na.rm = TRUE))
max_val <- ceiling(max(c(modex_combined$Men_Value, modex_combined$Women_Value), na.rm = TRUE))

# 2. 构造 polygon 区域数据（上三角 & 下三角）
female_dominant_area <- data.frame(
  x = c(min_val, min_val, max_val),
  y = c(min_val, max_val, max_val)
)

male_dominant_area <- data.frame(
  x = c(min_val, max_val, max_val),
  y = c(min_val, min_val, max_val)
)

# 3. 画图
# 1. 选出每个 subclass 中差异最大的前三个脂质
top3_lipids <- modex_combined %>%
  mutate(diff = abs(Women_Value - Men_Value)) %>%
  group_by(Label) %>%
  slice_max(order_by = diff, n = 3, with_ties = FALSE) %>%
  ungroup()

# 2. 修改绘图代码，应用你给出的范围并只标注 top3
p <- ggplot(modex_combined, aes(x = Men_Value, y = Women_Value, color = Label)) +
  # 女性 dominant 阴影
  geom_polygon(data = female_dominant_area, aes(x = x, y = y),
               fill = "plum", alpha = 0.15, inherit.aes = FALSE) +
  # 男性 dominant 阴影
  geom_polygon(data = male_dominant_area, aes(x = x, y = y),
               fill = "lightblue", alpha = 0.15, inherit.aes = FALSE) +
  
  # 对角线
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "black", size = 0.8) +
  
  # 散点
  geom_point(size = 4) +
  
  # 只标注 top3 的脂质
  geom_text_repel(data = top3_lipids,
                  aes(label = Lipids),
                  size = 3, max.overlaps = 100, show.legend = FALSE) +
  
  # 参考线
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  
  # 标注区域说明
  annotate("text", x = -0.25, y = 1, label = "Women dominant", color = "mediumpurple2", size = 5, fontface = "bold") +
  annotate("text", x = 0.7, y = -0.5, label = "Men dominant", color = "skyblue", size = 5, fontface = "bold") +
  
  # 主题与调色
  coord_equal(xlim = c(-0.75, 1.25), ylim = c(-0.75, 1.25)) +
  theme_minimal() +
  scale_color_manual(values = c(
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
    "DG" = "plum4",
    "LPG" = "grey66"
  )) +
  labs(
    x = "Men fold change",
    y = "Women fold change",
    color = "subclass"
  ) +
  theme(legend.position = "right")
p

###export
png(
  filename = "results/HALFMIN/scatter_mod_male vs female.png",
  type = "cairo", # 抗锯齿
  res = 300, # 300ppi 分辨率
  width = 2000, height = 2300,
  bg = "transparent" # 透明背景
)
print(p)
dev.off()
