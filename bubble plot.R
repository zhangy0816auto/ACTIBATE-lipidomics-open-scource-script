
rm(list=ls())
install.packages("readx1")
install.packages("hrbrthemes")
library(ggplot2)
library(readxl)
library(dplyr)
library(plotly)
library(viridis)
library(hrbrthemes)
library(writexl)
library(ComplexHeatmap)

#---------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------
#80adhe 
df_bubbleplot_80adhe <- read.xlsx("80_coherence/results/anova-filtered_LIPIDS_log2fc_NOadhe.xlsx",sheet = 2,colNames = T)
color_1 <- c("#ffe3ff","#650a6d")
# x+y axis
p <- ggplot(df_bubbleplot_80adhe,aes(x=Number_unsaturations,y=Number_carbons)) +
  labs(x="Number of unsaturations",y="Number of carbons") 
# label+gradient color
p <- p + geom_point(aes(size=count, color= -log(pvalue))) +
  scale_size_continuous(breaks = c(1, 2, 3),range = c(5, 10), guide = guide_legend(title = "Count"))+
  scale_color_gradient(low = color_1[1],high=color_1[2],name="negLog_pvalue")
# Initialization parameters
top="top"
bottom="bottom"
left="left"
right="right"
none="none"
legend_pos_par=right
# border and background
p <- p + theme(legend.position = legend_pos_par)
p <- p + theme(panel.grid=element_blank(),
               panel.border = element_blank(),
               legend.background = element_blank(),
               panel.background = element_rect(fill = "white", colour = NA),
               plot.background = element_rect(fill = "white", colour = NA),
               axis.text.x = element_text(size = 14), 
               axis.text.y = element_text(size = 14),
               axis.title.x = element_text(size = 14),
               axis.title.y = element_text(size = 14),
               axis.line.x = element_line(linetype = "solid", linewidth = 0.4, colour = "black"),
               axis.line.y = element_line(linetype = "solid", linewidth = 0.4, colour = "black"),
               axis.ticks=element_line(size=0.2))
p
#####extraction!!
png(
  filename = "80_coherence/results/ACTIBATE_TG_bubble_noadhe.png",
  type = "cairo", # 抗锯齿
  res = 300, # 300ppi 分辨率
  width = 2400, height = 2000,
  bg = "transparent" # 透明背景
)
print(p)
dev.off()


#heatmap
##TG carbon number+double bond
df_TGheatmap_80adhe <- read.xlsx("80_coherence/results/anova-filtered_LIPIDS_log2fc_NOadhe.xlsx",sheet = 3, colNames = TRUE, rowNames = TRUE)
col_fun = colorRamp2(c(-0.5, 0, 1), c("lightskyblue", "white", "red"))
col_fun(seq(-5, 5))
png(
  filename = "80_coherence/results/ACTIBATE_TG_heatmap_noadhe.png",
  type = "cairo", # 抗锯齿
  res = 300, # 300ppi 分辨率
  width = 2000, height = 2000,
  bg = "transparent" # 透明背景
)

Heatmap(df_TGheatmap_80adhe,
        names <- "1",
        cluster_rows = FALSE,
        cluster_columns = FALSE,
        row_names_side = "left",
        column_title = "Number of unsaturations", 
        row_title = "Number of carbons",
        column_title_side = "bottom",
        #rect_gp = gpar(col = "white", lwd = 2),
        heatmap_legend_param = list(title = "log2FC", at = c(-0.5,0,0.5,1)),
        width = unit(5, "in"),  # Adjust the width as needed
        height = unit(5, "in"),
        col = col_fun,
        column_names_rot = 0,
)
decorate_heatmap_body("1", {
  
  grid.rect(gp = gpar(fill = "transparent", col = "black", lwd = 2))
  
})
dev.off()

