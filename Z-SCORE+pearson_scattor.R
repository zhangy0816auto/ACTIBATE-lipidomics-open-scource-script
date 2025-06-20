
library(corrplot)
library(Hmisc)
library(ggplot2)
library(openxlsx)
library(circlize)
library(psych)
library(ggplot2)
df <- read.xlsx("80_coherence/ACTIBATE_raw data+log2 - 80%coherence.xlsx",sheet = 10, colNames = TRUE, rowNames = T)
### To increase the number of comparisons, by default R has this defect###
options(max.print=999999)
##z-score
df_zscore <- cbind(df[, 1:2], scale(df[, 3:797]))
write.xlsx(df_zscore,"80_coherence/results/80adherence_zscore.xlsx",dec = ".",colNames = TRUE, rowNames = TRUE)
####scatter plot
library(ggplot2)
df <- read.xlsx("80_coherence/results/80adherence_zscore.xlsx",sheet = 2, colNames = TRUE, rowNames = T)
#simple version
ggplot(data=df, aes(x=z_score, y=Δ_VO2max_relative)) + geom_point()
#simple version + lm + cl
  ##calcualte p-value and R with rcorr
res <- rcorr(df$z_score, df$Δ_VO2max_relative)
p_value <- res$P[1,2]
cor_value <- round(res$r[1,2], 2)

ggplot(data=df, aes(x=z_score, y=Δ_Time_to_exhaustion))+
      geom_point()+stat_smooth(method="lm")+
      annotate(geom = "text",x=1,y=-10,label=expression(italic(r)~"="~0.37~","~italic(P)~"="~0.0517),size = 6)+
      labs(x="z_score",y = "ΔTime to exhaustion(s)")+
      theme(axis.title.x=element_text(vjust=1,  
                                  size=18),  # X axis title
            axis.title.y=element_text(size=18))  # Y axis title
###export
png(
  filename = "80_coherence/results/z-score&Δ_VO2max_relative_r.png",
  type = "cairo", # 抗锯齿
  res = 300, # 300ppi 分辨率
  width = 2000, height = 1600,
  bg = "transparent" # 透明背景
)
ggplot(data=df, aes(x=z_score, y=Δ_VO2max_relative))+
  geom_point()+stat_smooth(method="lm")+
  annotate(geom = "text",x=0.5,y=-10,label=expression(italic(r)~"="~0.32~","~italic(P)~"="~0.152),size = 4)+
  labs(x="z_score",y = "VO2peak(ml/kg/min)")+
  theme(axis.title.x=element_text(vjust=1,  
                                  size=12),  # X axis title
        axis.title.y=element_text(size=12))  # Y axis title
dev.off()


png(
  filename = "80_coherence/results/z-score&Δ_Time_to_exhaustion_r.png",
  type = "cairo", # 抗锯齿
  res = 300, # 300ppi 分辨率
  width = 2000, height = 1600,
  bg = "transparent" # 透明背景
)
ggplot(data=df, aes(x=z_score, y=Δ_Time_to_exhaustion))+
  geom_point()+stat_smooth(method="lm")+
  annotate(geom = "text",x=0.5,y=-10,label=expression(italic(r)~"="~0.37~","~italic(P)~"="~0.099),size = 4)+
  labs(x="z_score",y = "ΔTime to exhaustion(s)")+
  theme(axis.title.x=element_text(vjust=1,  
                                  size=12),  # X axis title
        axis.title.y=element_text(size=12))  # Y axis title
dev.off()
VO2peak(ml/kg/min)
#t-test
library(matrixTests)
library(stats)
df <- read.xlsx("data/ACTIBATE_Z-score_moderate_newdata.xlsx",sheet = 8, colNames = TRUE, rowNames = TRUE)
ResponderMat <- df[df[,2]=="1",-c(1,2,3)]
NonResponderMat  <- df[df[,2]=="0",-c(1,2,3)]
result <- col_t_welch(ResponderMat, NonResponderMat)

# ANOVA
library(stats)
df <- read.xlsx("data/ACTIBATE_Z-score_moderate_newdata.xlsx",sheet = 8, colNames = TRUE, rowNames = TRUE)
str(df$VO2_responders_4cat)
df$VO2_responders_4cat <- as.factor(df$VO2_responders_4cat)
aov.manu <- aov(z_score ~ VO2_responders_4cat, data=df)
summary(aov.manu)

ggplot(data = df, mapping = aes(
  x = VO2_responders_4cat, y = z_score)) +
  geom_boxplot()

Responder_VO2max_rel




## partial correlation account for Sex
install.packages("ppcor")
library(dplyr)      # for data manipulation
library(ppcor)      # for partial correlations
# Load your data
df <- read.xlsx("80_coherence/results/80adherence_zscore.xlsx", sheet = 2, colNames = TRUE, rowNames = TRUE)

# Ensure numeric columns, and keep Sex as a covariate
df <- df %>%
  mutate_at(vars(1:5), as.numeric)  # Convert columns to numeric, assuming Sex column is excluded here

pcor.test(df[,3],df[,4],df[,1],method = c("pearson"))

pcor.test(df[,3],df[,5],df[,1],method = c("pearson"))

