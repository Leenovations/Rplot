library(gplots)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(ggbiplot)
library(ggrepel)
#------------------------------------------------------------------------#
Data <- read.table('', header=T, sep='\t')
Data <- Data[,c(-1,-2,-3)]
Data <- Data[complete.cases(Data),]

pca_res <- prcomp(t(Data))

samp_colors <- c("tan1", "coral", "lightskyblue") 
names(samp_colors) <- c('CR', 'Normal', 'NR')

samp_data <- data.frame(
  "GROUP" = rep(c('CR', 'NR', 'Normal'),c(12, 12, 18)),
  row.names = colnames(Data))

PCA <- ggbiplot(pca_res, ellipse = T, 
                var.axes = F, obs.scale = 1, var.scale = 1) +
  geom_point(aes(fill = samp_data$GROUP), size = 5, shape=21, stroke=1) +
  scale_color_manual(values = c("tan1", "coral", "lightskyblue"), 
                     labels = c('CR', 'Normal', 'NR')) +
  scale_fill_manual(values = c("tan1", "coral", "lightskyblue"), 
                    labels = c('CR', 'Normal', 'NR')) +
  ggtitle(paste0("PCA of ", 'DMR')) +
  # coord_fixed(ratio = 1) +
  theme_test() +
  theme(axis.title = element_text(size = 20),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        plot.title = element_text(size = 22, hjust = 0.5),
        legend.text = element_text(size = 16),
        legend.title = element_blank(),
        legend.position = "top")

ggsave(PCA, file='.pdf'))
