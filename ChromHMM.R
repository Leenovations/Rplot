library(ggplot2)
library(gplots)
library(dplyr)
#------------------------------------------------------------------#
setwd('/labmed/02.AML/08.GSE163522/02.Methylkit')
Data <- read.table('CR_NR_anno.tsv', sep='\t', header = T)
Data$Region <- gsub("Intron_\\d+", "Intron", Data$Region)
Data$Region <- gsub("Exon_\\d+", "Exon", Data$Region)
Data$meth.diff <- Data$meth.diff / 100
#------------------------------------------------------------------#
bxp <- ggplot(Data, aes(x=Region, y=meth.diff)) + 
  geom_boxplot(lwd = 0.6, width = 0.85, outlier.shape = NA,
               aes(fill=Region)) +
  scale_fill_manual(values=colorpanel(length(unique(Data$Region)), "white", "red")) + 
  ggtitle('NR / CR') +
  theme_bw() +
  ylim(-0.25 ,0.25) + 
  # geom_hline(yintercept = 0, linetype = "solid", color = "purple3", size = 1.5) + 
  theme(axis.title = element_text(size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 15, hjust = 0.5, face='bold'),
        axis.text.x = element_text(size=13, angle=90, vjust = 0.5, hjust = 0.9),
        legend.text = element_text(size = 10),
        legend.position = 'none') +
  xlab('') +
  ylab('Delta Methylation')
#------------------------------------------------------------------#
ggsave(bxp, file='CR.NR.pdf')
