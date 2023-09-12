library(ggpubr)
library(rstatix)
library(ggplot2)
#------------------------------------------------#
Data <- read.table('',
                   sep='\t',
                   header=T)

Gene <- unique(Data$Gene)
#------------------------------------------------#
for (gene in Gene){
  Sub <- subset(Data, Gene==gene)
  
  bxp <- ggplot(Sub, aes(x=Group, y=Meth, color=Group)) + 
    geom_boxplot(lwd = 0.6, width = 0.5, outlier.shape = NA) +
    scale_color_manual(values=c("tan1", "coral", "lightskyblue")) +
    geom_jitter(width=0.15, alpha=0.5) +
    theme_classic() +
    scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
    theme(axis.title = element_text(size = 15),
          plot.title = element_text(size = 15, hjust = 0.5, face='italic'),
          legend.text = element_text(size = 10),
          legend.position = 'none') +
    stat_compare_means(comparisons = list(c('CR','Normal'),
                                          c('Normal','NR'),
                                          c('CR','NR')),
                       method="wilcox.test",
                       label = "p.signif",
                       tip.length = 0.01,
                       bracket.size = 0.01) +
    ggtitle(sprintf('%s', gene)) +
    xlab('') +
    ylab('Percent Methylation')
  ggsave(paste0(gene, '.boxplot.pdf'))
}
