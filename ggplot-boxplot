library(ggpubr)
library(rstatix)
#------------------------------------------------#
Date <- Sys.Date()
Date <- format(today, "%y%m%d")
#------------------------------------------------#
for (gene in Gene){
  Sub <- subset(Total, Gene==gene)
  
  bxp <- ggplot(Sub, aes(x=Group, y=Methylation, color=Group)) + 
    geom_boxplot(lwd = 0.6, width = 0.5, outlier.shape = NA) +
    scale_color_manual(values=c("tan1", "coral", "lightskyblue")) +
    geom_jitter(width=0.15, alpha=0.5) +
    theme_classic() +
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
  ggsave(paste0(Path, Date,'.', gene, '.Plot.pdf'))
}
