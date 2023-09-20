library(ComplexHeatmap)
library(readxl)

Annotation <- read.table('~/Desktop/0825.BPDCN.Involve.Site.txt', 
                         sep = '\t',
                         header=T,
                         row.names = 1)

ha = HeatmapAnnotation(Involvement = Annotation$Involvement,
                       Age = Annotation$Age,
                       Sex = Annotation$Sex,
                       Induction=Annotation$Induction,
                       Contating=Annotation$Contating,
                       Transplantation=Annotation$Transplantation,
                       Survival=Annotation$Survival,
                       col = list(Involvement = c("Multiple skin ± systemic" = "springgreen4",
                                                  "Systemic without skin" = "springgreen3",
                                                  "Single skin" = "springgreen1"),
                                  Age = c('<60 years'='gold', '>=60 years'='lightgoldenrod1'),
                                  Sex = c('M'='steelblue1', 'F'='skyblue1'),
                                  Induction = c('AML-like chemotherapy'='slateblue4', 'ALL-like chemotherapy'='slateblue3', 'Lymphoma-like chemotherapy'='mediumpurple1'),
                                  Contating = c('Yes'='royalblue4', 'No'='gray80'),
                                  Transplantation = c('Allo-SCT'='indianred3', 'None'='gray80', 'Salvage auto-SCT'='lightpink', 'Salvage Allo-SCT'='hotpink1'),
                                  Survival = c('>=24 months'='violetred1', '<24 months'='plum2')),
                       annotation_height = unit(c(5, 5, 100), "mm"),
                       annotation_name_gp= gpar(fontsize = 8))

Onco.Data <- read_excel('~/Desktop/230919.BPDCN.Oncoprint.xlsx',
                        sheet=1)
Onco.Data <- as.data.frame(Onco.Data)
Onco.Data[is.na(Onco.Data)] <- ''
rownames(Onco.Data) <- Onco.Data[,1]
Onco.Data <- Onco.Data[,-1]

#color지정
col.Onco = c("Missense" = "#008000", "Truncating" = "red", "Splicing" = "orange", "CNV" = "navy")

alter_fun = list(
  background = function(x, y, w, h) {
    grid.rect(x, y, w-unit(0.948, "pt"), h-unit(0.948, "pt"), 
              gp = gpar(fill = "#CCCCCC", col = NA))
  },
  # big green
  Missense = function(x, y, w, h) {
    grid.rect(x, y, w-unit(0.948, "pt"), h-unit(0.948, "pt"), 
              gp = gpar(fill = col["Missense"], col = NA))
  },
  # big red
  Truncating = function(x, y, w, h) {
    grid.rect(x, y, w-unit(0.93, "pt"), h*0.33, 
              gp = gpar(fill = col["Truncating"], col = NA))
  },
  # big orange
  Splicing = function(x, y, w, h) {
    grid.rect(x, y, w-unit(0.948, "pt"), h-unit(0.948, "pt"), 
              gp = gpar(fill = col["Splicing"], col = NA))
  },
  # big navy
  CNV = function(x, y, w, h) {
    grid.rect(x, y, w-unit*0.33, h-unit(0.948, "pt"), 
              gp = gpar(fill = col["CNV"], col = NA))
  }
)

alter_fun = list(
  background = alter_graphic("rect", fill = "#CCCCCC"),
  Missense = alter_graphic("rect", fill = col.Onco["Missense"]),
  Truncating = alter_graphic("rect", height = 0.33, fill = col.Onco["Truncating"]),
  Splicing = alter_graphic("rect", fill = col.Onco["Splicing"]),
  CNV = alter_graphic("rect", width = 0.33, fill = col.Onco["CNV"])
)

column_title = "Altered in 13 of 13 BPDCN samples"
heatmap_legend_param = list(title = "Alternations", at = c("Missense", "Truncating", "Splicing", "CNV"), 
                            labels = c("Missense", "Truncating", "Splicing", "CNV"))

pdf('~/Desktop/BPDCN.Oncoprint.pdf', height=10)
a <- oncoPrint(Onco.Data[1:50,],
               alter_fun = alter_fun, col = col.Onco, 
               column_title = column_title, heatmap_legend_param = heatmap_legend_param,
               pct_side = "right", row_names_side = "left",
               alter_fun_is_vectorized = FALSE,
               bottom_annotation = ha,
               column_order = colnames(Onco.Data),
               row_names_gp = gpar(fontsize=7, fontface='italic'),
               pct_gp = gpar(fontsize=7),
               row_title_gp = gpar(fontsize=4))
draw(a, heatmap_legend_side = "bottom", annotation_legend_side = "bottom", merge_legend = TRUE,
     # row_split = rep(c("Cell proliferation", "DNA repair", "Chromatin remodeling", "Wnt pathway","Apoptosis"),c(9,10,10,5,5)),
     row_gap = unit(100, "mm"))
dev.off()
