library(VennDiagram)
tbl.mid <- read.table('Dopamine_control_vs_KO_clean.txt', row.names = 1, header = TRUE, sep = '\t')
tbl.hippo <- read.table('Hippo_control_vs_KO_clean.txt', row.names =1, header = TRUE, sep = '\t')
intersect(rownames(tbl.mid)[tbl.mid$log2FoldChange > 0], rownames(tbl.hippo)[tbl.hippo$log2FoldChange > 0 ])
intersect(rownames(tbl.mid)[tbl.mid$log2FoldChange < 0], rownames(tbl.hippo)[tbl.hippo$log2FoldChange < 0 ])
n.mid.KO.down <- length(rownames(tbl.mid[tbl.mid$log2FoldChange > 0,]))
n.mid.KO.up <- length(rownames(tbl.mid[tbl.mid$log2FoldChange < 0,]))
n.hippo.KO.down <- length(rownames(tbl.hippo[tbl.hippo$log2FoldChange > 0,]))
n.hippo.KO.up <- length(rownames(tbl.hippo[tbl.hippo$log2FoldChange < 0,]))

draw.pairwise.venn(area1 = n.mid.KO.down, area2 = n.hippo.KO.down, cross.area = 3,
                   catergory = c('Mibrain','Hippocampus'),
                   lwd = 2,
                   col=c("#440154ff", '#21908dff'),
                   fill = c(alpha("#440154ff",0.3), alpha('#21908dff',0.3)),
                   cex = 0.5,
                   cat.cex = 0.3,
                   cat.default.pos = "outer")
