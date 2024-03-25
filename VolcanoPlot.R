library(ggplot2)
library(ggrepel)
tbl = read.table('Dopamine_control_vs_KO_total.txt', sep = '\t', header = TRUE)
tbl <- tbl[,c(2,6)]
tbl <- cbind(rownames(tbl), tbl)
tbl$log2FoldChange <- tbl$log2FoldChange * -1
colnames(tbl) <- c('GeneName','log2FoldChange','padj')
cut_FC = 1.00
cut_FDR = 0.05
down.dopamine = c('Aldh1a7','Aldh1a1','Ddc','Slc6a3','Chrnb3','Th','Chrna6','Slc18a2','Slc10a4','Ntsr1',
                'Kcnn3','Sncg','Sv2c','Ret','Satb1','Snca','Drd2','Cadps2')
tbl_ready = tbl
tbl_ready$group = 'non_dopamine'
tbl_ready$group[tbl_ready$GeneName %in% down.dopamine] = 'down.dopamine'
tbl_ready <- tbl_ready[!is.na(tbl_ready$padj),]
colour = ifelse(tbl_ready$padj >= 0.05 | abs(tbl_ready$log2FoldChange) < 1, '#CCCCCC', 
                ifelse(tbl_ready$log2FoldChange > 1, '#FF3300', '#0066FF'))
tbl_ready = cbind(tbl_ready, colour)
tbl_ready_ordered = tbl_ready[order(tbl_ready$padj),]
tbl_ready_ordered$GeneName = factor(tbl_ready_ordered$GeneName, levels = tbl_ready_ordered$GeneName) 
tbl_ready_ordered$colour = factor(tbl_ready_ordered$colour, levels = c('#FF3300','#CCCCCC','#0066FF'))

p = ggplot(tbl_ready_ordered, aes(x = log2FoldChange, y = -log10(padj), color = colour,label = GeneName)) + geom_point(size = 1) + 
  scale_color_manual(values = c('#FF0000','#CCCCCC', '#0066FF'), labels = c('Up','No significance','Down')) +
  theme_linedraw() + theme(legend.position = 'top', panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           legend.title = element_blank())+
  xlim(-5,5) + geom_vline(xintercept = c(1,-1), linetype = "twodash", linewidth = 0.3) + 
  geom_hline(yintercept = -log10(0.05), linetype = "twodash", linewidth = 0.3) +
  geom_text_repel(data = subset(tbl_ready_ordered, group == 'down.dopamine'), colour = 'black',
                  nudge_x = c(0.1,0.1,0.1,0.1,0.1,0.1,0,0.2,-1,1.1,1,-1,-1,1,1,-1,-1,1),
                  nudge_y = c(0,0,0,0,0,0,8,8,-1,2,2,4,3,5,-2,1,0,2),
                  fontface = 'bold') 
  
#W : 730, H : 650 
p

