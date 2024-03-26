library(ggplot2)
library(scales)
library(svglite)
#Panther Output
tbl = read.table('Dopamine_control_vs_KO_control_UP_GO__Ratio_0.2_Cut_FDR_top10.txt',sep = '\t',header = TRUE)
tbl.clean = tbl[,c(1,3,4)]
colnames(tbl.clean) = c('Pathway', 'Input', 'FDR')
tbl.clean = tbl.clean[order(tbl.clean$FDR, decreasing = TRUE),]
tbl.clean$Pathway = c(gsub(pattern = "\\s[(].+[)]",replacement = "",tbl.clean$Pathway))
tbl.clean$Pathway = factor(tbl.clean$Pathway, levels = tbl.clean$Pathway)
#pdf(file = "Figure3_6_RatBrain_6Mo_vs_6Wo_GeneEnrichment_plot_v2.pdf", width = 3.3, height = 2)
p = ggplot(tbl.clean,aes(x = Pathway, y = -log(FDR, base = 10), width = 0.5)) +geom_bar(stat = 'identity',width = 1, color = 'black', lwd = 0.8,
                                                                                        fill = '#FFCC00') + 
  coord_flip()+
  theme_classic()+ scale_y_continuous(limits = c(0,4),expand = c(0,0)) +
  theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10), axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(), plot.title = element_blank(), axis.line = element_line(linewidth = 1, colour = 'black'), 
        axis.ticks.x = element_line(linewidth =  1, colour = 'black'), axis.ticks.y = element_line(linewidth =  1, colour = 'black'))+
  geom_text(aes(label = Input), position = position_dodge(width = 0.5), hjust = -0.5, size = 3)+
  ylab('-log10(FDR)')
  #scale_x_discrete(labels = wrap_format(20))

svglite(file = "240325_Dopamine_Ogt_KO_Down_KEGG.svg", width = 8, height = 6)
p
dev.off()
#gProfiler KEGG
tbl = read.csv("gProfiler_mmusculus_11-3-2023_1-53-51 PM__intersections_version_e110_eg57_p18_4b54a898.csv")
tbl.clean = tbl[c(1:9,11),c(2,9,5)]
