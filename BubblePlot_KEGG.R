library(ggplot2)
tbl <- read.csv('gProfiler_mmusculus_11-3-2023_1-53-51 PM__intersections_version_e110_eg57_p18_4b54a898.csv',
                header = TRUE)
tbl <- tbl[,c(2,6,9)]
tbl.clean <- tbl[order(tbl$intersection_size, tbl$negative_log10_of_adjusted_p_value),]
tbl.clean <- tbl.clean[6:length(tbl.clean$intersection_size)-1,]
tbl.clean$GeneRatio <- tbl.clean$intersection_size/101
colnames(tbl.clean) <- c('Pathway','adjp', 'GeneCount','GeneRatio')
tbl.clean$Pathway <- factor(tbl.clean$Pathway, levels = tbl.clean$Pathway)
p <- ggplot(tbl.clean, aes(x = GeneRatio, y = Pathway, size = GeneCount, fill = adjp)) +
  scale_size_continuous(range = c(4,16), breaks = c(5,10,15,20), limits = c(0,90)) + geom_point(shape = 21, color = "black") +
  scale_fill_viridis_c(option = "inferno", limits = c(0,4), 
                       breaks = c(0,1,2,3,4), labels = c(0,1,2,3,4)) +
  scale_x_continuous(limits = c(0.02,0.1)) +
  theme_bw(base_size = 12) +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
  guides(size = guide_legend(order = 1),
         colour = guide_legend(order = 2))

p  

#Edit without text
ggsave('Dopamine_KO_Down-regulated_KEGG_BubblePlotno_text.pdf', units = "mm", width = 100, height = 160)
