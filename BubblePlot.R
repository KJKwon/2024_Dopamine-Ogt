library(ggplot2)
tbl <- read.table('Dopamine_control_vs_KO_control_UP_GO__Ratio_0.2_Cut_FDR_top10.txt',sep = '\t', header = TRUE)
tbl <- tbl[,c(1,3,4)]
tbl.clean <- tbl[order(tbl$Client.Text.Box.Input..FDR.),]
tbl.clean$GeneRatio <- tbl.clean$Client.Text.Box.Input..88./88
colnames(tbl.clean) <- c('Pathway','GeneCount', 'FDR', 'GeneRatio')
tbl.clean$Pathway = c(gsub(pattern = "\\s[(].+[)]",replacement = "",tbl.clean$Pathway))
tbl.clean <- tbl.clean[order(tbl.clean$GeneRatio, -log10(tbl$Client.Text.Box.Input..FDR.)),]
tbl.clean$Pathway <- factor(tbl.clean$Pathway, levels = tbl.clean$Pathway)
p <- ggplot(tbl.clean, aes(x = GeneRatio, y = Pathway, size = GeneCount, fill = -log10(FDR))) +
  scale_size_continuous(range = c(4,16), breaks = c(5,10,15,20), limits = c(0,90)) + geom_point(shape = 21, color = "black") +
  scale_fill_viridis_c(option = "inferno", limits = c(0,6), 
                       breaks = c(0,2,4,6), labels = c(0,2,4,6)) +
  scale_x_continuous(limits = c(0.06,0.12)) +
  theme_bw(base_size = 12) +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
  guides(size = guide_legend(order = 1),
         colour = guide_legend(order = 2))

p  

ggsave('Dopamine_KO_Down-regulated_GO_BubblePlot_no_text.pdf', units = "mm", width = 100, height = 160)
