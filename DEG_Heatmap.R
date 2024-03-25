library(circlize)
library(ComplexHeatmap)
library(edgeR)
library(dendextend)
tbl = read.table('230918_KJI_Dopamine_STAR.M33_GRCm39_primary.RNAseq_result.txt', header = TRUE, sep = '\t')
tbl.target <- read.table('Heatmap_candidate_genelist.txt')
target.list <- tbl.target$V1
tbl.selected = tbl[tbl$Row.names %in% target.list,]
rownames(tbl.selected) = tbl.selected$Row.names
tbl.selected = tbl.selected[,c(-1)]
tbl.selected = tbl.selected[,c(1:5)]
tbl.selected.sc = apply(tbl.selected, 1, function(x){(x-mean(x))/sd(x)})
tbl.selected.sc = t(tbl.selected.sc)
col_fun = colorRamp2(c(-1.5,0,1.5), c("blue","white","red")) 
ht <- Heatmap(as.matrix(tbl.selected.sc), row_order = rownames(tbl.selected.sc), 
        #column_order = colnames(tbl.selected), 
        cluster_columns = TRUE,
        cluster_rows = TRUE,
        col = col_fun,
        heatmap_width = unit(8, "cm"),heatmap_height = unit(20,"cm"), show_column_names = TRUE,  
          heatmap_legend_param = list(grid_height = unit(8,"mm"),title = "z-score",
        title_gp = gpar(fontsize = 10), title_position = c("leftcenter-rot"), legend_height = unit(3,"cm")))


ht <- draw(ht)
col_dend <- rev(column_dend(ht))
