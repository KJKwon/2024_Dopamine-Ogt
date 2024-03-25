library(edgeR)
tbl <- read.table('230918_KJI_Dopamine_STAR.M33_GRCm39_primary.RNAseq_GeneName.txt', 
                  row.names =1 , header = TRUE, sep = '\t')
tbl.selected <- tbl[,c(2,4,6,8,10)]
tbl.selected.cpm <- cpm(tbl.selected)
keep <- apply(tbl.selected.cpm, 1, function(x) sum(x == 0) == 0)
tbl.clean.cpm <- tbl.selected.cpm[keep,]
tbl.DEG <- read.table('Dopamine_control_vs_KO_total.txt', row.names = 1, header = TRUE, sep = '\t')
tbl.DEG.clean <- tbl.DEG[,c(2,6)]
tbl.DEG.clean$Note <- NA
tbl.DEG.clean$Note <- ifelse(tbl.DEG.clean$log2FoldChange > 1 & tbl.DEG.clean$padj < 0.05, 'Control UP',
                             tbl.DEG.clean$Note)
tbl.DEG.clean$Note <- ifelse(tbl.DEG.clean$log2FoldChange < -1 & tbl.DEG.clean$padj < 0.05, 'KO UP',
                             tbl.DEG.clean$Note)
tbl.clean.cpm <- as.data.frame(tbl.clean.cpm)
tbl.clean.cpm$Average <- as.vector(apply(tbl.clean.cpm,1,mean))
tbl.total <- merge(tbl.clean.cpm,tbl.DEG.clean, by = 0, all.x = TRUE)
write.table(tbl.total, '230918_KJI_Dopamine_STAR.M33_GRCm39_primary.RNAseq_result.txt', 
            row.names = FALSE, col.names = TRUE, sep = '\t', quote = FALSE)









