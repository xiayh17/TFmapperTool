library(pool)

## 生物注释
## 除了坐标查询，其他可以接受ALL选项
cell_type_key <- "ALL"
cell_line_key <- "ALL"
tissue_type_key <- "ALL"

## 链接数据库
tfmapperdb <- dbPool(RSQLite::SQLite(), dbname = "data-raw/TFmapperDB.sqlite")

## 第一段 初步筛选6百多个表
## 在query_gene 和 query_coord 内部完成
## 用到的条件有
species_key <- species_pull[1]
ip_key <- ip_pull[1]

## 第二段 可用基因 或者 坐标搜索
## 根据用户的选择
## 1. 用户选择基因查询
## 基因线索
portion_key <- portion_pull[1]
gene_key <- "HPSE2"
## 根据 species ip portion 初步筛选出可选的基因
possible_gene <- gene_pull(tfmapperdb = tfmapperdb,
          species_key = species_key,
          ip_key = ip_key,
          portion_key = portion_key)

## 一下判断在UI界面不存在，因为用户只能选可能的基因
## 判断用户搜索的基因是否在可能的列表中
if (gene_key %in% possible_gene$GeneSymbol) {

  "start next search"

} else {

  "stop"

}
## 如果用户批量输入基因，判断有多少在，有多少不在

## 查询基因
gene_pos <- query_gene(tfmapperdb = tfmapperdb,
                       species_key = species_key,
                       ip_key = ip_key,
                       portion_key = portion_key,
                       gene_key = gene_key)


## 2. 用户选择坐标查询
## 坐标查询不接受多个选项
## 坐标线索
chr_key = "chr19"
start_key = 58348178
end_key = 58349178
## 对用户输入的坐标进行可行性检查
## 染色体是否在已知条件的合理范围
## 坐标是否在染色体的合理范围
possible_coord <-  coor_pull(tfmapperdb = tfmapperdb,
                      species_key = species_key,
                      ip_key = ip_key,
                      chr_key = chr_key)
## 查询坐标
gene_coord <- query_coord(tfmapperdb = tfmapperdb,
                          species_key = species_key,
                          chr_key = chr_key,
                          ip_key = ip_key,
                          start_key = start_key,
                          end_key = end_key)

## 根据上述结果查询可用生物类型
possible_bio <- query_bio(tfmapperdb = tfmapperdb,
          gene_pos)

## 根据可用生物类型进一步查询
bio <- c("Cell_line","Cell_type","Tissue_type")

cell_line_pull <- possible_bio$Cell_line
cell_type_pull <- possible_bio$Cell_type
tissue_type_pull <- possible_bio$Tissue_type

res_gene <-  query_anno(tfmapperdb = tfmapperdb,
                        gene_pos,
                        cell_line_key = cell_line_key,
                        cell_type_key = cell_type_key,
                        tissue_type_key = tissue_type_key)

res_coord <- query_anno(tfmapperdb = tfmapperdb,
                        gene_coord,
                        cell_line_key = cell_line_key,
                        cell_type_key = cell_type_key,
                        tissue_type_key = tissue_type_key)

res <- merge_query(gene_data = gene_pos,anno_info = res_gene)

save(res,file = "res.Rdata")
