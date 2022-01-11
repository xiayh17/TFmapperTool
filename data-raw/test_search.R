library(DBI)
library(pool)
library(RSQLite)
## 链接数据库
tfmapperdb <- dbPool(RSQLite::SQLite(), dbname = "data-raw/TFmapperDB.sqlite")

# DBI::dbListTables(tfmapperdb)[1:3]
#
# dt2 <- sqlQuery(channel=tfmapperdb, query = "select TOP 100 * from gene_info")
#
# result <- {if(!is.null(keyword))
#   dplyr::tbl(pool, dbplyr::sql(paste0("SELECT * FROM data WHERE (`gene_name` IN ('",paste0(keyword,collapse="','"),"'))")))
#   else dplyr::tbl(pool,"data")} %>%
#   dplyr::collect()

species_key <- "Homo sapiens"

ip_key <- "factor"

portion_key <- "Exon"

# query_species <-  glue("SELECT * FROM gene_info WHERE (`Species` IN ('",paste0(species_key,collapse="','"),"'))")
#
# if (species_key %in% Species_pull) {
#
#   reslut <- dplyr::tbl(tfmapperdb, dbplyr::sql(query_species)) %>%
#     dplyr::collect()
#
# }

## 这一步是为了基因查询的UI界面准备的
pre_query <- glue("SELECT `GeneSymbol`
                    FROM gene_info
                    WHERE (`Species` IN ('{species_key}'))
                    AND (`IP` IN ('{ip_key}'))
                    AND (`Portion` IN ('{portion_key}'))")

Gene_pull <- dplyr::tbl(tfmapperdb, dbplyr::sql(pre_query)) %>%
  dplyr::collect()

## 这一步初步筛选用于查询基因的677个子表
# length(DBI::dbListTables(tfmapperdb)) - 3
DBI::dbListTables(tfmapperdb)[1:3]
sub_query <- glue("SELECT *
                    FROM chr_table
                    WHERE (`Species` IN ('{species_key}'))
                    AND (`IP` IN ('{ip_key}'))
                    AND (`Portion` IN ('{portion_key}'))")

position_sub <- dplyr::tbl(tfmapperdb, dbplyr::sql(sub_query)) %>%
  dplyr::collect()

## 这一步根据上面初步得到的查询范围开始查询基因
## 基因的位置信息position_sub
## 和注释信息ann_info
## 运用多表查询和多表合并
gene_key <- "HPSE2"
position_sub <- unlist(position_sub["table"])
names(position_sub) <- position_sub
# gene_query <- paste0("SELECT * FROM ",position_sub, " WHERE (GeneSymbol"," IN ('",gene_key,"'))", collapse=" UNION ALL ")
#
# gene_pos <- dplyr::tbl(tfmapperdb, dbplyr::sql(gene_query)) %>%
#   dplyr::collect()

## 上面这个方法可以得到结果
## 但是不知道到底在哪张表里查到的，这个很重要，因为表名包含染色体信息
## 逐个查询或许会好一点
gene_pos_l <- lapply(position_sub, function(x){

  gene_query <- paste0("SELECT * FROM ",x, " WHERE (GeneSymbol"," IN ('",gene_key,"'))")
  print(gene_query)

  dplyr::tbl(tfmapperdb, dbplyr::sql(gene_query)) %>%
    dplyr::collect()

})

## 只保留非零行的数据框
## 并且添加染色体
gene_pos_l2 <- lapply(seq_along(gene_pos_l), function(x){

  infos <- names(gene_pos_l)
  ## 筛选并处理
  if(nrow(gene_pos_l[[x]]>0)){
    gene_pos_l[[x]][,"Chr"] <- str_split(infos[x],"_")[[1]][3]
    return(gene_pos_l[[x]])
  }

})

## 合并所有jieguo
gene_pos <- do.call(rbind,gene_pos_l2)


## 联合上一步的gene_pos 中的样品ID 获取注释信息
## 筛选生物信息
## 多列查询
cell_type_key <- "Embryonic Stem Cell"
cell_line_key <- "H9"
tissue_type_key <- "Embryo"
tmp_keys <- paste0(unique(unlist(gene_pos[,"DCid"])), collapse = ",")

anno_query <- glue("SELECT * FROM ann_info WHERE (DCid IN ({tmp_keys}))
                  AND (`Cell_line` IN ('{cell_line_key}'))
                  AND (`Cell_type` IN ('{cell_type_key}'))
                  AND (`Tissue_type` IN ('{tissue_type_key}'))")

gene_anno <- dplyr::tbl(tfmapperdb, dbplyr::sql(anno_query)) %>%
  dplyr::collect()

## 合并位置和注释信息
res <- merge(gene_anno,gene_pos,by="DCid")

## 这一步是为了坐标查询的UI准备的
## 以上根据基因查询，还要能够根据位置坐标查询，
## 运用聚合查询对表中坐标进行统计，为用户端输入提供参考
## 不同于基因查询，坐标信息包括chr start end

chr_key = "chr19"
start_key = 2950604
end_key = 2951604

## 根据染色体信息
## 子表要重新获取，因为之前gene是限制了portion的
## 这一步初步筛选用于查询基因的677个子表
# length(DBI::dbListTables(tfmapperdb)) - 3
DBI::dbListTables(tfmapperdb)[1:3]
chr_query <- glue("SELECT *
                    FROM chr_table
                    WHERE (`Species` IN ('{species_key}'))
                    AND (`Chr` IN ('{chr_key}'))
                    AND (`IP` IN ('{ip_key}'))")

chr_sub <- dplyr::tbl(tfmapperdb, dbplyr::sql(chr_query)) %>%
  dplyr::collect()

## 从这些表里面搜索符合条件的区域
## Start <= start_key
## Start + Length >=  end_key
# chr_tables <- paste0(unlist(chr_sub["table"]),collapse = ",")
chr_filter <- unlist(chr_sub["table"])
names(chr_filter) <- chr_filter
s_max_query <- paste0("SELECT MAX(`Start`) FROM ",unlist(chr_sub["table"]), collapse=" UNION ALL ")

start_max <- dplyr::tbl(tfmapperdb, dbplyr::sql(s_max_query)) %>%
  dplyr::collect() %>%
  max()

e_max_query <- paste0("SELECT MIN(`Start`+`Length`) FROM ",unlist(chr_sub["table"]), collapse=" UNION ALL ")

end_max <- dplyr::tbl(tfmapperdb, dbplyr::sql(e_max_query)) %>%
  dplyr::collect() %>%
  max()

coor_res_l <- lapply(chr_filter, function(x){

  coor_query <- paste0("SELECT * FROM ", x, " WHERE (Start >= ", start_key, ")  AND ((Start + Length) <= ", end_key, ")")

  dplyr::tbl(tfmapperdb, dbplyr::sql(coor_query)) %>%
    dplyr::collect()

})

## 只保留非零行的数据框
## 并且添加染色体
coor_res_l2 <- lapply(seq_along(coor_res_l), function(x){

  infos <- names(coor_res_l)
  ## 筛选并处理
  if(nrow(coor_res_l[[x]]>0)){
    coor_res_l[[x]][,"Chr"] <- str_split(infos[x],"_")[[1]][3]
    return(coor_res_l[[x]])
  }

})

## 合并所有jieguo
gene_coor <- do.call(rbind,coor_res_l2)

## 联合上一步的gene_coor 中的样品ID 获取注释信息
## 筛选生物信息
## 多列查询
cell_type_key <- "Embryonic Stem Cell"
cell_line_key <- "H9"
tissue_type_key <- "Embryo"
tmp_keys <- paste0(unique(unlist(gene_coor[,"DCid"])), collapse = ",")

anno_query <- glue("SELECT * FROM ann_info WHERE (DCid IN ({tmp_keys}))
                  AND (`Cell_line` IN ('{cell_line_key}'))
                  AND (`Cell_type` IN ('{cell_type_key}'))
                  AND (`Tissue_type` IN ('{tissue_type_key}'))")

gene_anno <- dplyr::tbl(tfmapperdb, dbplyr::sql(anno_query)) %>%
  dplyr::collect()

## 合并位置和注释信息
res2 <- merge(gene_anno,gene_coor,by="DCid")

poolClose(tfmapperdb)
