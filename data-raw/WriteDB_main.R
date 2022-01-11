# 此脚本用于读取数据
# 并将数据写入到数据库
# 从数据中提取所有的可能的搜索候选词
library(fs)
library(DBI)
library(glue)
library(do)
library(data.table)
library(reshape2)
library(RSQLite)
library(parallel)

##设置并行核心数
cores = 50
# 设置原始数据路径
sqldata_path <- "./sqlite"

## 注释信息表 -----
## 包含注释信息的表
## 文件筛选
files <- fs::dir_ls(path = glue("{sqldata_path}/metaFiles-batch2"))
files_full_qc <- files[grep(".*full_QC.txt",files)]
## 文件读取
fullqc_ls <- lapply(files_full_qc, function(x){
  read.delim(x)
})

## 合并fullqc_ls
full_QC <- do.call(rbind,fullqc_ls)

## gsm.id.list 文件
## 筛选出有用的注释信息
gsm.id.list <- read.table(file = glue("{sqldata_path}/metaFiles-batch2/gsm.id.list.txt"), quote= "\"", comment.char="")

length(unique(full_QC$DCid));length(unique(full_QC$GSMID));length(unique(gsm.id.list$V1))

## 以gsm.id.list为准
full_QC_f <- full_QC[full_QC$GSMID %in% gsm.id.list$V1,]

## 提取和保存索引字段的所有可能的搜索关键词
tmp0 <- lapply(c("Cell_line", "Cell_type", "Tissue_type"), function(x){
  eval(parse(text = glue("{x}_pull <- unique(full_QC_f${x});save({x}_pull,file = '{x}_pull.Rdata')")))
})

## 染色体位置信息表 ----
## 获取文件列表
files <- fs::dir_ls(sqldata_path,recurse = T)

## 获取染色体数据文件
chrloc_files <- files[grep("chr",files)]

chrdata_l <- mclapply(chrloc_files, function(x){

  ## 读取文件,并添加字段
  chrdata <- data.table::fread(x, header=FALSE,col.names = c("DCid", "Start", "Length", "GeneSymbol"))

}, mc.cores = getOption("mc.cores", cores))

## 清理表名
names(chrdata_l) <- basename(names(chrdata_l))

## 染色体位置信息表合集----
chr_table <- colsplit(names(chrdata_l),"_",c("Species", "IP", "Chr","Portion"))
chr_table[,"table"] <- names(chrdata_l)
## 规范Species信息
chr_table[,"Species"] <- Replace(chr_table[,"Species"],pattern = c("human:Homo sapiens","mouse:Mus musculus"))

## 提取和保存索引字段的所有可能的搜索关键词

tmp0 <- lapply(c("Species", "IP", "Chr","Portion"), function(x){
  eval(parse(text = glue("{x}_pull <- unique(chr_table${x});save({x}_pull,file = '{x}_pull.Rdata')")))
})

## 写入数据库----

## 链接数据库
tfmapperdb <- dbConnect(SQLite(), "TFmapperDB.sqlite")

## 染色体位置信息表合集----
dbWriteTable(tfmapperdb, SQL(get_names(chr_table)), chr_table, append = TRUE)

## 构建索引
mclapply(c("Species", "IP","Portion"), function(x){

  dbExecute(tfmapperdb, glue("CREATE INDEX IF NOT EXISTS {x}_chr_table ON chr_table ({x});"))

}, mc.cores = getOption("mc.cores", cores))

## 位置和基因名----
tmp <- lapply(seq_along(chrdata_l), function(x){

  ## 写入数据库
  dbWriteTable(tfmapperdb, SQL(names(chrdata_l)[[x]]), chrdata_l[[x]], append = TRUE)

})

## 构建索引
lapply(seq_along(chrdata_l), function(x){

  # dbExecute(tfmapperdb, glue("CREATE INDEX IF NOT EXISTS DCid ON {names(chrdata_l)[[x]]} (DCid);"))
  dbExecute(tfmapperdb, glue("CREATE INDEX IF NOT EXISTS Start_chrdata_{names(chrdata_l)[[x]]} ON {names(chrdata_l)[[x]]} (Start);"))
  dbExecute(tfmapperdb, glue("CREATE INDEX IF NOT EXISTS Length_{names(chrdata_l)[[x]]} ON {names(chrdata_l)[[x]]} (Length);"))
  dbExecute(tfmapperdb, glue("CREATE INDEX IF NOT EXISTS GeneSymbol_{names(chrdata_l)[[x]]} ON {names(chrdata_l)[[x]]} (GeneSymbol);"))

})

## 注释信息表 ----
## 写入数据库
dbWriteTable(tfmapperdb, SQL("ann_info"), full_QC_f, append = TRUE)

## 构建索引
#   - Cell_line # 细胞系
#   - Cell_type # 细胞类型
#   - Tissue_type # 组织类型

lapply(c("Cell_line","Cell_type","Tissue_type"), function(x){

  dbExecute(tfmapperdb, glue("CREATE INDEX IF NOT EXISTS {x} ON ann_info ({x});"))

})



## 数据库概览
dbListTables(tfmapperdb)

## 断开数据库
dbDisconnect(tfmapperdb)
