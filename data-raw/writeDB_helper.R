## 构建一个辅助表
## 可以根据用户的选择，提供候选基因
## Species IP Portion
library(fs)
library(kit)
library(DBI)
library(glue)
library(stringr)
library(RSQLite)
library(parallel)
library(data.table)

##设置并行核心数
cores = 50
# 设置原始数据路径
sqldata_path <- "./sqlite"

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

## 获取全部的基因
gene_l <- mclapply(seq_along(chrdata_l), function(x){

  gene_tmp <- chrdata_l[[x]][,"GeneSymbol"]
  gene <- data.table(gene_tmp[!kit::fduplicated(gene_tmp)])
  return(gene)

}, mc.cores = getOption("mc.cores", cores))

names(gene_l) <- names(chrdata_l)
infos <-  names(gene_l)
## 给基因一些信息 Species IP Portion
## 使用时根据用户的设置灵活变动Gene的候选信息
gene_l2 <- mclapply(seq_along(gene_l), function(x){

  gene_l[[x]][,"Species"] <- str_split(infos[x],"_")[[1]][1]
  gene_l[[x]][,"IP"] <- str_split(infos[x],"_")[[1]][2]
  gene_l[[x]][,"Portion"] <- str_split(infos[x],"_")[[1]][3]
  return(gene_l[[x]])

}, mc.cores = getOption("mc.cores", cores))

## 合并所有基因
geneDB <- do.call(rbind,gene_l2)

## 去重
geneDB <- geneDB[!kit::fduplicated(geneDB),]

## 规范Species信息
geneDB[, Species := as.character(Species)][Species == "human", Species := "Homo sapiens"]
geneDB[, Species := as.character(Species)][Species == "mouse", Species := "Mus musculus"]

## 写入数据库
## 链接数据库
tfmapperdb <- dbConnect(SQLite(), "data-raw/TFmapperDB.sqlite")

## 染色体位置信息表合集----
dbWriteTable(tfmapperdb, SQL("gene_info"), geneDB, append = F,overwrite = T)

## 构建索引
lapply(c("Species", "IP", "Portion"), function(x){

  dbExecute(tfmapperdb, glue("CREATE INDEX IF NOT EXISTS {x}_gene_info ON gene_info ({x});"))

})

## 数据库概览
dbListTables(tfmapperdb)

## 断开数据库
dbDisconnect(tfmapperdb)
