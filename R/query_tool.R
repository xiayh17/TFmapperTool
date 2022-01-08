## 注释信息查询函数
#' Query annotation information of gene data from `query_gene` or `query_coord`
#'
#' @param tfmapperdb default `tfmapperdb`, SQLiteConnection name
#' @param gene_data result from query_gene or query_coord
#' @param cell_line_key default `cell_line_key`, keywords of Cell line
#' @param cell_type_key default `cell_type_key`, keywords of Cell type
#' @param tissue_type_key default `tissue_type_key`, keywords of tissue type
#'
#' @importFrom glue glue
#' @importFrom dplyr tbl collect
#' @importFrom dbplyr sql
#'
#' @return a data.frame contains annotation results
#' @export
#'
#' @examples
#' \dontrun{
#' query_anno(gene_pos)
#' }
query_anno <- function(tfmapperdb = tfmapperdb,
                       gene_data,
                       cell_line_key = cell_line_key,
                       cell_type_key = cell_type_key,
                       tissue_type_key = tissue_type_key) {

  tmp_keys <- paste0(unique(unlist(gene_data[,"DCid"])), collapse = ",")

  cell_line_key <- sql_key(cell_line_key)
  cell_type_key <- sql_key(cell_type_key)
  tissue_type_key <- sql_key(tissue_type_key)

  anno_query <- glue("SELECT * FROM ann_info WHERE (DCid IN ({tmp_keys}))
                  AND (`Cell_line` IN ({cell_line_key}))
                  AND (`Cell_type` IN ({cell_type_key}))
                  AND (`Tissue_type` IN ({tissue_type_key}))")



  gene_anno <- tbl(tfmapperdb, sql(anno_query)) %>%
    collect()
  return(gene_anno)
}

## 根据基因进行查询的函数
#' Query gene data according to gene name
#'
#' @param tfmapperdb default `tfmapperdb`, SQLiteConnection name
#' @param species_key default `species_key`, keywords of Species
#' @param ip_key default `ip_key`, keywords of IP
#' @param portion_key default `portion_key`, keywords of Portion
#' @param gene_key default `gene_key`, keywords of  Gene
#'
#' @importFrom glue glue
#' @importFrom dplyr tbl collect
#' @importFrom dbplyr sql
#' @importFrom stringr str_split
#'
#' @return data.frame tbl contains gene data
#' @export
#'
#' @examples
#' \dontrun{
#' query_gene()
#' }
query_gene <- function(tfmapperdb = tfmapperdb,
                       species_key = species_key,
                       ip_key = ip_key,
                       portion_key = portion_key,
                       gene_key = gene_key) {

  species_key <- sql_key(species_key)
  ip_key <- sql_key(ip_key)
  portion_key <- sql_key(portion_key)
  gene_key <- sql_key(gene_key)

  sub_query <- glue("SELECT *
                    FROM chr_table
                    WHERE (`Species` IN ({species_key}))
                    AND (`IP` IN ({ip_key}))
                    AND (`Portion` IN ({portion_key}))")

  position_sub <- tbl(tfmapperdb, sql(sub_query)) %>%
    collect()
  position_sub <- unlist(position_sub["table"])
  names(position_sub) <- position_sub

  ### 对子表逐一查询
  gene_pos_l <- lapply(position_sub, function(x){

    gene_query <- paste0("SELECT * FROM ",x, " WHERE (GeneSymbol"," IN (",gene_key,"))")

    tbl(tfmapperdb, sql(gene_query)) %>%
      collect()

  })

  gene_pos_l2 <- lapply(seq_along(gene_pos_l), function(x){

    infos <- names(gene_pos_l)
    ## 筛选并处理
    if(nrow(gene_pos_l[[x]]>0)){
      gene_pos_l[[x]][,"Species"] <- str_split(infos[x],"_")[[1]][1]
      gene_pos_l[[x]][,"IP"] <- str_split(infos[x],"_")[[1]][2]
      gene_pos_l[[x]][,"Chr"] <- str_split(infos[x],"_")[[1]][3]
      gene_pos_l[[x]][,"Portion"] <- str_split(infos[x],"_")[[1]][4]
      return(gene_pos_l[[x]])
    }

  })

  gene_pos <- do.call(rbind,gene_pos_l2)

  return(gene_pos)

}

## 根据位置进行查询的函数
#' Query gene data from coordination of Start and End in a Chr
#'
#' @param tfmapperdb default `tfmapperdb`, SQLiteConnection name
#' @param species_key default `species_key`, keywords of Species
#' @param ip_key default `ip_key`, keywords of IP
#' @param chr_key default `chr_key`, keywords of Chr
#' @param start_key default `start_key`, coordination of Start
#' @param end_key default `end_key`, coordination of End
#'
#' @importFrom glue glue
#' @importFrom dplyr tbl collect
#' @importFrom dbplyr sql
#' @importFrom stringr str_split
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' query_coord()
#' }
query_coord <- function(tfmapperdb = tfmapperdb,
                        species_key = species_key,
                        chr_key = chr_key,
                        ip_key = ip_key,
                        start_key = start_key,
                        end_key = end_key) {

  species_key <- sql_key(species_key)
  ip_key <- sql_key(ip_key)

  chr_query <- glue("SELECT *
                    FROM chr_table
                    WHERE (`Species` IN ({species_key}))
                    AND (`Chr` IN ('{chr_key}'))
                    AND (`IP` IN ({ip_key}))")

  chr_sub <- tbl(tfmapperdb, sql(chr_query)) %>%
    collect()

  chr_filter <- unlist(chr_sub["table"])
  names(chr_filter) <- chr_filter

  coor_res_l <- lapply(chr_filter, function(x){
    coor_query <- paste0("SELECT * FROM ", x, " WHERE (Start >= ", start_key, ")
                         AND ((Start + Length) <= ", end_key, ")")
    tbl(tfmapperdb, sql(coor_query)) %>%
      collect()
  })

  coor_res_l2 <- lapply(seq_along(coor_res_l), function(x){
    infos <- names(coor_res_l)
    if(nrow(coor_res_l[[x]]>0)){
      coor_res_l[[x]][,"Species"] <- str_split(infos[x],"_")[[1]][1]
      coor_res_l[[x]][,"IP"] <- str_split(infos[x],"_")[[1]][2]
      coor_res_l[[x]][,"Chr"] <- str_split(infos[x],"_")[[1]][3]
      coor_res_l[[x]][,"Portion"] <- str_split(infos[x],"_")[[1]][4]
      return(coor_res_l[[x]])
    }
  })

  gene_coor <- do.call(rbind,coor_res_l2)
  return(gene_coor)

}

## 合并基因和位置信息的函数
#' Merge gene data from `query_gene` or `query_coord` and annotation data from `query_anno`
#'
#' @param gene_data data from `query_gene` or `query_coord`
#' @param anno_info data from `query_anno`
#'
#' @return a data frame contains results of query
#' @export
#'
#' @examples
#' \dontrun{
#' merge_query(gene_pos,gene_anno)
#' }
merge_query <- function(gene_data,anno_info) {
  ## 合并位置和注释信息
  res <- merge(gene_data,anno_info,by="DCid",all = TRUE)
  ## 规范Species信息
  res[,"Species.x"][res[,"Species.x"] == "human"] = "Homo sapiens"
  res[,"Species.x"][res[,"Species.x"] == "mouse"] = "Mus musculus"
  res[,"Species"] = res[,"Species.x"]
  res = res[,!(names(res) %in% c("Species.x","Species.y"))]
  return(res)
}

## 辅助函数
### 查询已选条件下的可能基因
#' Query possible gene for user
#'
#' @param tfmapperdb default `tfmapperdb`, SQLiteConnection name
#' @param species_key default `species_key`, keywords of Species
#' @param ip_key default `ip_key`, keywords of IP
#' @param portion_key default `portion_key`, keywords of Portion
#'
#' @importFrom glue glue
#' @importFrom dplyr tbl collect
#' @importFrom dbplyr sql
#'
#' @return possible gene
#' @export
#'
#' @examples
#' \dontrun{
#' gene_pull()
#' }
gene_pull <- function(tfmapperdb = tfmapperdb,
                      species_key = species_key,
                      ip_key = ip_key,
                      portion_key = portion_key) {

  species_key <- sql_key(species_key)
  ip_key <- sql_key(ip_key)
  portion_key <- sql_key(portion_key)

  pre_query <- glue("SELECT `GeneSymbol`
                    FROM gene_info
                    WHERE (`Species` IN ({species_key}))
                    AND (`IP` IN ({ip_key}))
                    AND (`Portion` IN ({portion_key}))")

  Gene_pull <- tbl(tfmapperdb, sql(pre_query)) %>%
    collect()
  return(Gene_pull)
}

### 查询已知条件下的位置的极值
#' Query min value of Start and max value of End
#'
#' @param tfmapperdb default `tfmapperdb`, SQLiteConnection name
#' @param species_key default `species_key`, keywords of Species
#' @param ip_key default `ip_key`, keywords of IP
#' @param chr_key default `chr_key`, keywords of Chr
#'
#' @importFrom glue glue
#' @importFrom dplyr tbl collect
#' @importFrom dbplyr sql
#'
#' @return possible coordination
#' @export
#'
#' @examples
#' \dontrun{
#' coor_pull()
#' }
coor_pull <- function(tfmapperdb = tfmapperdb,
                      species_key = species_key,
                      ip_key = ip_key,
                      chr_key = chr_key) {

  species_key <- sql_key(species_key)
  ip_key <- sql_key(ip_key)

  chr_query <- glue("SELECT *
                    FROM chr_table
                    WHERE (`Species` IN ({species_key}))
                    AND (`Chr` IN ({chr_key}))
                    AND (`IP` IN ({ip_key}))")

  chr_sub <- tbl(tfmapperdb, sql(chr_query)) %>%
    collect()

  chr_filter <- unlist(chr_sub["table"])
  names(chr_filter) <- chr_filter
  s_min_query <- paste0("SELECT MIN(`Start`) FROM ",chr_filter, collapse=" UNION ALL ")

  start_min <- tbl(tfmapperdb, sql(s_min_query)) %>%
    collect() %>%
    min()

  e_max_query <- paste0("SELECT MAX(`Start`+`Length`) FROM ",chr_filter, collapse=" UNION ALL ")

  end_max <- tbl(tfmapperdb, sql(e_max_query)) %>%
    collect() %>%
    max()

  return(c(start_min,end_max))
}

## 查询已知基因结果下可选细胞类型
query_bio <- function(tfmapperdb = tfmapperdb,
                       gene_data) {
  tmp_keys <- paste0(unique(unlist(gene_data[,"DCid"])), collapse = ",")

  anno_query <- glue("SELECT * FROM ann_info WHERE (DCid IN ({tmp_keys}))")

  gene_anno <- tbl(tfmapperdb, sql(anno_query)) %>%
    collect()
  return(gene_anno)
}

## 标准化用于查询的key
sql_key <- function(key) {
  if (length(key)==1 & key == "ALL") {

    key_name <- deparse(substitute(key))
    pull_name <-  gsub("key","pull",key_name)
    ke_sql <- paste0('"',get(pull_name),'"',collapse =  ',')

  } else {

    ke_sql <- paste0('"',key,'"',collapse =  ',')

  }

  return(ke_sql)

}
