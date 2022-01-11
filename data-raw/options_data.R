## 供函数使用的数据
## 所有可能的物种 IP portion

library(glue)

## Species

## IP

## Biological source
## Cell_line
## Cell_type
## Tissue_type

## Portion

types <- c("species","ip","portion")

for(i in types) {
  load(glue("data-raw/{i}_pull.Rdata"))
}

all <- paste0(types,"_pull",collapse = ",")

eval(parse(text = glue("usethis::use_data({all},internal = T, overwrite = TRUE)")))


## Gene Symbol
## 这个信息包含在sqlite文件中

## Start
## 这个信息包含在sqlite文件中

## Length
## 这个信息包含在sqlite文件中
