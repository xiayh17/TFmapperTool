library(tibble)

## 初步判断有哪些重要结果要在UI展示
## GSMID 支持GEO数据库跳转
## Factor
## 可视化跳转连接 WashU UCSC
## 坐标 Chr:start,end
## Portion
## Bio source
## IP
## 其他东西隐藏 给一个显示按钮
vip_col <- c("GSMID","Factor","Portion","IP","GeneSymbol","Tissue_type","Cell_line","Cell_type","Species","Chr","Start","Length")
all_col <- colnames(res)
other_col <- setdiff(all_col,vip_col)

res_re <- res[,c(vip_col,other_col)]

## 拼接坐标
## 这一步的结果可作为用户下载表格的来源
res_re <- add_column(res_re,
           Coordinate = paste0(res_re[,"Chr"], ":", res_re[,"Start"],"-",res_re[,"Start"] + res_re[,"Length"]),
           .after = "GeneSymbol")


## 添加超链接
## 这一步主要是为了用于UI展示，添加到其他数据库的超链接
## GSMID 连接到 https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSM1716769
## GeneSymbol 连接到 geneCard https://www.genecards.org/cgi-bin/carddisp.pl?gene=
## Coordinate 连接到 http://epigenomegateway.wustl.edu/legacy/?genome=hg38&datahub=http://dc2.cistrome.org/api/datahub/56669&coordinate=chr19:58356965-58359116
## Coordinate 连接到 http://genome.ucsc.edu/cgi-bin/hgTracks?db=hg38&hgct_customText=track+visibility%3D2+type%3DbigWig+bigDataUrl%3Dhttp%3A%2F%2Fdc2.cistrome.org%2Fgenome_browser%2Fbw%2F56669_treat.bw+name%3D%2256669_treat.bw%22&position=chr19:58356965-58359116

DT::datatable(res_re)

res_dt <- res_re
res_dt[,"GSMID"] <- gsmLink(res_re$GSMID,res_re$GSMID)
res_dt[,"GeneSymbol"] <- geneCardsLink(res_re$GeneSymbol,res_re$GeneSymbol)
res_dt <- add_column(res_dt,
                     `Genome Browser` = paste(washuLink(res_re$Species,res_re$Coordinate,"WashU"),
                                          ucscLink(res_re$Species,res_re$Coordinate,"UCSC")),
                    .after = "Coordinate")

DT::datatable(res_dt,style = "bootstrap4", escape = FALSE)

DT::datatable(res_dt,
              rownames = FALSE,style = "bootstrap4", escape = FALSE,
              extensions = 'Responsive',
              caption = htmltools::tags$caption(
                style = 'caption-side: bottom; text-align: center;',
                'Table ', htmltools::em('Result of Search')
              )
)
