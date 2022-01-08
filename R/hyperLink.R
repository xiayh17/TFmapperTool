# createLink for GeneCards ------------------------------------------------
geneCardsLink <- function(gene_symbol,button_text) {
  sprintf('<a href="https://www.genecards.org/cgi-bin/carddisp.pl?gene=%s" target="_blank" class="btn btn-link btn-sm">%s</a>',gene_symbol, button_text)
}

# createLink for GSMID ------------------------------------------------------
gsmLink <- function(gsmid, button_text) {
  sprintf('<a href="https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=%s" target="_blank" class="btn btn-link btn-sm">%s</a>',gsmid, button_text)
}

## createLink for Coordinate of cistrome WASHU
washuLink <- function(species,coordinate,button_text) {

  species_l <- lapply(species, function(x){

    if (!is.na(x)){
      if (x == "Homo sapiens") {
        y = "hg38"
      } else {
        y = "mm10"
      }
    } else {
      "unknow"
    }

    return(y)

  })

  species <- unlist(species_l)
  glue::glue('<a href="http://epigenomegateway.wustl.edu/legacy/?genome={species}&datahub=http://dc2.cistrome.org/api/datahub/56669&coordinate={coordinate}" target="_blank" class="btn btn-link btn-sm">{button_text}</a>')
}

## createLink for Coordinate of cistrome UCSC
ucscLink <- function(species,coordinate,button_text) {

  species_l <- lapply(species, function(x){

    if (!is.na(x)){
      if (x == "Homo sapiens") {
        y = "hg38"
      } else {
        y = "mm10"
      }
    } else {
      "unknow"
    }

    return(y)

  })

  species <- unlist(species_l)

  links <- glue::glue('<a href="http://genome.ucsc.edu/cgi-bin/hgTracks?db={species}&hgct_customText=track+visibility%3D2+type%3DbigWig+bigDataUrl%3Dhttp%3A%2F%2Fdc2.cistrome.org%2Fgenome_browser%2Fbw%2F56669_treat.bw+name%3D%2256669_treat.bw%22&position={coordinate}" target="_blank" class="btn btn-link btn-sm">{button_text}</a>')
}
