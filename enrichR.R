get_pubmed_title <- function(pubmed.id){
  if(length(pubmed.id) != 0){
    out <- vector()
    for(i in 1:length(pubmed.id)){
      out[i] <- ArticleTitle(EUtilsGet(EUtilsSummary(paste0(pubmed.id[i], "[uid]"), db="pubmed")))
    }
    return(out)
  }
  else{
    return("")
  }
}
