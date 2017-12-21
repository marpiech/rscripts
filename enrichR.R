get.pubmed.title <- function(pubmed_id){
  if(length(pubmed_id) != 0){
    out <- vector()
    for(i in 1:length(pubmed_id)){
      out[i] <- ArticleTitle(EUtilsGet(EUtilsSummary(paste0(pubmed_id[i], "[uid]"), db="pubmed")))
    }
    return(out)
  }
  else{
    return("")
  }
}
