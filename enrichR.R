load_package("RISmed")

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

get_enrichr <- function(genes, database, threshold = 0.05, maxn = 25, ngenes.threshold = 2) {
  
  enrichr_data = enrichr(genes, databases = database)

for(term in names(enrichr_data)){
  terms = c(head(enrichr_data[[term]]$Term, n = maxn))
  ppals = c(head(enrichr_data[[term]]$P.value, n = maxn))
  pvals = c(head(enrichr_data[[term]]$Adjusted.P.value, n = maxn))
  genes = c(head(enrichr_data[[term]]$Genes, n = maxn))
  terms = terms[pvals < threshold]
  ppals = ppals[pvals < threshold]
  genes = genes[pvals < threshold]
  pvals = pvals[pvals < threshold]
  
  require(stringr)
  ngenes = sapply(genes, function(x) {length(str_count(x, ";")) + 1})
  terms = terms[ngenes >= ngenes.threshold]
  ppals = ppals[ngenes >= ngenes.threshold]
  genes = genes[ngenes >= ngenes.threshold]
  pvals = pvals[ngenes >= ngenes.threshold]
  
  
  if (term == "GeneSigDB"){
    terms =  get_pubmed_title(c(sapply(strsplit(terms, "-"), head, 1)))
  }
  
  if (length(pvals) == 0){
    terms = c("BRAK")
    genes = c("BRAK")
    pvals = c("BRAK")
    ppals = c("BRAK")
  }
  return((cbind(terms, genes, pvals, ppals)))
  
  }
}
