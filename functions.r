library(RMeCab)
library(stringr)
library(purrr)


tf = function(df){
    return(t(t(df) / colSums(df)))
}

idf = function(df){
    doc_sums = (df > 0) %>% rowSums() + 1
    return(log2(ncol(df)/doc_sums))
}


get_cooc = function(string, pos=c('名詞', '動詞'), with_pos=F, unique=T, stopwords=c(), dic=''){
    if(length(string) == 0){
        return(c())
    }
    words = RMeCabC(string, 1, dic=dic) %>% unlist()
    targets = words[names(words) %in% pos] 
    if(unique){
        #targets = unique(targets)
        targets[!duplicated(targets)]
    }
    if(length(stopwords)){
        targets = targets[! targets %in% stopwords]
    }
    
    if(length(targets) < 2){
        return(c())
    }
    
    if(with_pos){
        targets = str_c(targets,'/',names(targets))
    }
    res = combn(sort(targets),2)
    
    result = paste(res[1,], '-', res[2,], sep='')
    return(result)
}


parse_cooc = function(strings, freq){
    res = str_split(strings, '-')
    w1 = str_split(map_chr(res,1), '/')
    w2 = str_split(map_chr(res,2), '/')
    result = as.data.frame(cbind(
        map_chr(w1,1), 
        map_chr(w2,1),
        str_c(map_chr(w1,2), '-', map_chr(w2,2))
    ))
    colnames(result) = c('N1', 'N2', 'POS')
    result['freq'] = freq
    return(result)
}

