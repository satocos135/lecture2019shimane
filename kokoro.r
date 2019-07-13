

# path = Sys.getenv('PATH')
# Sys.setenv(PATH= sub('MeCab', 'MeCab_sjis', path))
# Sys.getenv('PATH')
#Sys.getenv('PATH')


library('tidyverse')
library('RMeCab')
library('igraph')


getwd()
#setwd('../projects/lec')
setwd('../lecture2019/')

kokoro = read.delim('data/kokoro.tsv', header=T, sep='\t', stringsAsFactor=F, fileEncoding='utf8')

kokoro %>% head()

# 段落の長さの分布
kokoro[, 'content'] %>% str_length() %>% hist(breaks=40, xlab='Paragraph length', main='Histogram of paragraph length')

kokoro['length'] = kokoro[, 'content'] %>% str_length()
boxplot(length ~ part_id, data=kokoro, main='Paragraph length of each part')

kokoro['section_id2'] = kokoro['part_id'] * 100 + kokoro['section_id']
boxplot(length ~ section_id2, data=kokoro, main='Paragraph length of each section')

# 分析のため各部ごとに文章を結合する
parts = kokoro %>% group_by(part_id) %>% summarise(text = paste0(content, collapse=''))
parts = as.data.frame(parts)
dim(parts)


parts[, 'text'] %>% str_length()

count_noun = docMatrixDF(parts[,'text'], pos=c('名詞'))
count_noun = docMatrixDF(parts[,'text'], pos=c('名詞'), dic='dict/kokoro.dic')

# 全体を集計する
freq_noun = count_noun %>% rowSums()

# 全体を集計する
freq_noun %>% sort(decreasing=T) %>% plot(main='Distribution of noun frequency', xlab='Rank', ylab='Frequency')

freq_noun %>% sort(decreasing=T) %>% plot(main='Distribution of noun frequency', xlab='Rank', ylab='Frequency (log)', log='y')

freq_noun %>% sort(decreasing=T) %>% plot(main='Distribution of noun frequency', xlab='Rank (log)', ylab='Frequency (log)', log='xy')

freq_noun %>% sort() %>% tail(30) %>% barplot(horiz=T, las=1, main='Top 30 nouns', xlab='Frequency')

# (t(count_noun) / colSums(count_noun) ) %>% colSums()
stopwords = c('事','の','よう','それ','もの', '人', '何','一', 'ん','方','二','前','気','中','上','今','ため')
freq_noun[!names(freq_noun) %in% stopwords] %>% sort() %>% tail(30) %>% barplot(horiz=T, las=1, main='Top 30 nouns', xlab='Frequency')

stopwords = c('事','の','よう','それ','もの', '人', '何','一', 'ん','方','二','前','気','中','上','今','ため', '時', 'そこ', 'どこ', 'これ', 'そう')
freq_noun[!names(freq_noun) %in% stopwords] %>% sort() %>% tail(30) %>% barplot(horiz=T, las=1, main='Top 30 nouns', xlab='Frequency')

# PCA
mat = count_noun 
mat = mat[rowSums(mat) > 50, ]
mat = mat[!row.names(mat) %in% stopwords, ]
colnames(mat) = c('第一部', '第二部', '第三部')

mat_t = t(mat)
mat %>% head()

# 単語の頻度
result = (mat_t / colSums(mat)) %>% prcomp()
biplot(result)

# 確率の比
ratio = mat_t / colSums(mat)
ratio_t = t(ratio)
result = (ratio_t / colSums(ratio))  %>% prcomp()
biplot(result)



# 共起分析
# バイグラム

bigram = docDF(parts,col='text', type=1, pos=c('名詞'), N=2, nDF=1, dic='dict/kokoro_sjis.dic')
    
# distribution
bigram['freq'] = bigram[,5:7] %>% rowSums()
bigram[,'freq'] %>% sort(decreasing=T) %>% plot(log='y')
bigram[,'freq'] %>% sort(decreasing=T) %>% plot(log='xy')


bigram[1,]

net = bigram %>% 
    filter(freq > 20)  %>% 
    filter(! N1 %in% stopwords) %>%  
    filter(! N2 %in% stopwords)


net %>% graph_from_data_frame() %>% tkplot(vertex.color='SkyBlue', vertex.size=22)



net = bigram %>% 
    select(N1, N2, freq=Row1) %>% 
    filter(freq > 10)  %>% 
    filter(! N1 %in% stopwords) %>%  
    filter(! N2 %in% stopwords)


net %>% graph_from_data_frame() %>% tkplot(vertex.color='SkyBlue', vertex.size=22)


net = bigram %>% 
    select(N1, N2, freq=Row2) %>% 
    filter(freq > 5)  %>% 
    filter(! N1 %in% stopwords) %>%  
    filter(! N2 %in% stopwords)

net %>% graph_from_data_frame() %>% tkplot(vertex.color='SkyBlue', vertex.size=22)


net = bigram %>% 
    select(N1, N2, freq=Row3) %>% 
    filter(freq > 10)  %>% 
    filter(! N1 %in% stopwords) %>%  
    filter(! N2 %in% stopwords)


net %>% graph_from_data_frame() %>% tkplot(vertex.color='SkyBlue', vertex.size=22)





# net %>% graph_from_data_frame() %>% plot(vertex.color='SkyBlue', vertex.size=22)


bigram = docDF(parts,col='text', type=1, pos=c('名詞', '動詞', '形容詞', '副詞'), N=2, nDF=1, dic='dict/kokoro_sjis.dic')

# ojo-san

bigram %>% 
    select(N1, N2, freq=Row3) %>% 
    filter(N1 == 'お嬢さん' | N2 == 'お嬢さん') %>%
    filter(! N1 %in% stopwords) %>%  
    filter(! N2 %in% stopwords) %>%
    arrange(desc(freq))


net = bigram %>% 
    select(N1, N2, freq=Row3) %>% 
    filter(freq > 1)  %>% 
    filter(N1 == 'お嬢さん' | N2 == 'お嬢さん')  %>% 
    filter(! N1 %in% stopwords) %>%  
    filter(! N2 %in% stopwords)

net %>% graph_from_data_frame() %>% tkplot(vertex.color='SkyBlue', vertex.size=22)

stopwords = c('事','の','よう','それ','もの', '人', '何','一', 'ん','方','二','前','気','中','上','今','ため', '時', 'そこ', 'どこ', 'これ', 'そう',
              'いる', 'なる', 'する', 'いう', 'ある')



# watashi

bigram %>% 
    filter(N1 == '私' | N2 == '私')  %>% 
    filter(! N1 %in% stopwords) %>%  
    filter(! N2 %in% stopwords) %>%
    filter(str_detect(POS2, '非自立')) %>%
    arrange(desc(Row3)) %>% head(50)

bigram %>% 
    filter(N1 == '私' | N2 == '私')  %>% 
    filter(! N1 %in% stopwords) %>%  
    filter(! N2 %in% stopwords) %>%
    filter(!str_detect(POS2, '非自立')) %>%
    arrange(desc(Row3)) %>% head(20)

bigram %>% 
    filter(N1 == '私' | N2 == '私')  %>% 
    filter(! N1 %in% stopwords) %>%  
    filter(! N2 %in% stopwords) %>%
    filter(!str_detect(POS2, '非自立')) %>%
    filter(str_detect(POS2, '動詞')) %>%
    select(everything(), freq=Row3) %>% 
    arrange(desc(freq)) %>% head(30)

bigram %>% 
    filter(N1 == '私' | N2 == '私')  %>% 
    filter(! N1 %in% stopwords) %>%  
    filter(! N2 %in% stopwords) %>%
    filter(!str_detect(POS2, '非自立')) %>%
    filter(str_detect(POS1, '動詞')) %>%
    select(everything(), freq=Row3) %>% 
    arrange(desc(freq)) %>% head(30)


net = bigram %>% 
    filter(N1 == '私' | N2 == '私')  %>% 
    filter(str_detect(POS1, '名詞-名詞')) %>%
    select(N1, N2, freq=Row3) %>% 
    filter(freq > 4)  %>% 
    filter(! N1 %in% stopwords) %>%  
    filter(! N2 %in% stopwords)

net %>% graph_from_data_frame() %>% tkplot(vertex.color='SkyBlue', vertex.size=22)


net = bigram %>% 
    filter(N1 == '私' | N2 == '私') %>% 
    filter(str_detect(POS1, '動詞')) %>%
    select(N1, N2, freq=Row3) %>% 
    filter(freq > 4)  %>% 
    filter(! N1 %in% stopwords) %>%  
    filter(! N2 %in% stopwords)

net %>% graph_from_data_frame() %>% tkplot(vertex.color='SkyBlue', vertex.size=22)



net = bigram %>% 
    filter(N1 %in% c('私','Ｋ','お嬢さん') | N2 %in% c('私','Ｋ','お嬢さん')) %>% 
    filter(str_detect(POS1, '形容')) %>%
    select(N1, N2, freq=Row3) %>% 
    filter(freq > 0)  %>% 
    filter(! N1 %in% stopwords) %>%  
    filter(! N2 %in% stopwords)

net %>% graph_from_data_frame() %>% tkplot(vertex.color='SkyBlue', vertex.size=22)


net = bigram %>% 
    filter(str_detect(POS1, '形容')) %>%
    select(N1, N2, freq=Row3) %>% 
    filter(freq > 2)  %>% 
    filter(N1 != '私') %>%  
    filter(N2 != '私') %>%  
    filter(! N1 %in% stopwords) %>%  
    filter(! N2 %in% stopwords)

net %>% graph_from_data_frame() %>% tkplot(vertex.color='SkyBlue', vertex.size=22)



stopwords = c('事','の','よう','それ','もの', '人', '何','一', 'ん','方','二','前','気','中','上','今','ため', '時', 'そこ', 'どこ', 'これ', 'そう',
              'いる', 'なる', 'する', 'いう', 'ある', 'れる', 'られる', 'くれる')


## co-occurence analysis


#kokoro_sentence = read.delim('kokoro_sentence.tsv', header=T, sep='\t', stringsAsFactor=F, fileEncoding='UTF-8-BOM')
kokoro_sentence = read.delim('data/kokoro_sentence.tsv', header=T, sep='\t', stringsAsFactor=F, fileEncoding='UTF-8-BOM')
kokoro_sentence %>% head()

source('functions.r', encoding='utf8')




# get_cooc = function(string, pos=c('名詞', '動詞'), with_pos=F, unique=T, stopwords=c(), dic=''){
#     if(length(string) == 0){
#         return(c())
#     }
#     words = RMeCabC(string, 1, dic=dic) %>% unlist()
#     targets = words[names(words) %in% pos] 
#     if(unique){
#         #targets = unique(targets)
#         targets[!duplicated(targets)]
#     }
#     if(length(stopwords)){
#         targets = targets[! targets %in% stopwords]
#     }
#     
#     if(length(targets) < 2){
#         return(c())
#     }
#     
#     if(with_pos){
#         targets = str_c(targets,'/',names(targets))
#     }
#     res = targets %>% sort() %>% combn(2)
#     
#     result = paste(res[1,], '-', res[2,], sep='')
#     return(result)
# }
# 
# 
# parse_cooc = function(strings, freq){
#     res = str_split(strings, '-')
#     w1 = str_split(map_chr(res,1), '/')
#     w2 = str_split(map_chr(res,2), '/')
#     result = as.data.frame(cbind(
#         map_chr(w1,1), 
#         map_chr(w2,1),
#         str_c(map_chr(w1,2), '-', map_chr(w2,2))
#     ))
#     colnames(result) = c('N1', 'N2', 'POS')
#     result['freq'] = freq
#     return(result)
# }
# 





words = RMeCabC('今日はよい日で天気がよい', 1) %>% unlist()
words[!duplicated(words)]
targets = str_c(targets,'/',names(targets))
res = targets %>% sort() %>% combn(2)
w1 = str_split(res[1,], '/')
w2 = str_split(res[2,], '/')
as.data.frame(cbind(
    str_c(map_chr(w1,1), '-', map_chr(w2,1)),
    str_c(map_chr(w1,2), '-', map_chr(w2,2))
))



# test = text %>% str_split('。') %>% unlist()
# test = test[str_length(test) > 0]
# kokoro['sentences'] = kokoro[, 'content'] %>% str_split('。') 
# iconv(from='UTF-8', to='cp932')
# count_cooc %>% head() %>% lapply(get_cooc) %>% unlist()
# 
# 
# 
# test %>% iconv(from='UTF-8', to='cp932') %>% map(function(x){RMeCabC(x)})
# 
# test = 
# RMeCabC(test[1], 1)
# 
# 
# Encoding(test[1]) = 'cp932'
# Encoding(text) 
# Encoding(test) 
# test
# RMeCabC(text, 1)
# 
# RMeCabC(str_c(test[1], ''))

# Sys.setlocale("LC_CTYPE", "ja_JP.UTF8")
# Sys.setlocale("LC_CTYPE", "C")
# Sys.getlocale()

## sentenceごとにわける


stopwords = c('事','の','よう','それ','もの', '人', '何','一', 'ん','方','二','前','気','中','上','今','ため', '時', 'そこ', 'どこ', 'これ', 'そう',
              'いる', 'なる', 'する', 'いう', 'ある', 'れる', 'られる', 'くれる')

test = map(kokoro_sentence[,'content'], get_cooc, pos=c('名詞'), stopwords=stopwords) 
res = test %>% unlist() %>% table()
res %>% as.vector() %>% sort(decreasing=T) %>% head(100) %>% plot(type='p')
res %>% dim()

res %>% as.vector() %>% sort(decreasing=T) %>% plot()
res %>% as.vector() %>% sort(decreasing=T) %>% plot(log='y')
res %>% as.vector() %>% sort(decreasing=T) %>% plot(log='xy')



part1 = map(kokoro_sentence[kokoro_sentence$part_id == 1,'content'], 
            get_cooc, pos=c('名詞'), stopwords=stopwords, dic='dict/kokoro_sjis.dic') %>% unlist() %>% table()
part2 = map(kokoro_sentence[kokoro_sentence$part_id == 2,'content'], 
            get_cooc, pos=c('名詞'), stopwords=stopwords, dic='dict/kokoro_sjis.dic') %>% unlist() %>% table()
part3 = map(kokoro_sentence[kokoro_sentence$part_id == 3,'content'], 
            get_cooc, pos=c('名詞'), stopwords=stopwords, dic='dict/kokoro_sjis.dic') %>% unlist() %>% table()

df1 = as.data.frame(part1)
df2 = as.data.frame(part2)
df3 = as.data.frame(part3)
res = merge(x=df1, y=df2, by='.', all=T)
res = merge(x=res, y=df3, by='.', all=T)
colnames(res) = c('term', 'df1', 'df2', 'df3')
res[is.na(res)] = 0

res %>% filter(str_detect(term, 'Ｋ')) %>% 
    arrange(desc(df3)) %>% head()

mat = res

mat %>% filter(str_detect(term, 'Ｋ')) %>% arrange(desc(df3)) %>% head()
row.names(mat) = mat[,1]
mat = mat[2:4]
#mat = mat + 0.5
mat = mat[rowSums(mat) > 50, ]
colnames(mat) = c('第一部','第二部','第三部')
mat_t = t(mat)

ratio = mat_t / colSums(mat)
ratio_t = t(ratio)
result = (ratio_t / colSums(ratio))  %>% prcomp()

biplot(result)
#biplot(result, xlim=c(-0.7, 0.7), ylim=c(-0.7, 0.7))






res = map(kokoro_sentence[kokoro_sentence$part_id==3,'content'], get_cooc, pos=c('名詞', '形容詞'), with_pos=T, stopwords=stopwords, dic='dict/kokoro_sjis.dic')  %>%
    unlist() %>% table() 
res %>% head()

df = parse_cooc(names(res), as.vector(res))
df %>% head()


df$freq %>% sort(decreasing=T) %>% plot()
df$freq %>% sort(decreasing=T) %>% plot(log='y')


net = df %>% 
    filter(N1 %in% c('私','Ｋ','お嬢さん') | N2 %in% c('Ｋ','お嬢さん','私')) %>% 
    filter(str_detect(POS, '形容')) %>%
    filter(freq > 0)  %>% 
    filter(! N1 %in% stopwords) %>%  
    filter(! N2 %in% stopwords)

net %>% graph_from_data_frame() %>% as.undirected() %>% tkplot(vertex.color='SkyBlue', vertex.size=22)


net %>% graph_from_data_frame() %>% tkplot(vertex.color='SkyBlue', vertex.size=22)





stopwords = c('事','の','よう','それ','もの', '人', '何','一', 'ん','方','二','前','気','中','上','今','ため', '時', 'そこ', 'どこ', 'これ', 'そう',
              'いる', 'なる', 'する', 'いう', 'ある')


