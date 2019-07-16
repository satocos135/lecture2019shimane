###### Lecture1.r
### インストールされている必要のあるパッケージ
###     - dplyr
###     - stringr
###     - RMeCab
### 実行例はLecure1.ipynbファイルを参照のこと

# フォルダの設定
# getwd() # フォルダの確認
# setwd(解凍したフォルダのパス) # フォルダを指定


### dplyrによるパイプ処理

library("dplyr")

# サンプルデータセットの１つであるToothGrowthデータセットを使う

?ToothGrowth # データセットの詳細

### ToothGrowth dataset
### len 	numeric	Tooth length
### supp	factor	Supplement type (VC or OJ).
### dose	numeric	Dose in milligrams/day

## パイプ処理

head(ToothGrowth) # 普通にhead()

# パイプ演算子 %>% を使った書き方
ToothGrowth %>% head()

## 行の指定
# filter(): 条件にあう行だけを抽出する

ToothGrowth %>% filter(supp == "OJ") %>% head() # supp列の値がOJである行

## 列の指定
# select(): 指定した列を抽出
ToothGrowth %>% select(len, supp) %>% head() 

# 抽出した列を変えて出力できる
ToothGrowth %>% select(length=len, supp_type=supp) %>% head() # len列を「length」、supp列を「supp_type」として出力

### stringrによる文字列処理
library('stringr')

# サンプルデータ：月名のリスト
(months = month.name) 

## 条件にあう文字列を抽出する
# str_subset(): マッチする文字列を抽出
months %>% str_subset('r') # "r"を含む月名を抜き出す

# str_detect(): パターンににマッチするかどうか判別
months %>% str_detect('r') # 真偽値が返る


### 正規表現

# 正規表現を使った文字列の抽出
# str_extract(): マッチした部分をひとつだけ返す

# 普通の文字列
'abcdefg' %>% str_extract('bcd')　# bcdにマッチ

# ^: 行頭
# すべての文字: .
'abcdefg' %>% str_extract('^...') # 冒頭三文字にマッチ

# $: 行末
'abcdefg' %>% str_extract('...$') # 末尾三文字にマッチ

# str_extract_all(): マッチした部分をすべて返す
# |:「または」
'abcdefg' %>% str_extract_all('ab|ef') # abまたはefにマッチ

## 例文1

sentence = 'googleのトップページはhttps://www.google.co.jp/です'


# []: 文字クラス（括弧内にふくまれる文字のいずれか）:
# +: 1回以上の連続
sentence %>% str_extract('^[A-z]+') # 冒頭の1文字以上のアルファベットの連続

# ?: 0回または1回の繰り返し 
# *: 0回以上の繰り返し
# \\: エスケープシーケンス（特殊文字の効果を打ち消す）
sentence %>% str_extract('https?[A-z:/\\.]*') # httpまたはhttpsで始まるurlを抜き出す


## 例文2

sentence = "computational social sciences"


# 単語を抜き出す練習

sentence %>% str_extract_all('[A-z]+') # 単語(アルファベットの連続)をすべて抜き出す

sentence %>% str_extract_all('s[A-z]*') # sで始まる単語をすべて抜き出す

sentence %>% str_extract_all('[A-z]*al') # -alで終わる単語をすべて抜き出す

sentence %>% str_extract_all('[aiueo]') # 母音をすべて抜き出す


## 要素を含むかの判定: %in%

c('a','b','c','d','e') %in% c('a','e') # ベクトルの要素それぞれについて右側に含まれていればTRUE

### RMecabの基礎

# + MeCab: 形態素解析エンジン
# + RMeCab: MeCabをR上で使うためのパッケージ

library('RMeCab')

(test = RMeCabC('今日は本を読んだ') %>% unlist())


# %in%との組み合わせ
# names(): ベクトルについているラベルを抜き出す
test[names(test) %in% c('名詞', '動詞')] # 結果から名詞と動詞を抜き出す

# names(test) %in% c('名詞', '動詞')] # names()で抜き出したラベルが名詞または動詞であるか

RMeCabC('今日は本を読んだ', 1) %>% unlist()　# 第二引数に1を入力すると、動詞が原形になる


## クリーニングの実際

# 例文：
## 「切ってきて、丸めて、家の屋根にしげておくん。それはてんかごめんけどどこのやまいってきってきてもよいということになっちょった。」

text = '切ってきて、丸めて、家の屋根にしげておくん。それはてんかごめんけどどこのやまいってきってきてもよいということになっちょった。'
RMeCabC(text) %>% unlist()


# 表現を変える
text = '切ってきて、丸めて、家の屋根にすげておくん。それは天下御免でどこの山に行って切ってきてもよいということになっていた。'
RMeCabC(text) %>% unlist()


# 辞書の使い方

RMeCabC('まあそういうことで、余分な話はいいけん。') %>% unlist() # 辞書なし（デフォルト）

RMeCabC('まあそういうことで、余分な話はいいけん。', dic='dict/example.dic')  %>% unlist() # 辞書使用


# 活用のある語は活用ごとに登録しないといけないことに注意


