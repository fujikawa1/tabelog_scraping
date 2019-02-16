
library(rvest)


###１、食べログのサイト内情報を取得する関数tabelog_scraper_1p##

tabelog_scraper_1p <- function(url) {
  html <- read_html(url)
  ##データの取得
  #総店舗数の取得
  nShop_raw <- html_nodes(html, ".page-count") %>%
    html_text(.)
  
  #店舗名の取得
  shopName <- html_nodes(html, ".cpy-rst-name") %>% 
    html_text(.) 
  
  #食べログスコアの取得
  score <- html_nodes(html, ".list-rst__rating-val") %>%  
    html_text(.) 
  
  #レビュー数の取得
  nReview <- html_nodes(html, ".list-rst__rvw-count-target") %>% 
    html_text(.) 
  
  #その他関連情報の取得
  info <- html_nodes(html, ".cpy-area-genre") %>% 
    html_text(.)
  
  
  ##データの整形
  #データの長さ調整
  n <- 20
  length(shopName) = n
  length(score) = n
  length(nReview) = n
  length(info) = n
  
  #nShop_rawから文字を排除
  nShop_raw <- substr(nShop_raw, 22, 28)
  length(nShop_raw) = n
  
  #nReviewから文字を排除
  nReview <- gsub("件","",nReview)
  
  #取得したデータをデータフレームにまとめる
  tabelog_data[[pagenum]] <<- na.omit(data.frame(shopName,score,nReview,info))
  
  #サーバーへの負荷を軽減するため10秒休止
  Sys.sleep(10)
} 






###2、食べログサイト内の情報を複数ページに渡って取得する関数tabelog_scraper###

tabelog_scraper <- function(url) {
  #変数にurlを代入
  
  
  
  #データの取得
  html <- read_html(url)
  nShop_raw <- html_nodes(html, ".c-page-count") %>%
    html_text(.)
  
  shopName <- html_nodes(html, ".list-rst__rst-name-target") %>% 
    html_text(.) 
  
  score <- html_nodes(html, ".list-rst__rating-val") %>%  
    html_text(.) 
  
  nReview <- html_nodes(html, ".list-rst__rvw-count-target") %>% 
    html_text(.) 
  
  info <- html_nodes(html, ".cpy-area-genre") %>% 
    html_text(.)
  
  #データの整形
  n <- 20
  length(shopName) = n
  length(score) = n
  length(nReview) = n
  length(info) = n
  
  #nShop_rawから文字を排除
  nShop_raw <- substr(nShop_raw, 22, 28)
  pagesum <<- ceiling((as.numeric(substr(nShop_raw, 3, 5)))/20)
  length(nShop_raw) = n
  
  #nReviewから文字を排除
  nReview <- gsub("件","",nReview)
  
  #データフレームにデータをまとめる
  tabelog_data[[1]] <<- na.omit(data.frame(shopName,score,nReview,info))
  
  #進行状況確認用の出力
  print("1ページ目")
  
  #サーバーへの負荷を軽減するため10秒休止
  Sys.sleep(10)
  
  #tabelog_scraper_1pをページ数の分だけ繰り返す
  while(pagenum < pagesum) {
    url <- paste("https://tabelog.com/osaka/A2701/A270404/R368/rstLst/",as.integer(pagenum),"/?Srt=D&SrtT=rt&sort_mode=1&svd=20181206&svt=1900&svps=2",sep = "")
    tabelog_scraper_1p(url)
    print(paste(pagenum,"ページ目",sep=""))
    pagenum <<- pagenum + 1
  }
  return(tabelog_data)
} 


###３、使用する値の入力
#取得開始ページの指定
pagenum <<- 2

#取得ページのurlの指定
url1 <<- "https://tabelog.com/osaka/A2701/A270404/R368/rstLst/?Srt=D&SrtT=rt&sort_mode=1&svd=20181206&svt=1900&svps=2"

#データ保管用のリストの作成
tabelog_data <- list()


###４、データの出力
data <- tabelog_scraper(url)
data
