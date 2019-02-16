
library(rvest)


###�P�A�H�׃��O�̃T�C�g�������擾����֐�tabelog_scraper_1p##

tabelog_scraper_1p <- function(url) {
  html <- read_html(url)
  ##�f�[�^�̎擾
  #���X�ܐ��̎擾
  nShop_raw <- html_nodes(html, ".page-count") %>%
    html_text(.)
  
  #�X�ܖ��̎擾
  shopName <- html_nodes(html, ".cpy-rst-name") %>% 
    html_text(.) 
  
  #�H�׃��O�X�R�A�̎擾
  score <- html_nodes(html, ".list-rst__rating-val") %>%  
    html_text(.) 
  
  #���r���[���̎擾
  nReview <- html_nodes(html, ".list-rst__rvw-count-target") %>% 
    html_text(.) 
  
  #���̑��֘A���̎擾
  info <- html_nodes(html, ".cpy-area-genre") %>% 
    html_text(.)
  
  
  ##�f�[�^�̐��`
  #�f�[�^�̒�������
  n <- 20
  length(shopName) = n
  length(score) = n
  length(nReview) = n
  length(info) = n
  
  #nShop_raw���當����r��
  nShop_raw <- substr(nShop_raw, 22, 28)
  length(nShop_raw) = n
  
  #nReview���當����r��
  nReview <- gsub("��","",nReview)
  
  #�擾�����f�[�^���f�[�^�t���[���ɂ܂Ƃ߂�
  tabelog_data[[pagenum]] <<- na.omit(data.frame(shopName,score,nReview,info))
  
  #�T�[�o�[�ւ̕��ׂ��y�����邽��10�b�x�~
  Sys.sleep(10)
} 






###2�A�H�׃��O�T�C�g���̏��𕡐��y�[�W�ɓn���Ď擾����֐�tabelog_scraper###

tabelog_scraper <- function(url) {
  #�ϐ���url����
  
  
  
  #�f�[�^�̎擾
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
  
  #�f�[�^�̐��`
  n <- 20
  length(shopName) = n
  length(score) = n
  length(nReview) = n
  length(info) = n
  
  #nShop_raw���當����r��
  nShop_raw <- substr(nShop_raw, 22, 28)
  pagesum <<- ceiling((as.numeric(substr(nShop_raw, 3, 5)))/20)
  length(nShop_raw) = n
  
  #nReview���當����r��
  nReview <- gsub("��","",nReview)
  
  #�f�[�^�t���[���Ƀf�[�^���܂Ƃ߂�
  tabelog_data[[1]] <<- na.omit(data.frame(shopName,score,nReview,info))
  
  #�i�s�󋵊m�F�p�̏o��
  print("1�y�[�W��")
  
  #�T�[�o�[�ւ̕��ׂ��y�����邽��10�b�x�~
  Sys.sleep(10)
  
  #tabelog_scraper_1p���y�[�W���̕������J��Ԃ�
  while(pagenum < pagesum) {
    url <- paste("https://tabelog.com/osaka/A2701/A270404/R368/rstLst/",as.integer(pagenum),"/?Srt=D&SrtT=rt&sort_mode=1&svd=20181206&svt=1900&svps=2",sep = "")
    tabelog_scraper_1p(url)
    print(paste(pagenum,"�y�[�W��",sep=""))
    pagenum <<- pagenum + 1
  }
  return(tabelog_data)
} 


###�R�A�g�p����l�̓���
#�擾�J�n�y�[�W�̎w��
pagenum <<- 2

#�擾�y�[�W��url�̎w��
url1 <<- "https://tabelog.com/osaka/A2701/A270404/R368/rstLst/?Srt=D&SrtT=rt&sort_mode=1&svd=20181206&svt=1900&svps=2"

#�f�[�^�ۊǗp�̃��X�g�̍쐬
tabelog_data <- list()


###�S�A�f�[�^�̏o��
data <- tabelog_scraper(url)
data