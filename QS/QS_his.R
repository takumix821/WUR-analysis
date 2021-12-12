library("RSelenium")
library(stringr)

remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4444,
  browserName = "chrome")

remDr$open()

remDr$navigate("https://www.topuniversities.com/university-rankings/world-university-rankings/2020")

Sys.sleep(5)

rank = remDr$findElements(using = 'class name', value = "_univ-rank")
rank_col = unlist(lapply(rank, function(e) { e$getElementText() }))
rank_col = rank_col[rank_col != ""]

univ.list = remDr$findElements(using = 'class name', value = "uni-link")
univ_col = unlist(lapply(univ.list, function(e) { e$getElementText() }))

r_u.table = data.frame(a = rank_col, b = univ_col)

for (i in 1:106){
  n_page = remDr$findElements(using = 'class name', value = "page-link")
  n_page = n_page[[length(n_page)]]
  n_page$sendKeysToElement(list("laptops", key="enter"))
  Sys.sleep(5)
  
  
  rank = remDr$findElements(using = 'class name', value = "_univ-rank")
  rank_col = unlist(lapply(rank, function(e) { e$getElementText() }))
  rank_col = rank_col[rank_col != ""]
  
  univ.list = remDr$findElements(using = 'class name', value = "uni-link")
  univ_col = unlist(lapply(univ.list, function(e) { e$getElementText() }))
  
  r_u.table_new = data.frame(a = rank_col, b = univ_col)
  
  r_u.table = rbind(r_u.table, r_u.table_new)
}