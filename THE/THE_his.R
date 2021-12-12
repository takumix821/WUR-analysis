library("RSelenium")

remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4444,
  browserName = "chrome")

remDr$open()

remDr$navigate("https://www.timeshighereducation.com/world-university-rankings/2020/world-ranking")

#cookies = remDr$getAllCookies()

for (i in 1:length(cookies)){
  remDr$addCookie(cookies[[i]][['name']], cookies[[i]][['value']], path = "/")
}
Sys.sleep(5)
remDr$refresh()
Sys.sleep(5)


value = c("sorting_1", "ranking-institution-title")

web.elem = remDr$findElements(using = 'class name', value = "sorting_1")
len = length(web.elem)
rank_table = data.frame(matrix(NA, len, length(value)))

for (i in 1:length(value)){
  web.elem = remDr$findElements(using = 'class name', value = value[i])
  col = unlist(lapply(web.elem, function(e) { e$getElementText() }))
  rank_table[,i] = col
}

colnames(rank_table) = c("a", "b")

for(i in 1 : 55){
  a = "https://www.timeshighereducation.com/world-university-rankings/2020/world-ranking#!/page/"
  b = "/length/25/sort_by/rank/sort_order/asc/cols/stats"
  aPlusb = paste0(a,i,b)
  remDr$navigate(aPlusb)
  Sys.sleep(5)
  remDr$navigate(aPlusb)
  Sys.sleep(5)
  
  #from here
  
  web.elem = remDr$findElements(using = 'class name', value = "sorting_1")
  len = length(web.elem)
  rank_table_new = data.frame(matrix(NA, len, length(value)))
  
  for (i in 1:length(value)){
    web.elem = remDr$findElements(using = 'class name', value = value[i])
    col = unlist(lapply(web.elem, function(e) { e$getElementText() }))
    rank_table_new[,i] = col
  }
  
  colnames(rank_table_new) = c("a", "b")
  
  rank_table = rbind(rank_table, rank_table_new)
}

