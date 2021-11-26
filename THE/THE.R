library("RSelenium")

remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4444,
  browserName = "chrome")

remDr$open()

remDr$navigate("https://www.timeshighereducation.com/world-university-rankings/2022/world-ranking")

#cookies = remDr$getAllCookies()

for (i in 1:length(cookies)){
  remDr$addCookie(cookies[[i]][['name']], cookies[[i]][['value']], path = "/")
}
Sys.sleep(5)
remDr$refresh()
Sys.sleep(5)


value = c("sorting_1", "ranking-institution-title", "stats_number_students",
  "stats_student_staff_ratio", "stats_pc_intl_students", "stats_female_male_ratio")

web.elem = remDr$findElements(using = 'class name', value = "sorting_1")
len = length(web.elem)
the_ranking.table = data.frame(matrix(NA, len, length(value)))

for (i in 1:length(value)){
  web.elem = remDr$findElements(using = 'class name', value = value[i])
  col = unlist(lapply(web.elem, function(e) { e$getElementText() }))
  if (i >=3){
    the_ranking.table[,i] = col[2:length(col)]
  }
  else{
    the_ranking.table[,i] = col
  }
}

colnames(the_ranking.table) = c("rank", "University", "Total Students",
                            "student staff ratio", "pc of int'l students", "female:male ratio")

for(i in 1 : 84){
  a = "https://www.timeshighereducation.com/world-university-rankings/2022/world-ranking#!/page/"
  b = "/length/25/sort_by/rank/sort_order/asc/cols/stats"
  aPlusb = paste0(a,i,b)
  remDr$navigate(aPlusb)
  Sys.sleep(5)
  remDr$navigate(aPlusb)
  Sys.sleep(5)
  
  #from here
  
  web.elem = remDr$findElements(using = 'class name', value = "sorting_1")
  len = length(web.elem)
  ranking_table_new = data.frame(matrix(NA, len, length(value)))
  
  for (i in 1:length(value)){
    web.elem = remDr$findElements(using = 'class name', value = value[i])
    col = unlist(lapply(web.elem, function(e) { e$getElementText() }))
    if (i >=3){
      ranking_table_new[,i] = col[2:length(col)]
    }
    else{
      ranking_table_new[,i] = col
    }
  }
  
  colnames(ranking_table_new) = c("rank", "University", "Total Students",
                                  "student staff ratio", "pc of int'l students", "female:male ratio")
  
  the_ranking.table = rbind(the_ranking.table, ranking_table_new)
}

