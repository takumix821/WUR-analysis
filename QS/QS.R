# java -Dwebdriver.chrome.driver=D:chromedriver.exe -jar selenium-server-standalone-4.0.0-alpha-1.jar

library("RSelenium")
library(stringr)

remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4444,
  browserName = "chrome")

remDr$open()

remDr$navigate("https://www.topuniversities.com/university-rankings/world-university-rankings/2023")

Sys.sleep(5)

# href table
all_href_list = c()
all_univ_name = c()
all_univ_rank = c()
all_univ_n = 0


page.list = remDr$findElements(using = 'class name', value = "page-link")
maxpage = as.numeric(page.list[[length(page.list)-1]]$getElementText())

for (t in 1:(maxpage-5)){
  univ.list = remDr$findElements(using = 'class name', value = "uni-link")
  univ.rank = remDr$findElements(using = 'css selector', value = "div._univ-rank.hide-this-in-mobile-indi")
  univ_n = length(univ.list)
  all_univ_n = all_univ_n + univ_n
  href_list = c()
  name_list = c()
  rank_list = c()
  
  for (i in 1:univ_n){
    univ.elem = univ.list[[i]]
    rank.elem = univ.rank[[i]]
    univ_href = univ.elem$getElementAttribute("href")
    univ_name = univ.elem$getElementText()
    univ_rank = rank.elem$getElementText()
    href_list[i] = univ_href[[1]]
    name_list[i] = univ_name[[1]]
    rank_list[i] = univ_rank[[1]]
  }
  all_href_list = c(all_href_list, href_list)
  all_univ_name = c(all_univ_name, name_list)
  all_univ_rank = c(all_univ_rank, rank_list)
  
  n_page = remDr$findElements(using = 'class name', value = "page-link")
  n_page = n_page[[length(n_page)]]
  n_page$sendKeysToElement(list("laptops", key="enter"))
  Sys.sleep(5)
}

#href_table = data.frame(University = all_univ_name, href = all_href_list, rank = all_univ_rank)
#write.csv(href_table, "href_table_2023.csv", fileEncoding = "UTF-8")


# ranking table
qs_ranking.table = data.frame(matrix(nrow = 0, ncol = 10))
value = c("University", "Total students", "PG students",
          "UG students", "International students",
          "PG students", "UG students",
          "Total faculty staff", "Int'l staff",
          "Domestic staff")
colnames(qs_ranking.table) = value


for(i in 1:all_univ_n){
  remDr$navigate(all_href_list[i])
  Sys.sleep(5)
  
  # staff ¤lµe­±
  target = remDr$findElement(using = 'id', value = "studStaff_Tab")
  target$sendKeysToElement(list("laptops", key="enter"))
  Sys.sleep(5)
  # subsection (stud, inter, staff)
  
  studstaff = remDr$findElements(using = 'class name', value = 'studstaff-subsection')
  studstaff.list = c()
  if (length(studstaff) > 0){
    for (j in 1:length(studstaff)){
      sub.elem.list = studstaff[[j]]$findElements(using = 'class name', value = 'color-code-cont')
      sub.list.char = unlist(lapply(sub.elem.list, function(e) { e$getElementText() }))[1]
      sub.list = strsplit(sub.list.char, '\n')[[1]]
      studstaff.list = c(studstaff.list, sub.list)
    }
  }
  
  # univ_name
  uniinfo.elem = remDr$findElement(using = 'class name', value = "text-white")
  univ_name = as.character( uniinfo.elem$getElementText() )
  
  tryCatch({
  qs_ranking = data.frame(n = univ_name,
                          a = studstaff.list[which(studstaff.list == "Total students")+1],
                          b = studstaff.list[which(studstaff.list == "UG students")[1]+1],
                          c = studstaff.list[which(studstaff.list == "PG students")[1]+1],
                          d = studstaff.list[which(studstaff.list == "International students")+1],
                          e = studstaff.list[which(studstaff.list == "UG students")[2]+1],
                          f = studstaff.list[which(studstaff.list == "PG students")[2]+1],
                          g = studstaff.list[which(studstaff.list == "Total faculty staff")+1],
                          h = studstaff.list[which(studstaff.list == "Domestic staff")+1],
                          i = studstaff.list[which(studstaff.list == "Int'l staff")+1]
                          )
  }, error = function(e){
    qs_ranking = data.frame(n = univ_name,
                            a = NA,
                            b = NA,
                            c = NA,
                            d = NA,
                            e = NA,
                            f = NA,
                            g = NA,
                            h = NA,
                            i = NA)
  })
    
  value = c("University", "Total students", "PG students",
            "UG students", "International students",
            "PG students", "UG students",
            "Total faculty staff", "Int'l staff",
            "Domestic staff")
  colnames(qs_ranking) = value
  qs_ranking.table = rbind(qs_ranking.table, qs_ranking)
  
}


#write.csv(qs_ranking.table, "qs_table_2023.csv", fileEncoding = "UTF-8")


