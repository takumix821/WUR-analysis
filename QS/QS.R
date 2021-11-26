library("RSelenium")
library(stringr)

remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4444,
  browserName = "chrome")

remDr$open()

remDr$navigate("https://www.topuniversities.com/university-rankings/world-university-rankings/2022")

Sys.sleep(5)

# href table
all_href_list = c()
all_univ_n = 0

for (t in 1:130){
  univ.list = remDr$findElements(using = 'class name', value = "uni-link")
  univ_n = length(univ.list)
  all_univ_n = all_univ_n + univ_n
  href_list = c()
  
  for (i in 1:univ_n){
    univ.elem = univ.list[[i]]
    univ_href = univ.elem$getElementAttribute("href")
    href_list[i] = univ_href[[1]]
  }
  all_href_list = c(all_href_list, href_list)
  
  n_page = remDr$findElements(using = 'class name', value = "page-link")
  n_page = n_page[[length(n_page)]]
  n_page$sendKeysToElement(list("laptops", key="enter"))
  Sys.sleep(5)
}

# ranking table
qs_ranking.table = data.frame(matrix(nrow = 0, ncol = 14))
value = c("University", "Total students", "PG students",
          "UG students", "International students",
          "PG students", "UG students",
          "Total faculty staff", "Int'l staff",
          "Domestic staff", "Status",
          "Research Output", "Size", "rank")
colnames(qs_ranking.table) = value


for(i in 1:all_univ_n){
  remDr$navigate(all_href_list[i])
  Sys.sleep(5)
  
  try({
  
  # staff 子畫面
  target = remDr$findElement(using = 'id', value = "staff")
  target$sendKeysToElement(list("laptops", key="enter"))
  
  # student 人數
  student.elem = remDr$findElement(using = 'class name', value = "student")
  student.elem = student.elem$findElement(using = 'class name', value = "line")
  student = student.elem$getElementText()
  
  str_student = str_split(student, "\n")
  
  # faculty 人數
  faculty.elem = remDr$findElement(using = 'class name', value = "faculty")
  faculty = faculty.elem$getElementText()
  
  str_faculty = str_split(faculty, "\n")
  
  # 基本資訊
  uniinfo.elem = remDr$findElement(using = 'class name', value = "uni_info")
  uniinfo = uniinfo.elem$getElementText()
  
  str_uniinfo = str_split(uniinfo, "\n")
  
  # rank
  rank = remDr$findElement(using = 'class name', value = "cont")
  })
  
  # 有校名是基本的
  nam = remDr$findElement(using = 'class name', value = "programeTitle")

  tryCatch({
  qs_ranking = data.frame(n = nam$getElementText()[[1]],
                          a = str_student[[1]][which(str_student[[1]] == "Total students")+1],
                          b = str_student[[1]][which(str_student[[1]] == "PG students")[1]+1],
                          c = str_student[[1]][which(str_student[[1]] == "UG students")[1]+1],
                          d = str_student[[1]][which(str_student[[1]] == "International students")+1],
                          e = str_student[[1]][which(str_student[[1]] == "PG students")[2]+1],
                          f = str_student[[1]][which(str_student[[1]] == "UG students")[2]+1],
                          g = str_faculty[[1]][which(str_faculty[[1]] == "Total faculty staff")+1],
                          h = str_faculty[[1]][which(str_faculty[[1]] == "Int'l staff")+1],
                          i = str_faculty[[1]][which(str_faculty[[1]] == "Domestic staff")+1],
                          j = str_uniinfo[[1]][which(str_uniinfo[[1]] == "Status")+1],
                          k = str_uniinfo[[1]][which(str_uniinfo[[1]] == "Research Output")+1],
                          l = str_uniinfo[[1]][which(str_uniinfo[[1]] == "Size")+1],
                          m = rank$getElementText()[[1]]
                          )
  }, error = function(e){
    qs_ranking = data.frame(n = nam$getElementText()[[1]],
                            a = NA,
                            b = NA,
                            c = NA,
                            d = NA,
                            e = NA,
                            f = NA,
                            g = NA,
                            h = NA,
                            i = NA,
                            j = NA,
                            k = NA,
                            l = NA,
                            m = NA)
  })
    
  value = c("University", "Total students", "PG students",
            "UG students", "International students",
            "PG students", "UG students",
            "Total faculty staff", "Int'l staff",
            "Domestic staff", "Status",
            "Research Output", "Size", "rank")
  colnames(qs_ranking) = value
  qs_ranking.table = rbind(qs_ranking.table, qs_ranking)
  
}
