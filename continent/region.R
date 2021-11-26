library("RSelenium")
library(stringr)

remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4444,
  browserName = "chrome")

remDr$open()

remDr$navigate("https://www.topuniversities.com/university-rankings/world-university-rankings/2022")
Sys.sleep(5)

## 修改選單

oceania = c()

for (t in 1:5){
  univ.list = remDr$findElements(using = 'class name', value = "uni-link")
  univ_n = length(univ.list)
  list = c()
  
  for (i in 1:univ_n){
    univ.elem = univ.list[[i]]
    univ = univ.elem$getElementText()
    list[i] = univ[[1]]
  }
  oceania  = c(oceania, list)
  
  n_page = remDr$findElements(using = 'class name', value = "page-link")
  n_page = n_page[[length(n_page)]]
  n_page$sendKeysToElement(list("laptops", key="enter"))
  Sys.sleep(5)
}

write.table(oceania, "D:\\02統碩\\110上\\QS_THE\\continent\\oceania.csv", sep = ",")

