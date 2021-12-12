library(dplyr)
qs_total_2022 = read.csv("D:\\02参河\\110\\QS_THE\\QS\\qs_total_2022.csv")[,2:15]

qs_total_2022 <- qs_total_2022 %>% group_by(University) %>% filter (! duplicated(University))

write.csv(qs_total_2022, "D:\\02参河\\110\\QS_THE\\QS\\qs_total_2022_ぃ狡.csv")

the_total_2022 = read.csv("D:\\02参河\\110\\QS_THE\\THE\\the_total_2022.csv")[,2:7]

the_total_2022 <- the_total_2022 %>% group_by(University) %>% filter (! duplicated(University))

write.csv(the_total_2022, "D:\\02参河\\110\\QS_THE\\THE\\the_total_2022_ぃ狡.csv")
