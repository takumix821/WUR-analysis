### QS讀入
qs = read.csv("D:\\02統碩\\110上\\QS_THE\\QS\\qs_total_2022_不重複.csv")[,2:15]
qs = subset(qs, select = c(University, Total.students, rank))

# QS 數字格式
qs$Total.students = as.numeric(gsub(",", "", qs$Total.students))

### THE讀入
the = read.csv("D:\\02統碩\\110上\\QS_THE\\THE\\the_total_2022_不重複.csv")[,2:7]
the = subset(the, select = c(University, Total.Students, rank))

# THE 數字格式
the$Total.Students = as.numeric(gsub(",", "", the$Total.Students))

# QS 的z score
qsmean = mean(qs$Total.students)
qsstd = sd(qs$Total.students)

qs$z.students = (qs$Total.students - qsmean)/qsstd

# THE 的z score
themean = mean(the$Total.Students)
thestd = sd(the$Total.Students)

the$z.students = (the$Total.Students - themean)/thestd

# QS 的ranking
qs$ranking.students = rank(qs$Total.students)

# THE 的ranking
the$ranking.students = rank(the$Total.Students)

# merge
m = merge(the, qs, by.x = "University", by.y = "University")
m = subset(m, select = c(University, Total.students, z.students.y, ranking.students.y, rank.y, Total.Students, z.students.x, ranking.students.x, rank.x))
colnames(m) = c("University", "qs_students", "qs_z", "qs_ranking", "qs_rank", "the_students", "the_z", "the_ranking", "the_rank")

## 兩家機構的transformed資料差異
m$z_distance = m$qs_z - m$the_z
m$ranking_distance = m$qs_ranking - m$the_ranking
m$diff = m$qs_students - m$the_students
m$pdiff = round(m$diff/((m$qs_students +　m$the_students)/2),5)

# outlier
meandif = mean(m$diff)
meanpdif = mean(m$pdiff)
stddif = sd(m$diff)
stdpdif = sd(m$pdiff)

m_outlier = m[(m$diff > meandif + 1.5*stddif) |
                (m$diff < meandif - 1.5*stddif) |
                (m$pdiff > meanpdif + 1.5*stdpdif) |
                (m$pdiff < meanpdif - 1.5*stdpdif),]
m_no_outlier = m[!((m$diff > meandif + 1.5*stddif) |
                     (m$diff < meandif - 1.5*stddif) |
                     (m$pdiff > meanpdif + 1.5*stdpdif) |
                     (m$pdiff < meanpdif - 1.5*stdpdif)),]

for (i in 1:length(m$pdiff)){
  if (((m$diff[i] > meandif + 1.5*stddif) |
       (m$diff[i] < meandif - 1.5*stddif) |
       (m$pdiff[i] > meanpdif + 1.5*stdpdif) |
       (m$pdiff[i] < meanpdif - 1.5*stdpdif)) & m$diff[i] > 0){
    m$outlier[i] = "outlier & student qs > the"
  }
  else if (((m$diff[i] > meandif + 1.5*stddif) |
            (m$diff[i] < meandif - 1.5*stddif) |
            (m$pdiff[i] > meanpdif + 1.5*stdpdif) |
            (m$pdiff[i] < meanpdif - 1.5*stdpdif)) & m$diff[i] <= 0){
    m$outlier[i] = "outlier & student qs < the"
  }
  else{
    m$outlier[i] = "not outlier"
  }
}

library(ggplot2)
library(gridExtra)
qs.plot = ggplot(m, aes(qs_ranking, the_ranking))
qs.plot = qs.plot + geom_point(size = 0.5)
the.plot = ggplot(m, aes(qs_z, the_z))
the.plot = the.plot + geom_point(size = 0.5)
grid.arrange(qs.plot, the.plot, nrow = 1, top = "comparison of transformed students number from 2 institutes")


my.plot = ggplot(m, aes(diff, ranking_distance))
my.plot = my.plot + geom_point(size = 0.5) +
  labs(y = "transformed by ranking")
my.plot2 = ggplot(m, aes(diff, z_distance))
my.plot2 = my.plot2 + geom_point(size = 0.5) +
  labs(y = "transformed by z score")
grid.arrange(my.plot, my.plot2, nrow = 1, top = "'diff' of students number and 'diff' of transformed students number")

## transformed資料和原始資料相關係數
corr.r = cor.test(x=m$ranking_distance , y=m$diff, method = 'pearson')
print(corr.r)

corr.z = cor.test(x=m$z_distance , y=m$diff, method = 'pearson')
print(corr.z)

