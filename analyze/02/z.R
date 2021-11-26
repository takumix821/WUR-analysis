### qs
qs = read.csv("D:\\02参河\\110\\QS_THE\\QS\\qs_total_2022_ぃ狡.csv")[,2:15]
qs = subset(qs, select = c(University, Total.students, rank))

# qs 计Α
qs$Total.students = as.numeric(gsub(",", "", qs$Total.students))

### the
the = read.csv("D:\\02参河\\110\\QS_THE\\THE\\the_total_2022_ぃ狡.csv")[,2:7]
the = subset(the, select = c(University, Total.Students, rank))

# the 计Α
the$Total.Students = as.numeric(gsub(",", "", the$Total.Students))

# qs z
qsmean = mean(qs$Total.students)
qsstd = sd(qs$Total.students)

qs$z.students = (qs$Total.students - qsmean)/qsstd

# the z
themean = mean(the$Total.Students)
thestd = sd(the$Total.Students)

the$z.students = (the$Total.Students - themean)/thestd

# qs rank
qs$ranking.students = rank(qs$Total.students)

# the rank
the$ranking.students = rank(the$Total.Students)

# merge
m = merge(the, qs, by.x = "University", by.y = "University")
m = subset(m, select = c(University, Total.students, z.students.y, ranking.students.y, rank.y, Total.Students, z.students.x, ranking.students.x, rank.x))
colnames(m) = c("University", "qs_students", "qs_z", "qs_ranking", "qs_rank", "the_students", "the_z", "the_ranking", "the_rank")

m$z_distance = m$the_z - m$qs_z
m$ranking_distance = m$the_ranking - m$qs_ranking
m$diff = m$the_students - m$qs_students
m$pdiff = round(m$diff/((m$qs_students +m$the_students)/2),5)
m_outlier = m[(abs(m$pdiff) > 0.2) | (abs(m$diff) >1000),]
m_no_outlier = m[(abs(m$pdiff) < 0.4) & (abs(m$diff) <10000),]

library(ggplot2)
library(gridExtra)
my.plot = ggplot(m, aes(diff, ranking_distance))
my.plot = my.plot + geom_point(size = 0.5) +
  labs(y = "transformed by ranking")
my.plot2 = ggplot(m, aes(diff, z_distance))
my.plot2 = my.plot2 + geom_point(size = 0.5) +
  labs(y = "transformed by z score")
grid.arrange(my.plot, my.plot2, nrow = 1, top = "'diff' of students number and 'diff' of transformed students number")

qs.plot = ggplot(m_no_outlier, aes(qs_ranking, the_ranking, colour = abs(diff)))
qs.plot = qs.plot + geom_point(size = 0.5)
the.plot = ggplot(m_no_outlier, aes(qs_z, the_z, colour = abs(diff)))
the.plot = the.plot + geom_point(size = 0.5)
grid.arrange(qs.plot, the.plot, nrow = 1, top = "comparison of transformed students number from 2 institutes")

