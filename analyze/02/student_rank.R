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

# qs students rank
qs$r.students = rank(qs$Total.students)

# the students rank
the$r.students = rank(the$Total.Students)

# merge
m = merge(the, qs, by.x = "University", by.y = "University")
m = subset(m, select = c(University, Total.students, r.students.y, rank.y, Total.Students, r.students.x, rank.x))
colnames(m) = c("University", "qs_students", "qs_rstudents", "qs_rank", "the_students", "the_rstudents", "the_rank")

m$r_distance = m$the_rstudents - m$qs_rstudents


