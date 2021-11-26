### qs
qs = read.csv("D:\\02参河\\110\\QS_THE\\QS\\qs_total_2022_ぃ狡.csv")[,2:15]

# qs 计Α
qs$Total.students = as.numeric(gsub(",", "", qs$Total.students))
qs$International.students = as.numeric(gsub(",", "", qs$International.students))
qs$Total.faculty.staff = as.numeric(gsub(",", "", qs$Total.faculty.staff))
qs$Int.l.staff = as.numeric(qs$Int.l.staff)
qs$Domestic.staff = as.numeric(qs$Domestic.staff)
qs$PG.students = as.numeric(gsub("%", "", qs$PG.students))*0.01
qs$UG.students = as.numeric(gsub("%", "", qs$UG.students))*0.01
qs$PG.students.1 = as.numeric(gsub("%", "", qs$PG.students.1))*0.01
qs$UG.students.1 = as.numeric(gsub("%", "", qs$UG.students.1))*0.01


### the
the = read.csv("D:\\02参河\\110\\QS_THE\\THE\\the_total_2022_ぃ狡.csv")[,2:7]

# the 计Α
the$Total.Students = as.numeric(gsub(",", "", the$Total.Students))
the$student.staff.ratio = as.numeric(the$student.staff.ratio)
the$pc.of.int.l.students = as.numeric(gsub("%", "", the$pc.of.int.l.students))*0.01
for (i in 1:length(the$female.male.ratio)){
  if (grepl(":", the$female.male.ratio[i])){
    the$female.male.ratio[i] = round(as.numeric(strsplit(the$female.male.ratio[i], ":")[[1]][1])/as.numeric(strsplit(the$female.male.ratio[i], ":")[[1]][2]),2)
  }
}

# merge
m = merge(the, qs, by.x = "University", by.y = "University")

# the rank.x Α
m$the_rank = m$rank.x
for (i in 1:length(m$rank.x)){
  if (grepl('', m$rank.x[i])){
    m$the_rank[i] = (sum(as.numeric(strsplit(m$rank.x[i],split = '')[[1]]))-1)/2
  }
  else if(grepl('=', m$rank.x[i])){
    m$the_rank[i] = as.numeric(gsub(pattern = '=',replacement = '',x = m$rank.x[i]))
  }
  else if(grepl('+', m$rank.x[i])){
    m$the_rank[i] = as.numeric(gsub(pattern = '\\+',replacement = '',x = m$rank.x[i]))
  }
}
m$the_rank = as.numeric(m$the_rank)

m$the_grank = ifelse(m$the_rank <= 200, '1-200',
              ifelse(m$the_rank <= 400, '201-400',
              ifelse(m$the_rank <= 600, '401-600',
              ifelse(m$the_rank <= 800, '601-800',
              ifelse(m$the_rank <= 1000, '801-1000',
              ifelse(m$the_rank <= 1200, '1001-1200',
              '1200+'))))))


m = subset(m, select = c(University, Total.students, Total.Students, the_grank))
colnames(m) = c("University", "qs_students", "the_students", "group_rank")
m$diff = m$qs_students - m$the_students
m$pdiff = round(m$diff/((m$qs_students +m$the_students)/2),5)

boxplot(diff ~ group_rank, data = m)

aov.m = aov(diff ~ group_rank, data = m)
summary(aov.m)
