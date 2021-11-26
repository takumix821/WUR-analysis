### qs
qs_total_2022 = read.csv("D:\\02参河\\110\\QS_THE\\QS\\qs_total_2022_ぃ狡.csv")[,2:15]
rank_table_2020 = read.csv("D:\\02参河\\110\\QS_THE\\QS\\rank_table_2020.csv")[,2:3]
rank_table_2021 = read.csv("D:\\02参河\\110\\QS_THE\\QS\\rank_table_2021.csv")[,2:3]

qs1 = merge(qs_total_2022, rank_table_2020, by.x = "University", by.y = "b", all.x = TRUE)
qs = merge(qs1, rank_table_2021, by.x = "University", by.y = "b", all.x = TRUE)

# a.y rank2021 Α
qs$rank2021 = qs$a.y
for (i in 1:length(qs$rank2021)){
  if (grepl('-', qs$a.y[i])){
    qs$rank2021[i] = (sum(as.numeric(strsplit(qs$a.y[i],split = '-')[[1]]))-1)/2
  }
  else if(grepl('=', qs$a.y[i])){
    qs$rank2021[i] = as.numeric(gsub(pattern = '=',replacement = '',x = qs$a.y[i]))
  }
  else if(grepl('+', qs$a.y[i])){
  qs$rank2021[i] = as.numeric(gsub(pattern = '\\+',replacement = '',x = qs$a.y[i]))
  }
}
qs$rank2021 = as.numeric(qs$rank2021)

# a.x rank2020 Α
qs$rank2020 = qs$a.x
for (i in 1:length(qs$rank2020)){
  if (grepl('-', qs$a.x[i])){
    qs$rank2020[i] = (sum(as.numeric(strsplit(qs$a.x[i],split = '-')[[1]]))-1)/2
  }
  else if(grepl('=', qs$a.x[i])){
    qs$rank2020[i] = as.numeric(gsub(pattern = '=',replacement = '',x = qs$a.x[i]))
  }
  else if(grepl('+', qs$a.x[i])){
    qs$rank2020[i] = as.numeric(gsub(pattern = '\\+',replacement = '',x = qs$a.x[i]))
  }
}
qs$rank2020 = as.numeric(qs$rank2020)

qs$rank2021 = as.numeric(qs$rank2021)
qs$rank2020 = as.numeric(qs$rank2020)
qs$rise = qs$rank2020 - qs$rank2021 #2020 - 2021

# qs 琌ど
qs$qs_up = ifelse(qs$rise > 0, "+", ifelse(qs$rise < 0, "-", "="))

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

# qs ぃ笆
# qs 埃ぃ璶col
qs = subset(qs, select = -c(a.x,a.y))

### the
the_total_2022 = read.csv("D:\\02参河\\110\\QS_THE\\THE\\the_total_2022_ぃ狡.csv")[,2:7]
rank_table_2020 = read.csv("D:\\02参河\\110\\QS_THE\\THE\\rank_table_2020.csv")[,2:3]
rank_table_2021 = read.csv("D:\\02参河\\110\\QS_THE\\THE\\rank_table_2021.csv")[,2:3]

the1 = merge(the_total_2022, rank_table_2020, by.x = "University", by.y = "b", all.x = TRUE)
the = merge(the1, rank_table_2021, by.x = "University", by.y = "b", all.x = TRUE)


# a.y rank2021 Α
the$rank2021 = the$a.y
for (i in 1:length(the$rank2021)){
  if (grepl('', the$a.y[i])){
    the$rank2021[i] = (sum(as.numeric(strsplit(the$a.y[i],split = '')[[1]]))-1)/2
  }
  else if(grepl('=', the$a.y[i])){
    the$rank2021[i] = as.numeric(gsub(pattern = '=',replacement = '',x = the$a.y[i]))
  }
  else if(grepl('+', the$a.y[i])){
    the$rank2021[i] = as.numeric(gsub(pattern = '\\+',replacement = '',x = the$a.y[i]))
  }
}
the$rank2021 = as.numeric(the$rank2021)

# a.x rank2020 Α
the$rank2020 = the$a.x
for (i in 1:length(the$rank2020)){
  if (grepl('', the$a.x[i])){
    the$rank2020[i] = (sum(as.numeric(strsplit(the$a.x[i],split = '')[[1]]))-1)/2
  }
  else if(grepl('=', the$a.x[i])){
    the$rank2020[i] = as.numeric(gsub(pattern = '=',replacement = '',x = the$a.x[i]))
  }
  else if(grepl('+', the$a.x[i])){
    the$rank2020[i] = as.numeric(gsub(pattern = '\\+',replacement = '',x = the$a.x[i]))
  }
}
the$rank2020 = as.numeric(the$rank2020)

the$rank2020 = as.numeric(the$rank2020)
the$rank2021 = as.numeric(the$rank2021)
the$rise = the$rank2020 - the$rank2021 #2020 - 2021

# the 琌ど
the$the_up = ifelse(the$rise > 0, "+", ifelse(the$rise < 0, "-", "="))

# the 计Α
the$Total.Students = as.numeric(gsub(",", "", the$Total.Students))
the$student.staff.ratio = as.numeric(the$student.staff.ratio)
the$pc.of.int.l.students = as.numeric(gsub("%", "", the$pc.of.int.l.students))*0.01
for (i in 1:length(the$female.male.ratio)){
  if (grepl(":", the$female.male.ratio[i])){
    the$female.male.ratio[i] = round(as.numeric(strsplit(the$female.male.ratio[i], ":")[[1]][1])/as.numeric(strsplit(the$female.male.ratio[i], ":")[[1]][2]),2)
  }
}

# the  ぃэ
# the 埃ぃ璶col
the = subset(the, select = -c(a.x,a.y))

### ㄖ
m = merge(the, qs, by.x = "University", by.y = "University")
m = subset(m, select = c(University,Total.students,Total.Students,qs_up,the_up))
colnames(m) = c("University", "qs_students", "the_students", "qs_up", "the_up")
m$diff = m$qs_students - m$the_students

# 琩outlier
m_outlier = m[abs(m$diff) >1000,]

# outlier
m_not_outlier = m[abs(m$diff) <=10000,]

# 1 疭肛眎outlier na
m_not_na = m_not_outlier[!is.na(m_not_outlier$qs_up) & !is.na(m_not_outlier$the_up),]
# 2 玂痙┮Τ戈 na
#m_not_na = m[!is.na(m$qs_up) & !is.na(m$the_up),]

boxplot(diff~qs_up, data = m_not_na)
boxplot(diff~the_up, data = m_not_na)

aov.m = aov(diff ~ the_up + qs_up, data = m_not_na)
summary(aov.m)
aov.m = aov(diff ~ the_up, data = m_not_na)
summary(aov.m)
