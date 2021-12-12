### QS 合併, csv檔&欄位 整理
qs_total_2022 = read.csv("..\\..\\QS\\qs_total_2022_不重複.csv")[,2:15]
rank_table_2020 = read.csv("..\\..\\QS\\rank_table_2020.csv")[,2:3]
rank_table_2021 = read.csv("..\\..\\QS\\rank_table_2021.csv")[,2:3]

qs1 = merge(qs_total_2022, rank_table_2020, by.x = "University", by.y = "b", all.x = TRUE)
qs = merge(qs1, rank_table_2021, by.x = "University", by.y = "b", all.x = TRUE)

# a.y 即rank2021 格式
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

# a.x 即rank2020 格式
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

# QS 是否上升
qs$qs_up = ifelse(qs$rise > 0, "+", ifelse(qs$rise < 0, "-", "="))

# QS 數字格式
qs$Total.students = as.numeric(gsub(",", "", qs$Total.students))
qs$International.students = as.numeric(gsub(",", "", qs$International.students))
qs$Total.faculty.staff = as.numeric(gsub(",", "", qs$Total.faculty.staff))
qs$Int.l.staff = as.numeric(qs$Int.l.staff)
qs$Domestic.staff = as.numeric(qs$Domestic.staff)
qs$PG.students = as.numeric(gsub("%", "", qs$PG.students))*0.01
qs$UG.students = as.numeric(gsub("%", "", qs$UG.students))*0.01
qs$PG.students.1 = as.numeric(gsub("%", "", qs$PG.students.1))*0.01
qs$UG.students.1 = as.numeric(gsub("%", "", qs$UG.students.1))*0.01

# 校名 不改
# 刪除不重要的col
qs = subset(qs, select = -c(a.x,a.y))

### THE 合併, csv檔&欄位 整理
the_total_2022 = read.csv("..\\..\\THE\\the_total_2022_不重複.csv")[,2:7]
rank_table_2020 = read.csv("..\\..\\THE\\rank_table_2020.csv")[,2:3]
rank_table_2021 = read.csv("..\\..\\THE\\rank_table_2021.csv")[,2:3]

the1 = merge(the_total_2022, rank_table_2020, by.x = "University", by.y = "b", all.x = TRUE)
the = merge(the1, rank_table_2021, by.x = "University", by.y = "b", all.x = TRUE)

# a.y 即rank2021 格式
the$rank2021 = the$a.y
for (i in 1:length(the$rank2021)){
  if (grepl('–', the$a.y[i])){
    the$rank2021[i] = (sum(as.numeric(strsplit(the$a.y[i],split = '–')[[1]]))-1)/2
  }
  else if(grepl('=', the$a.y[i])){
    the$rank2021[i] = as.numeric(gsub(pattern = '=',replacement = '',x = the$a.y[i]))
  }
  else if(grepl('+', the$a.y[i])){
    the$rank2021[i] = as.numeric(gsub(pattern = '\\+',replacement = '',x = the$a.y[i]))
  }
}
the$rank2021 = as.numeric(the$rank2021)

# a.x 即rank2020 格式
the$rank2020 = the$a.x
for (i in 1:length(the$rank2020)){
  if (grepl('–', the$a.x[i])){
    the$rank2020[i] = (sum(as.numeric(strsplit(the$a.x[i],split = '–')[[1]]))-1)/2
  }
  else if(grepl('=', the$a.x[i])){
    the$rank2020[i] = as.numeric(gsub(pattern = '=',replacement = '',x = the$a.x[i]))
  }
  else if(grepl('+', the$a.x[i])){
    the$rank2020[i] = as.numeric(gsub(pattern = '\\+',replacement = '',x = the$a.x[i]))
  }
}

the$rank2020 = as.numeric(the$rank2020)
the$rank2021 = as.numeric(the$rank2021)
the$rise = the$rank2020 - the$rank2021 #2020 - 2021

# THE 是否上升
the$the_up = ifelse(the$rise > 0, "+", ifelse(the$rise < 0, "-", "="))

# THE 數字格式
the$Total.Students = as.numeric(gsub(",", "", the$Total.Students))
the$student.staff.ratio = as.numeric(the$student.staff.ratio)
the$pc.of.int.l.students = as.numeric(gsub("%", "", the$pc.of.int.l.students))*0.01
for (i in 1:length(the$female.male.ratio)){
  if (grepl(":", the$female.male.ratio[i])){
    the$female.male.ratio[i] = round(as.numeric(strsplit(the$female.male.ratio[i], ":")[[1]][1])/as.numeric(strsplit(the$female.male.ratio[i], ":")[[1]][2]),2)
  }
}

# 校名 不改
# 刪除不重要的col
the = subset(the, select = -c(a.x,a.y))

#### merge
m = merge(the, qs, by.x = "University", by.y = "University")
m = subset(m, select = c(University,Total.students,Total.Students,qs_up,the_up))
colnames(m) = c("University", "qs_students", "the_students", "qs_up", "the_up")
m$diff = m$qs_students - m$the_students
m$pdiff = round(m$diff/((m$qs_students + m$the_students)/2),5)

# calculate outliers
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


# bar chat of outlier
ggplot(data = m[!is.na(m$qs_up) ,], aes(x = qs_up, y = 1, fill = outlier)) + geom_bar(stat = "identity")
ggplot(data = m[!is.na(m$the_up) ,], aes(x = the_up, y = 1, fill = outlier)) + geom_bar(stat = "identity")

group = c('+', '-', '=')

# qs
for (i in 1:3){
  se <- function(x) sqrt(var(x)/length(x))
  cat('\n---group "',group[i],'"---\n')
  cat('Number of university: ',
      sum(m$qs_up == group[i], na.rm = T),'\n',
      'Number of outlier: ',
      sum(m_outlier$qs_up == group[i], na.rm = T),'\n',
      'percentage of outlier in group "',group[i],'": ',
      round(sum(m_outlier$qs_up == group[i], na.rm = T)/sum(m$qs_up == group[i], na.rm = T), 2),
      sep = '')
}

# the
for (i in 1:3){
  se <- function(x) sqrt(var(x)/length(x))
  cat('\n---group "',group[i],'"---\n')
  cat('Number of university: ',
      sum(m$the_up == group[i], na.rm = T),'\n',
      'Number of outlier: ',
      sum(m_outlier$the_up == group[i], na.rm = T),'\n',
      'percentage of outlier in group "',group[i],'": ',
      round(sum(m_outlier$the_up == group[i], na.rm = T)/sum(m$the_up == group[i], na.rm = T), 2),
      sep = '')
}

# compare between each group
ggplot(m_no_outlier[!is.na(m_no_outlier$qs_up),], aes(x = qs_up, y = diff, color = qs_up)) + geom_boxplot()
ggplot(m_no_outlier[!is.na(m_no_outlier$the_up),], aes(x = the_up, y = diff, color = the_up)) + geom_boxplot()


aov.m = aov(diff ~ the_up + qs_up, data = m)
summary(aov.m)

aov.m = aov(diff ~ the_up + qs_up, data = m_no_outlier)
summary(aov.m)

for (i in 1:3){
  se <- function(x) sqrt(var(x, na.rm = T)/sum(!is.na(x)))
  cat('\n---',group[i],'---\n')
  cat('Number of university: ',
      sum(m$qs_up == group[i], na.rm = T),'\n',
      'Mean of "diff": ',
      mean(m$diff[m$qs_up == group[i]], na.rm = T),'\n',
      ifelse(mean(m$diff[m$qs_up == group[i]], na.rm = T) > 0, 'Number of students of qs > the',ifelse(mean(m$diff[m$qs_up == group[i]], na.rm = T) < 0,'Number of students of qs < the','Number of students of qs = the')),'\n',
      'se of "diff": ',
      se(m$diff[m$qs_up == group[i]]),'\n',
      'p-value of t test if diff of "',group[i], '" = 0: ',
      round(t.test(m_no_outlier[m_no_outlier$qs_up == group[i],]$diff, mu = 0)$p.value, 4), ifelse(t.test(m_no_outlier[m_no_outlier$qs_up == group[i],]$diff, mu = 0)$p.value < 0.1, '*', ''),'\n',
      'p-value of t test if pdiff of "',group[i], '" = 0: ',
      round(t.test(m_no_outlier[m_no_outlier$qs_up == group[i],]$pdiff, mu = 0)$p.value, 4), ifelse(t.test(m_no_outlier[m_no_outlier$qs_up == group[i],]$pdiff, mu = 0)$p.value < 0.1, '*', ''),
      sep = '')
}

