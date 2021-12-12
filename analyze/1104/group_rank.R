library(ggplot2)

### QS 讀入， 欄位整理
qs = read.csv("..\\..\\QS\\qs_total_2022_不重複.csv")[,2:15]
qs$Total.students = as.numeric(gsub(",", "", qs$Total.students))
qs$International.students = as.numeric(gsub(",", "", qs$International.students))
qs$Total.faculty.staff = as.numeric(gsub(",", "", qs$Total.faculty.staff))
qs$Int.l.staff = as.numeric(qs$Int.l.staff)
qs$Domestic.staff = as.numeric(qs$Domestic.staff)
qs$PG.students = as.numeric(gsub("%", "", qs$PG.students))*0.01
qs$UG.students = as.numeric(gsub("%", "", qs$UG.students))*0.01
qs$PG.students.1 = as.numeric(gsub("%", "", qs$PG.students.1))*0.01
qs$UG.students.1 = as.numeric(gsub("%", "", qs$UG.students.1))*0.01
colnames(qs) = c('University', 'Total.students',	'PG',	'UG',	'Int.l.students',	'Int.l.PG', 'Int.l.UG', 'Total.staff',	'Int.l.staff',	'Domestic.staff',	'Status',	'Research.Output',	'Size',	'rank')

### THE 讀入， 欄位整理
the = read.csv("..\\..\\THE\\the_total_2022_不重複.csv")[,2:7]
the$Total.Students = as.numeric(gsub(",", "", the$Total.Students))
the$student.staff.ratio = as.numeric(the$student.staff.ratio)
the$pc.of.int.l.students = as.numeric(gsub("%", "", the$pc.of.int.l.students))*0.01
for (i in 1:length(the$female.male.ratio)){
  if (grepl(":", the$female.male.ratio[i])){
    the$female.male.ratio[i] = round(as.numeric(strsplit(the$female.male.ratio[i], ":")[[1]][1])/as.numeric(strsplit(the$female.male.ratio[i], ":")[[1]][2]),2)
  }
}
the = the[,c(2,3,4,5,6,1)]
colnames(the) = c('University', 'Total.Students', 'student.staff.ratio', 'Int.l.students.ratio',	'female.male.ratio', 'rank')

# merge
m = merge(the, qs, by.x = "University", by.y = "University")

# the rank.x 格式
m$the_rank = m$rank.x
for (i in 1:length(m$rank.x)){
  if (grepl('–', m$rank.x[i])){
    m$the_rank[i] = (sum(as.numeric(strsplit(m$rank.x[i],split = '–')[[1]]))-1)/2
  }
  else if(grepl('=', m$rank.x[i])){
    m$the_rank[i] = as.numeric(gsub(pattern = '=',replacement = '',x = m$rank.x[i]))
  }
  else if(grepl('+', m$rank.x[i])){
    m$the_rank[i] = as.numeric(gsub(pattern = '\\+',replacement = '',x = m$rank.x[i]))
  }
}
m$the_rank = as.numeric(m$the_rank)

m$the_grank = ifelse(m$the_rank <= 200, '1 1-200',
              ifelse(m$the_rank <= 400, '2 201-400',
              ifelse(m$the_rank <= 600, '3 401-600',
              ifelse(m$the_rank <= 800, '4 601-800',
              ifelse(m$the_rank <= 1000, '5 801-1000',
              ifelse(m$the_rank <= 1200, '6 1001-1200',
              '7 1200+'))))))

# m table 整理
m = subset(m, select = c(University, Total.students, Total.Students, the_grank))
colnames(m) = c("University", "qs_students", "the_students", "group_rank")
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

ggplot(data = m[!is.na(m$group_rank),], aes(x = group_rank, y = 1, fill = outlier)) + geom_bar(stat = "identity")


group = c('1 1-200', '2 201-400', '3 401-600', '4 601-800',
'5 801-1000', '6 1001-1200', '7 1200+')

for (i in 1:length(group)){
  se <- function(x) sqrt(var(x)/length(x))
  cat('\n---',group[i],'---\n')
  cat('Number of university: ',
      sum(m$group_rank == group[i], na.rm = T),'\n',
      'Number of outlier: ',
      sum(m_outlier$group_rank == group[i], na.rm = T),'\n',
      'percentage of outlier in ',group[i],': ',
      round(sum(m_outlier$group_rank == group[i], na.rm = T)/sum(m$group_rank == group[i], na.rm = T), 2),
      sep = '')
}

# compare between each groups
ggplot(m_no_outlier[m_no_outlier$group_rank != '',], aes(x = group_rank, y = diff, color = group_rank)) + geom_boxplot()

aov.m = aov(diff ~ group_rank, data = m)
summary(aov.m)

aov.m = aov(diff ~ group_rank, data = m_no_outlier)
summary(aov.m)


for (i in 1:length(group)){
  se <- function(x) sqrt(var(x, na.rm = T)/sum(!is.na(x)))
  cat('\n---',group[i],'---\n')
  cat('Number of university: ',
      sum(m$group_rank == group[i], na.rm = T),'\n',
      'Mean of "diff": ',
      mean(m$diff[m$group_rank == group[i]], na.rm = T),'\n',
      ifelse(mean(m$diff[m$group_rank == group[i]], na.rm = T) > 0, 'Number of students of qs > the',ifelse(mean(m$diff[m$group_rank == group[i]], na.rm = T) < 0,'Number of students of qs < the','Number of students of qs = the')),'\n',
      'se of "diff": ',
      se(m$diff[m$group_rank == group[i]]),'\n',
      'p-value of t test if diff of "',group[i], '" = 0: ',
      round(t.test(m_no_outlier[m_no_outlier$group_rank == group[i],]$diff, mu = 0)$p.value, 4), ifelse(t.test(m_no_outlier[m_no_outlier$group_rank == group[i],]$diff, mu = 0)$p.value < 0.1, '*', ''),'\n',
      'p-value of t test if pdiff of "',group[i], '" = 0: ',
      round(t.test(m_no_outlier[m_no_outlier$group_rank == group[i],]$pdiff, mu = 0)$p.value, 4), ifelse(t.test(m_no_outlier[m_no_outlier$group_rank == group[i],]$pdiff, mu = 0)$p.value < 0.1, '*', ''),
      sep = '')
}


