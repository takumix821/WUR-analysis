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

### merge
m = merge(the, qs, by.x = "University", by.y = "University")

# continent
m$continent = rep("", dim(m)[1])

oceania = read.csv("..\\..\\continent\\oceania.csv")
north_america = read.csv("..\\..\\continent\\north_america.csv")
latin_america = read.csv("..\\..\\continent\\latin_america.csv")
europe = read.csv("..\\..\\continent\\europe.csv")
asia = read.csv("..\\..\\continent\\asia.csv")
africa = read.csv("..\\..\\continent\\africa.csv")

for (i in 1:dim(m)[1]){
  if (sum(m$University[i] == oceania) > 0){
    m$continent[i] = "oceania"
  }
  if (sum(m$University[i] == north_america) > 0){
    m$continent[i] = "north_america"
  }
  if (sum(m$University[i] == latin_america) > 0){
    m$continent[i] = "latin_america"
  }
  if (sum(m$University[i] == europe) > 0){
    m$continent[i] = "europe"
  }
  if (sum(m$University[i] == asia) > 0){
    m$continent[i] = "asia"
  }
  if (sum(m$University[i] == africa) > 0){
    m$continent[i] = "africa"
  }
}

m = subset(m, select = c(University, Total.students, Total.Students, continent))
colnames(m) = c("University", "qs_students", "the_students", "continent")
m$diff = m$qs_students - m$the_students
m$pdiff = round(m$diff/((m$qs_students + m$the_students)/2),5)

continent = c("oceania", "north_america", "latin_america",
              "europe", "asia", "africa")

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
ggplot(data = m[m$continent != '',], aes(x = continent, y = 1, fill = outlier)) + geom_bar(stat = "identity")

for (i in 1:length(continent)){
  se <- function(x) sqrt(var(x)/length(x))
  cat('\n---',continent[i],'---\n')
  cat('Number of university: ',
      sum(m$continent == continent[i]),'\n',
      'Number of outlier: ',
      sum(m_outlier$continent == continent[i]),'\n',
      'percentage of outlier in ',continent[i],': ',
      round(sum(m_outlier$continent == continent[i])/sum(m$continent == continent[i]), 2),
      sep = '')
}

# compare between each continents
ggplot(m_no_outlier[m_no_outlier$continent != '',], aes(x = continent, y = diff, color = continent)) + geom_boxplot()

aov.m = aov(diff ~ continent, data = m)
summary(aov.m)

aov.m = aov(diff ~ continent, data = m_no_outlier)
summary(aov.m)

for (i in 1:length(continent)){
  se <- function(x) sqrt(var(x)/length(x))
  cat('\n---',continent[i],'---\n')
  cat('Number of university: ',
      sum(m$continent == continent[i]),'\n',
      'Mean of "diff": ',
      mean(m$diff[m$continent == continent[i]]),'\n',
      ifelse(mean(m$diff[m$continent == continent[i]]) > 0, 'Number of students of qs > the',ifelse(mean(m$diff[m$continent == continent[i]]) < 0,'Number of students of qs < the','Number of students of qs = the')),'\n',
      'se of "diff": ',
      se(m$diff[m$continent == continent[i]]),'\n',
      'p-value of t test if diff of ',continent[i], ' = 0: ',
      round(t.test(m_no_outlier[m_no_outlier$continent == continent[i],]$diff, mu = 0)$p.value, 4), ifelse(t.test(m_no_outlier[m_no_outlier$continent == continent[i],]$diff, mu = 0)$p.value < 0.1, '*', ''),'\n',
      'p-value of t test if pdiff of ',continent[i], ' = 0: ',
      round(t.test(m_no_outlier[m_no_outlier$continent == continent[i],]$pdiff, mu = 0)$p.value, 4), ifelse(t.test(m_no_outlier[m_no_outlier$continent == continent[i],]$pdiff, mu = 0)$p.value < 0.1, '*', ''),
      sep = '')
}

