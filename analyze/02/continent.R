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

m$continent = rep("", dim(m)[1])

oceania = read.csv("D:\\02参河\\110\\QS_THE\\continent\\oceania.csv")
north_america = read.csv("D:\\02参河\\110\\QS_THE\\continent\\north_america.csv")
latin_america = read.csv("D:\\02参河\\110\\QS_THE\\continent\\latin_america.csv")
europe = read.csv("D:\\02参河\\110\\QS_THE\\continent\\europe.csv")
asia = read.csv("D:\\02参河\\110\\QS_THE\\continent\\asia.csv")
africa = read.csv("D:\\02参河\\110\\QS_THE\\continent\\africa.csv")

# ㄖ瑆籔厩
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
m$pdiff = round(m$diff/((m$qs_students +m$the_students)/2),5)

# abs > 1000 outlier
m_abs1000 = m[abs(m$diff) >1000,]

# abs > 10000 outlier
m_abs10000 = m[abs(m$diff) >10000,]

# diff > 10000
m_above = m[m$diff >10000,]

# abs > 0.1
m_p_above = m[abs(m$pdiff) > 0.2,]

m_outlier = m[(abs(m$pdiff) > 0.2) | (abs(m$diff) >1000),]

# 1 疭肛眎outlier na
m_not_na = m_not_outlier[m_not_outlier$continent != '',]
# 2 玂痙┮Τ戈 na
#m_not_na = m[m$continent != '',]



continent = c("oceania", "north_america", "latin_america",
              "europe", "asia", "africa")
for (i in 1:length(continent)){
  se <- function(x) sqrt(var(x)/length(x))
  cat('\n---',continent[i],'---\n')
  cat('# of ',
  continent[i],':',
  sum(m$continent == continent[i]),'\n',
  'mean of ',
  continent[i],':',
  mean(m$diff[m$continent == continent[i]]),'\n',
  'se of ',continent[i],':',
  se(m$diff[m$continent == continent[i]]),'\n',
  '# of outlier in ',continent[i],':',
  sum(m_outlier$continent == continent[i]),'\n',
  'p of outlier in ',continent[i],':',
  sum(m_outlier$continent == continent[i])/sum(m$continent == continent[i]),
  sep = '')
}

boxplot(diff ~ continent, data = m)

aov.m = aov(diff ~ continent, data = m)
summary(aov.m)


