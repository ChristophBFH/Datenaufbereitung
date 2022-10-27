# Libraries ----
library(psych) # for describe()
library(dplyr) # to use the pipe %>%
library(readxl) #to read xlsx

# load data ----
df <- read_excel("C:/Users/cgolz/Berner Fachhochschule/PhDGolz - Privat - Privat/article 5/Analyse/df.xlsx")

df_1003 <- read_excel("C:/Users/cgolz/Berner Fachhochschule/L.011249-42-PFLW Präsentismus Gesamtprojekt - Ordner_BFH - Ordner_BFH/03_AP3_Datenanalyse/02_T0/1003/1003_T0_GESAMT.xlsx")


# read.csv()

# define NA ----
summary(df_1003$chron_krank_1) # -99 ist gemäss Codebook als Missing definiert
df_1003$chron_krank_1[df_1003$chron_krank_1==-99]<-NA
summary(df_1003$chron_krank_1) # Nun hat es 777 Missings

# messniveau ----
str(df_1003$ausbildung)
df_1003$ausbildung<- factor(df_1003$ausbildung, levels = c(1,2), labels = c("nein", "ja")) #was ist 1. im Codebook definiert und wie 2. wurde es programmiert im Tool?
str(df_1003$ausbildung)
table(df_1003$ausbildung)

# descriptiv ----
describe(df) # zeigt alle Werte pro Variable an
describe(df[c("age","sex")]) # ausgewählte Variablen
hist(df$age)
boxplot(df_1003$cq_zufried1)

# neue variable berehnen ----
df_1003$cq_anf_quan<-rowMeans(subset(df_1003, select = c(cq_anf_quan1,cq_anf_quan2,cq_anf_quan3)),na.rm = TRUE)
describe(df_1003$cq_anf_quan)

# Recode ----
#Kultur Leader
df_1003$kultur_leader1[df_1003$kultur_leader1==1]<-0
df_1003$kultur_leader1[df_1003$kultur_leader1==2]<-1
df_1003$kultur_leader1[df_1003$kultur_leader1==3]<-2
df_1003$kultur_leader1[df_1003$kultur_leader1==4]<-3
df_1003$kultur_leader1[df_1003$kultur_leader1==5]<-4
df_1003$kultur_leader1[df_1003$kultur_leader1==6]<-5
df_1003$kultur_leader1[df_1003$kultur_leader1==7]<-6
df_1003$kultur_leader1[df_1003$kultur_leader1==8]<-7
