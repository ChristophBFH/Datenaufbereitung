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

# Annahmen ----
scatterplotMatrix(~ GES_technostress+GSC_cq_LZK_aufgberu+GSC_cq_LZK_aufgstell+GSC_cq_LZK_burn+GSC_cq_LZK_zufrid+GSC_EQ5D_Gesundheitszust+GSC_nx_Schlaf+GSC_vKorff_KSZ_metr+GSC_WAI_Gesamt_Score+GES_technostress,
                  data=df.reg.subset.imput, smooth=FALSE, regLine=FALSE, ellipse=TRUE,
                  diagonal=FALSE, legend=list(coords="bottomleft"))


shapiro.test(df.reg.subset.imput$GES_technostress) #test for normal distribution = NOT


qq.ts<-lm(GES_technostress~GSC_cq_LZK_burn, data = df.reg.subset.imput) #QQ-Plot
plot(qq.ts)


plot(ecdf(qq.ts[['residuals']])) #CDF

vif(lm(hpq_absolutPraesentismus~ .,data = df.hpq_absolut_praesentismus.def1)) #for VIF Test

ncvTest(lm(hpq_absolutPraesentismus~ .,data = df.hpq_absolut_praesentismus.def2)) # for heteroscedastcity tests. If below 0.05 P value, we have heteroscedasticity

# ROW ID hinzufügen
df1<-tibble::rowid_to_column(df1)

#Unique ID from string
with(dfn, match(StringID, unique(StringID)))  
#https://stackoverflow.com/questions/48881942/add-numeric-id-corresponding-with-a-string-id-in-r

# Mache aus ä ein ae
a <- c("ä", "ae") #small a with colon
A <- c("Ä", "Ae") #big A with colon
u <- c("ü", "ue") #small u with colon
U <- c("Ü", "Ue") #big U with colon
o <- c("ö", "oe") #small o with colon
O <- c("Ö", "Oe") #big O with colon
m <- cbind(a,A,u,U,o,O, deparse.level=0)
swiss2 <- function(m, data){
  for (i in seq_len(ncol(m))){
    data <- gsub(paste(m[,i], sep=",")[1],paste(m[,i], sep=",")[2], data,
                 ignore.case=F, perl = F, fixed = F, useBytes = F)
  }
  data
}

#This runs the function from above
df1<-swiss2(m, df$text)
