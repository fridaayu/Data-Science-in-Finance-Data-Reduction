#input data set
library(openxlsx)
df <- read.xlsx("https://academy.dqlab.id/dataset/dqlab_pcadata.xlsx", sheet="3varb")
View(df)

#Standardized Variables
df <- scale(df, center = TRUE,scale = TRUE)
head(df,3)

#calculate correlation matrix
cormat <- cor(df)
cormat

#calculate eigen value and eigen vector
eigen <- eigen(cormat)
eigen

#determine the number of principal component
round(eigen$values/ncol(df),3)
round(cumsum(eigen$values/ncol(df)),3)
pr.out <- prcomp(df,scale. = TRUE,center = TRUE)
summary(pr.out)
library(factoextra)
fviz_eig(pr.out,addlabels = TRUE)
screeplot(pr.out,type = 'line')
abline(h=1,lty=3,col='red')
pr.out$rotation
biplot(pr.out,scale = 0)

#Calculate new score
head(df)
df_new <- df %*% pr.out$rotation
df_new[1:6,1:2]
a <- prcomp(df_new,rank. = 2)
summary(a)
?prcomp

#Test
library(openxlsx)

#Baca data pada sheet "3varb" dalam file https://academy.dqlab.id/dataset/dqlab_pcadata.xlsx
#dan simpan data dengan nama df_raw
df_raw <- read.xlsx("https://academy.dqlab.id/dataset/dqlab_pcadata.xlsx", sheet = "3varb")

#Tampilkan struktur data
str(df_raw)

#Tampilkan beberapa baris observasi dengan fungsi head()
head(df_raw)

#Lakukan analisa PCA dengan fungsi prcomp()
#simpan output dengan nama pr.out
pr.out <- prcomp(df_raw, center = TRUE, scale = TRUE, retx = TRUE)

#Tampilkan komponen output fungsi prcomp()
names(pr.out)

#Tampilkan output PCA
pr.out

#Tampilkan summary dari output PCA
summary(pr.out)

#Gambarkan scree plot
#Tambahkan garis horizontal sebagai panduan untuk menggunakan kriteria Kaiser
screeplot(pr.out, type = "line")
abline(h = 1, col = "red", lty = 3)

#Gambarkan biplot dengan menggunakan fungsi biplot()
biplot(pr.out, scale = 0)

