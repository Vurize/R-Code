library(readxl)
library(tseries)
library(rugarch)
library(rmgarch)
library(zoo)
library(stats)
library(moments)
library(WeightedPortTest)

# Data bencana alam

cat = read_xlsx("~/Tugas Akhir/Referensi/Dampak bencana alam terhadap volatilitas saham perusahaan PnC di Australia/australia cat.xlsx")
cat$`Tanggal Mulai` = as.Date(cat$`Tanggal Mulai`)

# Data saham dan indeks

s1 = read_xlsx("~/Tugas Akhir/Referensi/Dampak bencana alam terhadap volatilitas saham perusahaan PnC di Australia/Portofolio 1/QBE.xlsx")
s2 = read_xlsx("~/Tugas Akhir/Referensi/Dampak bencana alam terhadap volatilitas saham perusahaan PnC di Australia/Portofolio 1/SUN.xlsx")
s3 = read_xlsx("~/Tugas Akhir/Referensi/Dampak bencana alam terhadap volatilitas saham perusahaan PnC di Australia/Portofolio 1/IAG.xlsx")
asx = read_xlsx("~/Tugas Akhir/Referensi/Dampak bencana alam terhadap volatilitas saham perusahaan PnC di Australia/ASX.xlsx")

s1$Date = as.Date(s1$Date) # 6 Maret 2013 sampai 31 Januari 2020
s2$Date = as.Date(s2$Date)
s3$Date = as.Date(s3$Date)
asx$Date = as.Date(asx$Date)

s1$Close = as.numeric(s1$Close)
s2$Close = as.numeric(s2$Close)
s3$Close = as.numeric(s3$Close)
asx$Close = as.numeric(asx$Close)

s_1 = read.zoo(s1) 
s_2 = read.zoo(s2)
s_3 = read.zoo(s3)
s_asx = read.zoo(asx)


# konversi ke pengembalian saham
r_1 = diff(log(s_1))
r_2 = diff(log(s_2))
r_3 = diff(log(s_3))
r_asx = diff(log(s_asx))

# menyusun portofolio
tot = 22.799*10^9 + 16.807*10^9 + 13.455*10^9 # per 16 juni 2023
w_1 = 22.799*10^9/tot
w_2 = 16.807*10^9/tot
w_3 = 13.455*10^9/tot

r_p = w_1*r_1 + w_2*r_2 + w_3*r_3
plot(r_p)

# Uji stasioneritas data
adf.test(r_p) # Stasioner
adf.test(r_asx) #Stasioner

# ringkasan statistik
## Statistik Q untuk pengembalian kuadrat
Q_p = NULL
Q_asx = NULL
for(i in 1 :10){
  Q_p[i] = Box.test(r_p^2, type = "Ljung-Box", lag = i)[["p.value"]]
  Q_asx[i] = Box.test(r_asx^2, type = "Ljung-Box", lag = i)[["p.value"]]  
}
Q_p # signifikan untuk lag 1-10
Q_asx # signifikan untuk lag 1-10

## Uji LM untuk efek ARCH
detach("package:vars", unload = TRUE)
library(nortsTest)
l_p = NULL
l_asx = NULL
for(i in 1:10){
  l_p[i] = arch.test(ts(r_p^2),arch = "Lm" ,alpha = 0.05,lag.max = i)[["p.value"]]
  l_asx[i] = arch.test(ts(r_asx^2),arch = "Lm" ,alpha = 0.05,lag.max = i)[["p.value"]]
}
l_p # ada efek ARCH hingga 10 lag
l_asx # ada efek lag hingga 10 lag

# Model DCC GARCH
## Estimasi persamaan mean
library(vars)
VARselect(data.frame(r_p,r_asx), lag.max = 10) # Berdasarkan SIC orde lag 1
VAR(data.frame(r_p,r_asx), lag.max = 10, ic = "SC") # parameter pada model VAR(1)
varxfit(data.frame(r_p,r_asx), p = 1) 

# Diagnostik Persamaan mean
residu1 = resid(VAR(data.frame(r_p,r_asx), lag.max = 10, ic = "SC"))[,1]
sd_res1 = residu1/sd(residu1)
residu2 = resid(VAR(data.frame(r_p,r_asx), lag.max = 10, ic = "SC"))[,2]
sd_res2 = residu2/sd(residu2)
q_p = NULL
q_asx = NULL
for(i in 1 : 10){
  q_p[i] = Box.test(sd_res1, lag = i, type = "Ljung-Box")[["p.value"]]
  q_asx[i] = Box.test(sd_res2, lag = i, type = "Ljung-Box")[["p.value"]]
}
q_p # Tidak signifikan -> tidak ada autokorelasi
q_asx # Kesimpulan sama pada baris sebelumnya
detach("package:vars", unload = TRUE)
library(nortsTest)

# Hipotesis 1
# Prosedur :
# 1. Mencari estimasi volatilitas kondisional tahunan 
# 2. Membuat plot hasil langkah pertama
# 3. Uji dampak bencana dengan uji signifikansi terhadap persamaan (5)
# 4. Jika p-value < 5% maka gagal menolak H0

spec = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                  variance.model = list(model = "sGARCH", garchOrder = c(2, 2)),
                  distribution.model = "norm")
modelspec = dccspec(uspec = multispec(replicate(2,spec))
                    ,dccOrder = c(1,2), distribution = "mvnorm")
modelfit = dccfit(modelspec, data = data.frame(residu1, residu2))
# Grafik volatilitas
l = dim(modelfit@model[["modeldata"]][["data"]])[1]
y1 = data.frame("time" = s1$Date[(length(s1$Date)-l+1):length(s1$Date)],
                "estimated volatilities" = sqrt(250)*modelfit@model[["sigma"]][,1])
y2 = data.frame("time" = s1$Date[(length(s1$Date)-l+1):length(s1$Date)],
                "estimated volatilities" = sqrt(250)*modelfit@model[["sigma"]][,2])
plot(y1, type = "l") 
abline(v = as.Date(cat$CAT_Event_Start), col = "red")
abline(v = as.Date(cat$`h+5`), col = "blue")
abline(v = as.Date(cat$`h+15`), col = "black")
# abline(v = as.Date(dis$`Start Date`), col = "red")
# abline(v = as.Date(dis$`h+5`), col = "blue")
# abline(v = as.Date(dis$`h+15`), col = "black")
plot(y2, type = "l")
abline(v = as.Date(cat$CAT_Event_Start), col = "red")
abline(v = as.Date(cat$`h+5`), col = "blue")
abline(v = as.Date(cat$`h+15`), col = "black")
# abline(v = as.Date(dis$`Start Date`), col = "red")
# abline(v = as.Date(dis$`h+5`), col = "blue")
# abline(v = as.Date(dis$`h+15`), col = "black")

dis_p = data.frame(y1$time,residu1)
dis_p = read.zoo(dis_p)
dis_asx = data.frame(y2$time, residu2)
dis_asx = read.zoo(dis_asx)
regressor = read_excel("~/Tugas Akhir/Referensi/Dampak bencana alam terhadap volatilitas saham perusahaan PnC di Australia/Variabel dummy.xlsx")
str(regressor)
regressor$Date = as.Date(regressor$Date)

# Bencana 1 Badai Es Brisbane
dis_1 = window(dis_p, start = "2014-09-09", end = "2015-01-18")
dasx.1 = window(dis_asx, start = "2014-09-09", end = "2015-01-18")
plot.zoo(dis_1, ylim = c(-0.05,0.05))
abline(v = cat$`Tanggal Mulai`[1])
adf.test(dis_1) # stasioner
arch.test(ts(dis_1),arch = "Lm" ,alpha = 0.05,lag = 5) # efek ARCH
arch.test(ts(dasx.1),arch = "Lm" ,alpha = 0.05,lag = 5) # efek ARCH
Box.test(dis_1, lag = 10, type = "Ljung-Box") # Tidak ada autokorelasi
reg1 = data.frame(regressor$Date, regressor$`x_1 (5)`)
reg1 = read.zoo(reg1)
reg1 = window(reg1, start = "2014-09-01", end = "2015-01-31")
reg1 = matrix(reg1)
reg1.1 = data.frame(regressor$Date, regressor$`x_1 (15)`)
reg1.1 = read.zoo(reg1.1)
reg1.1 = window(reg1.1, start = "2014-09-01", end = "2015-01-31")
reg1.1 = matrix(reg1.1)
################
spec_1 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                    variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                    distribution.model = "norm") 
modelspec_1 = dccspec(uspec = multispec(replicate(2,spec_1)),dccOrder = c(1,1), distribution = "mvnorm")
modelfit_1 = dccfit(modelspec_1, data = data.frame(dis_1, dasx.1))
modelfit_1 # koefisien tidak signifikan
rcor(modelfit_1)
residu1.1 = dis_1/sigma(modelfit_1)[,1]
residu1.2 = dasx.1/sigma(modelfit_1)[,2]
for(i in 1:5){
print(Box.test(residu1.1^2, lag = i, type = "Ljung-Box"))
print(Box.test(residu1.2^2, lag = i, type = "Ljung-Box"))
print(Weighted.LM.test(dis_1,sigma(modelfit_1)[,1] , lag = i+2, fitdf = 2))
print(Weighted.LM.test(dasx.1, sigma(modelfit_1)[,2], lag = i+2, fitdf = 2))
}
h1 = read.zoo(data.frame(index(dis_1),modelfit_1@model[["sigma"]][,1]*sqrt(250)))
h1 = window(h1, start = "2014-11-01", end = "2015-12-01")
plot.zoo(h1,xlab = "", ylab = "h(t)", type = "l", main = "Brisbane Hailstorm (2014)",
         ylim = c(0,0.8))
abline(v = cat$`Tanggal Mulai`[1])

# Dampak jangka pendek
spec.1 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                   variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                                         external.regressors = reg1),
                   distribution.model = "norm") 
modelspec.1 = dccspec(uspec = multispec(replicate(2,spec.1)),dccOrder = c(1,1), distribution = "mvnorm")
modelfit.1 = dccfit(modelspec.1, data = data.frame(dis_1, dasx.1))
modelfit.1  # signifikan

# Dampak jangka panjang
spec.1_1 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                    variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                                          external.regressors = reg1.1),
                    distribution.model = "norm") 
modelspec.1_1 = dccspec(uspec = multispec(replicate(2,spec.1_1)),dccOrder = c(1,1), distribution = "mvnorm")
modelfit.1_1 = dccfit(modelspec.1_1, data = data.frame(dis_1, dasx.1)) # signifikan
# Model terbaik untuk bencana 1 adalah DCC(1,1)-GARCH(1,1) 

# Bencana 2 Marcia
dis_2 = window(dis_p, start = "2014-12-08", end = "2015-04-09")
dasx.2 = window(dis_asx, start = "2014-12-08", end = "2015-04-09")
plot.zoo(dis_2, ylim = c(-0.05,0.05))
abline(v = cat$`Tanggal Mulai`[2])
adf.test(dis_2) # Tidak stasioner
arch.test(ts(dis_2),arch = "Lm" ,alpha = 0.05,lag = 5) # Tidak ada efel ARCH
arch.test(ts(dasx.2),arch = "Lm" ,alpha = 0.05,lag = 5) 
Box.test(dis_2, lag = 10, type = "Ljung-Box") # Terdapat autokorelasi
reg2 = data.frame(regressor$Date, regressor$`x_2 (5)`)
reg2 = read.zoo(reg2)
reg2 = window(reg2, start = "2014-12-08", end = "2015-04-09")
reg2 = matrix(reg2)
reg2.1 = data.frame(regressor$Date, regressor$`x_2 (15)`)
reg2.1 = read.zoo(reg2.1)
reg2.1 = window(reg2.1, start = "2014-12-08", end = "2015-04-09")
reg2.1 = matrix(reg2.1)
################
spec_2 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                    variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                    distribution.model = "norm") 
modelspec_2 = dccspec(uspec = multispec(replicate(2,spec_2)),dccOrder = c(1,2), distribution = "mvnorm")
modelfit_2 = dccfit(modelspec_2, data = data.frame(dis_2, dasx.2))
modelfit_2 # koefisien ada signifikan
rcor(modelfit_2) # korelasi dinamis
residu2.1 = dis_2/sigma(modelfit_2)[,1]
residu2.2 = dasx.2/sigma(modelfit_2)[,2]
for(i in 1:5){
print(Box.test(residu2.1^2, lag = i, type = "Ljung-Box"))
print(Box.test(residu2.2^2, lag = i, type = "Ljung-Box"))
print(Weighted.LM.test(dis_2, sigma(modelfit_2)[,1], lag = i+2, fitdf = 2))
print(Weighted.LM.test(dasx.2, sigma(modelfit_2)[,2], lag = i+2, fitdf = 2))
}
h2 = modelfit_2@model[["sigma"]][,1]*sqrt(250)
plot(x = index(dis_2),y = h2,xlab = "", ylab = "h(t)", type = "l", 
     main = "Severe Tropical Cyclone Marcia (2015)", ylim = c(0,0.8))
abline(v = cat$`Tanggal Mulai`[2])

# Dampak jangka pendek
spec.2 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                    variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                                          external.regressors = reg2),
                    distribution.model = "norm") 
modelspec.2 = dccspec(uspec = multispec(replicate(2,spec.2)), dccOrder = c(1,2), distribution = "mvnorm")
modelfit.2 = dccfit(modelspec.2, data = data.frame(dis_2, dasx.2)) 
modelfit.2 # tidak signifikan

# Dampak jangka panjang
spec.2_1 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                      variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                                            external.regressors = reg2.1),
                      distribution.model = "norm") 
modelspec.2_1 = dccspec(uspec = multispec(replicate(2,spec.2_1)),dccOrder = c(1,2), distribution = "mvnorm")
modelfit.2_1 = dccfit(modelspec.2_1, data = data.frame(dis_2, dasx.2)) 
modelfit.2_1 # Tidak signifikan 

# Bencana 3&4 : Bencana April 2015 (ECL & Anzac day)
dis_34 = window(dis_p, start = "2015-02-21", end = "2015-07-07")
dasx.34 = window(dis_asx, start = "2015-02-21", end = "2015-07-07")
plot.zoo(dis_34, ylim = c(-0.05,0.05))
abline(v = cat$`Tanggal Mulai`[3])
abline(v = cat$`Tanggal Mulai`[4])
adf.test(dis_34) # stasioner
arch.test(ts(dis_34),arch = "Lm" ,alpha = 0.05,lag = 5) # efek ARCH
arch.test(ts(dasx.34),arch = "Lm" ,alpha = 0.05,lag = 5) 
Box.test(dis_34, lag = 10, type = "Ljung-Box") # Terdapat autokorelasi pada lag 6
reg34 = data.frame(regressor$Date, regressor$`x_3 (5)`, regressor$`x_4 (5)`)
reg34 = read.zoo(reg34)
reg34 = window(reg34, start = "2015-02-21", end = "2015-07-07")
reg34 = matrix(reg34, ncol = 2)
reg34.1 = data.frame(regressor$Date, regressor$`x_3&4 (40)`)
reg34.1 = read.zoo(reg34.1)
reg34.1 = window(reg34.1, start = "2015-02-21", end = "2015-07-07")
reg34.1 = matrix(reg34.1)
reg34.2 = data.frame(regressor$Date, regressor$`x_3&4 (50)`)
reg34.2 = read.zoo(reg34.2)
reg34.2 = window(reg34.2, start = "2015-02-21", end = "2015-07-07")
reg34.2 = matrix(reg34.2)
################
spec_34 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                    variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                    distribution.model = "norm") 
modelspec_34 = dccspec(uspec = multispec(replicate(2,spec_34)), dccOrder = c(1,1), distribution = "mvnorm")
modelfit_34 = dccfit(modelspec_34, data = data.frame(dis_34, dasx.34)) 
modelfit_34 # signifikan
rcor(modelfit_34) # korelasi dinamis
residu34.1 = dis_34/sigma(modelfit_34)[,1]
residu34.2 = dasx.34/sigma(modelfit_34)[,2]
for(i in 1:5){
print(Box.test(residu34.1^2, lag = i, type = "Ljung-Box"))
print(Box.test(residu34.2^2, lag = i, type = "Ljung-Box"))
print(Weighted.LM.test(dis_34, sigma(modelfit_34)[,1], lag = i+2, fitdf = 2))
print(Weighted.LM.test(dasx.34, sigma(modelfit_34)[,2], lag = i+2, fitdf = 2))
}
h34 = modelfit_34@model[["sigma"]][,1]*sqrt(250)
plot(x = index(dis_34),y = h34,xlab = "", ylab = "h(t)", type = "l", 
     main = "April (2015)", ylim = c(0,0.8))
abline(v = cat$`Tanggal Mulai`[3])
abline(v = cat$`Tanggal Mulai`[4])

# Dampak jangka pendek
spec.3 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                    variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                                          external.regressors = reg34),
                    distribution.model = "norm") 
modelspec.3 = dccspec(uspec = multispec(replicate(2,spec.3)), dccOrder = c(1,1), distribution = "mvnorm")
modelfit.3 = dccfit(modelspec.3, data = data.frame(dis_34, dasx.34)) 
modelfit.3 # tidak signifikan

# Dampak jangka panjang (40 hari)
spec.3_1 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                      variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                                            external.regressors = reg34.1),
                      distribution.model = "norm") 
modelspec.3_1 = dccspec(uspec = multispec(replicate(2,spec.3_1)),dccOrder = c(1,1), distribution = "mvnorm")
modelfit.3_1 = dccfit(modelspec.3_1, data = data.frame(dis_34, dasx.34)) 
modelfit.3_1 # tidak signifikan 

# Dampak jangka panjang (50 hari)
spec.3_2 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                      variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                                            external.regressors = reg34.2),
                      distribution.model = "norm") 
modelspec.3_2 = dccspec(uspec = multispec(replicate(2,spec.3_2)),dccOrder = c(1,1), distribution = "mvnorm")
modelfit.3_2 = dccfit(modelspec.3_2, data = data.frame(dis_34, dasx.34)) 
modelfit.3_2 # signifikan

# Bencana 5 East Coast Low 2016
dis_5 = window(dis_p, start = "2016-04-01", end = "2016-08-22")
dasx.5 = window(dis_asx, start = "2016-04-01", end = "2016-08-22")
plot.zoo(dis_5, ylim = c(-0.05,0.05))
abline(v = cat$`Tanggal Mulai`[5])
adf.test(dis_5) # stasioner
arch.test(ts(dis_5),arch = "Lm" ,alpha = 0.05,lag = 5) # efek ARCH
arch.test(ts(dasx.5),arch = "Lm" ,alpha = 0.05,lag = 5) 
Box.test(dis_5, lag = 10, type = "Ljung-Box") # Tidak terdapat autokorelasi
reg5 = data.frame(regressor$Date, regressor$`x_5 (5)`)
reg5 = read.zoo(reg5)
reg5 = window(reg5, start = "2016-04-01", end = "2016-08-22")
reg5 = matrix(reg5)
reg5.1 = data.frame(regressor$Date, regressor$`x_5 (15)`)
reg5.1 = read.zoo(reg5.1)
reg5.1 = window(reg5.1, start = "2016-04-01", end = "2016-08-22")
reg5.1 = matrix(reg5.1)
################
spec_5 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                    variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                    distribution.model = "norm") 
modelspec_5 = dccspec(uspec = multispec(replicate(2,spec_5)), dccOrder = c(1,1), distribution = "mvnorm")
modelfit_5 = dccfit(modelspec_5, data = data.frame(dis_5, dasx.5)) 
modelfit_5 # signifikan
rcor(modelfit_5) # korelasi konstan
residu5.1 = dis_5/sigma(modelfit_5)[,1]
residu5.2 = dasx.5/sigma(modelfit_5)[,2]
for(i in 1 :5){
print(Box.test(residu5.1^2, lag = i, type = "Ljung-Box"))
print(Box.test(residu5.2^2, lag = i, type = "Ljung-Box"))
print(Weighted.LM.test(dis_5, sigma(modelfit_5)[,1], lag = i+2, fitdf = 2))
print(Weighted.LM.test(dasx.5, sigma(modelfit_5)[,2], lag = i+2, fitdf = 2))
}
h5 = modelfit_5@model[["sigma"]][,2]*sqrt(250)
plot(x = index(dis_5),y = h5,xlab = "", ylab = "h(t)", type = "l", 
     main = "East Coast Low (2016)", ylim = c(0,0.8))
abline(v = cat$`Tanggal Mulai`[5])

# Dampak jangka pendek
spec.5 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                    variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                                          external.regressors = reg5),
                    distribution.model = "norm") 
modelspec.5 = dccspec(uspec = multispec(replicate(2,spec.5)), dccOrder = c(1,1), distribution = "mvnorm")
modelfit.5 = dccfit(modelspec.5, data = data.frame(dis_5, dasx.5)) 
modelfit.5 # signifikan untuk saham portofolio namun koefisien dcc tdk signifikan

# Dampak jangka panjang
spec.5_1 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                      variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                                            external.regressors = reg5.1),
                      distribution.model = "norm") 
modelspec.5_1 = dccspec(uspec = multispec(replicate(2,spec.5_1)),dccOrder = c(1,1), distribution = "mvnorm")
modelfit.5_1 = dccfit(modelspec.5_1, data = data.frame(dis_5, dasx.5))
modelfit.5_1 # signifikan

# Bencana 6 Badai Es November (2016)
dis_6 = window(dis_p, start = "2016-09-06", end = "2017-01-27")
dasx.6 = window(dis_asx, start = "2016-09-06", end = "2017-01-27")
plot.zoo(dis_6, ylim = c(-0.05,0.08))
abline(v = cat$`Tanggal Mulai`[6])
adf.test(dis_6) # stasioner
arch.test(ts(dis_6),arch = "Lm" ,alpha = 0.05,lag = 5) # ada efek ARCH
arch.test(ts(dasx.6),arch = "Lm" ,alpha = 0.05,lag = 5) 
Box.test(dis_6, lag = 10, type = "Ljung-Box") # tidak ada autokorelasi
reg6 = data.frame(regressor$Date, regressor$`x_6 (5)`)
reg6 = read.zoo(reg6)
reg6 = window(reg6, start = "2016-09-06", end = "2017-01-27")
reg6 = matrix(reg6)
reg6.1 = data.frame(regressor$Date, regressor$`x_6 (15)`)
reg6.1 = read.zoo(reg6.1)
reg6.1 = window(reg6.1, start = "2016-09-06", end = "2017-01-27")
reg6.1 = matrix(reg6.1)
################
spec_6 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                    variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                    distribution.model = "norm") 
modelspec_6 = dccspec(uspec = multispec(replicate(2,spec_6)), dccOrder = c(1,1), distribution = "mvnorm")
modelfit_6 = dccfit(modelspec_6, data = data.frame(dis_6, dasx.6)) 
modelfit_6 # signifikan
rcor(modelfit_6) # korelasi konstan
residu6.1 = dis_6/sigma(modelfit_6)[,1]
residu6.2 = dasx.6/sigma(modelfit_6)[,2]
for(i in 1: 5){
print(Box.test(residu6.1^2, lag = i, type = "Ljung-Box"))
print(Box.test(residu6.2^2, lag = i, type = "Ljung-Box"))
print(Weighted.LM.test(dis_6, sigma(modelfit_6)[,1], lag = i+2, fitdf = 2))
print(Weighted.LM.test(dasx.6, sigma(modelfit_6)[,2], lag = i+2, fitdf = 2))
}
h6 = read.zoo(data.frame(index(dis_6),modelfit_6@model[["sigma"]][,1]*sqrt(250)))
h6 = window(h6, start = "2016-11-01", end = "2016-11-30")
plot.zoo(h6,xlab = "", ylab = "h(t)", type = "l", main = "November Hailstorm (2016)",
         ylim = c(0,0.8))
abline(v = cat$`Tanggal Mulai`[6])

# Dampak jangka pendek
spec.6 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                    variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                                          external.regressors = reg6),
                    distribution.model = "norm") 
modelspec.6 = dccspec(uspec = multispec(replicate(2,spec.6)), dccOrder = c(1,1), distribution = "mvnorm")
modelfit.6 = dccfit(modelspec.6, data = data.frame(dis_6, dasx.6))
modelfit.6 # tidak signifikan

# Dampak jangka panjang
spec.6_1 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                      variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                                            external.regressors = reg6.1),
                      distribution.model = "norm") 
modelspec.6_1 = dccspec(uspec = multispec(replicate(2,spec.6_1)),dccOrder = c(1,1), distribution = "mvnorm")
modelfit.6_1 = dccfit(modelspec.6_1, data = data.frame(dis_6, dasx.6)) # signifikan
modelfit.6_1 # Tidak signifikan

# Bencana 7 Siklon Debbie (2017)
dis_7 = window(dis_p, start = "2017-01-06", end = "2017-05-31")
dasx.7 = window(dis_asx, start = "2017-01-06", end = "2017-05-31")
plot.zoo(dis_7, ylim = c(-0.05,0.05))
abline(v = cat$`Tanggal Mulai`[7])
adf.test(dis_7) # stasioner
arch.test(ts(dis_7),arch = "Lm" ,alpha = 0.05,lag = 5) # ada efek ARCH
arch.test(ts(dasx.7),arch = "Lm" ,alpha = 0.05,lag = 5) 
Box.test(dis_7, lag = 10, type = "Ljung-Box") # tidak terdapat autokorelasi
reg7 = data.frame(regressor$Date, regressor$`x_7 (5)`)
reg7 = read.zoo(reg7)
reg7 = window(reg7, start = "2017-01-06", end = "2017-05-31")
reg7 = matrix(reg7)
reg7.1 = data.frame(regressor$Date, regressor$`x_7 (15)`)
reg7.1 = read.zoo(reg7.1)
reg7.1 = window(reg7.1, start = "2017-01-06", end = "2017-05-31")
reg7.1 = matrix(reg7.1)
################
spec_7 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                    variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                    distribution.model = "norm") 
modelspec_7 = dccspec(uspec = multispec(replicate(2,spec_7)), dccOrder = c(1,1), distribution = "mvnorm")
modelfit_7 = dccfit(modelspec_7, data = data.frame(dis_7, dasx.7)) 
modelfit_7 # signifikan
rcor(modelfit_7) # korelasi dinamis
residu7.1 = dis_7/sigma(modelfit_7)[,1]
residu7.2 = dasx.7/sigma(modelfit_7)[,2]
for(i in 1:5){
print(Box.test(residu7.1^2, lag = i, type = "Ljung-Box"))
print(Box.test(residu7.2^2, lag = i, type = "Ljung-Box"))
print(Weighted.LM.test(dis_7, sigma(modelfit_7)[,1], lag = i+2, fitdf = 2))
print(Weighted.LM.test(dasx.7, sigma(modelfit_7)[,2], lag = i+2, fitdf = 2))
}
h7 = modelfit_7@model[["sigma"]][,1]*sqrt(250)
plot(x = index(dis_7),y = h7,xlab = "", ylab = "h(t)", type = "l", 
     main = "Cyclone Debbie (2017)", ylim = c(0,0.8))
abline(v = cat$`Tanggal Mulai`[7])

# Dampak jangka pendek
spec.7 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                    variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                                          external.regressors = reg7),
                    distribution.model = "norm") 
modelspec.7 = dccspec(uspec = multispec(replicate(2,spec.7)), dccOrder = c(1,1), distribution = "mvnorm")
modelfit.7 = dccfit(modelspec.7, data = data.frame(dis_7, dasx.7))
modelfit.7 # tidak signifikan

# Dampak jangka panjang
spec.7_1 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                      variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                                            external.regressors = reg7.1),
                      distribution.model = "norm") 
modelspec.7_1 = dccspec(uspec = multispec(replicate(2,spec.7_1)),dccOrder = c(1,1), distribution = "mvnorm")
modelfit.7_1 = dccfit(modelspec.7_1, data = data.frame(dis_7, dasx.7)) # signifikan
modelfit.7_1 # Tidak signifikan

# Bencana 8 & 9 (Nama : Bencana akhir 2018)
dis_89 = window(dis_p, start = "2018-11-04", end = "2019-03-28")
dasx.89 = window(dis_asx, start = "2018-11-04", end = "2019-03-28") 
plot.zoo(dis_89, ylim = c(-0.05,0.05))
abline(v = cat$`Tanggal Mulai`[8])
abline(v = cat$`Tanggal Mulai`[9])
adf.test(dis_89) # stasioner
arch.test(ts(dis_89),arch = "Lm" ,alpha = 0.05,lag = 5) # ada efek ARCH
arch.test(ts(dasx.89),arch = "Lm" ,alpha = 0.05,lag = 5) 
Box.test(dis_89, lag = 10, type = "Ljung-Box") # tidak Terdapat autokorelasi
reg89 = data.frame(regressor$Date, regressor$`x_8 (5)`, regressor$`x_9 (5)`)
reg89 = read.zoo(reg89)
reg89 = window(reg89, start = "2018-11-04", end = "2019-03-28")
reg89 = matrix(reg89, ncol = 2)
reg89.1 = data.frame(regressor$Date, regressor$`x_8&9 (40)`)
reg89.1 = read.zoo(reg89.1)
reg89.1 = window(reg89.1, start = "2018-11-04", end = "2019-03-28")
reg89.1 = matrix(reg89.1)
reg89.2 = data.frame(regressor$Date, regressor$`x_8&9 (50)`)
reg89.2 = read.zoo(reg89.2)
reg89.2 = window(reg89.2, start = "2018-11-04", end = "2019-03-28")
reg89.2 = matrix(reg89.2)
################
spec_89 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                     variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                     distribution.model = "norm") 
modelspec_89 = dccspec(uspec = multispec(replicate(2,spec_89)), dccOrder = c(2,1), distribution = "mvnorm")
modelfit_89 = dccfit(modelspec_89, data = data.frame(dis_89, dasx.89)) 
modelfit_89 # signifikan
rcor(modelfit_89) # korelasi dinamis
residu89.1 = dis_89/sigma(modelfit_89)[,1]
residu89.2 = dasx.89/sigma(modelfit_89)[,2]
for(i in 1:5){
print(Box.test(residu89.1^2, lag = i, type = "Ljung-Box"))
print(Box.test(residu89.2^2, lag = i, type = "Ljung-Box"))
print(Weighted.LM.test(dis_89, sigma(modelfit_89)[,1], lag = i+2, fitdf = 2))
print(Weighted.LM.test(dasx.89, sigma(modelfit_89)[,2], lag = i+2, fitdf = 2))
}
h89 = read.zoo(data.frame(index(dis_89),modelfit_89@model[["sigma"]][,1]*sqrt(250)))
h89 = window(h89, start = "2018-12-01", end = "2019-02-28")
plot.zoo(h89,xlab = "", ylab = "h(t)", type = "l", main = "End 2018", 
         ylim = c(0,0.8))
abline(v = cat$`Tanggal Mulai`[8])
abline(v = cat$`Tanggal Mulai`[9])

# Dampak jangka pendek
spec.89 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                    variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                                          external.regressors = reg89),
                    distribution.model = "norm") 
modelspec.89 = dccspec(uspec = multispec(replicate(2,spec.89)), dccOrder = c(2,1), distribution = "mvnorm")
modelfit.89 = dccfit(modelspec.89, data = data.frame(dis_89, dasx.89))
modelfit.89 # tidak signifikan

# Dampak jangka panjang 40 hari
spec.89_1 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                      variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                                            external.regressors = reg89.1),
                      distribution.model = "norm") 
modelspec.89_1 = dccspec(uspec = multispec(replicate(2,spec.89_1)),dccOrder = c(2,1), distribution = "mvnorm")
modelfit.89_1 = dccfit(modelspec.89_1, data = data.frame(dis_89, dasx.89)) # signifikan
modelfit.89_1 # Tidak signifikan

# Dampak jangka panjang 50 hari
spec.89_2 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                       variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                                             external.regressors = reg89.2),
                       distribution.model = "norm") 
modelspec.89_2 = dccspec(uspec = multispec(replicate(2,spec.89_2)),dccOrder = c(2,1), distribution = "mvnorm")
modelfit.89_2 = dccfit(modelspec.89_2, data = data.frame(dis_89, dasx.89)) # signifikan
modelfit.89_2 # Tidak signifikan

# Bencana 10 Badai Es 17 Novemn=ber (2019)
dis_10 = window(dis_p, start = "2019-09-09", end = "2020-01-30")
dasx.10 = window(dis_asx, start = "2019-09-09", end = "2020-01-30")
plot.zoo(dis_10, ylim = c(-0.05,0.05))
abline(v = cat$`Tanggal Mulai`[10])
adf.test(dis_10) # stasioner
arch.test(ts(dis_10),arch = "Lm" ,alpha = 0.05,lag = 5) # ada efek ARCH
arch.test(ts(dasx.10),arch = "Lm" ,alpha = 0.05,lag = 5) 
Box.test(dis_10, lag = 10, type = "Ljung-Box") # tidak Terdapat autokorelasi
reg10 = data.frame(regressor$Date, regressor$`x_10 (5)`)
reg10 = read.zoo(reg10)
reg10 = window(reg10, start = "2019-09-09", end = "2020-01-30")
reg10 = matrix(reg10)
reg10.1 = data.frame(regressor$Date, regressor$`x_10 (15)`)
reg10.1 = read.zoo(reg10.1)
reg10.1 = window(reg10.1, start = "2019-09-09", end = "2020-01-30")
reg10.1 = matrix(reg10.1)
################
spec_10 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                    variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                    distribution.model = "norm") 
modelspec_10 = dccspec(uspec = multispec(replicate(2,spec_10)), dccOrder = c(1,1), distribution = "mvnorm")
modelfit_10 = dccfit(modelspec_10, data = data.frame(dis_10, dasx.10))
modelfit_10 # signifikan
rcor(modelfit_10) # korelasi konstan
residu10.1 = dis_10/sigma(modelfit_10)[,1]
residu10.2 = dasx.10/sigma(modelfit_10)[,2]
for(i in 1:5){
print(Box.test(residu10.1^2, lag = i, type = "Ljung-Box"))
print(Box.test(residu10.2^2, lag = i, type = "Ljung-Box"))
print(Weighted.LM.test(dis_10, sigma(modelfit_10)[,1], lag = i+2, fitdf = 2))
print(Weighted.LM.test(dasx.10, sigma(modelfit_10)[,2], lag = i+2, fitdf = 2))
}
h10 = modelfit_10@model[["sigma"]][,1]*sqrt(250)
plot(x = index(dis_10),y = h10,xlab = "", ylab = "h(t)", type = "l", 
     main = "November 17 Hailstorm (2019)", ylim = c(0,0.8))
abline(v = cat$`Tanggal Mulai`[10])

# Dampak jangka pendek
spec.10 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                     variance.model = list(model = "sGARCH", garchOrder = c(1,1),
                                           external.regressors = reg10),
                     distribution.model = "norm") 
modelspec.10 = dccspec(uspec = multispec(replicate(2,spec.10)), dccOrder = c(1,1), distribution = "mvnorm")
modelfit.10 = dccfit(modelspec.10, data = data.frame(dis_10, dasx.10))
modelfit.10 # signifikan (portofolio)

# Dampak jangka panjang
spec.10_1 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                       variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                                             external.regressors = reg10.1),
                       distribution.model = "norm") 
modelspec.10_1 = dccspec(uspec = multispec(replicate(2,spec.10_1)),dccOrder = c(1,1), distribution = "mvnorm")
modelfit.10_1 = dccfit(modelspec.10_1, data = data.frame(dis_10, dasx.10)) # signifikan
modelfit.10_1 # signifikan (portofolio)

# Hipotesis 2
# Prosedur :
# 1. Membuat plot estimasi korelasi kondisional antar aset dari model DCC
# 2. Melakukan pembatasan interval waktu estimasi korelasi untuk bencana 3 dan bencana Nov 2019
# 3. Interval waktu untuk sebelum bencana terbatas pada [h-44, h-5]
# 4. Interval waktu untuk setelah bencana terbatas pada [h, h+20] dan [h, h+40]
# 5. Uji Jennrich rata2 korelasi sebelum bencana dengan setelah bencana
# 6. Jika p-value < 5% maka menolak H0
# Grafik Korelasi bencana 3
cor.list3 = rcor(modelfit_34)
cor.df3 <- data.frame(do.call(rbind, as.list(cor.list3)))
cor.df3 = cor.df3[!apply(cor.df3 == "1", 1, any), ]
cor.df3 = cor.df3[!duplicated(cor.df3)]
cor.df3 = zoo(cor.df3, order.by = index(dis_34))
plot.zoo(cor.df3, main = "Conditional Correlation of Portfolio and the ASX 300 Returns", xlab = "",ylab = "rho(t)", ylim = c(0,0.85))
abline(v = as.Date(cat$`Tanggal Mulai`[3]), col = "red")
abline(v = as.Date(cat$`Tanggal Mulai`[4]), col = "red")
abline(h = mean(cor.df3), lty = "dashed")

# Grafik Korelasi bencana November 2019
# cor.list4 = rcor(modelfit_4)
# cor.df4 <- data.frame(do.call(rbind, as.list(cor.list4)))
# cor.df4 = cor.df4[!apply(cor.df4 == "1", 1, any), ]
# cor.df4 = cor.df4[!duplicated(cor.df4)]
# cor.df4 = zoo(cor.df4, order.by = index(dis_4))
# plot.zoo(cor.df4, main = "Estimasi Korelasi Kondisional  dari Pengembalian Portofolio
#          dan Pasar Saham")
# abline(v = as.Date(cat$CAT_Event_Start[4]), col = "red")
# abline(h = mean(cor.df4), lty = "dashed")

# Rata-rata korelasi selama interval waktu tertentu
library(psych)
pre_3 = geometric.mean(window(cor.df3, start = "2015-02-24", end = "2015-04-15"))
# Min : 0.2891327, max : 0.6256127
#pre_4 = geometric.mean(window(cor.df4, start = "2016-04-01", end = "2016-05-27"))
# Min : 0.5681512, max : 0.7893476
pos3_1 = geometric.mean(window(cor.df3, start = "2015-04-22", end = "2015-05-19"))
# Min : 0.5360446, max : 0.7239616
pos3_2 = geometric.mean(window(cor.df3, start = "2015-04-22", end = "2015-06-17"))
# Min : 0.5360446, max : 0.7776127
#pos4_1 = geometric.mean(window(cor.df4, start = "2016-06-03", end = "2016-07-01"))
# Min : 0.5496797, max : 0.8621142
#pos4_2 = geometric.mean(window(cor.df4, start = "2016-06-03", end = "2016-07-30"))
# Min : 0.5496797, max : 0.8621142

# Matriks korelasi
r3 = cbind(c(1,pre_3),c(pre_3,1))
#r4 = cbind(c(1,pre_4),c(pre_4,1))
r3_1 = cbind(c(1,pos3_1),c(pos3_1,1))
r3_2 = cbind(c(1,pos3_2),c(pos3_2,1))
#r4_1 = cbind(c(1,pos4_1),c(pos4_1,1))
#r4_2 = cbind(c(1,pos4_2),c(pos4_2,1))

# Uji Jennrich
a = matrix(nrow = 2, ncol = 2)
a[1,1] = cortest.jennrich(r3, r3_1, n1 = 40,n2 = 20)$prob
#a[2,1] = cortest.jennrich(r4, r4_1, n1 = 40,n2 = 20)$prob
a[1,2] = cortest.jennrich(r3, r3_2, n1 = 40,n2 = 40)$prob
#a[2,2] = cortest.jennrich(r4, r4_2, n1 = 40,n2 = 40)$prob
a # Menandakan bencana alam tidak memiliki dampak yang signifikan terhadap korelasi
# antara portofolio asuransi dengan pasar saham.
# Bencana 3 paling mendekati signifikan

# Aplikasi VaR untuk hipotesis 2
# Prosedur :
# 1. Melakukan model fitting terhadap bencana-bencana yang signifikan pada 
#    hipotesis 2 dengan periode waktu [h-499,h].
# 2. Melakukan forecast volatilitas kondisional 1 hari ke depan untuk tiap aset.
# 3. Mencari korelasi antar aset pada waktu t+1 untuk model CC dan DCC.
# 4. Cari VaR dengan menggunakan persamaan (13) & (15)
r_p1 = window(r_p, start = "2013-05-29", end = "2015-04-23")
r_asx1 = window(r_asx,start = "2013-05-29", end = "2015-04-23")
vspec = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                   variance.model = list(model = "sGARCH", garchOrder = c(2, 2)),
                   distribution.model = "norm")
vmodelspec = dccspec(uspec = multispec(replicate(2,vspec)),VAR = TRUE, lag.max = 10,
                     lag.criterion = "SC",dccOrder = c(1,2), distribution = "mvnorm")
vmodelfit = dccfit(vmodelspec, data = data.frame(r_p1, r_asx1), out.sample = 1)
vmodelfit # VAR(1), DCC(1,2)-GARCH(2,2)
# mu pada dccfit adalah estimasi dari model VAR
predict = dccforecast(vmodelfit, n.ahead = 1, n.roll = 0)
h = sigma(predict)
h_p = h[1]
h_asx = h[2]
r = rcor(predict) 
r = data.frame(do.call(rbind, as.list(r)))
r = r$X2
vmean = varxfit(data.frame(r_p1,r_asx1)[1:500,1:2], p = 1)
var_pf = 10^6*1.65*sqrt(h_p^2+h_asx^2+2*r[1]*sqrt(h_p^2*h_asx^2))
var_p = 10^6*1.65*sqrt(h_p^2)
var_asx = 10^6*1.65*sqrt(h_asx^2)
1.96*sigma(predict)
