library(readxl)
library(tseries)
library(rugarch)
library(rmgarch)
library(ccgarch2)
library(zoo)
library(stats)
library(moments)
library(WeightedPortTest)

# Data bencana alam

cat = read_xlsx("~/Tugas Akhir/Referensi/Dampak bencana alam terhadap volatilitas saham perusahaan PnC di Australia/australia cat.xlsx")
cat$CAT_Event_Start = as.Date(cat$CAT_Event_Start)

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

# Uji Asumsi Data
# Uji stasioneritas data
adf.test(r_p) # Stasioner
adf.test(r_asx) #Stasioner

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
VARselect(data.frame(r_p,r_asx), lag.max = 10) # Berdasarkan AIC orde lag 1
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

dis_p = data.frame(y1$time,residu1) # Data residu VAR unruk model DCC-GARCH
dis_p = read.zoo(dis_p)
dis_asx = data.frame(y2$time, residu2)
dis_asx = read.zoo(dis_asx)
regressor = read_excel("~/Tugas Akhir/Referensi/Dampak bencana alam terhadap volatilitas saham perusahaan PnC di Australia/Variabel dummy.xlsx")
str(regressor) # Variabel dummy untuk hipotesis volatilitas
regressor$Date = as.Date(regressor$Date)

# Pembagian data
# Bencana 1
dis_1 = window(dis_p, start = "2014-09-09", end = "2015-01-18")
dasx.1 = window(dis_asx, start = "2014-09-09", end = "2015-01-18")
plot.zoo(dis_1, ylim = c(-0.05,0.05))
abline(v = cat$CAT_Event_Start[1])
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
Box.test(residu1.1^2, lag = 1, type = "Ljung-Box")
Box.test(residu1.2^2, lag = 1, type = "Ljung-Box")
Weighted.LM.test(dis_1,sigma(modelfit_1)[,1] , lag = 3, fitdf = 2)
Weighted.LM.test(dasx.1, sigma(modelfit_1)[,2], lag = 3, fitdf = 2)
h1 = read.zoo(data.frame(index(dis_1),modelfit_1@model[["sigma"]][,1]*sqrt(250)))
h1 = window(h1, start = "2014-11-01", end = "2015-12-01")
plot.zoo(h1,xlab = "", ylab = "h(t)", type = "l", main = "Badai Es Brisbane",
         ylim = c(0,0.8))
abline(v = cat$CAT_Event_Start[1])

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

# Bencana 2
dis_2 = window(dis_p, start = "2014-12-08", end = "2015-04-09")
dasx.2 = window(dis_asx, start = "2014-12-08", end = "2015-04-09")
plot.zoo(dis_2, ylim = c(-0.05,0.05))
abline(v = cat$CAT_Event_Start[2])
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
Box.test(residu2.1^2, lag = 1, type = "Ljung-Box")
Box.test(residu2.2^2, lag = 1, type = "Ljung-Box")
Weighted.LM.test(dis_2, sigma(modelfit_2)[,1], lag = 3, fitdf = 2)
Weighted.LM.test(dasx.2, sigma(modelfit_2)[,2], lag = 3, fitdf = 2)
h2 = modelfit_2@model[["sigma"]][,1]*sqrt(250)
plot(x = index(dis_2),y = h2,xlab = "", ylab = "h(t)", type = "l", 
     main = "Siklon Tropis Marcia ", ylim = c(0,0.8))
abline(v = cat$CAT_Event_Start[2])

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

# Bencana 3
dis_3 = window(dis_p, start = "2015-02-21", end = "2015-07-07")
dasx.3 = window(dis_asx, start = "2015-02-21", end = "2015-07-07")
plot.zoo(dis_3, ylim = c(-0.05,0.05))
abline(v = cat$CAT_Event_Start[3])
adf.test(dis_3) # stasioner
arch.test(ts(dis_3),arch = "Lm" ,alpha = 0.05,lag = 5) # efek ARCH
arch.test(ts(dasx.3),arch = "Lm" ,alpha = 0.05,lag = 5) 
Box.test(dis_3, lag = 10, type = "Ljung-Box") # Terdapat autokorelasi pada lag 6
reg3 = data.frame(regressor$Date, regressor$`x_3 (5)`)
reg3 = read.zoo(reg3)
reg3 = window(reg3, start = "2015-02-21", end = "2015-07-07")
reg3 = matrix(reg3)
reg3.1 = data.frame(regressor$Date, regressor$`x_3 (15)`)
reg3.1 = read.zoo(reg3.1)
reg3.1 = window(reg3.1, start = "2015-02-21", end = "2015-07-07")
reg3.1 = matrix(reg3.1)
################
spec_3 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                    variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                    distribution.model = "norm") 
modelspec_3 = dccspec(uspec = multispec(replicate(2,spec_3)), dccOrder = c(1,1), distribution = "mvnorm")
modelfit_3 = dccfit(modelspec_3, data = data.frame(dis_3, dasx.3)) 
modelfit_3 # signifikan
rcor(modelfit_3) # korelasi dinamis
residu3.1 = dis_3/sigma(modelfit_3)[,1]
residu3.2 = dasx.3/sigma(modelfit_3)[,2]
Box.test(residu3.1^2, lag = 1, type = "Ljung-Box")
Box.test(residu3.2^2, lag = 1, type = "Ljung-Box")
Weighted.LM.test(dis_3, sigma(modelfit_3)[,1], lag = 3, fitdf = 2)
Weighted.LM.test(dasx.3, sigma(modelfit_3)[,2], lag = 3, fitdf = 2)
h3 = modelfit_3@model[["sigma"]][,1]*sqrt(250)
plot(x = index(dis_3),y = h3,xlab = "", ylab = "h(t)", type = "l", 
     main = "East Coast Low (2015)")
abline(v = cat$CAT_Event_Start[3])

# Dampak jangka pendek
spec.3 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                    variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                                          external.regressors = reg3),
                    distribution.model = "norm") 
modelspec.3 = dccspec(uspec = multispec(replicate(2,spec.3)), dccOrder = c(1,1), distribution = "mvnorm")
modelfit.3 = dccfit(modelspec.3, data = data.frame(dis_3, dasx.3)) 
modelfit.3 # tidak signifikan

# Dampak jangka panjang
spec.3_1 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                      variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                                            external.regressors = reg3.1),
                      distribution.model = "norm") 
modelspec.3_1 = dccspec(uspec = multispec(replicate(2,spec.3_1)),dccOrder = c(1,1), distribution = "mvnorm")
modelfit.3_1 = dccfit(modelspec.3_1, data = data.frame(dis_3, dasx.3)) 
modelfit.3_1 # signifikan 

# Bencana 4
dis_4 = window(dis_p, start = "2016-04-01", end = "2016-08-22")
dasx.4 = window(dis_asx, start = "2016-04-01", end = "2016-08-22")
plot.zoo(dis_4, ylim = c(-0.05,0.05))
abline(v = cat$CAT_Event_Start[4])
adf.test(dis_4) # stasioner
arch.test(ts(dis_4),arch = "Lm" ,alpha = 0.05,lag = 5) # efek ARCH
arch.test(ts(dasx.4),arch = "Lm" ,alpha = 0.05,lag = 5) 
Box.test(dis_4, lag = 10, type = "Ljung-Box") # Tidak terdapat autokorelasi
reg4 = data.frame(regressor$Date, regressor$`x_4 (5)`)
reg4 = read.zoo(reg4)
reg4 = window(reg4, start = "2016-04-01", end = "2016-08-22")
reg4 = matrix(reg4)
reg4.1 = data.frame(regressor$Date, regressor$`x_4 (15)`)
reg4.1 = read.zoo(reg4.1)
reg4.1 = window(reg4.1, start = "2016-04-01", end = "2016-08-22")
reg4.1 = matrix(reg4.1)
################
spec_4 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                    variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                    distribution.model = "norm") 
modelspec_4 = dccspec(uspec = multispec(replicate(2,spec_4)), dccOrder = c(1,1), distribution = "mvnorm")
modelfit_4 = dccfit(modelspec_4, data = data.frame(dis_4, dasx.4)) 
modelfit_4 # signifikan
rcor(modelfit_4) # korelasi konstan
residu4.1 = dis_4/sigma(modelfit_4)[,1]
residu4.2 = dasx.4/sigma(modelfit_4)[,2]
Box.test(residu4.1^2, lag = 1, type = "Ljung-Box")
Box.test(residu4.2^2, lag = 1, type = "Ljung-Box")
Weighted.LM.test(dis_4, sigma(modelfit_4)[,1], lag = 3, fitdf = 2)
Weighted.LM.test(dasx.4, sigma(modelfit_4)[,2], lag = 3, fitdf = 2)
h4 = modelfit_4@model[["sigma"]][,1]*sqrt(250)
plot(x = index(dis_4),y = h4,xlab = "", ylab = "h(t)", type = "l", 
     main = "East Coast Low (2016)", ylim = c(0,0.8))
abline(v = cat$CAT_Event_Start[4])

# Dampak jangka pendek
spec.4 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                    variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                                          external.regressors = reg4),
                    distribution.model = "norm") 
modelspec.4 = dccspec(uspec = multispec(replicate(2,spec.4)), dccOrder = c(1,1), distribution = "mvnorm")
modelfit.4 = dccfit(modelspec.4, data = data.frame(dis_4, dasx.4)) 
modelfit.4 # signifikan untuk saham portofolio namun koefisien dcc tdk signifikan

# Dampak jangka panjang
spec.4_1 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                      variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                                            external.regressors = reg4.1),
                      distribution.model = "norm") 
modelspec.4_1 = dccspec(uspec = multispec(replicate(2,spec.4_1)),dccOrder = c(1,1), distribution = "mvnorm")
modelfit.4_1 = dccfit(modelspec.4_1, data = data.frame(dis_4, dasx.4))
modelfit.4_1 # signifikan

# Bencana 5
dis_5 = window(dis_p, start = "2016-09-06", end = "2017-01-27")
dasx.5 = window(dis_asx, start = "2016-09-06", end = "2017-01-27")
plot.zoo(dis_5, ylim = c(-0.05,0.08))
abline(v = cat$CAT_Event_Start[5])
adf.test(dis_5) # stasioner
arch.test(ts(dis_5),arch = "Lm" ,alpha = 0.05,lag = 5) # ada efek ARCH
arch.test(ts(dasx.5),arch = "Lm" ,alpha = 0.05,lag = 5) 
Box.test(dis_5, lag = 10, type = "Ljung-Box") # tidak ada autokorelasi
reg5 = data.frame(regressor$Date, regressor$`x_5 (5)`)
reg5 = read.zoo(reg5)
reg5 = window(reg5, start = "2016-09-06", end = "2017-01-27")
reg5 = matrix(reg5)
reg5.1 = data.frame(regressor$Date, regressor$`x_5 (15)`)
reg5.1 = read.zoo(reg5.1)
reg5.1 = window(reg5.1, start = "2016-09-06", end = "2017-01-27")
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
Box.test(residu5.1^2, lag = 1, type = "Ljung-Box")
Box.test(residu5.2^2, lag = 1, type = "Ljung-Box")
Weighted.LM.test(dis_5, sigma(modelfit_5)[,1], lag = 3, fitdf = 2)
Weighted.LM.test(dasx.5, sigma(modelfit_5)[,2], lag = 3, fitdf = 2)
h5 = read.zoo(data.frame(index(dis_5),modelfit_5@model[["sigma"]][,1]*sqrt(250)))
h5 = window(h5, start = "2016-11-01", end = "2016-11-30")
plot.zoo(h5,xlab = "", ylab = "h(t)", type = "l", main = "Badai Es November")
abline(v = cat$CAT_Event_Start[5])

# Dampak jangka pendek
spec.5 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                    variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                                          external.regressors = reg5),
                    distribution.model = "norm") 
modelspec.5 = dccspec(uspec = multispec(replicate(2,spec.5)), dccOrder = c(1,1), distribution = "mvnorm")
modelfit.5 = dccfit(modelspec.5, data = data.frame(dis_5, dasx.5))
modelfit.5 # tidak signifikan

# Dampak jangka panjang
spec.5_1 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                      variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                                            external.regressors = reg5.1),
                      distribution.model = "norm") 
modelspec.5_1 = dccspec(uspec = multispec(replicate(2,spec.5_1)),dccOrder = c(1,1), distribution = "mvnorm")
modelfit.5_1 = dccfit(modelspec.5_1, data = data.frame(dis_5, dasx.5)) # signifikan
modelfit.5_1 # Tidak signifikan

# Bencana 6
dis_6 = window(dis_p, start = "2017-01-06", end = "2017-05-31")
dasx.6 = window(dis_asx, start = "2017-01-06", end = "2017-05-31")
plot.zoo(dis_6, ylim = c(-0.05,0.05))
abline(v = cat$CAT_Event_Start[6])
adf.test(dis_6) # stasioner
arch.test(ts(dis_6),arch = "Lm" ,alpha = 0.05,lag = 5) # ada efek ARCH
arch.test(ts(dasx.6),arch = "Lm" ,alpha = 0.05,lag = 5) 
Box.test(dis_6, lag = 10, type = "Ljung-Box") # tidak terdapat autokorelasi
reg6 = data.frame(regressor$Date, regressor$`x_6 (5)`)
reg6 = read.zoo(reg6)
reg6 = window(reg6, start = "2017-01-06", end = "2017-05-31")
reg6 = matrix(reg6)
reg6.1 = data.frame(regressor$Date, regressor$`x_6 (15)`)
reg6.1 = read.zoo(reg6.1)
reg6.1 = window(reg6.1, start = "2017-01-06", end = "2017-05-31")
reg6.1 = matrix(reg6.1)
################
spec_6 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                    variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                    distribution.model = "norm") 
modelspec_6 = dccspec(uspec = multispec(replicate(2,spec_6)), dccOrder = c(1,1), distribution = "mvnorm")
modelfit_6 = dccfit(modelspec_6, data = data.frame(dis_6, dasx.6)) 
modelfit_6 # signifikan
rcor(modelfit_6) # korelasi dinamis
residu6.1 = dis_6/sigma(modelfit_6)[,1]
residu6.2 = dasx.6/sigma(modelfit_6)[,2]
Box.test(residu6.1^2, lag = 1, type = "Ljung-Box")
Box.test(residu6.2^2, lag = 1, type = "Ljung-Box")
Weighted.LM.test(dis_6, sigma(modelfit_6)[,1], lag = 3, fitdf = 2)
Weighted.LM.test(dasx.6, sigma(modelfit_6)[,2], lag = 3, fitdf = 2)
h6 = modelfit_6@model[["sigma"]][,1]*sqrt(250)
plot(x = index(dis_6),y = h6,xlab = "", ylab = "h(t)", type = "l", 
     main = "Siklon Debbie", ylim = c(0,0.8))
abline(v = cat$CAT_Event_Start[6])

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

# Bencana 7 & 8 (Nama : Bencana akhir 2018)
dis_78 = window(dis_p, start = "2018-11-04", end = "2019-03-28")
dasx.78 = window(dis_asx, start = "2018-11-04", end = "2019-03-28") 
plot.zoo(dis_78, ylim = c(-0.05,0.05))
abline(v = cat$CAT_Event_Start[7])
abline(v = cat$CAT_Event_Start[8])
adf.test(dis_78) # stasioner
arch.test(ts(dis_78),arch = "Lm" ,alpha = 0.05,lag = 5) # ada efek ARCH
arch.test(ts(dasx.78),arch = "Lm" ,alpha = 0.05,lag = 5) 
Box.test(dis_78, lag = 10, type = "Ljung-Box") # tidak Terdapat autokorelasi
reg78 = data.frame(regressor$Date, regressor$`x_7 (5)`, regressor$`x_8 (5)`)
reg78 = read.zoo(reg78)
reg78 = window(reg78, start = "2018-11-04", end = "2019-03-28")
reg78 = matrix(reg78, ncol = 2)
reg78.1 = data.frame(regressor$Date, regressor$`x_7&8 (40)`)
reg78.1 = read.zoo(reg78.1)
reg78.1 = window(reg78.1, start = "2018-11-04", end = "2019-03-28")
reg78.1 = matrix(reg78.1)
reg78.2 = data.frame(regressor$Date, regressor$`x_7&8 (50)`)
reg78.2 = read.zoo(reg78.2)
reg78.2 = window(reg78.2, start = "2018-11-04", end = "2019-03-28")
reg78.2 = matrix(reg78.2)
################
spec_78 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                     variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                     distribution.model = "norm") 
modelspec_78 = dccspec(uspec = multispec(replicate(2,spec_78)), dccOrder = c(2,1), distribution = "mvnorm")
modelfit_78 = dccfit(modelspec_78, data = data.frame(dis_78, dasx.78)) 
modelfit_78 # signifikan
rcor(modelfit_78) # korelasi dinamis
residu78.1 = dis_78/sigma(modelfit_78)[,1]
residu78.2 = dasx.78/sigma(modelfit_78)[,2]
Box.test(residu78.1^2, lag = 1, type = "Ljung-Box")
Box.test(residu78.2^2, lag = 1, type = "Ljung-Box")
Weighted.LM.test(dis_78, sigma(modelfit_78)[,1], lag = 3, fitdf = 2)
Weighted.LM.test(dasx.78, sigma(modelfit_78)[,2], lag = 3, fitdf = 2)
h78 = read.zoo(data.frame(index(dis_78),modelfit_78@model[["sigma"]][,1]*sqrt(250)))
h78 = window(h78, start = "2018-12-01", end = "2019-02-28")
plot.zoo(h78,xlab = "", ylab = "h(t)", type = "l", main = "Akhir 2018")
abline(v = cat$CAT_Event_Start[7])
abline(v = cat$CAT_Event_Start[8])

# Dampak jangka pendek
spec.78 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                    variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                                          external.regressors = reg78),
                    distribution.model = "norm") 
modelspec.78 = dccspec(uspec = multispec(replicate(2,spec.78)), dccOrder = c(2,1), distribution = "mvnorm")
modelfit.78 = dccfit(modelspec.78, data = data.frame(dis_78, dasx.78))
modelfit.78 # tidak signifikan

# Dampak jangka panjang 40 hari
spec.78_1 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                      variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                                            external.regressors = reg78.1),
                      distribution.model = "norm") 
modelspec.78_1 = dccspec(uspec = multispec(replicate(2,spec.78_1)),dccOrder = c(2,1), distribution = "mvnorm")
modelfit.78_1 = dccfit(modelspec.78_1, data = data.frame(dis_78, dasx.78)) # signifikan
modelfit.78_1 # Tidak signifikan

# Dampak jangka panjang 50 hari
spec.78_2 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                       variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                                             external.regressors = reg78.2),
                       distribution.model = "norm") 
modelspec.78_2 = dccspec(uspec = multispec(replicate(2,spec.78_2)),dccOrder = c(2,1), distribution = "mvnorm")
modelfit.78_2 = dccfit(modelspec.78_2, data = data.frame(dis_78, dasx.78)) # signifikan
modelfit.78_2 # Tidak signifikan

# Bencana 9 & 10 (Nama : Bencana November 2019)
dis_9 = window(dis_p, start = "2019-09-09", end = "2020-01-30")
dasx.9 = window(dis_asx, start = "2019-09-09", end = "2020-01-30")
plot.zoo(dis_9, ylim = c(-0.05,0.05))
abline(v = cat$CAT_Event_Start[9:10])
adf.test(dis_9) # stasioner
arch.test(ts(dis_9),arch = "Lm" ,alpha = 0.05,lag = 5) # ada efek ARCH
arch.test(ts(dasx.9),arch = "Lm" ,alpha = 0.05,lag = 5) 
Box.test(dis_9, lag = 10, type = "Ljung-Box") # tidak Terdapat autokorelasi
reg9 = data.frame(regressor$Date, regressor$`x_9 (5)`, regressor$`x_10 (5)`)
reg9 = read.zoo(reg9)
reg9 = window(reg9, start = "2019-09-09", end = "2020-01-30")
reg9 = matrix(reg9, ncol = 2)
reg9.1 = data.frame(regressor$Date, regressor$`x_9&10 (40)`)
reg9.1 = read.zoo(reg9.1)
reg9.1 = window(reg9.1, start = "2019-09-09", end = "2020-01-30")
reg9.1 = matrix(reg9.1)
reg9.2 = data.frame(regressor$Date, regressor$`x_9&10 (50)`)
reg9.2 = read.zoo(reg9.2)
reg9.2 = window(reg9.2,start = "2019-09-09", end = "2020-01-30")
reg9.2 = matrix(reg9.2)
################
spec_9 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                    variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                    distribution.model = "norm") 
modelspec_9 = dccspec(uspec = multispec(replicate(2,spec_9)), dccOrder = c(1,1), distribution = "mvnorm")
modelfit_9 = dccfit(modelspec_9, data = data.frame(dis_9, dasx.9))
modelfit_9 # signifikan
rcor(modelfit_9) # korelasi konstan
residu9.1 = dis_9/sigma(modelfit_9)[,1]
residu9.2 = dasx.9/sigma(modelfit_9)[,2]
Box.test(residu9.1^2, lag = 1, type = "Ljung-Box")
Box.test(residu9.2^2, lag = 1, type = "Ljung-Box")
Weighted.LM.test(dis_9, sigma(modelfit_9)[,1], lag = 3, fitdf = 2)
Weighted.LM.test(dasx.9, sigma(modelfit_9)[,2], lag = 3, fitdf = 2)
h9 = modelfit_9@model[["sigma"]][,1]*sqrt(250)
plot(x = index(dis_9),y = h9,xlab = "", ylab = "h(t)", type = "l", 
     main = "November 2019")
abline(v = cat$CAT_Event_Start[9])
abline(v = cat$CAT_Event_Start[10])

# Dampak jangka pendek
spec.9 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                     variance.model = list(model = "sGARCH", garchOrder = c(1,1),
                                           external.regressors = reg9),
                     distribution.model = "norm") 
modelspec.9 = dccspec(uspec = multispec(replicate(2,spec.9)), dccOrder = c(1,1), distribution = "mvnorm")
modelfit.9 = dccfit(modelspec.9, data = data.frame(dis_9, dasx.9))
modelfit.9 # signifikan (portofolio)

# Dampak jangka panjang 40 hari
spec.9_1 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                       variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                                             external.regressors = reg9.1),
                       distribution.model = "norm") 
modelspec.9_1 = dccspec(uspec = multispec(replicate(2,spec.9_1)),dccOrder = c(1,1), distribution = "mvnorm")
modelfit.9_1 = dccfit(modelspec.9_1, data = data.frame(dis_9, dasx.9)) # signifikan
modelfit.9_1 # signifikan (portofolio)

# Dampak jangka panjang 50 hari
spec.9_2 = ugarchspec(mean.model = list(armaOrder = c(0,0)),
                       variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                                             external.regressors = reg9.2),
                       distribution.model = "norm") 
modelspec.9_2 = dccspec(uspec = multispec(replicate(2,spec.9_2)),dccOrder = c(1,1), distribution = "mvnorm")
modelfit.9_2 = dccfit(modelspec.9_2, data = data.frame(dis_9, dasx.9)) # signifikan
modelfit.9_2 # signifikan (portofolio)

# Hipotesis 2
# Prosedur :
# 1. Membuat plot estimasi korelasi kondisional antar aset dari model DCC
# 2. Melakukan pembatasan interval waktu estimasi korelasi untuk bencana 3 dan bencana Nov 2019
# 3. Interval waktu untuk sebelum bencana terbatas pada [h-44, h-5]
# 4. Interval waktu untuk setelah bencana terbatas pada [h, h+20] dan [h, h+40]
# 5. Uji Jennrich rata2 korelasi sebelum bencana dengan setelah bencana
# 6. Jika p-value < 5% maka menolak H0
# Grafik Korelasi bencana 3
cor.list3 = rcor(modelfit_3)
cor.df3 <- data.frame(do.call(rbind, as.list(cor.list3)))
cor.df3 = cor.df3[!apply(cor.df3 == "1", 1, any), ]
cor.df3 = cor.df3[!duplicated(cor.df3)]
cor.df3 = zoo(cor.df3, order.by = index(dis_3))
plot.zoo(cor.df3, main = "Estimasi Korelasi Kondisional  dari Pengembalian Portofolio
         dan Pasar Saham", xlab = "",ylab = "rho(t)", ylim = c(0,0.85))
abline(v = as.Date(cat$CAT_Event_Start[3]), col = "red")
abline(h = mean(cor.df3), lty = "dashed")

# Grafik Korelasi bencana November 2019
cor.list4 = rcor(modelfit_4)
cor.df4 <- data.frame(do.call(rbind, as.list(cor.list4)))
cor.df4 = cor.df4[!apply(cor.df4 == "1", 1, any), ]
cor.df4 = cor.df4[!duplicated(cor.df4)]
cor.df4 = zoo(cor.df4, order.by = index(dis_4))
plot.zoo(cor.df4, main = "Estimasi Korelasi Kondisional  dari Pengembalian Portofolio
         dan Pasar Saham")
abline(v = as.Date(cat$CAT_Event_Start[4]), col = "red")
abline(h = mean(cor.df4), lty = "dashed")

# Rata-rata korelasi selama interval waktu tertentu
library(psych)
pre_3 = geometric.mean(window(cor.df3, start = "2015-02-24", end = "2015-04-15"))
# Min : 0.2891327, max : 0.6256127
pre_4 = geometric.mean(window(cor.df4, start = "2016-04-01", end = "2016-05-27"))
# Min : 0.5681512, max : 0.7893476
pos3_1 = geometric.mean(window(cor.df3, start = "2015-04-22", end = "2015-05-19"))
# Min : 0.5360446, max : 0.7239616
pos3_2 = geometric.mean(window(cor.df3, start = "2015-04-22", end = "2015-06-17"))
# Min : 0.5360446, max : 0.7776127
pos4_1 = geometric.mean(window(cor.df4, start = "2016-06-03", end = "2016-07-01"))
# Min : 0.5496797, max : 0.8621142
pos4_2 = geometric.mean(window(cor.df4, start = "2016-06-03", end = "2016-07-30"))
# Min : 0.5496797, max : 0.8621142

# Matriks korelasi
r3 = cbind(c(1,pre_3),c(pre_3,1))
r4 = cbind(c(1,pre_4),c(pre_4,1))
r3_1 = cbind(c(1,pos3_1),c(pos3_1,1))
r3_2 = cbind(c(1,pos3_2),c(pos3_2,1))
r4_1 = cbind(c(1,pos4_1),c(pos4_1,1))
r4_2 = cbind(c(1,pos4_2),c(pos4_2,1))

# Uji Jennrich
a = matrix(nrow = 2, ncol = 2)
a[1,1] = cortest.jennrich(r3, r3_1, n1 = 40,n2 = 20)$prob
a[2,1] = cortest.jennrich(r4, r4_1, n1 = 40,n2 = 20)$prob
a[1,2] = cortest.jennrich(r3, r3_2, n1 = 40,n2 = 40)$prob
a[2,2] = cortest.jennrich(r4, r4_2, n1 = 40,n2 = 40)$prob
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
1.96*sigma(vmodelfit)[500,] # 95% selang prediksi
