dados=ts(Dados_AT_exp[,2:5], start = c(2008,3), end = c(2018,9), frequency = 12)

plot(Dados_AT_exp$ATX)
plot(Dados_AT_exp$juros)
plot(Dados_AT_exp$cambio)
plot(Dados_AT_exp$pib_eua)

ATX = diff(Dados_AT_exp$ATX)
juros = diff(Dados_AT_exp$juros)
cambio = diff(Dados_AT_exp$cambio)
pib_eua = Dados_AT_exp$pib_eua[2:128]

n=length(dados[,1])

#Modelos log-log

loglog_0def = lm(log(ATX) ~ log(juros) + log(cambio) + pib_eua, data=dados)

loglog_3def = lm(log(ATX[4:n]) ~ log(juros[1:(n-3)]) + log(juros[2:(n-2)]) + log(juros[3:(n-1)]) + log(juros[4:n]) + 
                   log(cambio[1:(n-3)]) + log(cambio[2:(n-2)]) + log(cambio[3:(n-1)]) + log(cambio[4:n]) + 
                   pib_eua[1:(n-3)] + pib_eua[2:(n-2)] + pib_eua[3:(n-1)] + pib_eua[4:n], data = dados)


loglog_6def= lm(log(ATX[7:n]) ~ log(juros[1:(n-6)]) + log(juros[2:(n-5)]) + log(juros[3:(n-4)]) + log(juros[4:(n-3)]) + log(juros[5:(n-2)]) + log(juros[6:(n-1)]) + log(juros[7:n]) +
                  log(cambio[1:(n-6)]) + log(cambio[2:(n-5)]) + log(cambio[3:(n-4)]) + log(cambio[4:(n-3)]) + log(cambio[5:(n-2)]) + log(cambio[6:(n-1)]) + log(cambio[7:n]) +
                  pib_eua[1:(n-6)] + pib_eua[2:(n-5)] + pib_eua[3:(n-4)] + pib_eua[4:(n-3)] + pib_eua[5:(n-2)] + pib_eua[6:(n-1)] + pib_eua[7:n], data = dados)

loglog_12def = lm(log(ATX[13:n]) ~ log(juros[1:(n-12)]) + log(juros[2:(n-11)]) + log(juros[3:(n-10)]) + log(juros[4:(n-9)]) + log(juros[5:(n-8)]) + log(juros[6:(n-7)]) + log(juros[7:(n-6)]) + log(juros[8:(n-5)]) + log(juros[9:(n-4)]) + log(juros[10:(n-3)]) + log(juros[11:(n-2)]) + log(juros[12:(n-1)]) + log(juros[13:n]) +
                    log(cambio[1:(n-12)]) + log(cambio[2:(n-11)]) + log(cambio[3:(n-10)]) + log(cambio[4:(n-9)]) + log(cambio[5:(n-8)]) + log(cambio[6:(n-7)]) + log(cambio[7:(n-6)]) + log(cambio[8:(n-5)]) + log(cambio[9:(n-4)]) + log(cambio[10:(n-3)]) + log(cambio[11:(n-2)]) + log(cambio[12:(n-1)]) + log(cambio[13:n]) +
                    pib_eua[1:(n-12)] + pib_eua[2:(n-11)] + pib_eua[3:(n-10)] + pib_eua[4:(n-9)] + pib_eua[5:(n-8)] + pib_eua[6:(n-7)] + pib_eua[7:(n-6)] + pib_eua[8:(n-5)] + pib_eua[9:(n-4)] + pib_eua[10:(n-3)] + pib_eua[11:(n-2)] + pib_eua[12:(n-1)] + pib_eua[13:n], data = dados)

#Modelos log-lin


loglin_0def = lm(log(ATX) ~ juros + cambio + pib_eua, data=dados)

loglin_3def = lm(log(ATX[4:n]) ~ juros[1:(n-3)] + juros[2:(n-2)] + juros[3:(n-1)] + juros[4:n] + 
                   cambio[1:(n-3)] + cambio[2:(n-2)] + cambio[3:(n-1)] + cambio[4:n] + 
                   pib_eua[1:(n-3)] + pib_eua[2:(n-2)] + pib_eua[3:(n-1)] + pib_eua[4:n], data = dados)


loglin_6def= lm(log(ATX[7:n]) ~ juros[1:(n-6)] + juros[2:(n-5)] + juros[3:(n-4)] + juros[4:(n-3)] + juros[5:(n-2)] + juros[6:(n-1)] + juros[7:n] +
                  cambio[1:(n-6)] + cambio[2:(n-5)] + cambio[3:(n-4)] + cambio[4:(n-3)] + cambio[5:(n-2)] + cambio[6:(n-1)] + cambio[7:n] +
                  pib_eua[1:(n-6)] + pib_eua[2:(n-5)] + pib_eua[3:(n-4)] + pib_eua[4:(n-3)] + pib_eua[5:(n-2)] + pib_eua[6:(n-1)] + pib_eua[7:n], data = dados)

loglin_12def = lm(log(ATX[13:n]) ~ juros[1:(n-12)] + juros[2:(n-11)] + juros[3:(n-10)] + juros[4:(n-9)] + juros[5:(n-8)] + juros[6:(n-7)] + juros[7:(n-6)] + juros[8:(n-5)] + juros[9:(n-4)] + juros[10:(n-3)] + juros[11:(n-2)] + juros[12:(n-1)] + juros[13:n] +
                    cambio[1:(n-12)] + cambio[2:(n-11)] + cambio[3:(n-10)] + cambio[4:(n-9)] + cambio[5:(n-8)] + cambio[6:(n-7)] + cambio[7:(n-6)] + cambio[8:(n-5)] + cambio[9:(n-4)] + cambio[10:(n-3)] + cambio[11:(n-2)] + cambio[12:(n-1)] + cambio[13:n] +
                    pib_eua[1:(n-12)] + pib_eua[2:(n-11)] + pib_eua[3:(n-10)] + pib_eua[4:(n-9)] + pib_eua[5:(n-8)] + pib_eua[6:(n-7)] + pib_eua[7:(n-6)] + pib_eua[8:(n-5)] + pib_eua[9:(n-4)] + pib_eua[10:(n-3)] + pib_eua[11:(n-2)] + pib_eua[12:(n-1)] + pib_eua[13:n], data = dados)


#Modelos lin-lin


linlin_0def = lm(ATX ~ juros + cambio + pib_eua, data=dados)

linlin_3def = lm(ATX[4:n] ~ juros[1:(n-3)] + juros[2:(n-2)] + juros[3:(n-1)] + juros[4:n] + 
                   cambio[1:(n-3)] + cambio[2:(n-2)] + cambio[3:(n-1)] + cambio[4:n] + 
                   pib_eua[1:(n-3)] + pib_eua[2:(n-2)] + pib_eua[3:(n-1)] + pib_eua[4:n], data = dados)

linlin_6def= lm(ATX[7:n] ~ juros[1:(n-6)] + juros[2:(n-5)] + juros[3:(n-4)] + juros[4:(n-3)] + juros[5:(n-2)] + juros[6:(n-1)] + juros[7:n] +
                  cambio[1:(n-6)] + cambio[2:(n-5)] + cambio[3:(n-4)] + cambio[4:(n-3)] + cambio[5:(n-2)] + cambio[6:(n-1)] + cambio[7:n] +
                  pib_eua[1:(n-6)] + pib_eua[2:(n-5)] + pib_eua[3:(n-4)] + pib_eua[4:(n-3)] + pib_eua[5:(n-2)] + pib_eua[6:(n-1)] + pib_eua[7:n], data = dados)

linlin_12def = lm(ATX[13:n] ~ juros[1:(n-12)] + juros[2:(n-11)] + juros[3:(n-10)] + juros[4:(n-9)] + juros[5:(n-8)] + juros[6:(n-7)] + juros[7:(n-6)] + juros[8:(n-5)] + juros[9:(n-4)] + juros[10:(n-3)] + juros[11:(n-2)] + juros[12:(n-1)] + juros[13:n] +
                    cambio[1:(n-12)] + cambio[2:(n-11)] + cambio[3:(n-10)] + cambio[4:(n-9)] + cambio[5:(n-8)] + cambio[6:(n-7)] + cambio[7:(n-6)] + cambio[8:(n-5)] + cambio[9:(n-4)] + cambio[10:(n-3)] + cambio[11:(n-2)] + cambio[12:(n-1)] + cambio[13:n] +
                    pib_eua[1:(n-12)] + pib_eua[2:(n-11)] + pib_eua[3:(n-10)] + pib_eua[4:(n-9)] + pib_eua[5:(n-8)] + pib_eua[6:(n-7)] + pib_eua[7:(n-6)] + pib_eua[8:(n-5)] + pib_eua[9:(n-4)] + pib_eua[10:(n-3)] + pib_eua[11:(n-2)] + pib_eua[12:(n-1)] + pib_eua[13:n], data = dados)

AIC(loglog_0def)
AIC(loglog_3def)
AIC(loglog_6def)
AIC(loglog_12def)


AIC(loglin_0def)
AIC(loglin_3def)
AIC(loglin_6def)
AIC(loglin_12def)


AIC(linlin_0def)
AIC(linlin_3def)
AIC(linlin_6def)
AIC(linlin_12def)



summary(loglog_0def)
summary(loglog_3def)
summary(loglog_6def)
summary(loglog_12def)


summary(loglin_0def)
summary(loglin_3def)
summary(loglin_6def)
summary(loglin_12def)


summary(linlin_0def)
summary(linlin_3def)
summary(linlin_6def)
summary(linlin_12def)


#Teste Normalidade

#install.packages("olsrr")
library(olsrr)
ols_plot_resid_hist(loglog_12def)
ols_test_normality(loglog_12def) 

#Teste Heterocedasticidade

#install.packages("lmtest")
library(lmtest)
bptest(loglog_12def) 

#Teste Multicolinearidade
#install.packages("car")
library(car)
vif(loglog_12def)

#Determinando nível AR:

res_loglog_12def = resid(loglog_12def)
acf(res_loglog_12def)
pacf(res_loglog_12def)

#Teste de Autocorrelação

bgtest(loglog_12def, order = 3)



#Redução de Multicolinearidade de loglog_12def

loglog_12def_mod = lm(log(ATX[13:n]) ~ log(juros[1:(n-12)])  + log(juros[7:(n-6)])  + log(juros[10:(n-3)]) + log(juros[13:n]) +
                     log(cambio[1:(n-12)]) + log(cambio[7:(n-6)]) + log(cambio[10:(n-3)]) + log(cambio[13:n]) +
                     pib_eua[1:(n-12)] + pib_eua[7:(n-6)] + pib_eua[10:(n-3)] + pib_eua[13:n], data = dados)

vif(loglog_12def_mod)

#Verificando o ajuste de loglog_12def_mod

AIC(loglog_12def_mod) 

#Verificando normalidade dos resíduos de loglog_12def_mod

ols_plot_resid_hist(loglog_12def_mod)
ols_test_normality(loglog_12def_mod)

#Verificando heterocedasticidade em loglog_12def_mod

bptest(loglog_12def_mod)

#Determinando nível AR de loglog_12_def_mod:

res_loglog_12def_mod = resid(loglog_12def_mod)
acf(res_loglog_12def_mod)
pacf(res_loglog_12def_mod)

#Teste de Autocorrelação de loglog_12def_mod

bgtest(loglog_12def_mod, order = 2)

summary(loglog_12def_mod)


#Redução de Multicolinearidade de loglog_12def_mod


loglog_12def_mod2 = lm(log(ATX[13:n]) ~ log(juros[1:(n-12)]) + log(juros[13:n]) + 
                     log(cambio[1:(n-12)]) + log(cambio[13:n]) + 
                     pib_eua[1:(n-12)] + pib_eua[7:(n-6)] + pib_eua[10:(n-3)] + pib_eua[13:n], data = dados)

vif(loglog_12def_mod2)

#Verificando o ajuste de loglog_12def_mod2

AIC(loglog_12def_mod2) 

#Verificando normalidade dos resíduos de loglog_12def_mod2

ols_plot_resid_hist(loglog_12def_mod2)
ols_test_normality(loglog_12def_mod2) 

#Verificando heterocedasticidade em loglog_12def_mod2

bptest(loglog_12def_mod2) 

#Determinando nível AR de loglog_12_def_mod2:

res_loglog_12def_mod2 = resid(loglog_12def_mod2)
acf(res_loglog_12def_mod2)
pacf(res_loglog_12def_mod2)

#Teste de Autocorrelação de loglog_12def_mod2

bgtest(loglog_12def_mod2, order = 1)

summary(loglog_12def_mod2)



