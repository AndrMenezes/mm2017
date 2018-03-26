
# Definições gerais -------------------------------------------------------
# setwd('C:/Users/User/Dropbox/4° Série/Modelos Mistos/Trabalho') 
setwd('C:/Users/André Felipe/Dropbox/4° Série/Modelos Mistos/Trabalho') 

rm(list = ls(all.names = TRUE))

bib <- c('lme4', 'lmerTest', 'lsmeans', 'hnp', 'dplyr', 'ggplot2', 'RLRsim', 'nlme')
sapply(bib, require, character.only = T)

tema <- theme(text = element_text(size=20), panel.grid.minor = element_blank(), 
              panel.grid.major = element_line(size = 1.2))

# Leitura dos dados -------------------------------------------------------
dadosbosta         <- read.delim('laranja.txt', header = T)
idplanta           <- rep(1:80, each = 4)
dadosbosta$arvore  <- rep(idplanta, 5)
dadosbosta         <- dadosbosta %>% select(avaliacao, trat, arvore, diametro)
head(dadosbosta)

dados           <- aggregate(diametro ~ avaliacao + trat + arvore, data = dadosbosta, mean)
dados$avaliacao <- with(dados, ifelse(avaliacao == 2, 7, ifelse(avaliacao == 3, 14, ifelse(avaliacao == 4, 21, ifelse(avaliacao == 5, 28, 0)))))
dados           <- dados %>% rename(tempo_n = avaliacao)
dados$tempo_f   <- factor(dados$tempo_n)
dados$arvore    <- factor(dados$arvore)
str(dados)


# Análise descritiva ------------------------------------------------------
x11()
dados %>% ggplot(aes(x=tempo_n, y=diametro, color = arvore, group = arvore)) +
  stat_summary(fun.y = mean, geom="point", size = 2, shape = 15) +
  stat_summary(fun.y = mean, geom="line") +
  scale_x_continuous(breaks = unique(dados$tempo_n), labels = c("0", "7", "14", "21", "28")) +
  xlab("Dias") + ylab("Diâmetro(mm)") + theme_bw() + tema

dados %>% ggplot(aes(x=tempo_n, y=diametro, color = arvore, group = arvore)) +
  facet_wrap(~trat)+
  stat_summary(fun.y = mean, geom="point", size = 2, shape = 15) +
  stat_summary(fun.y = mean, geom="line") +
  scale_x_continuous(breaks = unique(dados$tempo_n), labels = c("0", "7", "14", "21", "28")) +
  xlab("Dias") + ylab("Diâmetro(mm)") + theme_bw() + tema

x11()
dados %>% ggplot(aes(x=tempo_n, y=diametro)) +
  facet_wrap(~arvore)+ geom_point() + geom_smooth(method = 'lm', se = F) +
  scale_x_continuous(breaks = unique(dados$tempo_n), labels = c("0", "7", "14", "21", "28")) +
  xlab("Dias") + ylab("Diâmetro(mm)") + theme_bw() + tema

dados %>% ggplot(aes(x=tempo_n, y=diametro)) +
  facet_wrap(~arvore)+ geom_point(aes(color = trat)) + geom_smooth(method = 'lm', se = F) +
  scale_x_continuous(breaks = unique(dados$tempo_n), labels = c("0", "7", "14", "21", "28")) +
  xlab("Dias") + ylab("Diâmetro(mm)") + theme_bw() + tema


boxplot(diametro ~ tempo_f, data = dados, col = 'gray')
boxplot(diametro ~ trat, data = dados, col = 'gray')


# Modelos -----------------------------------------------------------------
m1 <- diametro ~ trat + tempo_f + trat * tempo_f 
m2 <- diametro ~ trat + tempo_f + trat * tempo_f + (1 | arvore) 
m3 <- diametro ~ trat + tempo_f + trat + (1 | arvore) 


# Ajuste modelo efeitos fixos ---------------------------------------------
fit1 <- lm(diametro ~ trat + tempo_f + trat * tempo_f , data = dados)
anova(fit1); #summary(fit1)

# Ajuste modelos efeitos fixos e aleatórios  ------------------------------
fit2 <- lme(fixed = diametro ~ trat + tempo_f + trat * tempo_f, data = dados, 
            random = ~ 1 | arvore)
anova(fit2); #summary(fit2)

fit3 <- lme(fixed = diametro ~ trat + tempo_f, data = dados, random = ~ 1 | arvore)
anova(fit3); #summary(fit3)

# Comparação de modelos ---------------------------------------------------
anova(fit2, fit1) # rejeita H0

anova(fit3, fit1) # rejeita H0

anova(fit3, fit2) # não rejeita H0

# escolhe o modelo fit3 (modelo sem interação e com efeito aleatório de arvore)

# Análise das estruturas de covariâncias ----------------------------------
fit.CS   <- lme(fixed = diametro ~ trat + tempo_f, data = dados, 
                random = ~ 1 | arvore, correlation = corCompSymm())
fit.AR1  <- lme(fixed = diametro ~ trat + tempo_f, data = dados, 
                random = ~ 1 | arvore, correlation = 
                  corAR1(form = ~ as.numeric(tempo_f)|arvore))
fit.Gaus <- lme(fixed = diametro ~ trat + tempo_f, data = dados, 
                random = ~ 1 | arvore, correlation = 
                  corGaus(form = ~ as.numeric(tempo_f)|arvore, nugget = T))
summary(fit.Gaus)




t <- lme(fixed = diametro ~ trat + avaliacao + trat:avaliacao, data = dados, 
         random = ~1|bloco, correlation = corAR1())
t <- lme(fixed = diametro ~ trat + avaliacao + trat:avaliacao, data = dados, 
         random = ~1|bloco, correlation = corGaus(form = ~ avaliacao|bloco))
summary(t)
VarCorr(t)
getVarCov(t)


library(influence.ME)
model3.infl <- influence(model3, obs=TRUE)
par(mfrow=c(1,1))
plot(cooks.distance(model3.infl))
