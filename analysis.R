# Definições gerais -------------------------------------------------------
setwd('C:/Users/User/Dropbox/4° Série/Modelos Mistos/Trabalho') 
# setwd('C:/Users/André Felipe/Dropbox/4° Série/Modelos Mistos/Trabalho') 

rm(list = ls(all.names = TRUE))

bib <- c('lme4', 'lmerTest', 'lsmeans', 'hnp', 'dplyr', 'ggplot2', 'RLRsim', 'nlme', 'xtable', 'influence.ME')
sapply(bib, require, character.only = T)


dados <- read.table(file = 'planta-final.txt', sep = ',', header = T)
dados$tempo_f <- factor(dados$tempo_f)
dados$arvore  <- factor(dados$arvore)
head(dados)
str(dados)

# setwd('C:/Users/André Felipe/Dropbox/4° Série/Modelos Mistos/Trabalho/Relatório')
setwd('C:/Users/User/Dropbox/4° Série/Modelos Mistos/Trabalho/Relatório')

# Descritiva --------------------------------------------------------------

pdf(file = "boxplot-trat.pdf", width = 10.5, height = 6.5)
par(mar = c(3.2, 3.2, 1.5, 1.5), cex = 1.6)
boxplot(diametro ~ trat, data = dados,  xlab = '', ylab = '', cex = 0.6, col = 'gray')
mtext("Dose", side = 1, line = 2.0, cex = 1.8)
mtext("Diâmetro (mm)", side = 2, line = 2, cex = 1.8)
graphics.off()

pdf(file = "boxplot-tempo.pdf", width = 10.5, height = 6.5)
par(mar = c(3.2, 3.2, 1.5, 1.5), cex = 1.6)
boxplot(diametro ~ tempo_f, data = dados,  xlab = '', ylab = '', cex = 0.6, col = 'gray')
points(x = unique(dados$tempo_f), y = tapply(dados$diametro, dados$tempo_f, mean), pch = 16, col = 'red', cex = 0.8)
mtext("Dias após a avaliação", side = 1, line = 2.0, cex = 1.8)
mtext("Diâmetro (mm)", side = 2, line = 2, cex = 1.8)
graphics.off()

x11()
dados %>% ggplot(aes(x = tempo_f, y = diametro, group = interaction(tempo_f, trat))) +
  geom_boxplot(aes(fill = factor(trat)), color = 'black') +
  stat_summary(aes(group = 1), fun.y = mean, geom="line", color = 'black') +
  stat_summary(aes(group = 1), fun.y = mean, geom="point", color = 'gold') +
  labs(y = 'Diâmetro (mm)', x = 'Dias após a avaliação', fill = 'Dose: ') +
  theme_bw() +
  theme(text = element_text(size=20), panel.grid.minor = element_blank(), legend.position="top",
        panel.grid.major = element_line(size = 0.4, linetype = 'dotted', colour = 'gray'))
ggsave(filename = 'boxplot-tempo-trat.pdf', width = 9, height = 6)



# Ajuste do modelo --------------------------------------------------------
mod1 <- lme(fixed = diametro ~ trat + tempo_n + trat*tempo_n, data = dados, random = ~ 1 | arvore)
mod2 <- lme(fixed = diametro ~ trat + tempo_n, data = dados, random = ~ 1 | arvore)
mod3 <- lme(fixed = diametro ~ tempo_n, data = dados, random = ~ 1 | arvore)
anova(mod1, mod2, mod3)
2 * (logLik(mod2) - logLik(mod1))

print(xtable(anova(mod2), digits = 4))


# Comparações das estrutura de correlação ---------------------------------
m.CS   <- lme(fixed = diametro ~ trat + tempo_n + trat * tempo_n, data = dados, random = ~ 1 | arvore, 
              correlation = corCompSymm(form = ~tempo_n|arvore))
m.Exp  <- lme(fixed = diametro ~ trat + tempo_n + trat * tempo_n, data = dados, random = ~ 1 | arvore,
              correlation = corExp(form = ~tempo_n|arvore, nugget = T))
m.Gaus <- lme(fixed = diametro ~ trat + tempo_n + trat * tempo_n, data = dados, random = ~ 1 | arvore,
              correlation = corGaus(form = ~tempo_n|arvore, nugget = T))
anova(m.CS, m.Exp)
anova(m.CS, m.Gaus)
anova(m.Exp, m.Gaus)



# Modelo escolhido --------------------------------------------------------
mod3 <- lme(fixed = diametro ~ tempo_n, data = dados, random = ~ 1 | arvore)
summary(mod3)
intervals(mod3)
mod3 <- lmer(diametro ~ tempo_n + (1 | arvore), data = dados)
res <- summary(mod3)
round(as.data.frame(res$coefficients)[, -3], 4)
round(confint(mod3), 4)
rand(mod3)

# Resíduos ----------------------------------------------------------------

## Resíduos marginais (erro aleatório)
my.hnp <- hnp(mod3, halfnormal = T, how.many.out = T, paint.out = T, plot = T)
pdf(file = "hnp.pdf", width = 11, height = 7)
par(mar = c(3.5, 3.5, 1.2, 0.6), cex = 1.8)
plot(my.hnp, xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', cex = 0.6, ylim = c(0, 10))
mtext("Percentil da N(0, 1)", side = 1, line = 2.0, cex = 1.8)
mtext("Resíduos marginais", side = 2, line =2, cex = 1.8)
abline(h = seq(0, 10, l = 5), v=seq(0, 3, l = 5), col = "gray", lty = "dotted")
axis(1, seq(0, 3, l = 5))
axis(2, seq(0, 10, l = 5), FF(seq(0, 10, l = 5), 1))
graphics.off()


## resíduos de efeitos aleatórios
r2 <- random.effects(mod3)$arvore
pdf(file = "qq-ranef.pdf", width = 11, height = 7)
par(mar = c(3.5, 3.5, 1.2, 0.6), cex = 1.8)
qqnorm(r2[, 1], xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', cex = 0.6, main = ""); qqline(r2[, 1])
mtext("Percentil da N(0, 1)", side = 1, line = 2.0, cex = 1.8)
mtext("Resíduos de efeitos aleatórios", side = 2, line =2, cex = 1.8)
abline(h = seq(-1.5, 1.5, l = 5), v=seq(-2, 2, l = 5), col = "gray", lty = "dotted")
axis(2, seq(-1.5, 1.5, l = 5))
axis(1, seq(-2, 2, l = 5), FF(seq(-2, 2, l = 5), 1))
graphics.off()

## ajustado versus residuo
x = fitted(mod3); y = residuals(mod3); Rx = range(x); Ry = range(y)
pdf(file = "pred.pdf", width = 11, height = 7)
par(mar = c(3.5, 3.5, 1.2, 0.6), cex = 1.8)
plot(y ~ x, xlab = '', ylab = '', cex = 0.8, xaxt = 'n', yaxt = 'n')
mtext("Valores ajustados", side = 1, line = 2.0, cex = 1.8)
mtext("Resíduos marginais", side = 2, line =2, cex = 1.8)
abline(h = seq(Ry[1], Ry[2], l = 5), v=seq(Rx[1], Rx[2], l = 5), col = "gray", lty = "dotted")
axis(2, seq(Ry[1], Ry[2], l = 5), FF(seq(Ry[1], Ry[2], l = 5), 1))
axis(1, seq(Rx[1], Rx[2], l = 5), FF(seq(Rx[1], Rx[2], l = 5), 1))
graphics.off()

## influencia
lmer3.infl <- influence(mod3, obs=TRUE)
cook <- cooks.distance(lmer3.infl)
x = 1:nrow(dados); y = cooks.distance(lmer3.infl); Rx = range(x); Ry = range(y)
pdf(file = "cook.pdf", width = 11, height = 7)
par(mar = c(3.5, 3.5, 1.2, 0.6), cex = 1.8)
plot(y, xlab = '', ylab = '', cex = 0.8, xaxt = 'n', yaxt = 'n')
mtext("Índice das observações", side = 1, line = 2.0, cex = 1.8)
mtext("Distância de Cook", side = 2, line = 2, cex = 1.8)
abline(h = seq(Ry[1], Ry[2], l = 5), v=seq(Rx[1], Rx[2], l = 5), col = "gray", lty = "dotted")
axis(2, seq(Ry[1], Ry[2], l = 5), FF(seq(Ry[1], Ry[2], l = 5), 3))
axis(1, seq(Rx[1], Rx[2], l = 5), FF(seq(Rx[1], Rx[2], l = 5), 0))
graphics.off()





