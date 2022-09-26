# instalando reglin
if(!require(remotes)) install.packages("remotes")
remotes::install_github("fndemarqui/reglin")

# carregando pacoes
library(reglin)
library(tidyverse)
library(ggpubr)

# plotando grafico
ggplot(pureza, aes(x=percentual, y=pureza)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)  

# 
fit <- lm(pureza ~ percentual, data = pureza)
pureza <- fortify(fit)

ggplot(pureza, aes(x=percentual, y=pureza)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)  +
  geom_segment(aes(x = percentual, y = pureza, 
                   xend = percentual, yend = .fitted), color = "red")

############ 2.2 INFERÊNCIA ###########

# ajustando o modelo:
mod <- lm(pureza ~ percentual, data = pureza)

# verificando a classe do objeto mod:
class(mod)

# extraíndo um sumário do modelo ajustado:
summary(mod)

# função anova
anova(mod)

# para obter a tabela ANOVA, independentemente do número de covariáveis entrando no modelo
reglin::tab_anova()

########## PREVISÕES 2.3 ############

# ajustando o modelo:
fit <- lm(pureza ~ percentual, data = pureza)

# especificando os valores desejados para a covariável percentual:
newcov <- data.frame(percentual = seq(0.9, 1.5, by = 0.1))

# IC para respostas médias de novas observações:
predict(fit, newdata = newcov, interval = "confidence")

# IC para para previsõeos de novas observações:
predict(fit, newdata = newcov, interval = "prediction")

########## INFERÊNCIA 3.3 ##########
library(reglin)
library(tidyverse)

set.seed(1234567890)
n <- 25
sigma <- 3
beta <- c(10, -2, 1.2)
data <- data.frame(x1 = rnorm(n), x2 = rnorm(n))

simdata <- rlm(~x1+x2, data = data, beta = beta, sigma = sigma)
glimpse(simdata)

mod <- lm(y~x1+x2, data=simdata)

# para saber se ao menos uma variável é significante
tab_anova(mod)

# analisa cada uma das variáveis e indica qual delas é significante
summary(mod)

######### COEFICIENTE DE DETERMINAÇÃO 3.5 ##########
library(reglin)
library(tidyverse)
library(ggpubr)
library(gridExtra)

set.seed(1234567890)
n <- 50
sigma1 <- 1
sigma2 <- 2
beta <- c(10, -2)
cov <-data.frame(
  x = rnorm(n)
)

dados1 <- rlm(~x, data = cov, beta = beta, sigma = sigma1)
dados2 <- rlm(~x, data = cov, beta = beta, sigma = sigma2)

p1 <- ggplot(dados1, aes(x=x, y=y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  xlim(-2.5, 2.5) +
  ylim(0, 20) +
  stat_regline_equation(label.x = 1.2, label.y = 15, aes(label = ..rr.label..))

p2 <- ggplot(dados2, aes(x=x, y=y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)  + 
  xlim(-2.5, 2.5) +
  ylim(0, 20) + 
  stat_regline_equation(label.x = 1.2, label.y = 15, aes(label = ..rr.label..))

grid.arrange(p1, p2, ncol = 2)

##################
library(reglin)
library(tidyverse)
library(gridExtra)

set.seed(123456789) # fixando a semente para garantir a reproducibilidade
n <- 50             # tamanho a amostra
beta <- c(-2, 1.5)  # especificando os coeficientes
sigma <- 0.5        # desvio padrão associado ao erro 

# função não linear (nos betas!)
f <- function(x, beta){
  exp(beta[1] + beta[2]*x)
} 

# modelo linear
tb1 <- tibble(
  x = rnorm(n),
  y = beta[1] + beta[2]*x + rnorm(n, sd = 1.5)
)

# modelo não linear:
tb2 <- tibble(
  x = rnorm(n),
  y = f(x, beta) + rnorm(n, sd = 0.05)
)

fit1 <- lm(y~x, data = tb1)
p1 <- fortify(fit1) %>%
  ggplot(aes(x=x, y=y)) +
  geom_point() +
  ggtitle("relação linear") + 
  geom_smooth(method = "lm", se = FALSE)  +
  geom_segment(aes(x = x, y = y, xend = x, yend = .fitted), color = "red") +
  stat_regline_equation(label.x = -2, label.y = 1.5, aes(label = ..rr.label..))

fit2 <- lm(y~x, data = tb2)
p2 <- fortify(fit2) %>%
  ggplot(aes(x=x, y=y)) +
  geom_point() +
  ggtitle("relação não linear")  +
  geom_smooth(method = "lm", se = FALSE)  +
  geom_segment(aes(x = x, y = y, xend = x, yend = .fitted), color = "red") +
  stat_regline_equation(label.x = -1, label.y = 3.5, aes(label = ..rr.label..))

grid.arrange(p1, p2, nrow = 1)

############ SOMA DE QUADRADOS EXTRA 3.7 #############
library(reglin)
library(tidyverse)
set.seed(1234567890)

n <- 50
sigma <- 2
beta <- c(-10, 0, 0.5, 0, -0.9)
covs <-data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n),
  x4 = rnorm(n)
)

simdata <- rlm(~ ., data = covs, beta = beta, sigma = sigma)
glimpse(simdata)

m2 <- lm(y ~ x1+x2+x3+x4, data = simdata)
M2 <- lm(y ~ ., data = simdata)
m2

M2

m1 <- lm(y ~ x2+x4, data = simdata)
M1 <- update(M2, ~ . -x1 - x3)

m1

M1

soma_extra <- function(model1, model2){
  SQRes1 <- sum(residuals(model1)^2)
  SQRes2 <- sum(residuals(model2)^2)
  df1 <- model1$df 
  df2 <- model2$df 
  gl <- df1 - df2
  SQExtra <- SQRes1-SQRes2
  F0 <- (SQExtra/gl)/(SQRes2/df2)
  pvalor <- pf(F0, gl, df2, lower.tail=FALSE)
  saida <- data.frame("SQExtra" = SQExtra, "gl" = gl, 
                      "F0" = F0, "pvalor" = pvalor)
  return(saida)
}

soma_extra(m1, m2)

anova(m1, m2)

anova_check <- function(model){
  
  # cálculo da soma de quadrados dos resíduos
  SQRes <- function(model){
    return(sum(residuals(model)^2))
  }
  
  # extração os graus de liberdade dos resíduos
  gl <- function(model){
    return(model$df.residual)
  }
  
  formula <- formula(model)
  mf <- model.frame(model)
  X <- stats::model.matrix(formula, data = mf)
  y <- model.response(data = mf)
  vars <- colnames(X)
  p <- ncol(X)
  fit <- list()
  for(i in 1:p){
    fit[[i]] <- lm(y~0+X[, 1:i])
  }
  df <- data.frame(
    SQRes = unlist(lapply(fit, SQRes)),
    df = unlist(lapply(fit, gl))  
  )
  rownames(df) <- vars
  return(df)
}

mod1 <- lm(y~x1, data = simdata)
mod2 <- update(mod1, ~ . + x2)
mod3 <- update(mod2, ~ . + x3)
mod4 <- update(mod3, ~ . + x4)

formula(mod1)

formula(mod2)

formula(mod3)

formula(mod4)

out <- anova_check(mod4)
out

anova(mod4)

round(abs(diff(out$SQRes)), 3)

F0 <- abs(diff(out$SQRes))/(sigma(mod4)^2)
round(F0, 4)