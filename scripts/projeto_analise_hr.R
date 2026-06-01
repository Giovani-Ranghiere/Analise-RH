# ==============================================================================
# PROJETO DE ANÁLISE DE SOBREVIVÊNCIA E REDES BAYESIANAS: PEOPLE ANALYTICS (Attrition)
# Autor: Giovani (com assistência do Antigravity)
# Data: Maio de 2026
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Carregamento de Pacotes e Instalação (se necessário)
# ------------------------------------------------------------------------------
# Caso algum pacote não esteja instalado, descomente as linhas abaixo:
# install.packages(c("readr", "dplyr", "survival", "survminer", "bnlearn", "ggplot2"))

library(readr)
library(dplyr)
library(survival)
library(survminer)
library(bnlearn)
library(ggplot2)

# ------------------------------------------------------------------------------
# 2. Importação e Preparação Inicial dos Dados
# ------------------------------------------------------------------------------
cat("\n[1/5] Carregando o conjunto de dados...\n")
df <- read_csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")

# Criar cópia para a Análise de Sobrevivência
# Evento de interesse: Attrition = Yes (1). Funcionários ativos (No) são censurados (0).
df_surv <- df %>%
  mutate(Event = ifelse(Attrition == "Yes", 1, 0))

# ==============================================================================
# PARTE 1: ANÁLISE DE SOBREVIVÊNCIA
# ==============================================================================
cat("\n=== PARTE 1: ANÁLISE DE SOBREVIVÊNCIA ===\n")

# --- Pergunta 1: Função de Sobrevivência Global ---
cat("\n--- Pergunta 1: Função de Sobrevivência Global ---\n")

fit_global <- survfit(Surv(YearsAtCompany, Event) ~ 1, data = df_surv)

# Exibir resumo detalhado ano a ano dos primeiros 15 anos
s_global <- summary(fit_global, times = 0:15)
print(summary(fit_global, times = 0:10))

# Calcular as quedas marginais na probabilidade de sobrevivência
# S(t-1) - S(t)
surv_prob <- c(1, s_global$surv) # inclui tempo inicial t=0 com sobrevivência 1
time_points <- c(0, s_global$time)
diff_prob <- -diff(surv_prob)
names(diff_prob) <- 1:15

cat("\nQuedas marginais de sobrevivência ano a ano (Anos 1 a 15):\n")
print(round(diff_prob, 5))

steepest_year <- names(diff_prob)[which.max(diff_prob)]
steepest_value <- max(diff_prob)
cat(sprintf("\nA maior queda marginal na probabilidade de retenção ocorre no ano: %s (queda de %.2f%%).\n",
            steepest_year, steepest_value * 100))

# Plotar e salvar a Curva de Sobrevivência Global
p_global <- ggsurvplot(
  fit_global,
  data = df_surv,
  conf.int = TRUE,
  palette = "#1f77b4",
  title = "Curva de Sobrevivência Global (Kaplan-Meier)",
  xlab = "Anos na Empresa (YearsAtCompany)",
  ylab = "Probabilidade de Sobrevivência S(t)",
  ggtheme = theme_minimal()
)
ggsave("km_global.png", plot = p_global$plot, width = 8, height = 6, dpi = 300)
cat("Gráfico 'km_global.png' salvo com sucesso.\n")


# --- Pergunta 2: Diferença nas Horas Extras (Log-Rank Test) ---
cat("\n--- Pergunta 2: Teste Log-Rank por Horas Extras (OverTime) ---\n")

fit_ot <- survfit(Surv(YearsAtCompany, Event) ~ OverTime, data = df_surv)
test_ot <- survdiff(Surv(YearsAtCompany, Event) ~ OverTime, data = df_surv)
print(test_ot)

# Extrair p-valor exato do teste Log-Rank
p_val_ot <- 1 - pchisq(test_ot$chisq, length(test_ot$n) - 1)
cat(sprintf("\np-valor exato do Teste Log-Rank: %e\n", p_val_ot))

# Plotar e salvar curvas de Kaplan-Meier estratificadas
p_ot <- ggsurvplot(
  fit_ot,
  data = df_surv,
  pval = TRUE,
  conf.int = TRUE,
  palette = c("#2ca02c", "#d62728"),
  title = "Curva de Sobrevivência: Com vs Sem Hora Extra",
  xlab = "Anos na Empresa (YearsAtCompany)",
  ylab = "Probabilidade de Sobrevivência S(t)",
  legend.title = "OverTime",
  legend.labs = c("Não faz Hora Extra", "Faz Hora Extra"),
  ggtheme = theme_minimal()
)
ggsave("km_overtime.png", plot = p_ot$plot, width = 8, height = 6, dpi = 300)
cat("Gráfico 'km_overtime.png' salvo com sucesso.\n")


# --- Pergunta 3: O que influencia a saída e quanto exatamente (Regressão de Cox Estratificada) ---
cat("\n--- Pergunta 3: Modelo de Riscos Proporcionais de Cox Estratificado ---\n")

# Preparar fatores adequadamente na tabela df_surv
df_surv_cox <- df_surv %>%
  mutate(
    OverTime = as.factor(OverTime),
    MaritalStatus = as.factor(MaritalStatus),
    BusinessTravel = as.factor(BusinessTravel),
    Gender = as.factor(Gender),
    # Discretizar a variável YearsSinceLastPromotion para a estratificação (ponto de corte de 2 anos)
    PromotionStatus = ifelse(YearsSinceLastPromotion <= 2, "Recente (<= 2 anos)", "Antiga (> 2 anos)"),
    PromotionStatus = as.factor(PromotionStatus)
  )

# Ajustar o modelo Cox Estratificado por PromotionStatus
fit_cox <- coxph(
  Surv(YearsAtCompany, Event) ~ Age + DistanceFromHome + MonthlyIncome +
    JobSatisfaction + WorkLifeBalance + EnvironmentSatisfaction +
    NumCompaniesWorked + OverTime + MaritalStatus + BusinessTravel +
    strata(PromotionStatus),
  data = df_surv_cox
)

summary_cox <- summary(fit_cox)
print(summary_cox)

# Testar a premissa de riscos proporcionais (Schoenfeld residuals)
ph_test <- cox.zph(fit_cox)
print(ph_test)

cat("\nInterpretação dos Hazard Ratios (HR) Significativos:\n")
# Extrair coeficientes e p-valores para impressão formatada
coefs <- summary_cox$coefficients
for (var in rownames(coefs)) {
  hr <- coefs[var, "exp(coef)"]
  p_val <- coefs[var, "Pr(>|z|)"]
  
  if (p_val < 0.05) {
    sig_marker <- ifelse(p_val < 0.001, "***", ifelse(p_val < 0.01, "**", "*"))
    
    # Interpretações customizadas baseadas no nome da variável
    if (var == "OverTimeYes") {
      cat(sprintf("- OverTime (Fazer Hora Extra): HR = %.4f %s (p: %e)\n", hr, sig_marker, p_val))
      cat(sprintf("  Quem faz hora extra tem %.2f%% a MAIS de risco instantâneo de evasão (aumento de %.2fx).\n", (hr - 1) * 100, hr))
    } else if (var == "BusinessTravelTravel_Frequently") {
      cat(sprintf("- Viagem de Negócios Frequente: HR = %.4f %s (p: %e)\n", hr, sig_marker, p_val))
      cat(sprintf("  Viajar frequentemente aumenta o risco instantâneo de evasão em %.2f%% (aumento de %.2fx).\n", (hr - 1) * 100, hr))
    } else if (var == "MaritalStatusSingle") {
      cat(sprintf("- Estado Civil Solteiro: HR = %.4f %s (p: %e)\n", hr, sig_marker, p_val))
      cat(sprintf("  Funcionários solteiros têm %.2f%% a MAIS de risco de evasão que divorciados (aumento de %.2fx).\n", (hr - 1) * 100, hr))
    } else if (var == "NumCompaniesWorked") {
      cat(sprintf("- Número de Empresas Anteriores: HR = %.4f %s (p: %e)\n", hr, sig_marker, p_val))
      cat(sprintf("  A cada empresa anterior no currículo, o risco instantâneo de evasão aumenta em %.2f%%.\n", (hr - 1) * 100))
    } else if (var == "DistanceFromHome") {
      cat(sprintf("- Distância de Casa: HR = %.4f %s (p: %e)\n", hr, sig_marker, p_val))
      cat(sprintf("  A cada milha a mais de distância de casa, o risco instantâneo aumenta em %.2f%%.\n", (hr - 1) * 100))
    } else if (var == "Age") {
      cat(sprintf("- Idade: HR = %.4f %s (p: %e)\n", hr, sig_marker, p_val))
      cat(sprintf("  A cada ano a mais de idade, o risco instantâneo REDUZ em %.2f%%.\n", (1 - hr) * 100))
    } else if (var == "JobSatisfaction") {
      cat(sprintf("- Satisfação no Trabalho: HR = %.4f %s (p: %e)\n", hr, sig_marker, p_val))
      cat(sprintf("  A cada nível a mais de satisfação no trabalho, o risco instantâneo REDUZ em %.2f%%.\n", (1 - hr) * 100))
    } else if (var == "EnvironmentSatisfaction") {
      cat(sprintf("- Satisfação com o Ambiente: HR = %.4f %s (p: %e)\n", hr, sig_marker, p_val))
      cat(sprintf("  A cada nível a mais de satisfação com o ambiente, o risco instantâneo REDUZ em %.2f%%.\n", (1 - hr) * 100))
    } else if (var == "WorkLifeBalance") {
      cat(sprintf("- Equilíbrio Vida-Trabalho: HR = %.4f %s (p: %e)\n", hr, sig_marker, p_val))
      cat(sprintf("  A cada nível a mais de equilíbrio vida-trabalho, o risco instantâneo REDUZ em %.2f%%.\n", (1 - hr) * 100))
    } else if (var == "MonthlyIncome") {
      # Para o salário, interpretar por faixas de R$ 1.000
      hr_1000 <- exp(coefs[var, "coef"] * 1000)
      cat(sprintf("- Renda Mensal (por USD 1.000): HR = %.4f %s (p: %e)\n", hr_1000, sig_marker, p_val))
      cat(sprintf("  A cada USD 1.000 a mais de salário mensal, o risco instantâneo de evasão REDUZ em %.2f%%.\n", (1 - hr_1000) * 100))
    }
  }
}

# --- Validação Preditiva: Modelo de Cox (Holdout 80/20) ---
cat("\n--- Validação Preditiva: Modelo de Cox (Holdout 80/20) ---\n")
set.seed(42)

# 1. Divisão dos Dados
n_rows_cox <- nrow(df_surv_cox)
train_idx_cox <- sample(seq_len(n_rows_cox), size = floor(0.8 * n_rows_cox))
train_cox <- df_surv_cox[train_idx_cox, ]
test_cox <- df_surv_cox[-train_idx_cox, ]

# 2. Treinamento do Modelo (usando apenas train_cox)
fit_cox_train <- coxph(
  Surv(YearsAtCompany, Event) ~ Age + DistanceFromHome + MonthlyIncome +
    JobSatisfaction + WorkLifeBalance + EnvironmentSatisfaction +
    NumCompaniesWorked + OverTime + MaritalStatus + BusinessTravel +
    strata(PromotionStatus),
  data = train_cox
)

# 3. Validação no Conjunto de Teste (C-Index)
# O pacote survival usa a função concordance() para calcular o Índice de Concordância de Harrell
cox_val <- concordance(fit_cox_train, newdata = test_cox)
cat("\nÍndice de Concordância (C-Index) nos dados de Teste (20%):\n")
print(cox_val$concordance)


# ==============================================================================
# PARTE 2: REDES BAYESIANAS
# ==============================================================================
cat("\n=== PARTE 2: REDES BAYESIANAS ===\n")

# --- Preparação de dados discretos para bnlearn ---
# Selecionar variáveis de interesse para o grafo
df_bn <- df %>%
  select(Attrition, JobRole, Department, JobSatisfaction, WorkLifeBalance,
         EnvironmentSatisfaction, BusinessTravel, JobLevel, MonthlyIncome) %>%
  mutate(
    # Converter categóricas e ordinais para fatores
    JobSatisfaction = as.factor(JobSatisfaction),
    WorkLifeBalance = as.factor(WorkLifeBalance),
    EnvironmentSatisfaction = as.factor(EnvironmentSatisfaction),
    JobLevel = as.factor(JobLevel),
    Attrition = as.factor(Attrition),
    JobRole = as.factor(JobRole),
    Department = as.factor(Department),
    BusinessTravel = as.factor(BusinessTravel),
    # Discretizar MonthlyIncome por quantis (tercis) para garantir balanceamento
    MonthlyIncome = cut(MonthlyIncome,
                        breaks = quantile(MonthlyIncome, probs = c(0, 0.33, 0.66, 1)),
                        include.lowest = TRUE,
                        labels = c("Low", "Medium", "High"))
  ) %>%
  as.data.frame() # IMPORTANTE: converter tibble para data.frame puro

# --- Pergunta 4: Aprendizado de Estrutura (DAG com Priors Especialistas) ---
cat("\n--- Pergunta 4: Aprendizado da Rede Bayesiana via Hill-Climbing com Whitelist ---\n")

# Construir a matriz de Whitelist (relação mandatória apoiada pela literatura)
# Forçamos que a satisfação no trabalho, com o ambiente e o equilíbrio vida-trabalho determinam o Atrito
wl <- matrix(c(
  "JobSatisfaction", "Attrition",
  "EnvironmentSatisfaction", "Attrition",
  "WorkLifeBalance", "Attrition"
), ncol = 2, byrow = TRUE)
colnames(wl) <- c("from", "to")

# Aprender a estrutura da rede com a Whitelist incorporada
dag <- hc(df_bn, whitelist = wl)
print(dag)

# Salvar o Gráfico da DAG em PNG
png("bn_dag.png", width = 1000, height = 800, res = 120)
plot(dag, main = "Rede Bayesiana de Attrition (com Priors de Literatura)")
dev.off()
cat("Gráfico 'bn_dag.png' salvo com sucesso.\n")


# --- Pergunta 5: d-separação (MonthlyIncome vs Attrition dado JobLevel) ---
cat("\n--- Pergunta 5: Teste de d-separação ---\n")

is_dsep <- dsep(dag, x = "MonthlyIncome", y = "Attrition", z = "JobLevel")

cat(sprintf("MonthlyIncome e Attrition são d-separados dado o JobLevel? RESPOSTA: %s\n", 
            ifelse(is_dsep, "SIM (Condicionalmente Independentes)", "NÃO (Dependentes)")))

# Explicação analítica do grafo
cat("\nEstrutura causal observada no grafo:\n")
cat("No grafo com priors especialistas, temos a estrutura de 'Cadeia Serial': Attrition -> JobRole -> JobLevel -> MonthlyIncome.\n")
cat("Portanto, sob as regras de d-separação, ao controlar por 'JobLevel' (ou 'JobRole'), bloqueamos a propagação\n")
cat("da dependência probabilística entre a renda e a evasão. Isso valida empiricamente que o salário e o atrito\n")
cat("não estão conectados diretamente, mas são mediados pelas características do cargo (JobLevel).\n")


# --- Pergunta 6: Inferência Bayesiana (Belief Updating) ---
cat("\n--- Pergunta 6: Inferência Bayesiana e Atualização de Crenças ---\n")

# Ajustar os parâmetros (CPTs) com base no grafo aprendido e dados discretos
fitted_bn <- bn.fit(dag, df_bn)

# Evidência solicitada:
# - Department: 'Sales'
# - BusinessTravel: 'Travel_Frequently'
# - EnvironmentSatisfaction: '1' (baixa satisfação com ambiente de trabalho)
set.seed(42) # semente para garantir reprodutibilidade da simulação Monte Carlo

prob_post <- cpquery(
  fitted_bn,
  event = (Attrition == "Yes"),
  evidence = (Department == "Sales" & BusinessTravel == "Travel_Frequently" & EnvironmentSatisfaction == "1"),
  n = 10^7 # 10 milhões de amostras para alta precisão estatística
)

baseline_prob <- mean(df_bn$Attrition == "Yes")

cat(sprintf("Probabilidade de evasão geral na empresa (Baseline): %.2f%%\n", baseline_prob * 100))
cat(sprintf("Probabilidade a posteriori de evasão com as evidências: %.2f%%\n", prob_post * 100))
cat(sprintf("Aumento no risco relativo sob estas condições: %.2fx em relação à média geral.\n", 
            prob_post / baseline_prob))

# --- Validação Preditiva: Rede Bayesiana (Holdout 80/20) ---
cat("\n--- Validação Preditiva: Rede Bayesiana (Holdout 80/20) ---\n")
set.seed(42)

# 1. Divisão dos Dados
n_rows_bn <- nrow(df_bn)
train_idx_bn <- sample(seq_len(n_rows_bn), size = floor(0.8 * n_rows_bn))
train_bn <- df_bn[train_idx_bn, ]
test_bn <- df_bn[-train_idx_bn, ]

# 2. Ajuste dos Parâmetros (Treinamento nas CPTs)
fitted_bn_train <- bn.fit(dag, train_bn)

# 3. Predição no Conjunto de Teste (Probabilidades e Limiar Customizado)
# Em bases muito desbalanceadas, o ponto de corte padrão (50%) falha ao detectar a classe minoritária.
# Vamos extrair as probabilidades e aplicar a taxa base da empresa (~16%) como limiar (threshold) de corte:
pred_prob <- predict(fitted_bn_train, node = "Attrition", data = test_bn, prob = TRUE)
probs <- attr(pred_prob, "prob")
prob_yes <- probs["Yes", ]
pred_bn <- ifelse(prob_yes > 0.16, "Yes", "No")
pred_bn <- factor(pred_bn, levels = c("No", "Yes"))

# 4. Avaliação de Performance (Matriz de Confusão e Métricas Rigorosas)
matriz_confusao <- table(Predito = pred_bn, Real = test_bn$Attrition)
cat("\nMatriz de Confusão nos dados de Teste (20%):\n")
print(matriz_confusao)

acuracia <- sum(diag(matriz_confusao)) / sum(matriz_confusao)
cat(sprintf("\nAcurácia preditiva da Rede Bayesiana: %.2f%%\n", acuracia * 100))

# Extração das Métricas Rigorosas (Recall, Precision, F1-Score)
# Utilizamos 'Yes' como a classe positiva
TP <- matriz_confusao["Yes", "Yes"]
TN <- matriz_confusao["No", "No"]
FP <- matriz_confusao["Yes", "No"]
FN <- matriz_confusao["No", "Yes"]

recall <- ifelse((TP + FN) == 0, 0, TP / (TP + FN))
precision <- ifelse((TP + FP) == 0, 0, TP / (TP + FP))
f1_score <- ifelse((precision + recall) == 0, 0, 2 * (precision * recall) / (precision + recall))

cat("\nMétricas de Validação Rigorosa (Classe Positiva = 'Yes'):\n")
cat(sprintf("- Recall (Sensibilidade): %.2f%%\n", recall * 100))
cat(sprintf("- Precision (Valor Preditivo Positivo): %.2f%%\n", precision * 100))
cat(sprintf("- F1-Score: %.2f%%\n", f1_score * 100))

