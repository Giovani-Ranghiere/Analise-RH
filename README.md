# People Analytics: Diagnóstico Causal e Sobrevivência (HR Attrition)

## Visão Geral do Projeto
Este projeto tem um objetivo claro: **diagnosticar as causas raízes da evasão de talentos e mapear o "porquê" e o "quando" as pessoas saem**, em vez de ser apenas um "oráculo de predição". 

Em cenários reais de Recursos Humanos, o comportamento humano possui ruídos incalculáveis (fatores pessoais não mapeados em bancos de dados). Dessa forma, construir modelos preditivos "caixa-preta" visando apenas métricas de acurácia mascaradas leva a falhas graves. Essa arquitetura prioriza a **Inferência Causal** e a **Explicabilidade** para direcionar intervenções reais do RH. Aceitemos um *trade-off* consciente: trocar poder preditivo bruto por interpretação estratégica.

## Base de Dados e Dicionário de Variáveis
* **Fonte:** Arquivo `WA_Fn-UseC_-HR-Employee-Attrition.csv` (Dataset padrão e sintetizado pela IBM).
* **Variável Resposta (Y):** `Attrition` (Evasão), modelada como status censurado para sobrevivência (1 = Yes, 0 = No) e nó alvo na rede probabilística.
* **Variável Temporal (T):** `YearsAtCompany` (Anos na Empresa).
* **Principais Variáveis Preditoras (X):** Fatores demográficos, características do cargo (`JobRole`, `BusinessTravel`, `OverTime`) e métricas contínuas e ordinais de satisfação/remuneração.
* **Amostra (n):** 1.470 colaboradores (com taxa base de evasão em torno de 16%).

## Metodologia Estatística e Diagnóstica
O projeto foi ancorado em três pilares analíticos:

### 1. Análise de Sobrevivência (O Fator Tempo)
* **Estimação de Kaplan-Meier:** Curvas não-paramétricas para identificar os anos de maior gargalo de retenção e estratificações (com validação via **Teste Log-Rank**).
* **Modelo Semiparamétrico de Cox:** Avaliação quantitativa exata (Hazard Ratios) para descobrir o quanto cada variável acelera o risco de saída. O modelo foi refinado via estratificação por 'Status de Promoção' após violar as premissas iniciais dos resíduos de Schoenfeld.

### 2. Redes Bayesianas (A Arquitetura Causal)
* **Aprendizado de Estrutura:** Algoritmo *Hill-Climbing* acoplado a **Priors Especialistas (Whitelist)** para forçar a conexão de fatores de clima organizacional, mitigando a miopia estatística pura.
* **d-separação:** Testes formais de independência condicional para isolar efeitos diretos (ex: comprovar que salário não gera atrito diretamente, mas é bloqueado pelo nível do cargo).
* **Atualização de Crenças (Inference):** Simulações de Monte Carlo com 10 milhões de amostras para prever cenários condicionados, como a probabilidade exata de saída de um funcionário insatisfeito que viaja frequentemente.

### 3. Validação Preditiva Otimizada (Threshold Customizado)
* **Holdout Out-of-Sample (80/20):** Separação estrita de conjuntos de treino e teste.
* **Ajuste Ciente de Desbalanceamento:** Por focar em diagnóstico e interceptação antecipada, um modelo padrão com *threshold* preditivo de 50% ignoraria as raras evasões reais (entregando Recall zerado e enganando o negócio com ~84% de acurácia). Adotamos um **limiar estratégico de 16%** (base rate da empresa), assumindo propositalmente mais alarmes falsos (baixo custo pro RH) para garantir que mais de 50% dos talentos em risco fossem identificados com sucesso (Recall > 50%).

## Tecnologias
* **Linguagem:** R
* **Dependências Principais:** `tidyverse`, `survival`, `survminer`, `bnlearn`, `caret`.

## Instruções de Reprodução
1. Clone o repositório.
2. Posicione o arquivo `WA_Fn-UseC_-HR-Employee-Attrition.csv` na raiz do diretório.
3. Instale as bibliotecas requeridas executando:
```R
install.packages(c("readr", "dplyr", "survival", "survminer", "bnlearn", "ggplot2"))
```
4. Execute o arquivo `projeto_analise_hr.R`.
