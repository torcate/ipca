---
output: github_document
---
<!-- README.md is generated from README.Rmd. Please edit that file -->
```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%"
)
```
# ipca
O objetivo deste pacote é gerar um **modelo automatizado de previsão da inflação** da economia brasileira medida pelo **[IPCA/IBGE](https://www.ibge.gov.br/estatisticas/economicas/precos-e-custos/9256-indice-nacional-de-precos-ao-consumidor-amplo.html)**. Além de realizar a modelagem e previsão, o pacote reporta os resultados através de uma [dashboard](https://torcate.github.io/github//ipca/) resumo.
Este pacote desenvolvido em `R`.
## Licença
Copyright 2025 FIESP. All rights reserved.
## Sobre o workflow
Utilizando o pacote `{caret}` e o `{HDeconometrics}`, são estimados cinco modelos: CSR, LASSO, Bagging, Random Walk e Ensemble, além de comparação com um benchmark de mercado (Focus), que são avaliados utilizando o método da validação cruzada.
As etapas realizadas e funcionalidades referentes podem ser descritas conforme abaixo:
| Procedimento | Função |
| ------------ | ------ |
| 1) Coleta e tratamento de dados | `ipca::get_data()` e `ipca::ts_transform()` |
| 2) Treinamento dos modelos (validação cruzada e cálculo de acurácia) | `ipca::train_models()` |
| 3) Previsão fora da amostra do modelo Ensemble | `ipca::forecast_ensemble()` |
| 4) Automatização das etapas 1 a 3 e criação de dashboard de resultados | `ipca::build_dashboard()` |
## Instalação
O pacote pode ser instalado através do [GitHub](https://github.com/) com o `{remotes}`:
```{r}
# if (!require("remotes")) install.packages("remotes")
remotes::install_github("torcate/ipca")
```
## Utilização
A principal função do pacote é capaz de reproduzir todo o workflow e gerar uma dashboard de resultados (arquivo HTML da Flexdashboard):
```{r}
library(ipca)
td <- tempdir()
build_dashboard(save_at = td)
```
Uma prévia do resultado pode ser conferida abaixo:
![](docs/printscreen.png)
Para utilizar as demais funções verifique as documentações referentes.
```{r}

```
