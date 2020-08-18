library(dplyr)
library(knitr)
library(kableExtra)


# Importar arquivos
item_contrato <- data.table::fread("info_item_contrato_novo.csv", encoding="UTF-8", colClasses=c("id_orgao"="character"))
info_contrato <- data.table::fread("info_contrato.csv", encoding="UTF-8", colClasses=c("id_orgao"="character"))
item_licitacao <- data.table::fread("info_item_licitacao.csv", encoding="UTF-8", colClasses=c("id_orgao"="character"))
orgaos <- data.table::fread("d8ag0132l0g7r7_public_orgao.csv", encoding="UTF-8", colClasses=c("id_orgao"="character", "cd_municipio_ibge" = "character"))
casos <- data.table::fread("HIST_PAINEL_COVIDBR_13ago2020.csv", colClasses = c("codmun" = "character"))
ibge <- data.table::fread("IBGE.csv", encoding="UTF-8", colClasses=c("Código do Município"="character"))
pop <- data.table::fread("populacao.csv", encoding="UTF-8", colClasses=c("Cód."="character"))

# Limpar variáveis
ibge <- janitor::clean_names(ibge)
casos <- janitor::clean_names(casos)
pop <- janitor::clean_names(pop)
casos <- janitor::clean_names(casos)

# Filtrar apenas valores máximos de casos e óbitos
max_casos <- casos %>% group_by(codmun) %>% top_n(1, casos_acumulado)

# Filtrar apenas dados de 2017 do IBGE
ibge <- filter(ibge, ano == 2017)
ibge <- mutate(ibge, cod_sem_ver = substr(codigo_do_municipio, 1,6))

# Pegar ID dos municípios
cont_licit_efetivadas <- item_contrato %>%
  left_join(select(orgaos,!(home_page) ), by="id_orgao") %>%
  group_by(cd_municipio_ibge, id_licitacao) %>%
  summarize(count=n()) %>%
  group_by(cd_municipio_ibge) %>%
  summarize(count=n())

# Somar valores contratados
soma_mun_item_contr <- left_join(item_contrato, select(orgaos,!(home_page) ), by="id_orgao") %>%
group_by(cd_municipio_ibge) %>% 
summarise(soma_vl_item_contrato = sum(vl_item_contrato), soma_vl_item_contrato = sum(vl_item_contrato), soma_qt_itens_contrato = sum(qt_itens_contrato)) %>%
  left_join(select(ibge,"codigo_do_municipio","nome_do_municipio","impostos_liquidos_de_subsidi","produto_interno_bruto_a_prec","produto_interno_bruto_per_cap","cod_sem_ver"), by= c("cd_municipio_ibge" = "codigo_do_municipio")) %>%
  left_join(select(pop, "cod", "x2019"), by= c("cd_municipio_ibge" = "cod")) %>%
  left_join(select(max_casos, codmun, casos_acumulado, obitos_acumulado), by= c("cod_sem_ver" = "codmun")) %>%
  left_join(cont_licit_efetivadas, by = c("cd_municipio_ibge")) %>%
  unique()

# Calculcar variáveis proporcionais
soma_mun_item_contr <- soma_mun_item_contr %>%
  mutate(valor_sobre_pib = soma_vl_item_contrato/produto_interno_bruto_a_prec) %>%
  mutate(valor_sobre_pop = soma_vl_item_contrato/x2019) %>%
  mutate(valor_sobre_casos = soma_vl_item_contrato/casos_acumulado) %>%
  mutate(valor_sobre_obitos = soma_vl_item_contrato/obitos_acumulado) %>%
  mutate(casos_sobre_hab = casos_acumulado/x2019) %>%
  mutate(obitos_sobre_hab = obitos_acumulado/x2019)


# Filtrar os 24 municípios com valores zerados pagos
soma_mun_item_contr <- filter(soma_mun_item_contr, soma_vl_item_contrato != 0)

# Exportar 10 maiores valores
head(arrange(select(soma_mun_item_contr,nome_do_municipio, soma_vl_item_contrato, valor_sobre_pop, valor_sobre_casos),desc(soma_vl_item_contrato)), n = 10) %>%
  kable(align="l", format.args = list(big.mark = ","), digits=2) %>%
  kable_styling(bootstrap_options = c("striped"), position = "center")
