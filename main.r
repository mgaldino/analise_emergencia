library(dplyr)

item_contrato <- data.table::fread("info_item_contrato_novo.csv", encoding="UTF-8", colClasses=c("id_orgao"="character"))
info_contrato <- data.table::fread("info_contrato.csv", encoding="UTF-8", colClasses=c("id_orgao"="character"))
item_licitacao <- data.table::fread("info_item_licitacao.csv", encoding="UTF-8", colClasses=c("id_orgao"="character"))
orgaos <- data.table::fread("d8ag0132l0g7r7_public_orgao.csv", encoding="UTF-8", colClasses=c("id_orgao"="character", "cd_municipio_ibge" = "character"))
casos <- readxl::read_xlsx("HIST_PAINEL_COVIDBR_13ago2020.xlsx",)
ibge <- data.table::fread("IBGE.csv", encoding="UTF-8", colClasses=c("Código do Município"="character"))
pop <- data.table::fread("populacao.csv", encoding="UTF-8", colClasses=c("Cód."="character"))


ibge <- janitor::clean_names(ibge)
casos <- janitor::clean_names(casos)
pop <- janitor::clean_names(pop)

soma_mun_item_contr <- left_join(item_contrato, select(orgaos,!(home_page) ), by="id_orgao") %>%
group_by(cd_municipio_ibge) %>% 
summarise(soma_vl_item_contrato = sum(vl_item_contrato), soma_vl_item_contrato = sum(vl_item_contrato), soma_qt_itens_contrato = sum(qt_itens_contrato)) %>%
  left_join(select(ibge,"codigo_do_municipio","nome_do_municipio","impostos_liquidos_de_subsidios","produto_interno_bruto_a_prec","produto_interno_bruto_per_cap"), by= c("cd_municipio_ibge" = "codigo_do_municipio")) %>%
  left_join(select(pop, "cod", "x2019"), by= c("cd_municipio_ibge" = "cod")) %>%
  unique() %>%
  left_join