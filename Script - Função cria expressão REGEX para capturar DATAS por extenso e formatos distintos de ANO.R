regex_data <- function() {
  meses <- "(janeiro|fevereiro|março|abril|maio|junho|julho|agosto|setembro|outubro|novembro|dezembro)"
  
  ano_regex <- paste0(
    "(",
    "([0-9]{4})",           # YYYY
    "|",
    "([0-9]\\.[0-9]{3})",   # Y.YYY
    "|",
    "([0-9]{2}\\b)",  # YY, mas isolado de outro número 
    ")"
  )
  
  dia_e_mes <- paste0(
    "(",
    "(((\\d[0])?\\d[1-9]{1})",
    "|",
    "(\\d[1-2]{1}\\d[0-9]{1})",
    "|",
    "(\\d[3]{1}\\d[0,1]{1}))",
    "[\\-\\/\\. _º°od]?",
    "((\\d[0]?\\d[1-9]{1})",
    "|",
    "(\\d[1]{1}\\d[0-2]{1}))",
    "[\\-\\/\\. _º°om]?",
    ")"
  )
  
  data_extenso <- paste0("[0-9]{1,2}\\s+de\\s+", meses, "\\s+de\\s+(?:[0-9]{4}|[0-9]\\.[0-9]{3})")
  
  paste0("(", dia_e_mes, ano_regex, "|", data_extenso, ")")
}


# OU
# 

regex_data <- function() {
  
  # Meses por extenso
  meses <- "(janeiro|fevereiro|março|abril|maio|junho|julho|agosto|setembro|outubro|novembro|dezembro)"
  
  # Componentes numéricos
  dia_regex <- "(0?[1-9]|[12][0-9]|3[01])"
  mes_regex <- "(0?[1-9]|1[0-2])"
  sep_regex <- "[-\\/. _º°odm]?"   # separadores comuns e bagunçados
  
  # Combinação de dia e mês
  dia_e_mes <- paste0("(", dia_regex, sep_regex, mes_regex, sep_regex, ")")
  
  # Ano em formatos YYYY ou Y.YYY
  ano_regex <- "(\\d{4}|\\d\\.\\d{3}|\\d{2}\\b)"
  
  # Data por extenso (ex: 30 de junho de 2023)
  data_extenso <- paste0("\\b\\d{1,2}\\s+de\\s+", meses, "\\s+de\\s+", ano_regex)
  
  # Expressão final: data numérica ou por extenso
  paste0("(", dia_e_mes, ano_regex, "|", data_extenso, ")")
}
