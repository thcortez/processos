# extrai informações de texto a partir de gatilhos definidos pelo usuário, utiliza regex para localizar matches

processar_arquivo_parte2 <- function(caminho_arquivo) {
  nome_arquivo <- basename(caminho_arquivo)
  num_arquivo  <- str_extract(nome_arquivo, "\\d+")
  
  texto_bruto  <- readLines(caminho_arquivo, encoding = "UTF-8", warn = FALSE)
  texto_unico  <- paste(texto_bruto, collapse = "\n")
  texto_limpo  <- limpar_texto(texto_unico)
  
  # Gatilhos invertidos (que eram "fim" antes):
  padrao_inicio <- paste0("(", paste(inicio_triggers_2, collapse="|"), ")")
  padrao_fim    <- paste0("(", paste(fim_triggers_2,    collapse="|"), ")")
  
  inicios <- str_locate_all(texto_limpo, regex(padrao_inicio, ignore_case=TRUE))[[1]]
  fins    <- str_locate_all(texto_limpo, regex(padrao_fim,    ignore_case=TRUE))[[1]]
  
  if (is.null(inicios) || is.null(fins) || nrow(inicios)==0 || nrow(fins)==0) {
    return(data.frame())
  }
  
  inicios <- inicios[order(inicios[,1]), ]
  fins    <- fins[order(fins[,1]), ]
  
  resultados <- data.frame()
  bloco_id   <- 1
  pos_fim_usada <- 1
  
  for (i in seq_len(nrow(inicios))) {
    pos_ini <- inicios[i, "end"] + 1
    
    while (pos_fim_usada <= nrow(fins) && fins[pos_fim_usada, "start"] <= pos_ini) {
      pos_fim_usada <- pos_fim_usada + 1
    }
    if (pos_fim_usada > nrow(fins)) break
    
    pos_fim <- fins[pos_fim_usada, "start"] - 1
    trecho  <- substr(texto_limpo, pos_ini, pos_fim)
    
    # Chama a função que vai extrair MÚLTIPLOS blocos desse trecho
    df_blocos <- extrair_varios_blocos(trecho)
    
    if (nrow(df_blocos) > 0) {
      # Adiciona colunas de identificação
      df_blocos <- cbind(
        data.frame(arquivo_id = num_arquivo,
                   bloco_id   = bloco_id + seq_len(nrow(df_blocos)) - 1,
                   stringsAsFactors=FALSE),
        df_blocos
      )
      bloco_id <- bloco_id + nrow(df_blocos)
      
      resultados <- rbind(resultados, df_blocos)
    }
  }
  
  resultados
}
