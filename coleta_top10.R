# Instalar pacotes se ainda não tiver
# install.packages(c("rvest", "dplyr", "stringr", "tibble", "purrr", "readr", "lubridate", "googlesheets4", "gargle", "httr"))

library(rvest)
library(dplyr)
library(stringr)
library(tibble)
library(purrr)
library(readr)
library(lubridate)
library(googlesheets4)
library(gargle)
library(httr)

# ------------------------------------------------------------
# 1. CONFIGURAÇÕES
# ------------------------------------------------------------

sheet_url <- "https://docs.google.com/spreadsheets/d/1jmk7sxp8WGAvo9g8sDLZUHrxy_efAoIQp0cRr6bvl58/edit?gid=0#gid=0"

plataformas <- tibble(
  plataforma = c("Netflix", "Prime Video", "HBO Max", "Disney+"),
  url = c(
    "https://flixpatrol.com/top10/netflix/brazil/",
    "https://flixpatrol.com/top10/amazon-prime/brazil/",
    "https://flixpatrol.com/top10/hbo-max/brazil/",
    "https://flixpatrol.com/top10/disney/brazil/"
  )
)

nomes_posicoes <- c(
  "Primeiro", "Segundo", "Terceiro", "Quarto", "Quinto",
  "Sexto", "Sétimo", "Oitavo", "Nono", "Décimo"
)

linha_vazia <- function(plataforma_nome) {
  tibble(
    Data = format(Sys.time(), tz = "America/Sao_Paulo", format = "%d/%m/%Y"),
    Horario = format(Sys.time(), tz = "America/Sao_Paulo", format = "%H:%M"),
    Streaming = plataforma_nome
  ) |>
    bind_cols(
      as_tibble(
        setNames(
          as.list(rep(NA_character_, 10)),
          nomes_posicoes
        )
      )
    )
}

# ------------------------------------------------------------
# 2. FUNÇÃO PARA PEGAR TOP 10 MOVIES
# ------------------------------------------------------------

pegar_top10_movies <- function(plataforma_nome, url) {
  
  message("Coletando: ", plataforma_nome)
  
  page <- NULL
  
  for (tentativa in 1:3) {
    
    tentativa_resultado <- tryCatch({
      
      resposta <- httr::GET(
        url,
        httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"),
        httr::timeout(30)
      )
      
      httr::stop_for_status(resposta)
      
      html <- httr::content(resposta, as = "text", encoding = "UTF-8")
      page <- read_html(html)
      
      TRUE
      
    }, error = function(e) {
      message("Tentativa ", tentativa, " falhou para ", plataforma_nome, ": ", e$message)
      FALSE
    })
    
    if (tentativa_resultado) {
      break
    }
    
    Sys.sleep(5)
  }
  
  if (is.null(page)) {
    warning(paste("Não consegui acessar:", plataforma_nome))
    return(linha_vazia(plataforma_nome))
  }
  
  tabela_movies_node <- page |>
    html_element(
      xpath = "//*[self::h2 or self::h3 or self::h4][contains(., 'TOP 10 Movies')]/following::table[1]"
    )
  
  if (length(tabela_movies_node) == 0 || is.na(tabela_movies_node)) {
    warning(paste("Não encontrei TOP 10 Movies para", plataforma_nome))
    return(linha_vazia(plataforma_nome))
  }
  
  movies <- tabela_movies_node |>
    html_table(fill = TRUE)
  
  names(movies) <- make.names(names(movies), unique = TRUE)
  
  movies <- movies |>
    slice(1:10)
  
  if (nrow(movies) == 0) {
    warning(paste("Tabela vazia para", plataforma_nome))
    return(linha_vazia(plataforma_nome))
  }
  
  linha_completa <- apply(movies, 1, paste, collapse = " ")
  linha_completa <- str_squish(linha_completa)
  
  rank <- as.character(movies[[1]]) |>
    str_extract("[0-9]+")
  
  rank[is.na(rank)] <- as.character(seq_len(length(rank))[is.na(rank)])
  
  mudanca <- as.character(movies[[2]]) |>
    str_squish() |>
    str_replace_all("−", "-") |>
    str_replace_all("—", "–")
  
  mudanca[mudanca == "" | is.na(mudanca)] <- "0"
  
  dias <- linha_completa |>
    str_extract("[0-9]+\\s*d") |>
    str_replace_all("\\s+", "")
  
  dias[is.na(dias)] <- "0d"
  
  titulo <- as.character(movies[[3]]) |>
    str_squish() |>
    str_remove("\\s+[0-9]+\\s*d$") |>
    str_squish()
  
  posicoes <- paste0(
    rank, ". ",
    titulo,
    " (", mudanca, "; ", dias, ")"
  )
  
  # Garante que sempre teremos 10 colunas
  if (length(posicoes) < 10) {
    posicoes <- c(posicoes, rep(NA_character_, 10 - length(posicoes)))
  }
  
  posicoes <- posicoes[1:10]
  
  resultado <- tibble(
    Data = format(Sys.time(), tz = "America/Sao_Paulo", format = "%d/%m/%Y"),
    Horario = format(Sys.time(), tz = "America/Sao_Paulo", format = "%H:%M"),
    Streaming = plataforma_nome
  ) |>
    bind_cols(
      as_tibble(
        setNames(
          as.list(posicoes),
          nomes_posicoes
        )
      )
    )
  
  return(resultado)
}

# ------------------------------------------------------------
# 3. COLETAR DADOS
# ------------------------------------------------------------

base_hoje <- pmap_dfr(
  plataformas,
  ~ pegar_top10_movies(..1, ..2)
)

print(base_hoje)

# ------------------------------------------------------------
# 4. AUTENTICAR GOOGLE SHEETS
# ------------------------------------------------------------

gs4_auth(
  path = "service-account.json",
  scopes = "https://www.googleapis.com/auth/spreadsheets"
)

# ------------------------------------------------------------
# 5. TESTAR ACESSO À PLANILHA
# ------------------------------------------------------------

gs4_get(sheet_url)

# ------------------------------------------------------------
# 6. SALVAR NA ABA "top10"
# ------------------------------------------------------------

sheet_append(
  ss = sheet_url,
  data = base_hoje,
  sheet = "top10"
)

print("Coleta salva com sucesso no Google Sheets!")
