# Instalar pacotes se ainda não tiver
# install.packages(c("rvest", "dplyr", "stringr", "tibble", "purrr", "readr", "lubridate", "googlesheets4", "gargle"))

library(rvest)
library(dplyr)
library(stringr)
library(tibble)
library(purrr)
library(readr)
library(lubridate)
library(googlesheets4)
library(gargle)

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

# ------------------------------------------------------------
# 2. FUNÇÃO PARA PEGAR TOP 10 MOVIES
# ------------------------------------------------------------

pegar_top10_movies <- function(plataforma_nome, url) {
  
  page <- read_html(url)
  
  tabela_movies_node <- page |>
    html_element(
      xpath = "//*[self::h2 or self::h3 or self::h4][contains(., 'TOP 10 Movies')]/following::table[1]"
    )
  
  if (length(tabela_movies_node) == 0 || is.na(tabela_movies_node)) {
    warning(paste("Não encontrei TOP 10 Movies para", plataforma_nome))
    return(tibble())
  }
  
  movies <- tabela_movies_node |>
    html_table(fill = TRUE)
  
  names(movies) <- make.names(names(movies), unique = TRUE)
  
  movies <- movies |>
    slice(1:10)
  
  # Junta a linha inteira para facilitar a extração de dias no Top 10
  linha_completa <- apply(movies, 1, paste, collapse = " ")
  linha_completa <- str_squish(linha_completa)
  
  # Rank: normalmente está na primeira coluna
  rank <- as.character(movies[[1]]) |>
    str_extract("[0-9]+")
  
  # Se por algum motivo não vier rank, usa 1:10
  rank[is.na(rank)] <- as.character(seq_len(length(rank))[is.na(rank)])
  
  # Mudança de posição: normalmente está na segunda coluna
  mudanca <- as.character(movies[[2]]) |>
    str_squish() |>
    str_replace_all("−", "-") |>
    str_replace_all("—", "–")
  
  mudanca[mudanca == "" | is.na(mudanca)] <- "0"
  
  # Dias no top 10: procura padrão tipo "8 d", "1 d", etc.
  dias <- linha_completa |>
    str_extract("[0-9]+\\s*d") |>
    str_replace_all("\\s+", "")
  
  dias[is.na(dias)] <- "0d"
  
  # Título: normalmente está na terceira coluna
  titulo <- as.character(movies[[3]]) |>
    str_squish() |>
    str_remove("\\s+[0-9]+\\s*d$") |>
    str_squish()
  
  # Monta texto final de cada posição
  posicoes <- paste0(
    rank, ". ",
    titulo,
    " (", mudanca, "; ", dias, ")"
  )
  
  nomes_posicoes <- c(
    "Primeiro", "Segundo", "Terceiro", "Quarto", "Quinto",
    "Sexto", "Sétimo", "Oitavo", "Nono", "Décimo"
  )
  
  resultado <- tibble(
    Data = format(Sys.Date(), "%d/%m/%Y"),
    Horario = format(Sys.time(), "%H:%M"),
    Streaming = plataforma_nome
  ) |>
    bind_cols(
      as_tibble(
        setNames(
          as.list(posicoes),
          nomes_posicoes[seq_along(posicoes)]
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
