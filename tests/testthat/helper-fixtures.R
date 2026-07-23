values_with_header_json <- function() {
  paste0(
    '[{"NC":"Nível Territorial (Código)",',
    '"NN":"Nível Territorial","V":"Valor"},',
    '{"NC":"1","NN":"Brasil","V":"0.16"}]'
  )
}

values_without_header_json <- function() {
  '[{"NC":"1","NN":"Brasil","V":"0.16"}]'
}

descriptor_fixture <- function() {
  list(
    Id = 1419,
    Nome = paste(
      "IPCA - Variação mensal, acumulada no ano,",
      "acumulada em 12 meses"
    ),
    PeriodoDisponibilidade = "janeiro 2012 a fevereiro 2012",
    Periodos = list(
      list(Codigo = 201201, Nome = "janeiro 2012"),
      list(Codigo = 201202, Nome = "fevereiro 2012")
    ),
    Variaveis = list(
      list(
        Id = 63,
        Nome = "IPCA - Variação mensal",
        UnidadeMedida = "%",
        PeriodoDisponibilidadeExcecao = ""
      ),
      list(
        Id = 2265,
        Nome = "IPCA - Variação acumulada em 12 meses",
        UnidadeMedida = "%",
        PeriodoDisponibilidadeExcecao = "dezembro 2012 a dezembro 2019"
      )
    ),
    Classificacoes = list(
      list(
        Id = 315,
        Nome = "Geral, grupo, subgrupo, item e subitem",
        Categorias = list(
          list(Id = 7169, Nome = "Índice geral"),
          list(Id = 7170, Nome = "Alimentação e bebidas")
        )
      )
    ),
    NiveisTerritoriais = list(
      list(
        Id = 1,
        Nome = "Brasil",
        QuantidadeUnidadesAtivas = 1
      ),
      list(
        Id = 7,
        Nome = "Região Metropolitana até 2020",
        QuantidadeUnidadesAtivas = 10
      ),
      list(
        Id = 6,
        Nome = "Município",
        QuantidadeUnidadesAtivas = 6
      )
    )
  )
}

catalog_fixture <- function() {
  list(
    list(
      id = "D1",
      nome = "Preços",
      agregados = list(
        list(
          id = "7060",
          nome = "IPCA - Variação mensal e acumulada"
        ),
        list(
          id = "1737",
          nome = "IPCA - Série histórica"
        )
      )
    ),
    list(
      id = "D2",
      nome = "Contas",
      agregados = list(
        list(
          id = "5932",
          nome = "Sistema de Contas Nacionais"
        )
      )
    )
  )
}

fake_http_response <- function(status = 200L, body = "[]") {
  structure(
    list(
      url = "https://apisidra.ibge.gov.br/values/t/1",
      status_code = as.integer(status),
      headers = list("content-type" = "application/json; charset=UTF-8"),
      all_headers = list(),
      cookies = data.frame(),
      content = charToRaw(enc2utf8(body)),
      date = Sys.time(),
      times = numeric(),
      request = list(method = "GET"),
      handle = NULL
    ),
    class = "response"
  )
}
