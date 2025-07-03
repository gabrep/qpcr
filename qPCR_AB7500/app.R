library(shiny)
library(dplyr)
library(janitor)
library(DT)
library(writexl)
library(ggplot2)
library(viridis)  # Para paleta de cores no gráfico
library(tools)


# Função para detectar a linha inicial do CSV
ler_csv_corrigido <- function(arquivo) {
  linhas <- readLines(arquivo)  # Lê o arquivo como texto bruto
  
  # Encontra a primeira linha que contém "Well"
  linha_inicio <- which(grepl("(?i)^(Well|Sample)", linhas))[1] - 1  # Pega o índice e ajusta; (?i) torna a busca case-insensitive
  
  # Lê o CSV a partir da linha correta
  df <- read.csv(arquivo, sep = ";", header = FALSE, skip = linha_inicio, stringsAsFactors = FALSE)
  
  # Se der erro com ';', tenta com ',' como separador
  if (ncol(df) < 3) {
    df <- read.csv(arquivo, sep = ",", header = FALSE, skip = linha_inicio, stringsAsFactors = FALSE)
  }
  
  return(df)
}


# Interface do Usuário
ui <- fluidPage(
  titlePanel("Processador de arquivos - qPCR AB 7500"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Carregar arquivo (.csv ou excel)",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv", ".xls", ".xlsx"), buttonLabel = "Selecione ou arraste o arquivo"),
      actionButton("process", "Processar Dados"),
      downloadButton("download", "Baixar dados processados")  # Botão para download
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Dados Processados", DTOutput("table")),
        tabPanel("Média dos Valores", DTOutput("mean_table")),
        tabPanel("Replicatas com alta \nvariabilidade", 
                 plotOutput("variabilidade_plot"), 
                 DTOutput("variabilidade_table"))
      )
    )
  )
)

# Servidor
server <- function(input, output) {
  
  dados_processados <- reactiveVal(NULL)
  dados_medios <- reactiveVal(NULL)
  raw_df <- reactiveVal(NULL)  # Armazena os dados brutos
  dif_replicatas <- reactiveVal(NULL)
  nome_arquivo <- reactiveVal("dados_processados.xlsx")  # Nome padrão inicial
  
  observeEvent(input$process, {
    req(input$file)  # Verifica se um arquivo foi carregado
    
    # Obtém o nome do arquivo original e adiciona o sufixo "_processado"
    nome_original <- sub("\\.csv$|\\.xls$|\\.xlsx$", "", input$file$name) # Remove extensão do nome
    nome_arquivo(paste0(nome_original, "_processado.xlsx"))  # Define nome final
    
    # Identifica a extensão do arquivo
    ext <- tools::file_ext(input$file$name)
    
    # Lendo o arquivo de acordo com o tipo
    if (ext == "csv") {
      df <- ler_csv_corrigido(input$file$datapath)
    } else if (ext %in% c("xls", "xlsx")) {
      df <- read_excel(input$file$datapath, col_names = FALSE)
    } else {
      stop("Formato de arquivo não suportado!")
    }
    # Filtragem dos dados
    df <- df[grepl("Well|^[A-Z]\\d", df[,1]), ]
    
    # Limpeza e seleção de colunas
    df <- df %>%
      janitor::row_to_names(1) %>%
      janitor::clean_names() %>%
      dplyr::select(well, sample_name, detector, ct, std_dev_ct, tm)
    
    # Convertendo colunas para os tipos adequados
    df$sample_name <- factor(df$sample_name)
    df$detector <- factor(df$detector)
    df <- cbind(df[,1:3], apply(df[,4:6], 2, function(x) as.numeric(x)))
    
    # Armazena os dados processados e brutos
    dados_processados(df)
    raw_df(df)
    
    # Criando o dataframe com médias
    summ_df <- df %>%
      group_by(sample_name, detector) %>%
      summarise(mean_ct = mean(ct, na.rm = TRUE), .groups = "drop")
    
    # Armazena os dados médios
    dados_medios(summ_df)
    
    # Criando o dataframe de diferenças
    df_diff <- df %>% group_by(sample_name, detector) %>% 
      summarise(diff_ct=max(ct) - min(ct), .groups = 'drop') %>% 
      filter(., diff_ct > 1)
    
    # Armazena as diferenças
    dif_replicatas(df_diff)
  })
  
  # Exibe a tabela processada
  output$table <- renderDT({
    req(dados_processados())
    datatable(dados_processados(), options = list(pageLength = 10))
  })
  
  # Exibe a tabela com valores médios
  output$mean_table <- renderDT({
    req(dados_medios())
    datatable(dados_medios(), options = list(pageLength = 10))
  })
  
  # Função para exportar os dados como Excel
  output$download <- downloadHandler(
    filename = function() {
      nome_arquivo()  # Usa o nome dinâmico baseado no arquivo de entrada
    },
    content = function(file) {
      write_xlsx(
        list(
          "Dados Processados" = dados_processados(),
          "Média dos Valores" = dados_medios()
        ),
        path = file
      )
    }
  )
  
  # Exibe o gráfico de variabilidade entre replicatas
  output$variabilidade_plot <- renderPlot({
    req(dif_replicatas())
    
    # Gráfico de dispersão de variabilidade entre replicatas
    ggplot(dif_replicatas(),aes(y = as.factor(sample_name), x = diff_ct, color = detector)) +
      geom_segment(aes(x = 0, xend = diff_ct, yend = as.factor(sample_name)), linewidth = 1) +
      geom_point(size = 4) +
      labs(title = "Amostras com alta variabilidade",
           x = "Diferença de Ct",
           y = "Amostra",
           color = "Detector") +
      scale_color_manual(values=c(viridis::turbo(20)))+
      theme_minimal()+
      theme(axis.text = element_text(size=12),
            legend.text = element_text(size=10))
  }, height = 500, width = 600)
  
}

# Rodando o App
shinyApp(ui, server)
