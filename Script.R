# Asegura que todos los paquetes necesarios de R están instalados y activados.
if (!require("pxR")) {
  install.packages("pxR")
  library("pxR")
}

if (!require("shiny")) {
  install.packages("shiny")
  library("shiny")
}

if (!require("shinyjs")) {
  install.packages("shinyjs")
  library("shinyjs")
}

if (!require("shinythemes")) {
  install.packages("shinythemes")
  library("shinythemes")
}

if (!require("ggplot2")) {
  install.packages("ggplot2")
  library("ggplot2")
}

# Importa las variables y convierte el formato de miles de euros a euros.
hipotecasPorEntidad <-
  as.data.frame(read.px("https://www.ine.es/jaxiT3/files/t/es/px/13897.px?nocab=1"))
hipotecasPorFincas <-
  as.data.frame(read.px("https://www.ine.es/jaxiT3/files/t/es/px/13896.px?nocab=1"))
hipotecasPorEntidad[which(hipotecasPorEntidad$Número.e.importe == "Importe de hipotecas"), ]$value <-
  hipotecasPorEntidad[which(hipotecasPorEntidad$Número.e.importe == "Importe de hipotecas"), ]$value * 1000
hipotecasPorFincas[which(hipotecasPorFincas$Número.e.importe == "Importe de hipotecas"), ]$value <-
  hipotecasPorFincas[which(hipotecasPorFincas$Número.e.importe == "Importe de hipotecas"), ]$value * 1000

# --------------------
# | APLICACIÓN SHINY |
# --------------------

# El objeto ui representa la interaz de usuario, el esqueleto visual de la página. Recoge lo que le proporciona el objeto server y lo presenta.
ui <- fluidPage(
  # Permite ocultar y mostrar objetos HTML mediante JavaScript de manera interactiva.
  useShinyjs(),
  
  # Selecciona un estilo de diseño del paquete "shinythemes".
  theme = shinytheme("paper"),
  
  # Encabezado de la página donde se presenta el logotipo de la universidad y el nombre de la asignatura.
  tags$h1(
    tags$img(src = "http://www.upm.es/sfs/Rectorado/Gabinete%20del%20Rector/Logos/UPM/Logotipo%20con%20Leyenda/LOGOTIPO%20leyenda%20color%20PNG.png", width = 200),
    tags$b("Políticas Sectoriales")
  ),
  
  # Contiene la totalidad de la interfaz gráfica. Está dividido en cuatro pestañas, y la pestaña de gráficas de variables a su vez en otras cuatro.
  mainPanel(width = 12,
            tabsetPanel(
              # Pestaña de gráficas de variables.
              tabPanel("Gráficas de variables",
                       fluidRow(
                         # Bloque lateral izquierdo para seleccionar una variable.
                         column(2,
                                wellPanel(
                                  radioButtons(
                                    inputId = "variable",
                                    label = "Selecciona una variable de estudio:",
                                    choiceNames = list(
                                      "Hipotecas por entidad de concesión",
                                      "Hipotecas por naturaleza de la finca"
                                    ),
                                    choiceValues = list("hipotecasPorEntidad",
                                                        "hipotecasPorFincas"),
                                    selected = character(0)
                                  )
                                )),
                         # Bloque central de gráficos que se divide en tres pestañas correspondientes a cada tipo de gráfico.
                         column(6,
                                tabsetPanel(
                                  tabPanel("Tabla de datos",
                                           wellPanel(tableOutput("tabla"))),
                                  tabPanel("Gráfico",
                                           wellPanel(plotOutput("grafico"))),
                                  tabPanel("Histograma",
                                           wellPanel(plotOutput("histograma")))
                                )),
                         # Bloque lateral derecho para seleccionar el período.
                         column(4,
                                uiOutput("periodo"))
                       )),
              # Pestaña de la presentación del proyecto. No es funcional porque la aplicación donde esté funcionando necesita tener el archivo.
              tabPanel(
                "Presentacion",
                wellPanel(#includeHTML("Soporte/Presentacion.html")
                  "Depende de dónde esté funcionando la aplicación.")
              ),
              # Pestaña de descargas donde puede descargarse todos los archivos del proyecto para visualizarlos y ejecutarlos localmente. No es funcional porque donde esté funcionando la aplicación se requiere disponer de los archivos localmente.
              tabPanel("Centro de descargas",
                       fluidRow(
                         column(
                           4,
                           wellPanel(
                             "Descargar código completo en R",
                             tags$br(),
                             downloadButton("descargarCodigo", label = "Descargar")
                           )
                         ),
                         column(
                           4,
                           wellPanel(
                             "Descargar presentación (en .pptx)",
                             tags$br(),
                             downloadButton("descargarInforme", label = "Descargar")
                           )
                         ),
                         column(
                           4,
                           wellPanel(
                             "Descargar datos (procesados .RData y originales en CSV)",
                             tags$br(),
                             downloadButton("descargarDatos", label = "Descargar")
                           )
                         )
                       ))
            ))
)


# El objeto server es el responsable de proporcionarle a UI todos los datos solicitados y las funciones que requieran procesamiento en el back-end de la app.
server <- function(input, output, session) {
  # Función responsable de renderizar a tiempo real la tabla (primera forma visualización de los datos).
  output$tabla <- renderTable({
    datosTabla()
  },
  width = "100%",
  striped = TRUE,
  bordered = TRUE,
  align = "l",
  digits = 0)
  
  # Genera el bloque de selección del período, número o importe, la entidad concesionaria (sólo visible para hipotecas por entidad) y la naturaleza de la finca (sólo disponible para hipotecas por naturaleza de la finca).
  output$periodo <- renderUI({
    fluidPage(mainPanel(
      wellPanel(
        selectInput(
          inputId = "periodo",
          "Período:",
          unique(hipotecasPorEntidad$Periodo),
          multiple = FALSE,
          selectize = TRUE,
          width = NULL,
          size = NULL
        ),
        selectInput(
          inputId = "numeimp",
          "Número o importe:",
          unique(hipotecasPorEntidad$Número.e.importe),
          multiple = FALSE,
          selectize = TRUE,
          width = NULL,
          size = NULL
        ),
        selectInput(
          inputId = "entidad",
          "Entidad concesionaria:",
          unique(hipotecasPorEntidad$Entidad.que.concede.el.préstamo),
          multiple = FALSE,
          selectize = TRUE,
          width = NULL,
          size = NULL
        ),
        selectInput(
          inputId = "naturaleza",
          "Naturaleza de la finca:",
          unique(hipotecasPorFincas$Naturaleza.de.la.finca),
          multiple = FALSE,
          selectize = TRUE,
          width = NULL,
          size = NULL
        )
      )
    ))
  })
  
  # Si hipotecas por entidad está seleccionada, entonces la opción de selección de la naturaleza de la finca se oculta. El mismo efecto se produce para la opción de entidad de concesión en el caso de que hipotecas por naturaleza esté seleccionada.
  observeEvent(input$variable, {
    if (input$variable == "hipotecasPorEntidad") {
      shinyjs::hide(id = "naturaleza")
      shinyjs::show(id = "entidad")
    } else {
      shinyjs::show(id = "naturaleza")
      shinyjs::hide(id = "entidad")
    }
  })
  
  # Genera el comando de R dinámicamente para solicitar el gráfico de totales.
  output$grafico <- renderPlot(
    ggplot(
      datosGrafico(),
      aes(
        x = datosGrafico()$Periodo,
        y = datosGrafico()$value,
        group = 1
      )
    ) +
      theme(axis.text.x = element_text(angle = 90)) +
      geom_point() + geom_line(color = "red") +
      geom_smooth(method = lm, se = FALSE) +
      labs(title = "Gráfico de totales", x = "Período", y = "Cantidad") +
      scale_x_discrete(limits = rev(levels(
        datosGrafico()$Periodo
      )),
      breaks = hipotecasPorEntidad[seq(0, nrow(datosGrafico()), by =
                                         12), 1])
  )
  
  # Genera dinámicamente los datos que se utilizarán en el gráfico
  datosGrafico <- eventReactive({
    input$variable
    input$numeimp
    input$entidad
    input$naturaleza
  }, {
    if (input$variable == "hipotecasPorEntidad") {
      return(
        subset(
          hipotecasPorEntidad,
          hipotecasPorEntidad$Número.e.importe == input$numeimp
          &
            hipotecasPorEntidad$Entidad.que.concede.el.préstamo == input$entidad
          &
            hipotecasPorEntidad$Comunidades.Autonomas == "Total Nacional"
        )[, -c(2:4)]
      )
    } else {
      return(
        subset(
          hipotecasPorFincas,
          hipotecasPorFincas$Número.e.importe == input$numeimp
          &
            hipotecasPorFincas$Naturaleza.de.la.finca == input$naturaleza
          &
            hipotecasPorFincas$Comunidades.Autonomas == "Total Nacional"
        )[, -c(2:4)]
      )
    }
  })
  
  # Genera dinámicamente los datos que se utilizarán en la tabla de datos.
  datosTabla <- eventReactive({
    input$variable
    input$periodo
    input$numeimp
    input$entidad
    input$naturaleza
  }, {
    if (input$variable == "hipotecasPorEntidad") {
      return(
        subset(
          hipotecasPorEntidad,
          hipotecasPorEntidad$Periodo == input$periodo
          &
            hipotecasPorEntidad$Número.e.importe == input$numeimp
          &
            hipotecasPorEntidad$Entidad.que.concede.el.préstamo == input$entidad
        )[, 4:5]
      )
    } else {
      return(
        subset(
          hipotecasPorFincas,
          hipotecasPorFincas$Periodo == input$periodo
          &
            hipotecasPorFincas$Número.e.importe == input$numeimp
          &
            hipotecasPorFincas$Naturaleza.de.la.finca == input$naturaleza
        )[, 4:5]
      )
    }
  })
  
  # Genera dinámicamente los datos que se utilizarán en el histograma
  datosHistograma <- eventReactive({
    input$variable
    input$numeimp
    input$periodo
    input$entidad
    input$naturaleza
  }, {
    if (input$variable == "hipotecasPorEntidad") {
      return(
        subset(
          hipotecasPorEntidad,
          hipotecasPorEntidad$Número.e.importe == input$numeimp
          &
            hipotecasPorEntidad$Entidad.que.concede.el.préstamo == input$entidad
          &
            hipotecasPorEntidad$Periodo == input$periodo
        )[-1, -c(2:3)]
      )
    } else {
      return(
        subset(
          hipotecasPorFincas,
          hipotecasPorFincas$Número.e.importe == input$numeimp
          &
            hipotecasPorFincas$Naturaleza.de.la.finca == input$naturaleza
          &
            hipotecasPorFincas$Periodo == input$periodo
        )[-1, -c(2:3)]
      )
    }
  })
  
  # Genera el comando de R dinámicamente para solicitar el histograma
  output$histograma <- renderPlot(
    ggplot(
      data = datosHistograma(),
      aes(
        x = datosHistograma()$Comunidades.Autonomas,
        y = datosHistograma()$value
      )
    ) +
      geom_bar(stat = "identity") +
      labs(title = "Histograma por CC. AA.", x = "CC. AA.", y = "Cantidad") +
      coord_flip()
  )
}

# Enlaza la interfaz de usuario y el back-end del servidor para dar vida a la aplicación.
shinyApp(ui, server)