# app.R
# ---- Paquetes ----
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)
library(rsconnect)

# ---- Datos ----
# Ahora leemos el archivo comprimido (ligero) para que GitHub lo acepte
Death <- readRDS("Death.rds")

# Mantener sólo "Medium" si existiera Variants
if ("Variant" %in% names(Death)) {
  Death <- Death %>% filter(is.na(Variant) | Variant == "Medium")
}

# Lista de ubicaciones
locations_vec <- Death %>%
  filter(!is.na(Location)) %>%
  distinct(Location) %>%
  arrange(Location) %>%
  pull(Location)

# ---- UI ----
ui <- fluidPage(
  titlePanel("Proyecciones de muertes por año (WPP)"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectizeInput(
        inputId = "location",
        label   = "Selecciona región (Location):",
        choices = locations_vec,
        options = list(placeholder = "Escribe para buscar...", closeAfterSelect = TRUE)
      ),
      radioButtons(
        inputId = "metric",
        label   = "Sexo / Total:",
        choices = c("Total" = "DeathTotal",
                    "Hombres" = "DeathMale",
                    "Mujeres" = "DeathFemale"),
        selected = "DeathTotal",
        inline = FALSE
      ),
      helpText("El gráfico muestra la suma anual (todas las edades) para la selección.")
    ),
    
    mainPanel(
      width = 9,
      plotlyOutput("line_plot", height = "520px"),
      br(),
      fluidRow(
        column(
          width = 12,
          div(
            style = "display:flex; gap:12px; flex-wrap:wrap;",
            downloadButton("dl_csv", "⬇️ Descargar serie (CSV)"),
            downloadButton("dl_png", "⬇️ Descargar gráfico (PNG)")
          )
        )
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  observe({
    if (is.null(input$location) && length(locations_vec) > 0) {
      updateSelectizeInput(session, "location", selected = locations_vec[1])
    }
  })
  
  series_reactive <- reactive({
    req(input$location, input$metric)
    
    Death %>%
      filter(Location == input$location) %>%
      group_by(Time) %>%
      summarise(
        value = sum(.data[[input$metric]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(Time)
  })
  
  build_plot <- reactive({
    df <- series_reactive()
    validate(need(nrow(df) > 0, "No hay datos para la selección actual."))
    
    ylab_txt <- switch(
      input$metric,
      "DeathMale"   = "Muertes (Hombres)",
      "DeathFemale" = "Muertes (Mujeres)",
      "DeathTotal"  = "Muertes (Total)"
    )
    
    ggplot(df, aes(x = Time, y = value)) +
      geom_line(linewidth = 1) +
      geom_point() +
      labs(
        x = "Año",
        y = ylab_txt,
        title = paste0("Muertes proyectadas por año — ", input$location)
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
  })
  
  output$line_plot <- renderPlotly({
    ggplotly(build_plot(), tooltip = c("x", "y")) %>%
      layout(hovermode = "x unified")
  })
  
  output$dl_csv <- downloadHandler(
    filename = function() {
      metric_tag <- switch(
        input$metric,
        "DeathMale"   = "Hombres",
        "DeathFemale" = "Mujeres",
        "DeathTotal"  = "Total"
      )
      paste0("WPP_Deaths_Serie_", gsub("[^A-Za-z0-9]+","_", input$location), "_", metric_tag, ".csv")
    },
    content = function(file) {
      df <- series_reactive()
      utils::write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  output$dl_png <- downloadHandler(
    filename = function() {
      metric_tag <- switch(
        input$metric,
        "DeathMale"   = "Hombres",
        "DeathFemale" = "Mujeres",
        "DeathTotal"  = "Total"
      )
      paste0("WPP_Deaths_Grafico_", gsub("[^A-Za-z0-9]+","_", input$location), "_", metric_tag, ".png")
    },
    content = function(file) {
      p <- build_plot()
      ggsave(
        filename = file,
        plot = p,
        width = 10, height = 6, units = "in", dpi = 300
      )
    }
  )
}

shinyApp(ui, server)