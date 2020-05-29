#' Visualise simulation results
#'
#' This function launches a Shiny application able to visualize and explore simulation data.
#'
#' Simulation data must be loaded using `get_results`
#'
#' @param res Results loaded with `get_results`
#' @export
viz_results <- function(res) {
  shinyApp(
    navbarPage(
      "Navbar!",
      tabPanel(
        "Input Data",
        sidebarLayout(
          sidebarPanel(
            width = 2,
            selectInput("select",
              label = h3("Select input"),
              choices = list(
                "Generation units" =  "all_gen",
                "Lines" =  "all_lines",
                "Choice 3" = 3
              ),
              selected = 1
            )
          ),
          mainPanel(
            width = 10,
            DT::dataTableOutput("inputs")
          )
        )
      ),
      tabPanel(
        "Dispatch",
        sidebarLayout(
          sidebarPanel(
            width = 2,
            selectInput("dis_zone",
              label = h3("Select zone"),
              choices =
                res$data$ZONES,
              selected = 1
            )
          ),
          mainPanel(
            width = 10,
            # plotOutput("dispatch")
            plotlyOutput("dispatch")
          )
        )
      ),
      tabPanel(
        "Summary plots",
        sidebarLayout(
          sidebarPanel(
            width = 2,
            selectInput("sum_plot_type",
              label = h3("Select summary"),
              choices = list(
                "Annual generation" = "annual_gen"
              ),

              selected = 1
            )
          ),
          mainPanel(
            width = 10,
            # plotOutput("dispatch")
            plotlyOutput("sum_plot")
          )
        )
      )
    ),
    function(input, output, session) {
      ## DISPATCH PLOT ##
      output$dispatch <- renderPlotly({
        ggplotly(
          res$plots$dispatch[[input$dis_zone]]
        )
      })

      ## SUMMARY PLOT ##
      output$sum_plot <- renderPlotly({
        if (input$sum_plot_type == "annual_gen") {
          ggplotly(
            res$plots$annual_gen
            # res$plots$dispatch$ES
          )
        }
      })

      ## SHOW DATA TABLES ##
      output$inputs <- DT::renderDataTable({
        if (input$select == "all_gen") {
          DT::datatable(res$data$all_gen %>%
            mutate(
              bus = res$data$ZONES[bus + 1]
            ),
          caption = "List of all the generation units used in the simulation",
          options = list(autoWidth = TRUE),
          filter = "top",
          colnames = c(
            "name", "zone", "Tech", "Fuel", "Cost",
            "CO2 per MWh", "Pmax (MW)",
            "Storage Max (MWh)", "Pmin", "Storage min"
          )
          ) %>%
            DT::formatRound(c(5:6), 2) %>%
            DT::formatRound(c(7:10), 1) %>%
            DT::formatStyle(columns = 1:10, fontSize = "75%")
        } else if (input$select == "all_lines") {
          DT::datatable(res$data$all_lines %>%
            mutate(
              from = res$data$ZONES[from + 1],
              to = res$data$ZONES[to + 1]
            ),
          caption = "List of all the lines used in the simulation",
          options = list(autoWidth = TRUE),
          filter = "top",
          colnames = c(
            "name", "from", "to", "Capacity (MW)"
          )
          ) %>%
            DT::formatRound(c(4), 1) %>%
            DT::formatStyle(columns = 1:4, fontSize = "75%")
        }
      })
    }
  )
}
