library(shiny)
library(DT)

#............................................................
# User Interface
#...........................................................
ui <- fluidPage(
  #  App title ----
  headerPanel("CHIM A1c Overdue Forecasting"),

  # Sidebar panel for inputs ----
  sidebarLayout(
    # Input: Read CSV ----
    sidebarPanel(
      fileInput("file1", "Choose Excel or CSV File", accept = c(".xlsx", "csv")),

      # Input: Filtering Dates ----
      sliderInput("tto", label = "Days Until A1c Overdue",
                  min = 0, max = 180, value = 30, step = 10),
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      h1("Future Overdue A1c"),
      DTOutput("A1ctbl_future"),
      h1("Future and To-Date Overdue A1cs"),
      DTOutput("A1ctbl_all"),
      plotOutput("cumA1cplot")
    )
  )


)

#............................................................
# Work/Server
#...........................................................
server <- function(input, output) {
  library(tidyverse)
  library(lubridate)
  library(cowplot)
  library(DT)
  #......................
  # read excel
  #......................
  mydata <- reactiveVal(data.frame())
  observeEvent(input$file1, {
    # read in
    file <- input$file1
    ext <- tools::file_ext(file$datapath)

    if (is.null(input$file1)) {
      return(NULL)
    } else if (ext == "xlsx") {
      df <- readxl::read_excel(path = file$datapath) %>%
        dplyr::select(c("Patient", "MRN", "PCP", "Age", "Last HBA1C Date"))
      colnames(df)[5] <- "last_a1c_date"
    } else if (ext == "csv") {
      df <- readr::read_csv(file = file$datapath) %>%
        dplyr::select(c("Patient", "MRN", "PCP", "Age", "Last HBA1C Date"))
      colnames(df)[5] <- "last_a1c_date"
    }

    df <- df %>%
      dplyr::mutate(last_a1c_date = lubridate::dmy(last_a1c_date)) %>%
      magrittr::set_colnames(tolower(colnames(.))) %>%
      dplyr::mutate(age = gsub(" y.o.", "", age),
                    age = as.numeric(age)) %>%
      dplyr::filter(age <= 75)  # filter age

    # store in global
    # https://stackoverflow.com/questions/57104314/in-shiny-loading-data-then-preprocessing-to-the-global-environment-then-showi
    mydata(df)
  })

  #......................
  # get table today to future out
  #......................
  today <- lubridate::ymd(Sys.Date())
  output$A1ctbl_future <- DT::renderDataTable(
    if (is.null(input$file1)) {
      DT::datatable(data.frame())
    } else {
      DT::datatable(
        mydata() %>%
          dplyr::filter( (last_a1c_date + 365) <= (today + input$tto)) %>%
          dplyr::filter( (last_a1c_date >= (today-365)) ) %>%
          magrittr::set_colnames(c("Patient", "MRN", "PCP", "Age", "Last HBA1C Date")),

        extensions = 'Buttons',
        # thanks to this person https://github.com/rstudio/DT/issues/267
        options = list(
          dom = "Blfrtip", buttons =
            list("copy", list(
              extend = "collection"
              , buttons = c("csv", "excel", "pdf")
              , text = "Download"
            ) ), # end of buttons customization

          # customize the length menu
          lengthMenu = list( c(10, 20, -1) # declare values
                               , c(10, 20, "All") # declare titles
          ), # end of lengthMenu customization
          pageLength = 10


        ), # end of options

        class = "display"
      )
    }
  )


  #......................
  # get table all out
  #......................
  output$A1ctbl_all <- DT::renderDataTable(
    if (is.null(input$file1)) {
      DT::datatable(data.frame())
    } else {
      DT::datatable(
        mydata() %>%
          dplyr::filter( (last_a1c_date + 365) <= (today + input$tto)) %>%
          magrittr::set_colnames(c("Patient", "MRN", "PCP", "Age", "Last HBA1C Date")),

        extensions = 'Buttons',

        # thanks to this person https://github.com/rstudio/DT/issues/267
        options = list(
          dom = "Blfrtip", buttons =
            list("copy", list(
              extend = "collection"
              , buttons = c("csv", "excel", "pdf")
              , text = "Download"
            ) ), # end of buttons customization

          # customize the length menu
          lengthMenu = list( c(10, 20, -1) # declare values
                             , c(10, 20, "All") # declare titles
          ), # end of lengthMenu customization
          pageLength = 10


        ), # end of options

        class = "display"
      )
    }
  )

  #......................
  # Cumulative Incidence Plot
  #......................
  output$cumA1cplot <- renderPlot({
    if (is.null(input$file1)) {
      ggplot() +
        theme_void()
    } else {
      # white space
      whitespaceplot <- ggplot() +
        theme_void()
      # plot
      plotdf <- mydata() %>%
        dplyr::mutate(day_overdue = last_a1c_date + 365,
                      ticker = 1) %>%
        dplyr::arrange(day_overdue)
      plotdf$ticker <- cumsum(plotdf$ticker)

      # today
      today <- tibble::tibble(
        today = lubridate::ymd(Sys.Date()),
        height = mean(plotdf$ticker),
        label = "Today")


      cumincidplot <- ggplot() +
        geom_vline(data = today,
                   aes(xintercept = today),
                   color = "#ef3b2c", alpha = 0.5) +
        ggrepel::geom_text_repel(data = today,
                                 aes(x = today,
                                     y = height,
                                     label = label),
                                 color = "#ef3b2c", alpha = 0.8,
                                 fontface = "bold") +
        geom_line(data = plotdf,
                  aes(x = day_overdue, ticker)) +
        ggtitle("Cumulative Incidence of A1c Overdue by Date") +
        ylab("Cumulative Incidence") +
        xlab("Date") + theme_bw() +
        scale_x_date() +
        xlim(c(today$today - 182, today$today + 182)) +
        theme(axis.text.x = element_text(angle = 45, face = "bold",
                                         hjust = 1),
              plot.title = element_text(vjust = 0.5, hjust = 0.5,
                                        face = "bold"))

      cowplot::plot_grid(whitespaceplot, cumincidplot, align = "v", ncol = 1,
                         rel_heights = c(0.2, 0.8))
    }
  })
}

shinyApp(ui, server)
