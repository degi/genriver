# REFFERENCES
# https://shiny.posit.co/r/articles/improve/modules/
#

#' Editable table UI using excelR library
#'
#' @param id
#' @param title
#' @param is_paginated
#'
#' @return
#' @export
#' @import excelR
#'
#' @examples
table_edit_ui <- function(id,
                          title = "",
                          is_paginated = F) {
  ns <- NS(id)
  vspace <- "30px"
  if (is_paginated)
    vspace <- "90px"
  div(style = "height:100%;overflow:auto;",
      span(
        div(
          style = "float:right;",
          actionButton(
            ns("upload_btn"),
            "",
            icon = icon("upload"),
            class = "toolbar_button"
          )
          |> tooltip(
            "Upload data",
            id = ns("upload_tt"),
            options = list(customClass = "custom-tooltip")
          )
        ) |>
          popover(
            title = div(icon("upload"), "Upload data"),
            id = ns("upload_pop"),
            fileInput(ns("upload_file"), NULL, accept = c(".csv"))
          ),
        title
      ),
      
      excelR::excelOutput(ns("table_edit"), height = paste0("calc(100% - ", vspace, ")")))
}

table_edit_server <- function(id,
                              data,
                              col_title = NULL,
                              col_type = NULL,
                              col_disable = NULL,
                              col_source = NULL,
                              allowRowModif = F,
                              nrow = 0,
                              pagination = NULL,
                              csvFileName = "table_data") {
  
  moduleServer(id, function(input, output, session) {
    if (is.null(data))
      return()
    
    col_names <- colnames(data)
    
    if (is.null(col_title)) {
      col_title <- tools::toTitleCase(col_names)
      col_title <- gsub("_", " ", col_title)
    }
    
    
    col_render <- rep(NA, ncol(data))
    col_width <- col_render
    
    col_align <- rep("left", ncol(data))
    
    if (is.null(col_type)) {
      col_type_data <- sapply(data, class)
    } else {
      col_type_data <- col_type
    }
    
    col_type <- rep(NA, ncol(data))
    idx <- which(tolower(col_type_data) == "date")
    for (i in idx) {
      col_type[i] <- "calendar"
    }
    idx <- which(col_type_data == "logical")
    for (i in idx) {
      col_type[i] <- "checkbox"
    }
    idx <- which(col_names == "color")
    for (i in idx) {
      col_type[i] <- "color"
      col_title[i] <- ""
      col_render[i] <- "square"
      col_align[i] <- "center"
      col_width[i] <- 30
    }
    idx <- which(col_type_data == "numeric")
    for (i in idx) {
      col_align[i] <- "right"
    }
    idx <- which(col_type_data == "dropdown")
    for (i in idx) {
      col_type[i] <- "dropdown"
    }
    
    if (is.null(col_disable)) {
      col_disable <- rep(F, ncol(data))
    }
    
    
    data_column <- data.frame(
      title = col_title,
      type = col_type,
      render = col_render,
      align = col_align,
      width = col_width,
      readOnly = col_disable
      # source = col_source
    )
    
    if (!is.null(col_source)) {
      data_column$source <- col_source
    }
    
    table_data_edit <- reactiveVal(data)
    
    output$table_edit <- excelR::renderExcel({
      ns <- session$ns
      df <- table_data_edit()
      if (is.null(df))
        return()
      # print(df)
      excelR::excelTable(
        data = df,
        columns = data_column,
        tableOverflow = T,
        tableWidth = "100%",
        tableHeight = "100%",
        allowDeleteColumn = F,
        allowRenameColumn = F,
        allowInsertColumn = F,
        allowDeleteRow = allowRowModif,
        allowInsertRow = allowRowModif,
        rowDrag = allowRowModif,
        minDimensions = c(NA, nrow),
        pagination = pagination,
        autoIncrement = T,
        dateFormat = "DD-Mon-YYYY",
        csvFileName = csvFileName,
        defaultColWidth = 100,
        includeHeadersOnDownload = T
      )
    })
    
    table_data <- reactiveVal(data)
    
    observeEvent(input$table_edit, {
      inp <- input$table_edit
      df_input <- excel_to_R(inp)
      names(df_input) <- col_names
      df_input[df_input == ""] <- NA
      df_input <- df_input[rowSums(!is.na(df_input)) > 0, ]
      idx <- which(col_type_data == "numeric")
      for (i in idx) {
        df_input[[i]] <- as.numeric(df_input[[i]])
      }
      table_data(df_input)
    })
    
    date_formats <- c("%d-%b-%Y", "%d-%m-%Y", "%d/%m/%Y", "%Y-%m-%d", "%Y/%m/%d")
    # date_format <- "%d-%b-%Y"
    
    observeEvent(input$upload_file, {
      fpath <- input$upload_file$datapath
      df <- read.csv(fpath)
      toggle_popover("upload_pop", show = F)
      if (ncol(df) > length(col_type_data))
        df <- df[c(1:length(col_type_data))]
      # print(df)
      for (i in 1:length(col_type_data)) {
        if(i <= ncol(df)) {
          if (tolower(col_type_data[i]) == "date") {
            df[[i]] <- as.Date(df[[i]], tryFormats = date_formats)
            # print(df[[i]] )
          } else if (col_type_data[i] == "numeric") {
            df[[i]] <- as.numeric(df[[i]])
          } else if (col_type_data[i] == "color") {
            if(!is_color_code(df[[i]])) {
              df[[i]] <- "#000"
            }
          }
        } else {
          if (col_type_data[i] == "numeric") {
            df[,col_names[i]] <- NA
          } else {
            df[,col_names[i]] <- ""
          }
        }
      }
      colnames(df) <- col_names
      # colnames(df) <- col_names[1:ncol(df)]
      
      # if(ncol(df) <- ncol(col_names))
      
      table_data_edit(df)
      table_data(df)
    })
    
    observeEvent(input$upload_btn, {
      toggle_tooltip("upload_tt", show = F)
    })
    
    return(table_data)
  })
}

is_color_code <- function(x) {
  res <- try(col2rgb(x), silent = TRUE)
  return(!"try-error" %in% class(res))
}
