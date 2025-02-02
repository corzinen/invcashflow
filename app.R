# app.R
library(shiny)
library(DT)
library(DBI)
library(RSQLite)
library(shinyjs)
library(bslib)

# --- Database Setup ---
db_file <- "properties.db"
conn <- dbConnect(SQLite(), db_file)

# Create the table only if it does not exist. (This way, data will persist across app reloads.)
dbExecute(conn, "
  CREATE TABLE IF NOT EXISTS properties (
    id INTEGER PRIMARY KEY,
    property_name TEXT,
    price REAL,
    sellers_equity REAL,
    percent_down REAL,
    sqft REAL,
    price_per_sqft REAL,
    assumable_ir REAL,
    term_months REAL,
    assumed_mortgage REAL,
    mortgage_payment REAL,
    hoa REAL,
    taxes REAL,
    landscaping REAL,
    power REAL,
    internet REAL,
    water REAL,
    management_fee REAL,
    total_cost REAL,
    rent REAL,
    cash_flow REAL,
    year_built INTEGER,
    rooms INTEGER,
    bathrooms REAL,
    private_pool INTEGER,
    community_pool INTEGER,
    created_at TEXT
  )
")

# --- Helper Functions ---

# PMT calculation (similar to Excel)
pmt <- function(rate, nper, pv) {
  if(rate == 0) return(pv / nper)
  rate * pv / (1 - (1 + rate)^(-nper))
}

# Save new property record, now including the additional property details.
save_property <- function(conn, property) {
  dbExecute(conn, "INSERT INTO properties (
      property_name, price, sellers_equity, percent_down, sqft, price_per_sqft,
      assumable_ir, term_months, assumed_mortgage, mortgage_payment, hoa, taxes,
      landscaping, power, internet, water, management_fee, total_cost, rent,
      cash_flow, year_built, rooms, bathrooms, private_pool, community_pool, created_at
    ) VALUES (
      :property_name, :price, :sellers_equity, :percent_down, :sqft, :price_per_sqft,
      :assumable_ir, :term_months, :assumed_mortgage, :mortgage_payment, :hoa, :taxes,
      :landscaping, :power, :internet, :water, :management_fee, :total_cost, :rent,
      :cash_flow, :year_built, :rooms, :bathrooms, :private_pool, :community_pool, :created_at
    )", params = property)
}

# Update an existing property record, including the additional property details.
update_property <- function(conn, property, id) {
  dbExecute(conn, "UPDATE properties SET
      property_name = :property_name,
      price = :price,
      sellers_equity = :sellers_equity,
      percent_down = :percent_down,
      sqft = :sqft,
      price_per_sqft = :price_per_sqft,
      assumable_ir = :assumable_ir,
      term_months = :term_months,
      assumed_mortgage = :assumed_mortgage,
      mortgage_payment = :mortgage_payment,
      hoa = :hoa,
      taxes = :taxes,
      landscaping = :landscaping,
      power = :power,
      internet = :internet,
      water = :water,
      management_fee = :management_fee,
      total_cost = :total_cost,
      rent = :rent,
      cash_flow = :cash_flow,
      year_built = :year_built,
      rooms = :rooms,
      bathrooms = :bathrooms,
      private_pool = :private_pool,
      community_pool = :community_pool,
      created_at = :created_at
    WHERE id = :id", params = c(property, id = id))
}

# Load saved properties from the database.
# If the connection is closed, reconnect.
load_properties <- function(conn) {
  if (!dbIsValid(conn)) {
    conn <<- dbConnect(SQLite(), db_file)
  }
  dbGetQuery(conn, "SELECT * FROM properties ORDER BY created_at DESC")
}

# Pivot properties for side-by-side comparison.
pivot_properties <- function(properties_df) {
  if(nrow(properties_df) == 0) return(data.frame(Parameter = character(0)))
  
  param_list <- list(
    "Price ($)"             = function(r) formatC(r$price, format = "f", digits = 2, big.mark = ","),
    "Equity/Down Payment ($)"   = function(r) formatC(r$sellers_equity, format = "f", digits = 2, big.mark = ","),
    "Down Payment as %"     = function(r) paste0(formatC(r$percent_down * 100, format = "f", digits = 2), " %"),
    "SQFT"                  = function(r) formatC(r$sqft, format = "f", digits = 0, big.mark = ","),
    "Price per SQFT ($)"    = function(r) formatC(r$price_per_sqft, format = "f", digits = 2, big.mark = ","),
    "Assumable IR"          = function(r) formatC(r$assumable_ir, format = "f", digits = 3),
    "Term (months)"         = function(r) r$term_months,
    "Assumed Mortgage ($)"  = function(r) formatC(r$assumed_mortgage, format = "f", digits = 2, big.mark = ","),
    "Mortgage Payment"      = function(r) formatC(r$mortgage_payment, format = "f", digits = 2, big.mark = ","),
    "HOA"                   = function(r) formatC(r$hoa, format = "f", digits = 2, big.mark = ","),
    "Taxes"                 = function(r) formatC(r$taxes, format = "f", digits = 2, big.mark = ","),
    "Landscaping"           = function(r) formatC(r$landscaping, format = "f", digits = 2, big.mark = ","),
    "Power"                 = function(r) formatC(r$power, format = "f", digits = 2, big.mark = ","),
    "Internet"              = function(r) formatC(r$internet, format = "f", digits = 2, big.mark = ","),
    "Water"                 = function(r) formatC(r$water, format = "f", digits = 2, big.mark = ","),
    "Management Fee"        = function(r) formatC(r$management_fee, format = "f", digits = 2, big.mark = ","),
    "Total Cost"            = function(r) formatC(r$total_cost, format = "f", digits = 2, big.mark = ","),
    "Expected Rent"         = function(r) formatC(r$rent, format = "f", digits = 2, big.mark = ","),
    "Monthly Cash Flow"     = function(r) formatC(r$cash_flow, format = "f", digits = 2, big.mark = ","),
    "Year Built"            = function(r) r$year_built,
    "Rooms"                 = function(r) r$rooms,
    "Bathrooms"             = function(r) r$bathrooms,
    "Private Pool"          = function(r) ifelse(r$private_pool == 1, "Yes", "No"),
    "Community Pool"        = function(r) ifelse(r$community_pool == 1, "Yes", "No")
  )
  
  comparison <- data.frame(Parameter = names(param_list), stringsAsFactors = FALSE)
  
  for(i in seq_len(nrow(properties_df))) {
    r <- properties_df[i, ]
    prop_values <- sapply(param_list, function(f) f(r))
    prop_col_name <- r$property_name
    comparison[[prop_col_name]] <- prop_values
  }
  comparison
}

# --- Shiny App UI using bslib ---
ui <- page_sidebar(
  window_title = "PCF",
  sidebar = sidebar(
    title = "Property Cash Flow & Comparison Tool (v1.0)",
    width = 400,
    id = "sidebar-panel",
    card(
      actionButton("save_btn", "Save", class = "btn-primary", style = "margin-top: 25px;"),
      textInput("property_name", "Name:", value = "")
    ),
    card(
      accordion(
        open = FALSE,
        multiple = FALSE,
        id = 'inputs',
        accordion_panel(
          title = "Property & Mortgage Inputs",
          layout_columns(
            numericInput("price", span(style = "font-size:12px;", "Price ($):"), value = 399999, min = 0, step = 1000),
            numericInput("sellers_equity", span(style = "font-size:12px;", "Equity/Down Payment ($):"), value = 100000, min = 0, step = 1000)
          ),
          layout_columns(
            numericInput("sqft", span(style = "font-size:12px;", "SQFT:"), value = 2000, min = 100, step = 10),
            numericInput("assumable_ir", span(style = "font-size:12px;", "Assumable IR:"), value = 0.055, min = 0, step = 0.001)
          ),
          layout_columns(
            numericInput("term_months", span(style = "font-size:12px;", "Term (months):"), value = 360, min = 1, step = 1),
            numericInput("hoa", span(style = "font-size:12px;", "HOA ($):"), value = 100, min = 0, step = 1)
          )
        ),
        accordion_panel(
          title = "Monthly Expenses",
          layout_columns(
            numericInput("annual_taxes", span(style = "font-size:12px;", "Annual Taxes ($):"), value = 3220, min = 0, step = 10),
            numericInput("landscaping", span(style = "font-size:12px;", "Landscaping ($):"), value = 300, min = 0, step = 10)
          ),
          layout_columns(
            numericInput("power", span(style = "font-size:12px;", "Power ($):"), value = 200, min = 0, step = 10),
            numericInput("internet", span(style = "font-size:12px;", "Internet ($):"), value = 80, min = 0, step = 5)
          ),
          layout_columns(
            numericInput("water", span(style = "font-size:12px;", "Water ($):"), value = 100, min = 0, step = 5),
            numericInput("rent", span(style = "font-size:12px;", "Expected Rent ($):"), value = 2500, min = 0, step = 50)
          )
        ),
        accordion_panel(
          title = "Property Details",
          layout_columns(
            numericInput("year_built", span(style = "font-size:12px;", "Year Built:"), value = 2020, min = 1800, step = 1)
          ),
          layout_columns(
            numericInput("rooms", span(style = "font-size:12px;", "Rooms:"), value = 3, min = 1, step = 1),
            numericInput("bathrooms", span(style = "font-size:12px;", "Bathrooms:"), value = 2, min = 1, step = 0.5)
          ),
          layout_columns(
            checkboxInput("private_pool", span(style = "font-size:12px;", "Private Pool"), value = FALSE),
            checkboxInput("community_pool", span(style = "font-size:12px;", "Community Pool"), value = FALSE)
          )
        )
      )
    )
  ),
  tabsetPanel(
    tabPanel("Current Calculation",
             h3("Calculated Results"),
             DTOutput("calc_results")
    ),
    tabPanel("Comparison",
             h3("Properties Comparison"),
             DTOutput("comparison_table")
    ),
    tabPanel("Manage Properties",
             h3("Saved Properties"),
             DTOutput("manage_table")
    )
  ),
  sidebar_collapsed = FALSE,
  theme = bs_theme(bootswatch = "shiny")
)

# --- Server ---
server <- function(input, output, session) {
  useShinyjs()  # initialize shinyjs
  
  # Reactive value to hold saved properties.
  properties_data <- reactiveVal(load_properties(conn))
  
  # Reactive value to track if we are editing a property.
  editing_property_id <- reactiveVal(NULL)
  
  # Function to refresh the reactive properties.
  refresh_properties <- function() {
    properties_data(load_properties(conn))
  }
  
  # Reactive expression to compute the current property calculation.
  current_calc <- reactive({
    price <- input$price
    sellers_equity <- input$sellers_equity
    sqft <- input$sqft
    percent_down <- if (price > 0) sellers_equity / price else NA
    price_per_sqft <- if (sqft > 0) price / sqft else NA
    assumed_mortgage <- price - sellers_equity
    monthly_rate <- input$assumable_ir / 12
    term <- input$term_months
    mortgage_payment <- pmt(monthly_rate, term, assumed_mortgage)
    taxes <- input$annual_taxes / 12
    management_fee <- 0.1 * input$rent
    total_cost <- mortgage_payment + input$hoa + taxes +
      input$landscaping + input$power + input$internet + input$water + management_fee
    cash_flow <- input$rent - total_cost
    
    list(
      price = price,
      sellers_equity = sellers_equity,
      percent_down = percent_down,
      sqft = sqft,
      price_per_sqft = price_per_sqft,
      assumable_ir = input$assumable_ir,
      term_months = input$term_months,
      assumed_mortgage = assumed_mortgage,
      mortgage_payment = mortgage_payment,
      hoa = input$hoa,
      taxes = taxes,
      landscaping = input$landscaping,
      power = input$power,
      internet = input$internet,
      water = input$water,
      management_fee = management_fee,
      total_cost = total_cost,
      rent = input$rent,
      cash_flow = cash_flow
    )
  })
  
  # Render the current calculation table.
  output$calc_results <- renderDT({
    calc <- current_calc()
    
    property_basics <- data.frame(
      Category = "Property Basics",
      Parameter = c("Price ($)",
                    "Equity/Down Payment ($)",
                    "Down Payment as %",
                    "SQFT",
                    "Price per SQFT ($)"),
      Value = c(
        formatC(calc$price, format = "f", digits = 2, big.mark = ","),
        formatC(calc$sellers_equity, format = "f", digits = 2, big.mark = ","),
        paste0(formatC(calc$percent_down * 100, format = "f", digits = 2), " %"),
        formatC(calc$sqft, format = "f", digits = 0, big.mark = ","),
        formatC(calc$price_per_sqft, format = "f", digits = 2, big.mark = ",")
      ),
      stringsAsFactors = FALSE
    )
    
    mortgage_info <- data.frame(
      Category = "Mortgage Info",
      Parameter = c("Assumed Mortgage ($)", "Mortgage Payment"),
      Value = c(
        formatC(calc$assumed_mortgage, format = "f", digits = 2, big.mark = ","),
        formatC(calc$mortgage_payment, format = "f", digits = 2, big.mark = ",")
      ),
      stringsAsFactors = FALSE
    )
    
    operating_costs <- data.frame(
      Category = "Monthly Operating Costs",
      Parameter = c("HOA",
                    "Taxes",
                    "Landscaping",
                    "Power",
                    "Internet",
                    "Water",
                    "Management Fee",
                    "Total Cost"),
      Value = c(
        formatC(calc$hoa, format = "f", digits = 2, big.mark = ","),
        formatC(calc$taxes, format = "f", digits = 2, big.mark = ","),
        formatC(calc$landscaping, format = "f", digits = 2, big.mark = ","),
        formatC(calc$power, format = "f", digits = 2, big.mark = ","),
        formatC(calc$internet, format = "f", digits = 2, big.mark = ","),
        formatC(calc$water, format = "f", digits = 2, big.mark = ","),
        formatC(calc$management_fee, format = "f", digits = 2, big.mark = ","),
        formatC(calc$total_cost, format = "f", digits = 2, big.mark = ",")
      ),
      stringsAsFactors = FALSE
    )
    
    income_flow <- data.frame(
      Category = "Income & Cash Flow",
      Parameter = c("Expected Rent", "Monthly Cash Flow"),
      Value = c(
        formatC(calc$rent, format = "f", digits = 2, big.mark = ","),
        formatC(calc$cash_flow, format = "f", digits = 2, big.mark = ",")
      ),
      stringsAsFactors = FALSE
    )
    
    results_df <- rbind(property_basics, mortgage_info, operating_costs, income_flow)
    
    datatable(
      results_df,
      extensions = "RowReorder",
      options = list(
        rowReorder = TRUE,
        pageLength = nrow(results_df),
        dom = 't'
      ),
      rownames = FALSE
    )
  }, server = FALSE)
  
  # Save or update the current property when the Save/Update button is clicked.
  observeEvent(input$save_btn, {
    calc <- current_calc()
    # Prepare a list of property values including the new property details.
    property <- list(
      property_name    = input$property_name,
      price            = calc$price,
      sellers_equity   = calc$sellers_equity,
      percent_down     = calc$percent_down,
      sqft             = calc$sqft,
      price_per_sqft   = calc$price_per_sqft,
      assumable_ir     = calc$assumable_ir,
      term_months      = calc$term_months,
      assumed_mortgage = calc$assumed_mortgage,
      mortgage_payment = calc$mortgage_payment,
      hoa              = calc$hoa,
      taxes            = calc$taxes,
      landscaping      = calc$landscaping,
      power            = calc$power,
      internet         = calc$internet,
      water            = calc$water,
      management_fee   = calc$management_fee,
      total_cost       = calc$total_cost,
      rent             = calc$rent,
      cash_flow        = calc$cash_flow,
      year_built       = input$year_built,
      rooms            = input$rooms,
      bathrooms        = input$bathrooms,
      private_pool     = ifelse(input$private_pool, 1, 0),
      community_pool   = ifelse(input$community_pool, 1, 0),
      created_at       = as.character(Sys.time())
    )
    
    # Check if we are editing an existing property.
    if (!is.null(editing_property_id())) {
      update_property(conn, property, editing_property_id())
      showNotification(paste("Updated property", input$property_name), type = "message")
      editing_property_id(NULL)
      updateActionButton(session, "save_btn", label = "Save")
    } else {
      if(nchar(trimws(input$property_name)) == 0) {
        property$property_name <- paste("Property", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      }
      save_property(conn, property)
      showNotification(paste("Saved", property$property_name), type = "message")
    }
    refresh_properties()
  })
  
  # Render the comparison table by pivoting the saved properties.
  output$comparison_table <- renderDT({
    comp_df <- pivot_properties(properties_data())
    datatable(
      comp_df,
      extensions = "ColReorder",
      options = list(
        colReorder = TRUE,
        dom = 't',
        pageLength = nrow(comp_df)
      ),
      rownames = FALSE
    )
  }, server = FALSE)
  
  # Render the Manage Properties table with Edit and Delete buttons.
  output$manage_table <- renderDT({
    df <- properties_data()
    if(nrow(df) == 0) {
      return(datatable(data.frame(Message = "No properties saved."), options = list(dom = 't')))
    }
    
    df$Edit <- sprintf(
      '<button class="btn btn-warning btn-sm" onclick="Shiny.setInputValue(\'edit_prop\', %d, {priority: \'event\'})">Edit</button>',
      df$id
    )
    df$Delete <- sprintf(
      '<button class="btn btn-danger btn-sm" onclick="Shiny.setInputValue(\'delete_prop\', %d, {priority: \'event\'})">Delete</button>',
      df$id
    )
    display_df <- df[, c("id", "property_name", "created_at", "Edit", "Delete")]
    
    datatable(
      display_df,
      escape = FALSE,
      options = list(dom = 't', pageLength = nrow(display_df))
    )
  }, server = FALSE)
  
  # When an Edit button is clicked, load that property into the inputs.
  observeEvent(input$edit_prop, {
    prop_id <- as.integer(input$edit_prop)
    df <- properties_data()
    prop <- df[df$id == prop_id, ]
    if(nrow(prop) == 1) {
      updateTextInput(session, "property_name", value = prop$property_name)
      updateNumericInput(session, "price", value = prop$price)
      updateNumericInput(session, "sellers_equity", value = prop$sellers_equity)
      updateNumericInput(session, "sqft", value = prop$sqft)
      updateNumericInput(session, "assumable_ir", value = prop$assumable_ir)
      updateNumericInput(session, "term_months", value = prop$term_months)
      updateNumericInput(session, "hoa", value = prop$hoa)
      updateNumericInput(session, "annual_taxes", value = prop$taxes * 12)  # taxes are stored monthly; adjust if needed.
      updateNumericInput(session, "landscaping", value = prop$landscaping)
      updateNumericInput(session, "power", value = prop$power)
      updateNumericInput(session, "internet", value = prop$internet)
      updateNumericInput(session, "water", value = prop$water)
      updateNumericInput(session, "rent", value = prop$rent)
      
      # Update the new property details.
      updateNumericInput(session, "year_built", value = prop$year_built)
      updateNumericInput(session, "rooms", value = prop$rooms)
      updateNumericInput(session, "bathrooms", value = prop$bathrooms)
      updateCheckboxInput(session, "private_pool", value = as.logical(prop$private_pool))
      updateCheckboxInput(session, "community_pool", value = as.logical(prop$community_pool))
      
      editing_property_id(prop_id)
      updateActionButton(session, "save_btn", label = "Update")
      showNotification(paste("Editing property", prop$property_name), type = "warning")
    }
  })
  
  # Delete a property when the corresponding delete button is clicked.
  observeEvent(input$delete_prop, {
    prop_id <- as.integer(input$delete_prop)
    dbExecute(conn, "DELETE FROM properties WHERE id = ?", params = list(prop_id))
    showNotification(paste("Deleted property ID", prop_id), type = "warning")
    refresh_properties()
  })
  
  session$onSessionEnded(function() {
    dbDisconnect(conn)
  })
}

shinyApp(ui, server)
