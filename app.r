library(shiny)
library(randomForest)

# Download and load the dataset
url <- "https://archive.ics.uci.edu/static/public/601/ai4i+2020+predictive+maintenance+dataset.zip"

zip_file <- tempfile(fileext = ".zip")
data_folder <- tempfile()
dir.create(data_folder)

download.file(url, zip_file, mode = "wb", quiet = TRUE)
unzip(zip_file, exdir = data_folder)

csv_file <- list.files(
  data_folder,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(csv_file) == 0) {
  stop("Dataset CSV not found.")
}

data <- read.csv(csv_file[1])
names(data) <- make.names(names(data))

# Prepare model data
df <- data.frame(
  Type = factor(data[["Type"]]),
  AirTemp = data[["Air temperature [K]"]],
  ProcessTemp = data[["Process temperature [K]"]],
  Speed = data[["Rotational speed [rpm]"]],
  Torque = data[["Torque [Nm]"]],
  ToolWear = data[["Tool wear [min]"]],
  Failure = factor(data[["Machine failure"]], levels = c(0, 1))
)

set.seed(123)
rows <- sample(seq_len(nrow(df)), floor(0.8 * nrow(df)))
train <- df[rows, ]

model <- randomForest(
  Failure ~ Type + AirTemp + ProcessTemp +
    Speed + Torque + ToolWear,
  data = train,
  ntree = 200
)

# Website interface
ui <- fluidPage(
  titlePanel("FaultWatch AI"),
  h4("Predictive Maintenance Dashboard"),
  p("Enter machine sensor readings to estimate failure risk."),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("type", "Product Type",
                  choices = c("L", "M", "H"), selected = "M"),
      
      numericInput("air", "Air Temperature (K)",
                   value = 300, min = 250, max = 350),
      
      numericInput("process", "Process Temperature (K)",
                   value = 310, min = 250, max = 400),
      
      numericInput("speed", "Rotational Speed (rpm)",
                   value = 1500, min = 0, max = 5000),
      
      numericInput("torque", "Torque (Nm)",
                   value = 40, min = 0, max = 200),
      
      numericInput("wear", "Tool Wear (minutes)",
                   value = 100, min = 0, max = 300),
      
      actionButton("predict", "Predict Machine Failure")
    ),
    
    mainPanel(
      h3("Prediction Result"),
      textOutput("result"),
      textOutput("risk"),
      plotOutput("chart")
    )
  )
)

# Prediction logic
server <- function(input, output) {
  
  prediction <- eventReactive(input$predict, {
    req(input$air, input$process, input$speed,
        input$torque, input$wear)
    
    new_machine <- data.frame(
      Type = factor(input$type, levels = levels(df$Type)),
      AirTemp = input$air,
      ProcessTemp = input$process,
      Speed = input$speed,
      Torque = input$torque,
      ToolWear = input$wear
    )
    
    probability <- predict(
      model, newdata = new_machine, type = "prob"
    )[1, "1"]
    
    list(probability = probability)
  })
  
  output$result <- renderText({
    req(prediction())
    if (prediction()$probability >= 0.30) {
      "Potential failure detected — inspect the machine."
    } else {
      "Lower predicted failure risk — continue monitoring."
    }
  })
  
  output$risk <- renderText({
    req(prediction())
    paste0(
      "Predicted failure probability: ",
      round(prediction()$probability * 100, 2), "%"
    )
  })
  
  output$chart <- renderPlot({
    req(prediction())
    barplot(
      prediction()$probability * 100,
      names.arg = "Failure risk",
      ylim = c(0, 100),
      ylab = "Predicted probability (%)",
      col = "steelblue",
      main = "Machine Failure Risk"
    )
  })
}

shinyApp(ui = ui, server = server)