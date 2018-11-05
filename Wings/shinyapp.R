library(shiny)
library(tidyverse)

menu <- read_csv("WingsPrice.txt")
menu <- mutate(menu, per.Unit = Price / Count)
menuChoices <- as.vector(menu$Count)
bestmenuChoices <- c(25, 50, 75, 100, 125, 150, 200)

# UI side code to create interface
ui <- fluidPage(
  titlePanel("Ordering chicken wings at Danny’s Wok\n"),
  
  # Sidebar with a slider input for number of wings ordered
  sidebarLayout(
    sidebarPanel(
      numericInput(
        inputId = "wings",
        label = "How many chicken wings do you want to order?:",
        value = 25,
        min = 4,
        width = '33%'
        )
    ),
  
  # Main panel output
  mainPanel(tableOutput("orderSummary")), position = "left")
)

# Server side code to render output
server <- function(input, output) {
  output$orderSummary <- renderTable({
    myOrder <- input$wings
    
    orderTib <- NULL
    baseOrder <- 0
    myMod <- 0
    orderDex.A <- 0
    orderDex.B <- 0
    modDex <- 0
    reduceBase <- 0
    reduceMod <- 0
    newDex <- 0
    baseCost <- 0
    modCost <- 0
    cost <- 0
    puCost <- 0


    if (myOrder %in% menuChoices) {
      orderDex.0 <- which(menu$Count == myOrder)
      baseOrder <- 1
      baseCost <- menu$Price[orderDex.0]
      cost <- menu$Price[orderDex.0]
      puCost <- menu$per.Unit[orderDex.0]
      puCost <- format(round(puCost, 4), nsmall = 4)
      
      orderTib <- tibble(
        x = c("Order", "Total"),
        Order.count = c(baseOrder, "—"),
        Wings = c(menu$Count[orderDex.0], myOrder),
        Cost = c(as.numeric(baseCost), as.numeric(cost)),
        Cost.per.wing = c("—", puCost)
      )
      
    } else {
      baseOrder <- myOrder %/% 25 #integer division
      myMod <- myOrder %% 25 #remainder (modulus)
      
      if (myMod > 0 & myMod < 4) {
        #print("A")
        #fix menu glitch where there are no orders between 1 and 3
        baseOrder <- baseOrder - 1
        myMod <- myMod + 25
        modDex <- which(menu$Count == myMod)
      }
      
      orderDex.A <- which(menu$Count == 25)
      modDex <- which(menu$Count == myMod)
      baseCost <- menu$Price[orderDex.A] * baseOrder
      baseCost <- format(round(baseCost, 2), nsmall = 2)
      modCost <- format(round(menu$Price[modDex], 2), nsmall = 2)
      cost <-
        format(round((
          as.numeric(baseCost) + as.numeric(modCost)
        ), 2), nsmall = 2)
      puCost <- as.numeric(cost) / myOrder
      puCost <- format(round(puCost, 4), nsmall = 4)
      
      orderTib <- tibble(
        x = c("Order", "Order", "Total"),
        Order.count = c(baseOrder, 1, "—"),
        Wings = c(menu$Count[orderDex.A], menu$Count[modDex], myOrder),
        Cost = c(
          as.numeric(baseCost),
          as.numeric(modCost),
          as.numeric(cost)
        ),
        Cost.per.wing = c("—", "—", puCost)
      )
    }
#    orderTib

    print(orderTib)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
