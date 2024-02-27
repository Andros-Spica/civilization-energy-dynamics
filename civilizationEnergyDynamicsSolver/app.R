#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(deSolve)
library(ggplot2)
library(gridExtra)

lv_model <- function(pars, init = init, times = times) {
    # initial state 
    state <- init
    # derivative
    deriv <- function(t, state, pars) {
        with(as.list(c(state, pars)), {
            Einvest = E * (1 - explorationBudgetShare)
            EreturnedByInvestment = Einvest * eroi
            Eavailable = R / eroi
            Emax = min(EreturnedByInvestment, Eavailable)
            d_E <- intrinsicGrowthRate * E * (1 - E / Emax)
            d_R <- formationRate - Emax
            return(list(c(E = d_E, R = d_R)))
        })
    }
    # solve
    ode(y = state, times = times, func = deriv, parms = pars)
}

# print today's date
today <- Sys.Date()
updateDate <- format(today, format="%d %b %Y")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Civilization Energy Dynamics model"),
    h4(paste0("by Alex Shenderov and Andreas Angourakis", " (", updateDate, ")")),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            withMathJax(),
            h3("Initial values"),
            sliderInput("initE",
                        "initial E:",
                        min = 0,
                        max = 10,
                        value = 1),
            sliderInput("initR",
                        "initial R:",
                        min = 0,
                        max = 1E6,
                        value = 1E4),
            h3("Parameters"),
            sliderInput("intrinsicGrowthRate",
                        "intrinsic growth rate (r):",
                        min = 0.01,
                        max = 0.25,
                        value = 0.1),
            sliderInput("explorationBudgetShare",
                        "exploration budget share (\\(\\beta\\)):",
                        min = 0,
                        max = 1,
                        value = 0.1),
            sliderInput("eroi",
                        "EROI:",
                        min = 5,
                        max = 90,
                        value = 30),
            sliderInput("formationRate",
                        "formation rate \\((\\delta\\)):",
                        min = 1E-6,
                        max = 1E6,
                        value = 1E-6)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("simResults", height = "500px"),
           h4("Model equations (derivates)"),
           withMathJax(),
           fluidRow(
               column(6,
                      helpText("Energy flux of a civilization:"),
                      helpText("$$\\frac{dE(t)}{dt}=rE(t)(1 - \\frac{E(t)}{E_{max}(t)})$$"),
                      helpText("$$E_{max}(t)=\\min\\left\\{ EROI(E(t)(1-\\beta)), R(t)/EROI \\right\\}$$")
               ),
               column(6,
                      
                      helpText("Resource or energy source (asymptotically available):"),
                      helpText("$$\\frac{dR(t)}{dt}=\\delta - E_{max}(t)$$")
                      )
           )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # input #######################################################
    
    plotScale = 2
    
    # times
    times <- seq(0, 500, by = 1)
    
    # output #######################################################
    
    output$simResults <- renderPlot({
        # parameters
        list_input <- reactiveValuesToList(input)
        
        # initial state 
        init <- with(list_input, c(E = initE, R = initR))
        
        parameters <- with(list_input, c(
            intrinsicGrowthRate = intrinsicGrowthRate, 
            explorationBudgetShare = explorationBudgetShare, 
            eroi = eroi,
            formationRate = formationRate
        ))
        
        # simulation #######################################################
        lv_results <- data.frame(lv_model(pars = parameters, init = init, times = times))
        
        dtplot <- ggplot(data = lv_results, aes(x = time)) +
            geom_line(data = lv_results, aes(x = time, y = E, color = "E")) +
            geom_line(data = lv_results, aes(x = time, y = R, color = "R")) +
            geom_hline(yintercept = 0, linetype = "dashed") +
            coord_trans(y = "sqrt") +
            theme_bw() +
            theme(
                axis.title = element_text(size = plotScale * 10),
                axis.text = element_text(size = plotScale * 10)
            )
        xyplot <- ggplot(data = lv_results, aes(x = E, y = R, colour = time)) +
            geom_path(linewidth = 3, alpha = 0.5, lineend="round") +
            scale_fill_continuous(high = "#132B43", low = "#56B1F7") +
            scale_x_log10() + scale_y_log10()
        grid.arrange(dtplot, xyplot)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
