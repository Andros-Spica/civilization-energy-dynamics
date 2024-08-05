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

lv_model_v3 <- function(pars, init = init, times = times) {
    # initial state 
    state <- init
    # derivative
    deriv <- function(t, state, pars) {
        with(as.list(c(state, pars)), {
            Ninvest = N * (1 - explorationBudgetShare)
            NreturnedByInvestment = Ninvest * eroi
            Navailable = R / eroi
            Nmax = min(NreturnedByInvestment, Navailable)
            d_N <- intrinsicGrowthRate * N * (Nmax - N)
            d_R <- formationRate - Nmax
            return(list(c(N = d_N, R = d_R)))
        })
    }
    # solve
    ode(y = state, times = times, func = deriv, parms = pars)
}
lv_model_v4 <- function(pars, init = init, times = times) {
    # initial state 
    state <- init
    # derivative
    deriv <- function(t, state, pars) {
        with(as.list(c(state, pars)), {
            P <- (R_0 - R + delta * t) / (R_0 + delta * t)
            theta = ((2 * theta_max - theta_0 + sqrt((2 * theta_max - theta_0) ^ 2 - theta_0 ^2)) * P + theta_0) * (1 - P)
            K <- (k_2 * R * (theta - 1))
            d_N <- k_1 * N * (K - N)
            d_R <- delta - N
            d_t = 1
            return(list(c(N = d_N, R = d_R, t = d_t)))
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
            selectInput("modelVersion", "Choose a model version:",
                        list(`v3` = "v3",
                             `v4` = "v4")
            ),
            withMathJax(),
            h3("Initial values"),
            sliderInput("initN",
                        "initial N:",
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
                        "Malthusian or intrinsic growth rate (r or \\( k_1 \\)):",
                        min = 0.01,
                        max = 0.25,
                        value = 0.1),
            conditionalPanel(
                condition = "input.modelVersion == 'v3'",
                sliderInput("explorationBudgetShare",
                            "exploration budget share (\\(\\beta\\)):",
                            min = 0,
                            max = 1,
                            value = 0.1)
            ),
            conditionalPanel(
                condition = "input.modelVersion == 'v4'",
                sliderInput("conversionRate",
                            "Critical Resource conversion rate (\\( k_2 \\)):",
                            min = 0,
                            max = 1,
                            value = 0.1)
            ),
            conditionalPanel(
                condition = "input.modelVersion == 'v3'",
                sliderInput("eroi",
                            "EROI \\((\\theta\\)):",
                            min = 5,
                            max = 90,
                            value = 30)
            ),
            conditionalPanel(
                condition = "input.modelVersion == 'v4'",
                sliderInput("eroi_init",
                            "Init. EROI \\((\\theta_0\\)):",
                            min = 5,
                            max = 90,
                            value = 10),
                sliderInput("eroi_max",
                            "Max. EROI \\((\\theta_{max}\\)):",
                            min = 5,
                            max = 90,
                            value = 30)
            ),
            sliderInput("formationRate",
                        "formation or drip rate \\((\\delta\\)):",
                        min = 1E-6,
                        max = 1E6,
                        value = 1E-6)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("simResults", height = "500px"),
           h4("Model equations (derivates)"),
           conditionalPanel(
               condition = "input.modelVersion == 'v3'",
               withMathJax(),
               fluidRow(
                   column(6,
                          helpText("Energy flux of a civilization:"),
                          helpText("$$\\frac{dN(t)}{dt}=rN(t)(1 - \\frac{N(t)}{N_{max}(t)})$$"),
                          helpText("$$N_{max}(t)=\\min\\left\\{ EROI(N(t)(1-\\beta)), R(t)/EROI \\right\\}$$")
                   ),
                   column(6,
                          
                          helpText("Resource or energy source (asymptotically available):"),
                          helpText("$$\\frac{dR(t)}{dt}=\\delta - N_{max}(t)$$")
                   )
               )
           ),
           conditionalPanel(
               condition = "input.modelVersion == 'v4'",
               withMathJax(),
               fluidRow(
                   column(6,
                          helpText("Energy flux of a civilization:"),
                          #helpText("$$\\frac{dN(t)}{dt}=rN(t)(1 - \\frac{N(t)}{N_{max}(t)})$$"),
                          #helpText("$$N_{max}(t)=\\min\\left\\{ EROI(N(t)(1-\\beta)), R(t)/EROI \\right\\}$$")
                   ),
                   column(6,
                          
                          helpText("Resource or energy source (asymptotically available):"),
                          #helpText("$$\\frac{dR(t)}{dt}=\\delta - N_{max}(t)$$")
                   )
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
    times <- seq(0, 10, by = 1)
    
    # output #######################################################
    
    output$simResults <- renderPlot({
        
        list_input <- reactiveValuesToList(input)
        
        if (input$modelVersion == "v3") {
            # initial state 
            init <- with(list_input, c(N = initN, R = initR))
            
            # parameters
            parameters <- with(list_input, c(
                intrinsicGrowthRate = intrinsicGrowthRate, 
                explorationBudgetShare = explorationBudgetShare, 
                eroi = eroi,
                formationRate = formationRate
            ))
            
            # simulation #######################################################
            
            lv_results <- data.frame(lv_model_v3(pars = parameters, init = init, times = times))
            
        }
        if (input$modelVersion == "v4") {
            # initial state 
            init <- with(list_input, c(N = initN, R = initR, t = 0))
            
            # parameters
            parameters <- with(list_input, c(
                k_1 = intrinsicGrowthRate, 
                k_2 = conversionRate, 
                theta_max = eroi_max,
                theta_0 = eroi_init,
                delta = formationRate,
                R_0 = initR
            ))
            
            # simulation #######################################################
            
            lv_results <- data.frame(lv_model_v4(pars = parameters, init = init, times = times))
            
        }
        
        dtplot <- ggplot(data = lv_results, aes(x = time)) +
            geom_line(data = lv_results, aes(x = time, y = N, color = "N")) +
            geom_line(data = lv_results, aes(x = time, y = R, color = "R")) +
            geom_hline(yintercept = 0, linetype = "dashed") +
            coord_trans(y = "sqrt") +
            theme_bw() +
            theme(
                axis.title = element_text(size = plotScale * 10),
                axis.text = element_text(size = plotScale * 10)
            )
        xyplot <- ggplot(data = lv_results, aes(x = N, y = R, colour = time)) +
            geom_path(linewidth = 3, alpha = 0.5, lineend="round") +
            scale_fill_continuous(high = "#132B43", low = "#56B1F7") +
            scale_x_log10() + scale_y_log10()
        grid.arrange(dtplot, xyplot)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
