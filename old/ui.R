library(shiny)
library(psych)
library(plotly)
library(rsconnect)


shinyUI(
  fluidPage( theme="simplex.min.css",
    
    tags$h2("Pricing Derivatives using Monte Carlo simulation"),
    # tags$h2("Retirement: simulating wealth with random returns, inflation and withdrawals"),
    p("An implementation of the Monte Carlo method for estimating the values of Options, using Log-Normal random walks to simulate Stock Prices."),
    hr(),
    # Setup the page title
    # tagList(tags$head(tags$title("Derivatives Simulator")), h1(textOutput("title"))),    
    
    sidebarLayout(
      sidebarPanel(width = 3,
        uiOutput("userPanel"),
        hr(),
        sliderInput("So", "Initial Stock value:", value = 50, step = .01, min = 30, max = 100, sep = ",", pre = "$"), br(),
        # sliderInput("K", "Strike Price:", value = 50, step = .01, min = 0, max = 1000, sep = ",", pre = "$"),
        # hr(),
        sliderInput("r", "Risk-Free rate (r):", value = 8, step = .01, min = 0, max = 15, sep = ",", post = "%"), br(),
        # hr(),
        sliderInput("d", HTML("Continous dividend (&#948):"), value = 2, step = .01, min = 0, max = 15, sep = ",", post = "%"), br(),
        # hr(),
        sliderInput("sigma", HTML("Volatility (&#963):"), value = 20, step = .01, min = 0, max = 50, sep = ",", post = "%"), br(),
        # hr(),
        # dateRangeInput("daterange", "Date range:", start  = "2015-01-01", end    = "2015-12-31",
        #                                            min    = "2000-01-01", max    = "2050-12-31", format = "mm/dd/yyyy", separator = " - "),
        sliderInput("numSim", "Number of Simulations:", value = 100, step = 1, min = 1, max = 1000, sep = ","), br(),
        numericInput("numDays", "Days until Expiration:", value = 365, min = 1, max = 10000, step = 1),
        hr()
        # actionButton("simButton", "Simulate!")
        # p(actionButton("simButton",
        #                "Re-run simulation", icon("random")
        # ))
        # helpText("The graph on the right shows a boxplot of the departure " , "delays for the airline(s) your username &delta is allowed to view.")
      ),
      mainPanel(
        plotOutput("stock_plot", width = "1050px", height = "400px"),
        fixedRow(
          column(5, wellPanel(HTML('
              
              
              <div class="form-group shiny-input-container" style = "width:80px;font-size: 12px">
                  <label for="strikeO">Strike Price</label>
                  <input id="strikeO" type="number" class="form-control" value="50" min="40" max="60" step="1"/>
              </div>
              <table id="summaryTable" style = "font-size:12px" class="tabla">
                <tr>
                  <th style="width:180px;font-size: 16px;border-bottom: solid"> Options </th>
                  <th style="width:70px;font-size: 16px;border-bottom: solid"> <div > <B>Call</B> </div> </th>
                  <th style="width:70px;font-size: 16px;border-bottom: solid"> <div > <B>Put</B> </div> </th>
                </tr>
                <tr>
                  <td > <B>Black-Scholes</B> </div> </td>
                  <td > <div id="callT" class="shiny-text-output"></div> </td>
                  <td> <div id="putT" class="shiny-text-output"></div> </td>
                </tr>
                <tr>
                  <td > <B>Monte-Carlo</B> </div> </td>
                  <td > <div id="CallMC" class="shiny-text-output"></div> </td>
                  <td > <div id="PutMC" class="shiny-text-output"></div> </td>
                </tr>
                <tr>
                  <td > <B> Asian Price Average</B> </div> </td>
                  <td > <div id="CallAP" class="shiny-text-output"></div> </td>
                  <td > <div id="PutAP" class="shiny-text-output"></div> </td>
                </tr>
                <tr>
                  <td > <B> Asian Strike Average</B> </div> </td>
                  <td > <div id="CallAS" class="shiny-text-output"></div> </td>
                  <td > <div id="PutAS" class="shiny-text-output"></div> </td>
                </tr>
                <tr>
                  <td > <B> All-or-Nothing</B> </div> </td>
                  <td > <div id="CallBin" class="shiny-text-output"></div> </td>
                  <td > <div id="PutBin" class="shiny-text-output"></div> </td>
                </tr>

              </table>'),
                 HTML('<br/>
              <div class="form-group shiny-input-container" style = "width:80px;font-size: 12px">
                  <label for="barrier">Barrier</label>
                  <input id="barrier" type="number" class="form-control" value="55" min="40" max="60" step="1"/>
              </div>
              <table id="summaryTableBarrier" class="tabla">
                <tr>
                  <th style="width:180px;font-size: 16px;border-bottom: solid"> Barrier Options </th>
                  <th style="width:70px;font-size: 16px;border-bottom: solid"> <div > <B>Call</B> </div> </th>
                  <th style="width:70px;font-size: 16px;border-bottom: solid"> <div > <B>Put</B> </div> </th>
                </tr>
                <tr>
                  <td > <B>Knock In</B> </div> </td>
                  <td > <div id="CallKI" class="shiny-text-output"></div> </td>
                  <td> <div id="PutKI" class="shiny-text-output"></div> </td>
                </tr>
                <tr>
                  <td > <B>Knock Out</B> </div> </td>
                  <td > <div id="CallKO" class="shiny-text-output"></div> </td>
                  <td> <div id="PutKO" class="shiny-text-output"></div> </td>
                </tr>  
              </table>'))
                 ),
          column(7, 
                 HTML('<div style="margin:40px 0 0 180px"> <b style = "font-size:14px"> Option Premium <b/></div>') ,
                 
                 plotlyOutput("option_plot", width = "600px", height = "320px")
                 )
          
        )
        
        
      )
      )
  )
)

