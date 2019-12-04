
# # Shiny app to help visualise our results from the OSR bias modelling project.
# # Julio Rincones-Gamboa
# # j.a.r.gamboa@gmail.com
# # jgamboa@bio.tamu.edu
# # P.I. Dr. Heath Blackmon
# # coleoguy@gmail.com

library(shiny)
library(shinyWidgets)
library(viridis)
library(ggplot2)



# Define UI for application that draws a violin plot
ui <- fluidPage(
    
    # Application title
    titlePanel("osRBias: A visualization tool to demonstrate the effect of operational
               sex ratio bias on sexually antagonistic variation"),
    # Choose sex, then the model, dominance factor for the allele beneficial 
    # to the common sex, and the selection coefficient.
    sidebarLayout(
        sidebarPanel(
            width = 3,
            selectInput("sex", "Select the common sex:", 
                        c("males", "females", "none"),selected="none"),
            uiOutput("model"),
            uiOutput("domin"),
            uiOutput("sel"),
        ),

# Show a plot of the generated distribution
mainPanel(
    plotOutput("distPlot")
    )
  )
)

options(shiny.maxRequestSize=300*1024^2)

server <- function(input, output) {
    #load the full dataset
     dataset <- read.csv("dataset.csv", 
                        header = TRUE, sep = ",", as.is = T, check.names = F,
                        row.names = 1) 
    #subset by sex
    choice <- reactive({
        a <- dataset
        if(input$sex != "none") a <- dataset[dataset$sex == input$sex, ]
        return(a)
    })
    
    # vector of models within the dataset
    model <- reactive(sort(unique(choice()$model)))
    
    #subset by selected model
    
    choice2 <- reactive({
        a <- choice()
        if(input$model != "none") a <- a[a$model == input$model, ]
        return(a)
    }) 
    
    # vector of available dominance factors for the allele beneficial to the 
    # common sex.
    domin <- reactive(sort(unique(choice2()$h)))
    
    # subset by dominance factor
    choice3 <- reactive({
        a <- choice2()
        if(input$domin != "none") a <- a[a$h == input$domin, ]
        return(a)
    }) 
    
    # vector of available selection coefficients
    
    sel <- reactive(sort(unique(choice3()$s)))
    
    # subset by selection coefficient
    choice4 <- reactive({
        a <- choice3()
        if(input$sel != "none") a <- a[a$s == input$sel, ]
        return(a)
    }) 
    
    # this makes the model dropdown menu
    output$model <- renderUI({
        selectInput("model", "Select a model:", choices = c("None", model()))
    })
    
    # this makes the dominance dropdown menu
    output$domin <- renderUI({
        selectInput("domin", "Select the dominance factor of the allele 
                    beneficial to the common sex:", choices = c("none", domin()))
    })
    
    # this makes the selection dropdown menu
    output$sel <- renderUI({
        selectInput("sel", "Selection coefficient:", choices = c("none", sel()))
    })
    
    # make final data selection function
    final.dat <- reactive({
        a <- choice()
        if(input$sex != "none") a <- a[a$sex == input$sex, ]
        if(input$model != "none") a <- a[a$model == input$model, ]
        if(input$domin != "none") a <- a[a$h == input$domin, ]
        if(input$sel != "none") a <- a[a$s == input$sel, ]
        return(a)
    })
    
  
    output$distPlot <- renderPlot({
        ggplot(final.dat(), aes(y=freq, x=as.factor(comm))) + ylim(0, 1) +
            geom_hline(yintercept = .5, alpha=.5) +
            geom_violin(aes(fill=as.factor(osr)),
                        stat="ydensity", position="dodge",
                        alpha=0.8, trim=TRUE, scale="area") +
            theme_light() +
            theme(legend.position = "right", 
                  legend.text = element_text(size = 16),
                  text=element_text(family="sans", 
                                    face="plain", color="#000000", 
                                    size=15, hjust=0.5, vjust=0.5)) + 
            guides(fill= guide_legend(title= "OSR")) +
            ylab("Allele frequency") +
            xlab("Common sex number") +
            # ggtitle("XY model-Autosomes") +
            scale_fill_viridis(discrete=TRUE) +
            scale_color_viridis(discrete=TRUE)
    })
}
shinyApp(ui,server)