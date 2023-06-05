# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(shiny)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(stringr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Sneaky Researchers: How Arbitary and Hidden Decisions can Affect Research Results"),
    p("You are an intern at a consulting firm and your boss asks you to conduct
             some market research. She thinks that those with smartphones spend more hours on the internet.
             After finding a data set from Statistics Canada, it's time for data cleaning
             and analysis. Let's see how your arbitrary choices will affect the 
             research outcome."),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("choice_drop_na",
                         "How do you deal with Missing Values?",
                         choices = c("Drop any row with NA",
                                     "Drop rows where the outcome is NA",
                                     "Keep all NAs and let R handle it")),
            tags$hr(),
            sliderInput("choice_sample_size",
                        "What sample size do you use?",
                        min = 100,
                        max = 3600,
                        value = 1100,
                        step = 500),
            tags$hr(),
            p("You choose to model the relationship with a linear model"),
            checkboxGroupInput("choice_variables",
                               "What other variables will you include in your model?",
                               choiceNames = c("Attending a school, college, CEGEP or university",
                                           "Employment Status",
                                           "Gender"),
                               choiceValues = c("is_in_school",
                                                "is_employed",
                                                "gender")),
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            h3("Possible differences based of Model Specifications"),
            p("Depending on your choice, the estimated difference between smartphone users and non-users varies!"),
            plotOutput("choice_viz"),
            h3("Result:"),
            textOutput("result_show_text"),
            textOutput("sig_show_text")
        )
    ),
    tags$br(),
    tags$br(),
    fluidRow(column(12, 
        "Created by Jessica Xu and Alex Yu under the supervision of Liza Bolton."), align = "right")
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Seed and data
    set.seed(520)
    df <- read_csv("data/cius_2020_pumf_clean.csv", show_col_types = FALSE)
    
    # All possible model specifications to permute over
    base_mod <- "total_internet_time ~ use_smartphone"
    all_mods <- c(base_mod,
                  str_c(base_mod, " + is_in_school"),
                  str_c(base_mod, " + is_employed"),
                  str_c(base_mod, " + gender"),
                  str_c(base_mod, " + is_in_school + is_employed"),
                  str_c(base_mod, " + is_in_school + gender"),
                  str_c(base_mod, " + is_employed + gender"),
                  str_c(base_mod, " + is_in_school + is_employed + gender")
    )
    all_sample_sizes <- c(100, 600, 1100, 1600, 2100, 2600, 3100, 3600)
    
    # Process the user's NA choice
    dropped_df <- reactive({
        if (identical(input$choice_drop_na, "Drop any row with NA")) {
            return(df %>% na.omit())
        } else if (identical(input$choice_drop_na, "Drop rows where the outcome is NA")) {
            return(df %>% drop_na(total_internet_time))
        } else {
            return(df[1:3600,])
        }
        # return(result)
    })
    
    # Calculate the model based off the user's choice of NA drop
    outcomes_df <- reactive({
        set.seed(520)
        coeffs <- c() # coefficients for the sex var
        pvals <- c() # p-values
        models <- c() # model specifications+sample size
        
        for(i in 1:length(all_mods)){
            formula <- as.formula(all_mods[i]) # model specification
            for(j in 1:length(all_sample_sizes)){
                model <- lm(formula,
                            data = sample_n(dropped_df(), all_sample_sizes[j])) # linear regression
                res <- summary(model)$coefficients
                coeffs <- c(coeffs, res["use_smartphoneTRUE", 1])
                pvals <- c(pvals, res["use_smartphoneTRUE", 4])
                models <- c(models, str_c(all_mods[i], all_sample_sizes[j], sep = ", "))
            }
        }
        ids <- 1:(length(all_mods)*length(all_sample_sizes)) # model ids
        
        outcome <- data.frame(index = ids, coeffs, pvals, models) %>% 
            mutate(sig = (pvals < 0.05))
        
        return(outcome)
    })
    
    # Get details based off user's selection
    results_vector <- reactive({
        # Choice of variables
        # NA if user didn't select that var, var name if they did select it
        var1 <- NA              #is_in_school
        var2 <- NA              #is_employed
        var3 <- NA              #gender
        print(input$choice_variables)
        if ("is_in_school" %in% input$choice_variables) {
            var1 <- "is_in_school"
        } 
        if ("is_employed" %in% input$choice_variables) {
            var2 <- "is_employed"
        } 
        if ("gender" %in% input$choice_variables) {
            var3 <- "gender"
        }
        covars <- c(var1, var2, var3)
        
        # parse the model that the user selected
        if (sum(is.na(covars)) == 3) {
            sel_model <- paste(
                base_mod, ", ", input$choice_sample_size, sep = ""
            )
        } else {
            sel_model <- paste(base_mod, " + ",
                               paste(covars[!is.na(covars)], collapse = " + "),
                               ", ", input$choice_sample_size, sep = "")
        }
        
        # Grab the row which the user selected
        sel_model_row <- outcomes_df()[which(outcomes_df()$models == sel_model),]
        # get details from the row
        sel_coeff <- round(sel_model_row$coeffs,2)
        sel_pval <- round(sel_model_row$pvals, 6)
        sel_sig <- sel_model_row$sig
        
        return(c(sel_coeff, sel_pval, sel_sig, sel_model))
    })
    
    # Output result
    output$result_show_text <- renderText({str_c(
        "On average, the difference in internet usage time between smartphone users and non-users over a typical week is ", 
        results_vector()[1], 
        " hours. ")})

    output$sig_show_text <- renderText({
        if (results_vector()[3]) {
            return(str_c("With a p-value of ", results_vector()[2], ", your findings are significant!"))
        } else {
            return(str_c("However, with a p-value of ", results_vector()[2], ", your findings are NOT significant."))
        }
    })
    
    # Follow code is relevant to generating the visualization
    
    output$choice_viz <- renderPlot({
        sel_outcome <- outcomes_df() %>% 
            mutate(selected = (models == results_vector()[4])) # identify which specifications user selected
        
        sel_outcome %>% 
            ggplot(aes(x = index, y = coeffs, fill = sig, alpha = selected)) + 
            geom_bar(stat = "identity") +
            theme_minimal() +
            scale_fill_discrete("Is this result\nsignificant? (p < 0.05)",
                                labels = c("No", "Yes")) +
            scale_alpha_discrete(range = c(0.25, 0.9),
                                 guide = "none") + 
            theme(axis.ticks.x = element_blank(),
                  axis.text.x = element_blank(),
                  legend.position = "bottom") + 
            labs(x = "Possible Model Specifications", 
                 y = "Difference in Internt Usage")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
