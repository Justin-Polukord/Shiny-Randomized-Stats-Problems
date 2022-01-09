#Stats Practice App


#Needed Packages/Functions
library(dplyr)

#Sum of squares function
ss <- function(data){
  round(sum((data - mean(data))^2), 3)
}

#Building Data Set
x <- runif(1, -99, 99) %>% round() #Mean
y <- runif(1, 0, 20) %>% round() #Standard Deviation
z <- rnorm(100, x, y) %>% round #A Dataset of A Hundred Thousand Numbers

sampleStatsOne <- sample(z, 5, replace = F) #Random Sample of Dataset Z (n = 5)

#User Interface

ui <- fluidPage(
  mainPanel(
    h4(strong("1. Use the following data set to answer the questions")),
    textOutput("sample_one"),
    
    h5(strong("1a. Find the sum")), #Question 1a
    textInput("ans1a","Answer", "0"),
    textOutput("sans1a"),
    
    h5(strong("1b. Find the mean")), #Question 1b
    #(textOutput("sample_one")),
    textInput("ans1b","Answer", "0"),
    textOutput("sans1b"),
    
    h5(strong("1c. Find the sum of squares")), #Question 1c
    #(textOutput("sample_one")),
    textInput("ans1c","Answer", "0"),
    textOutput("sans1c"),
    
    h5(strong("1d. Find the variance")), #Question 1d
    #(textOutput("sample_one")),
    textInput("ans1d","Answer", "0"),
    textOutput("sans1d"),
    
    h5(strong("1e. Find the standard deviation")), #Question 1e
    #(textOutput("sample_one")),
    textInput("ans1e","Answer", "0"),
    textOutput("sans1e"),
    
    h4(strong("2. Find the Z Score for these three numbers")),
    tableOutput("zScores"),
    tableOutput("measures_one"),
    textInput("ans2a","Number One", "10"),
    textOutput("sans2a"),
    #textOutput("answer"),
    textInput("ans2b","Number Two", "10"),
    textOutput("sans2b"),
    textInput("ans2c","Number Three", "10"),
    textOutput("sans2c"),
    
    #Question Three
    h4(strong("3. Calculate the stand errors for the listed sample sizes")),
    tableOutput("measures_two"),
    
    h5(strong("5")),
    textInput("ans3a", "Answer", "0"),
    textOutput("sans3a"),
    
    h5(strong("10")),
    textInput("ans3b", "Answer", "0"),
    textOutput("sans3b"),
    
    h5(strong("15")),
    textInput("ans3c", "Answer", "0"),
    textOutput("sans3c"),
    
    h5(strong("30")),
    textInput("ans3d", "Answer", "0"),
    textOutput("sans3d"),
    
    h5(strong("100")),
    textInput("ans3e", "Answer", "0"),
    textOutput("sans3e"),
    
    
    #Question Four: Time For Big Boy Stats Practice
    h4(strong("4. Is the null or alternative hypothesis retained? (The server operation that says if the answer is correct or not needs to built)")),
    tableOutput("p_data"),
    radioButtons("ans4a", h5("Which is retained?"),
                       choices = list("Not Answered" = 2,
                                      "Null Hypothesis" = 0,
                                      "Alternative Hypothesis" = 1), 
                 selected = 2
                 ),
    tableOutput("ans4d")
  )
)

#Server

server <- function(input, output){
  
  output$sample_one <- renderText({
    sampleStatsOne
    
    
  })
  #Answer 1a
  output$sans1a <- renderText({
    if(round(sum(sampleStatsOne), 3) == as.numeric(input$ans1a)){
      print("ye")
    } else {
      print("Nope.")
    }
    
  })
  
  #Answer 1b
  output$sans1b <- renderText({
    
    if(round(mean(sampleStatsOne), 3) == as.numeric(input$ans1b)){
      print("ye")
    } else {
      print("Nope.")
    }
    
  })
  #Answer 1c
  output$sans1c <- renderText({
    
    if(round(ss(sampleStatsOne), 3) == as.numeric(input$ans1c)){
      print("ye")
    } else {
      print("Nope.")
    }
    
  })
  #Answer 1d
  output$sans1d <- renderText({
    
    if(round(var(sampleStatsOne), 3) == as.numeric(input$ans1d)){
      print("ye")
    } else {
      print("Nope.")
    }
    
  })
  #Answer 1e
  output$sans1e <- renderText({
    
    if(round(sd(sampleStatsOne), 3) == as.numeric(input$ans1e)){
      print("ye")
    } else {
      print("Nope.")
    }
  })
  #Answer 2
  #Generating The Z Scores
  zScore <- sample(z, 3, replace = F) #Finding a random number then getting the Z Score
  zScore <- data.frame(zScore)
  colnames(zScore) <- c("Numbers")
  
  #Creating the answers for the z scores
  zScore_ans <- zScore %>%
    mutate(Z_Score = (Numbers - x)/y)
  
  #Outputing the Z Scores 
  output$zScores <- renderTable({
    zScore
  })
  
  output$answer <- renderText({
    round(zScore_ans$Z_Score[1], 2)
  })
    
  #Outputing the needed measures
  stats <- data.frame("Paramater Mean" = x, "Paramater SD" = y)
  
  output$measures_one <- renderTable({
    stats
  })
  
  #Fist Number Answer  
    output$sans2a <- renderText({
      if(round(zScore_ans$Z_Score[1], 2) == as.numeric(input$ans2a)){
        print("ye")
      } else {
        print("Nope.")
      }
  })
    
    #Second Number Answer  
    output$sans2b <- renderText({
      if(round(zScore_ans$Z_Score[2], 2) == as.numeric(input$ans2b)){
        print("ye")
      } else {
        print("Nope.")
      }
    })
    
    #Third Number Answer  
    output$sans2c <- renderText({
      if(round(zScore_ans$Z_Score[3], 2) == as.numeric(input$ans2c)){
        print("ye")
      } else {
        print("Nope.")
      }
    })
  
  #Question Three Answers
    #Redoing the SD and Mean
   
     output$measures_two <- renderTable({
      stats
    })
    
     #Question a
    output$sans3a <- renderText({
      if(round(y/sqrt(5), 3) == as.numeric(input$ans3a)){
        print("ye")
      } else {
        print("Nope.")
      }
    })
    
    #Question b
    output$sans3b <- renderText({
      if(round(y/sqrt(10), 3) == as.numeric(input$ans3b)){
        print("ye")
      } else {
        print("Nope.")
      }
    })
    
    #Question c
    output$sans3c <- renderText({
      if(round(y/sqrt(15), 3) == as.numeric(input$ans3c)){
        print("ye")
      } else {
        print("Nope.")
      }
    })
    
    #Question d
    output$sans3d <- renderText({
      if(round(y/sqrt(30), 3) == as.numeric(input$ans3d)){
        print("ye")
      } else {
        print("Nope.")
      }
    })
    
    #Question e
    output$sans3e <- renderText({
      if(round(y/sqrt(100), 3) == as.numeric(input$ans3e)){
        print("ye")
      } else {
        print("Nope.")
      }
    })
    
  #Question Four
    #Data Set
    N_mean <- runif(1, 0, 99) %>% round() #Mean
    N_SD <- runif(1, 0, 20) %>% round() #Standard Deviation
    N_pop <- rnorm(100000, N_mean, N_SD) %>% round #A Data set of A Hundred Thousand Numbers
    
    #sample size
    n <- round(runif(1, 30, 100))
    
    #Standard Error
    SE <- round(sd(N_pop)/sqrt(n), 2) 
    
    #Is it going to be significant or not?
    chances <- runif(1, min = 0, max = 1) 
    
    
    #Not significant
    if(chances < .5 ){
      #No
      change <- "none"
      base_z <- 0
      ans4 <- 0
      #New Standard Error
      N_pop <- N_pop
      sample <- sample(N_pop, n, replace = F)
      xm <- mean(sample)
      
      #Significant
    } else {
      ans4 <- 1
      change <- "yes"
      base_z <- round(runif(1, 1.645, 3.49), 2)
      xm <- (base_z * SE) + mean(N_pop)
    }
    
    practice_dataQ4 <- cbind(Population.Mean = mean(N_pop),Population.SD = sd(N_pop), Sample.Mean = xm, Sample.Size = n)
    
    #Visualizing the data needed
    output$p_data <- renderTable({
      practice_dataQ4
    })
    
    nope <- "Wrong"
    nope <- cbind(answer = nope)
    
    #Displaying the answer
    output$"ans4d" <- renderTable({
      #Null Hypothesis Retained
      if(ans4 == 0){
        if(ans4 == input$ans4a){
            z_score <- round(((xm-mean(N_pop))/SE), 4)
            answersQ4 <- cbind(change, z_score)
        } else {
          nope
        }
      } else {
        #Alternative Hypothesis is Retained
        if (ans4 == input$ans4a) {
            z_score <- round((xm-mean(N_pop))/SE, 4)
            cohens_d <-round((xm-mean(N_pop))/sd(N_pop), 4)
            answers <- cbind(change, z_score, cohens_d)
        } else {
          nope
        }
      }
    })
    
}



#Run the app
shinyApp(ui, server)

#if(sum(sampleStatsOne) == is.numeric(ans1a)){
#  print("Nope")
#} else {
#  print("ye")
#}
