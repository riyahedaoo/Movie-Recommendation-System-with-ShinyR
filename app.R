library(shiny)
library(dplyr)
library(data.table)
library(readr)
library(recommenderlab)  

#import data
ratings_matrix <- read_csv("ratingsmatrix.csv")
BigMovie <- read_csv("BigMovie.csv")

genre_list <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")

#Popular movies Function 
popular <- function(ratings,n){
  ratings <- as.matrix(ratings)
  ratings <- as(ratings,"realRatingMatrix")
  test <- ratings[1,]
  
  modelpop <- Recommender(data=ratings, method ="POPULAR")
  recomm <- predict(object = modelpop, newdata=test, n=n)
  c <- as(recomm, "list")
  print(subset(BigMovie, movieId %in% as.list(as.data.frame(c)[,1]))[2:4])
}


#IBCF movies Function 
ibcf <- function(ratings,test, n){
  
  modelibcf <- Recommender(data=ratings, method ="IBCF")
  recomm <- predict(object = modelibcf, newdata=test, n=n)
  c <- as(recomm, "list")
  print(subset(BigMovie, movieId %in% as.list(as.data.frame(c)[,1]))[2:4])
}




ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  
  fluidRow(
    titlePanel(h1("Get the most popular movies filtering as follows:", align="center")),
    hr(),
    
    column(6,
           titlePanel("Sort by your fav Genre:"),
           selectInput(inputId = "g", label=h3("Select any one Genre:"), choices = genre_list),
           dataTableOutput(outputId = "g_table")
    ),
    column(6,
           titlePanel("Sort by the Year you want:"),
           selectInput(inputId = "y", label=h3("Select any one Year:"), choices=c(1915:2015)),
           dataTableOutput(outputId = "y_table")
    )
  ),
  hr(),
  hr(),
  br(),
  fluidRow(
    column(12,
      titlePanel(h1("Get personalised Movie Recommendations:", align="center")),
      hr(),
      selectInput(inputId = "m", label=h3("Select any one Genre:"), choices = genre_list, multiple = T),
      h3(textOutput(outputId = "text1")),
      dataTableOutput(outputId="table"), 
      textInput(inputId = "movie", label=h3("Type the name of any one movie from above you liked the most:"), width = "80%"),
      sliderInput(inputId = "n", label=h3("Number of Recommendations:") ,min=1 ,max=10 ,value=5),
      h3(textOutput(outputId = "text2")),
      dataTableOutput(outputId = "table2"),
    )
  ),
  hr(),
  hr(),
)

server <- function(input, output){
  
  output$g_table <- renderDataTable(({
    c <- which(genre_list==input$g)
    r <- subset(BigMovie, BigMovie[,c+4]==1)$movieId
    ratings <- ratings_matrix[as.character(r)]
    popular(ratings,5)
  }))
  
  output$y_table <- renderDataTable(({
    r <- subset(BigMovie, year==input$y)
    if(length(r$movieId)<5){
      r[2:4]
    }
    else{
      r <- r$movieId
      ratings <- ratings_matrix[as.character(r)]
      popular(ratings,5)
    }
  }))
  
  output$text1 <- renderText(({
    if(!is.null(input$m)){
    "These are the top 5 movies for your slected genre"
    }
  }))
  
  output$table <- renderDataTable({
    if(!is.null(input$m)){
      c <- which(genre_list==input$m)
      r <- subset(BigMovie, BigMovie[,c+4]==1)$movieId
      ratings <- ratings_matrix[as.character(r)]
      print("Top 5 movies")
      df=popular(ratings,5)
    }
    else{
      df=data.frame()
    }
   df
  })
  
  output$text2 <- renderText(({
    "Recommending the top movies for you:"
  }))
  
  output$table2 <- renderDataTable(({
    if(input$movie != ""){
      c <- subset(BigMovie, title %in% input$movie)$movieId[1]
      m <- ratings_matrix[as.character(c)]
      m <- which(m==max(m,na.rm = T))[1]
      
      c <- which(genre_list==input$m)
      r <- subset(BigMovie, BigMovie[,c+4]==1)$movieId
      ratings <- ratings_matrix[as.character(r)]
      ratings <- as.matrix(ratings)
      ratings <- as(ratings,"realRatingMatrix")
      ratings <- binarize(ratings,minRating=3)
      test <- ratings[m,]
      
      ibcf(ratings,test,input$n)
    }
    else{
      data.frame()
    }
  }))
}

shinyApp(ui, server)
