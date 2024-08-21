library(shiny)
library(shinythemes)
library(ggplot2)

# Bernouilli 

bernoulli = function(p,k){
  if ( (k==0 | k==1) & (p>0 & p<1) ){
    probabilite = p**k * (1-p)**(1-k)
    print(probabilite)
  } else
    print("Entrer des valeurs correctes de k ou de p")
}

repartition.bern = function(p,k){
  i     = 0
  somme = 0
  while(i<=k){
    if ( (k==0 | k==1) & (p>0 & p<1) ){
      probabilite = p**i * (1-p)**(1-i)
      somme = somme + probabilite
      i = i+1
    } else
      print("Entrer des valeurs correctes de k ou de p")
  }
  print(somme)
}


# Binomiale 

binome = function(n, p, k){
  if(k<=n & (p>0 && p<1) & n>0 && is.numeric(n) && n%%1==0){
    combinaison = factorial(n)/(factorial(k)*factorial(n-k))
    probabilite  = combinaison * p**k * (1-p)**(n-k)
    print(probabilite)
  } else{
    print("Entrer des valeurs correctes de n, p et k")
  }
}


repartition.binom = function(n, p, k){
  i = 0
  somme = 0
  if(k<=n & (p>0 & p<1) & n>0 & is.numeric(n) & n%%1==0){
    while(i<=k){
      combinaison=factorial(n)/(factorial(i)*factorial(n-i))
      probabilite= combinaison * p**i * (1-p)**(n-i)
      somme = somme + probabilite
      i = i + 1
    }
  }else{
    print("Entrer des valeurs correctes de n, de p ou de k")
  }
  print(somme)
}


# Poisson 

poisson = function(lambda, k){
  if(lambda>0 && k>0 && is.numeric(k) && k%%1==0){
    probabilite = (exp(-lambda)*lambda**k)/factorial(k)
    print(probabilite)
  } else {
    print("Entrer des valeurs correctes de k ou de lambda")
  }
}

repartition.poiss = function(lambda, k){
  i = 0
  somme = 0
  if(lambda>0 && k>0 && is.numeric(k) && k%%1==0){
    while(i<=k){
      probabilite = (exp(-lambda)*lambda**i)/factorial(i)
      somme = somme + probabilite
      i = i + 1
    }
  }else{
    print("Entrer des valeurs correctes de n, de p ou de k")
  }
  print(somme)
}

# Loi geométrique 


geometrique = function(p, k){
  if(k>=0 &  (p>0 & p<1) &  is.numeric(k) & k%%1==0){
    probabilite = p*(1-p)**(k-1)
    print(probabilite)
  }else{
    print("Entrer des valeurs correctes de p ou de k")
  }
}


repartition.geom = function(p, k){
  i = 1
  somme = 0
  if(k>=0 &  (p>0 & p<1) &  is.numeric(k) & k%%1==0){
    while(i<=k){
      probabilite = p*(1-p)**(i-1)
      somme = somme + probabilite
      i = i + 1
    }
  }else{
    print("Entrer des valeurs correctes de p ou de k")
  }
  print(somme)
}


# Uniforme discrete 

uniforme = function(n){
  if(n>0 & is.numeric(n) & n%%1==0){
    probabilite = 1/n
    print(probabilite)
  } else{
    print("Entrer une valeur correcte de n")
  }
}


repartition.unif.d = function(n, k){
  if((k>=1 & k<n) & n>0 & is.numeric(n) & n%%1==0){
    probabilite = floor(k)/n
    print(probabilite)
  } else{
    if(k<1){
      print(0)
    } else{
      if(k>=n){
        print(1)
      }else{
        print("Entrer une valeur correcte de n ou de k")
      }
    }
  }
}











ui <- fluidPage(
  theme=shinytheme("sandstone") ,
  titlePanel("Statistiques et fonction de répartition des lois de Probabilité discrètes"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("lign", "Sélectionner une valeur de n :", value = 10, min = 1, max = 100, step = 1),
      numericInput("prob", "Sélectionner une valeur de p :", value = 0.5, min = 0, max = 1, step = 0.01),
      numericInput("lambda", "Sélectionner une valeur de lambda :", value = 0.5, min = 0, max = ,100, step = 0.1),
      sliderInput("select0", "Valeur k pour calculer P(X ≤ k)", value = 1, min = 0, max = 100, step = 1),
      selectInput("select", "Choisir une loi de probabilité:", 
                  choices = c("Binomiale", "Poisson", "Géométrique", "Uniforme discrète", "Binomiale négative")),
      selectInput("select2", "Choisir une couleur", choices = colors(), selected = "blue")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Statistiques Calculées", 
                 verbatimTextOutput("statsOutput")),
        tabPanel("Fonction de Répartition", 
                 plotOutput("cdfPlot")),
        tabPanel("Probabilité: P(X ≤ k)", 
                 verbatimTextOutput("probOutput")),
        tabPanel("Informations sur la Loi", 
                 uiOutput("infoOutput"))
      )
    )
  )
)

# Server: Reactive calculations for selected distribution
server <- function(input, output) {
  
  # Reactive expression to generate the distribution data based on the selected input
  distribution_data <- reactive({
    switch(input$select,
           "Binomiale" = rbinom(input$lign, size = input$lign, prob = input$prob),
           "Poisson" = rpois(input$lign, lambda = input$lambda),
           "Géométrique" = rgeom(input$lign, prob = input$prob),
           "Uniforme discrète" = runif(input$lign, min = 0, max = 100),
           "Binomiale négative" = rnbinom(input$lign, size = input$lign, prob = input$prob))
  })
  
  # Output: Affichage du calcul des statistiques 
  output$statsOutput <- renderPrint({
    data <- distribution_data()
    
    # Moyenne, variance, médiane, écart-type
    moyenne <- mean(data)
    variance <- var(data)
    mediane <- median(data)
    ecart_type <- sd(data)
    
    cat("Loi sélectionnée:", input$select, "\n",
        "Moyenne:", round(moyenne, 2), "\n",
        "Variance:", round(variance, 2), "\n",
        "Médiane:", round(mediane, 2), "\n",
        "Écart-type:", round(ecart_type, 2))
  })
  
  # Fonction de repartition
  cdf_data <- reactive({
    k <- seq(0, 100, by = 1)
    cdf <- switch(input$select,
                  "Binomiale" = pbinom(k, size = input$lign, prob = input$prob),
                  "Poisson" = ppois(k, lambda = input$prob * input$lign),
                  "Géométrique" = pgeom(k, prob = input$prob),
                  "Uniforme discrète" = punif(k, min = 0, max = 100),
                  "Binomiale négative" = pnbinom(k, size = input$lign, prob = input$prob))
    data.frame(k = k, CDF = cdf)
  })
  
  # Output: Plot the CDF
  output$cdfPlot <- renderPlot({
    data <- cdf_data()
    
    # Graphique
    ggplot(data, aes(x = k, y = CDF, color = Lois)) +
      geom_point(size = 1.2, colour = input$select2) +
      labs(title = paste("Fonctions de répartition - Loi", input$select),
           x = "Valeur de k", y = "Fonction de répartition (CDF)") +
      scale_x_continuous(breaks = seq(0, 100, by = 5)) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # Output: Calcule de la P(X ≤ k)
  output$probOutput <- renderPrint({
    prob <- switch(input$select,
                   "Binomiale" = pbinom(input$select0, size = input$lign, prob = input$prob),
                   "Poisson" = ppois(input$select0, lambda = input$prob * input$lign),
                   "Géométrique" = pgeom(input$select0, prob = input$prob),
                   "Uniforme discrète" = punif(input$select0, min = 0, max = 100),
                   "Binomiale négative" = pnbinom(input$select0, size = input$lign, prob = input$prob))
    
    cat("Pour la loi", input$select, "et k =", input$select0, ":\n",
        "P(X ≤", input$select0, ") =", round(prob, 4))
  })
  
  output$infoOutput <- renderUI({
    info <- switch(input$select,
                   "Binomiale" = HTML("
                   <h4>Loi Binomiale</h4>
                   <p><strong>Lois de probabilités:</strong> P(X = k) = (n .C. k) p<sup>k</sup> (1-p)<sup>n-k</sup></p>
                   <p><strong>Espérance:</strong> E(X) = n × p</p>
                   <p><strong>Variance:</strong> Var(X) = n × p × (1-p)</p>
                 "),
                   "Poisson" = HTML("
                   <h4>Loi de Poisson</h4>
                   <p><strong>Lois de probabilités:</strong> P(X = k) = (λ<sup>k</sup> e<sup>-λ</sup>) / k!</p>
                   <p><strong>Espérance:</strong> E(X) = λ</p>
                   <p><strong>Variance:</strong> Var(X) = λ</p>
                 "),
                   "Géométrique" = HTML("
                   <h4>Loi Géométrique</h4>
                   <p><strong>Lois de probabilités:</strong> P(X = k) = p (1-p)<sup>k-1</sup></p>
                   <p><strong>Espérance:</strong> E(X) = 1 / p</p>
                   <p><strong>Variance:</strong> Var(X) = (1-p) / p<sup>2</sup></p>
                 "),
                   "Uniforme discrète" = HTML("
                   <h4>Loi Uniforme Discrète</h4>
                   <p><strong>Lois de probabilités:</strong> P(X = k) = 1 / n</p>
                   <p><strong>Espérance:</strong> E(X) = (n + 1) / 2</p>
                   <p><strong>Variance:</strong> Var(X) = (n<sup>2</sup> - 1) / 12</p>
                 "),
                   "Binomiale négative" = HTML("
                   <h4>Loi Binomiale Négative</h4>
                   <p><strong>Lois de probabilités:</strong> P(X = k) = (k+r-1 .C. k) p<sup>r</sup> (1-p)<sup>k</sup></p>
                   <p><strong>Espérance:</strong> E(X) = r × (1-p) / p</p>
                   <p><strong>Variance:</strong> Var(X) = r × (1-p) / p<sup>2</sup></p>
                 ")
    )
    info
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
