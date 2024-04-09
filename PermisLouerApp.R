library(sp)
library(readxl)
library(sf)
library(dplyr)
library(knitr)
library(shiny)
library(shinythemes)

Table <- read.csv("LISTE COMMUNES PERMIS DE LOUER.csv")
commune <- st_read("France.gpkg")

# Nettoyage des noms de communes avec des expressions régulières
Table$ASNIERES <- gsub("<[[:digit:]]+>", "", Table$ASNIERES) # Supprime les séquences de chiffres entre "<" et ">"
Table$ASNIERES <- gsub("<[^>]+>", "", Table$ASNIERES) # Supprime tout ce qui est entre "<" et ">"
Table$ASNIERES <- gsub("\\s*\\(.*?\\)\\s*", "", Table$ASNIERES) # Suppression du texte entre parenthèses et les parenthèses elles-mêmes

names(Table)[1] <- "com"
names(commune)[2] <- "com"

# Ajouter une colonne "Existe"
commune <- commune %>% mutate(Existe = "oui")
commune <- commune %>% 
  mutate(departement = substr(id, 1, 2))

# Fonction pour nettoyer la chaîne
clean_string <- function(text) {
  text <- iconv(text, to = "UTF-8", sub = "byte")
  text <- toupper(text)
  text <- gsub("\\s", "-", text)
  text <- gsub("[^A-Za-z0-9-]", "", text)
  return(text)
}

# Appliquer la fonction à la colonne com
commune$com <- sapply(commune$com, clean_string)
Table$com <- sapply(Table$com, clean_string)

#jointure attributaire
join <- merge(x = Table , y = commune, by= "com", all.x =TRUE)

# Afficher le nom des communes Qui sont toucher
names(join) [1] <- "Commune_touche"

#on cherche par SQL les critère suivant :
communes_existantes <- join %>% 
  filter(Existe == "oui", departement == '06')%>%
  select(Commune_touche)


################################################################################
############################## Application shiny ###############################
################################################################################


# Définition de l'application Shiny
ui <- fluidPage(
  theme = shinytheme('superhero'),
  titlePanel("Communes Touchées"),
  sidebarLayout(
    sidebarPanel(
      textInput("departement", "Entrez le département (XX) :", value = "06")
    ),
    mainPanel(
      verbatimTextOutput("resultat")
    )
  )
)

server <- function(input, output) {
  observe({
    # Afficher le nom des communes qui sont touchées
    names(join)[1] <- "Commune_touche"
    communes_existantes <- join %>%
      filter(Existe == "oui", departement == input$departement) %>%
      select(Commune_touche)
    
    # Afficher le résultat sous forme de table
    output$resultat <- renderPrint({
      cat("Les communes qui sont touchées sont les suivantes :\n")
      kable(data.frame(Communes = communes_existantes), format = "markdown")
    })
  })
}

# Exécution de l'application Shiny
shinyApp(ui = ui, server = server)

# Configuration et déploiement de l'application
rsconnect::setAccountInfo(name='ali-sayer',
                          token='54196D71C5084DD8D6B5D58657C9E247',
                          secret='2Tr1PsABl6kKjGZhBez47T6qRIVmydX/kCgrRnce')

# Déployer l'application
rsconnect::deployApp('D:/QGIS_Ali/Platformes shiny')
