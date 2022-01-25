library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(plotly)
library(png)




fluidPage(#shinythemes::themeSelector(),
  theme = shinytheme("flatly"),
  titlePanel("Big Data Analytics"),
  
  
  navbarPage(
    br(),
    br(),
    tabPanel(icon("home"),          
             fluidRow(
               
               
               column(width=7,
                      br(),
                      br(),
                      p("L'objectif de cette application est de montrer les différentes méthodes d'apprentissage statistique supervisé:",
                        br(),
                        strong("- Les méthodes des arbres de décision et d'aggrégation"),
                        em("      ( Forets alétoires et Boosting )"),
                        br(),
                        strong("- Les méthodes de pénalisation"),
                        em("      ( Lasso, Elastic-Net et Adaptive Lasso )")
                        ,style="text-align:left;color:black;background-color:lavender;padding:15px;border-radius:10px")),
               br(),
               br(),
               br(),
               
               column(width = 7,
                      br(),
                      br(),
                      p("Apprentissage supervisé de classification permet de prévoir la variable cible binaire
         (Retard) de notre base de données", strong("Give me some credit"),". Cette dernière contient 11 variables, dont 1 variable cible qualitative et 10 variables quantatives. ",style="text-align:left;color:black;background-color:lavender;padding:15px;border-radius:10px"))
             ),
             hr(),
             br(),
             br(),
             br(),
             br(),
             tags$style(".fa-database {color:#E87722}"),
             h3(p(icon("database",lib = "font-awesome"),em("Kaggle: Give me some money "),style="color:black;text-align:center")),
             fluidRow(column(width=2),column(DT::dataTableOutput("RawData"),
                                             width = 7)),
             
             fluidRow(column(width=7,
                             br(),
                             br(),
                             p(strong("Description des variables de base de données:"),
                               br(),
                               br(),
                               br(),
                               strong("Retard:"),"(binaire) Client a connu un retard de paiement de 90 jours ou pire.",
                               br(),
                               br(),
                               strong("Solde_Rev:"),'(pourcentage) Solde total sur cartes de crédit et marges de crédit personnelles sauf immobilier et dette sans versement comme les prets automobiles divisé par la somme des limites de crédit.',
                               br(),
                               br(),
                               strong("Age:")," (continue) Age de l'emprunteur en année. ",
                               br(),
                               br(),
                               strong("N_dep30_50:"),'(continue) Nombre de fois un emprunteur a entre 30 et 59 jours passé.',
                               br(),
                               br(),
                               strong("P_dette:"),"(pourcentage) Paiement mensuels de la dette, pension alimentaire et frais de subsistance sur le revenu brut mensuel.",
                               br(),
                               br(),
                               strong("Rev_mois:"),"(continue) Revenu mensuel.",
                               br(),
                               br(),
                               strong("N_pret_ouvert:"),"(continue) Nombre de prets ouverts (comme un pret automobile ou hypothécaire) et des lignes de crédit (cartes de crédit).",
                               br(),
                               br(),
                               strong("N_dep90:"),"(continue) Nombre de fois qu'un emprunteur a été en reatard de plus de 90 jours ou plus .",
                               br(),
                               br(),
                               strong("N_hypo_immo:"), "(continue) Nombre d'hypotheques et de biens immobiliers, prets immobiliers dont l'habitation lignes de crédit sur les fods propres.",
                               br(),
                               br(),
                               strong("N_dep60_89:"),"(continue) Nombre de fois qu'un emprunteur a été entre 60 et 89 jours en défaut mais pas pire que les 2 derniere années.",
                               br(),
                               br(),
                               strong("P_charge:"),"(continue) Nombre de personnes a charges hors l'emprunteur (conjoint, enfants, etc)."
                               ,style="text-align:left;color:black;background-color:beige;padding:15px;border-radius:10px"))),
             
             hr(),
             p(em("Developé par"),br("COULIBALY , DOCET , NGUETTIA,  PERALTE"),style="text-align:left; font-family: times")
             
             
             
             
    ),
    tabPanel("Statistiques Descriptives",
             fluidRow(column(width=2),column(h1( "Imputation des valeurs manquantes"
                                                 ,style="text-align:center;color:green;background-color:white;padding:15px;border-radius:10px", width= 1000),
                                             width = 8)),
             fluidRow(column(width=2),column(p("Nous allons observer les valeurs manquantes pour aboutir à une suppression ou à une transformation de certaines observations.",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px", width=1000),
                                             width = 8)),
             fluidRow(column(width=3),column(plotOutput("missvalue"),
                                             width = 8)),
             fluidRow(column(width=2),column(p("Uniquement 2 variables possèdent des valeurs manquantes (Rev_mois et P_charges) avec 2% des observations sont des valeurs manquantes.
                                Avec ce taux faible de valeurs manquantes, Nous avons fait une imputation par la médiane des lignes similaires."
                                               ,style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                             width = 8)),             
             fluidRow(column(width=2),column(h1( "Statistiques descriptives des variables continues"
                                                 ,style="text-align:center;color:green;background-color:white;padding:15px;border-radius:10px"),
                                             width = 9)), br(),br(),         
             
             
             sidebarLayout(
               sidebarPanel(selectInput(inputId = "var",
                                        label = "Variables:",
                                        choices = c("Age","Solde_Rev", "N_dep30_50", "P_dette","Rev_mois", "N_pret_ouvert",
                                                    "N_dep90","N_hypo_immo", "N_dep60_89", "P_charge")),
                            selectInput(inputId = "graph",
                                        label = "Graphique",
                                        choices = c("histogram", "box")
                                        
                            )   ,    
               ),
               mainPanel(column(plotlyOutput("quanti"),width = 10))
             ),
             fluidRow(column(width=1),column(h1( "Statistiques descriptives de la variable cible"
                                                 ,style="text-align:center;color:green;background-color:white;padding:15px;border-radius:10px"),
                                             width = 9)) ,
             sidebarLayout(
               sidebarPanel(selectInput(inputId = "variable_quali",
                                        label = "Variables:",
                                        choices = "Retard"),
                            
               ),
               mainPanel(column(plotlyOutput("quali"),width = 10))
             ),br(),br(),br(),p(em("Developé par"),br("COULIBALY , DOCET , NGUETTIA,  PERALTE"),style="text-align:left; font-family: times")
             
    ),
    tabPanel("Méthode de Pénalisation",
             fluidRow(column(width=2),
                      column(
                        h4(p("Regression  pénalisée",style="color:black;text-align:center")),
                        width=8,style="background-color:lavender;border-radius: 10px")
             ),
             br(),
             fluidRow(column(width=2),
                      column(
                        br(),
                        p("Les méthodes  pénalisées permettent de réduire la variabilité des estimateurs via la régularisation des coefficients.",br(),
                          "Le principe est d'accepter une légère augmentation du biais pour obtenir une réduction plus que proportionnelle de la variance.",
                          br(),
                          " Pour cela on va devoir diriger (réguler) un peu plus fermement la modélisation en imposant des contraintes sur les paramètres estimés de la régression
                        (contraintes) sur les paramètres estimés de la régression.",
                          br(),
                          "Dans ces méthodes de pénalisation la diminution de l'erreur de prédiction espérée va améliorer.",
                          br(),
                          "Les méthodes les plus populaires de la régression pénalisée sont:",
                          br(),"-",strong("Ridge"), "est la première méthode de pénalisation.",
                          br(),"- Les régressions", strong("Lasso"),", ",strong("Adaptive Lasso"),"et ",strong("Elastic-Net"),
                          "vont plus loin que la réduction de la variance, elles procèdent à la sélection automatique de variables",
                          br(),
                          br(),
                          br(),
                          br(),
                          "La modélisation de ces différentes méthodes ce fait sur l'échantillon d'apprentissage
                        et la validation croisée ce fait sur l'échantillon de validation.",
                          style="color:black;text-align:justify"), 
                        withMathJax(),
                        width=8,style="background-color:lavender;border-radius: 10px")
             ),br(),br(),
             tabsetPanel(tabPanel("Régression Ridge", 
                                  br(),
                                  br(),
                                  br(),
                                  column(width=2),
                                  column(
                                    h4(p("La régression Ridge",style="color:black;text-align:center")),
                                    width=8,style="background-color:beige;border-radius: 10px;text-align:center"),
                                  br(),
                                  br(),
                                  br(),
                                  withMathJax(),
                                  p('$$ L( \\beta; \\lambda)=\\frac{1}{n} \\sum_{i=1}^n[y_i log(p(X_i,Z_i))+ (1-y_i)log(1-p(X_i,Z_i))]- \\lambda*\\sum_{j=1}^p\\beta_j^2   $$ ',style="color:black;border:1px solid black;background-color:white"),
                                  br(),
                                  p("On va donc ajouter une contrainte sur les coefficients lors de la modélisation pour maitriser l'amplitude de leurs valeurs.",
                                    br(),
                                    br(),
                                    "Les principes de cette méthode:", br(),
                                    "- Le scrinkage: rétrécissement des plages de valeurs que peuvent prendre les paramètres estimées ", br(),
                                    "- Les variables x doivent etre centrée et réduites (z) pour éviter que les variables à forte 
                                variance aient trop d'influence.",
                                    br(),
                                    "- Norme-2, des coefficients",
                                    br(),
                                    br(),
                                    "Lambda est le coefficient de pénalité qui permet de controler l'impact de la pénalité, cette dernière est à fixer. Dans la formule, on multiplie lambda 
                                à la fonction de pénalité, et c'est sur ce derniers que lambda controle l'impact des coefficients",br(),
                                    "Pour déterminer lambda ''optimal'' on aura recours à la validation croisée:5-Fold. L'idée est de choisir une valeur lambda de sorte que l'estimateur de ridge minimise l'erreur de prédiction ou l'erreur de généralisation."
                                    ,style="background-color:beige;border-radius: 10px"),
                                  
                                  sidebarLayout(
                                    sidebarPanel(
                                      p("fixer le paramètre lambda ou sélectionner la validation croisée")  ,
                                      sliderInput('lamda', "choisissez la valeur du paramètre",min=0, max=20 ,step=0.5,value=0),
                                      radioButtons("CV", "Cross validation",c("Validation croisée"=1,"paramètre fixer"=2)) ),
                                    mainPanel(
                                      h3(p(strong('Model summary',style="color:green")),align="center"),
                                      tags$head(tags$style("#resume_ridge{height: 400px;width:auto;border: 1px solid black; background-color: lavender}")),
                                      fluidRow(column(width=2),column(verbatimTextOutput("resume_ridge"),width = 8)),
                                      
                                      
                                      h3(p(strong('Courbe ROC du Model final',style="color:green")),align="center"),
                                      tags$head(tags$style("#courbe_ridge{height: 600px;width:auto;border: 1px solid black; background-color: lavender}")),
                                      
                                      fluidRow(column(width=2),column(plotOutput("courbe_ridge"),width = 8))
                                    )) ),
                         
                         tabPanel("Régression Lasso",
                                  br(),
                                  br(),
                                  br(),
                                  column(width=2),
                                  column(
                                    h4(p("La régression Lasso",style="color:black;text-align:center")),
                                    width=8,style="background-color:beige;border-radius: 10px;text-align:center"),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  p('$$ L( \\beta; \\lambda)=\\frac{1}{n} \\sum_{i=1}^n[y_i log(p(X_i,Z_i))+ (1-y_i)log(1-p(X_i,Z_i))]- \\lambda*\\sum_{j=1}^p|\\beta_j|   $$',style="color:black;border:1px solid black;background-color:white"), 
                                  br(),
                                  br(),
                                  p("Le lasso (Least Absolute Shrinkage And Selection Operation), très similaire au Ridge, le lambda est aussi le coefficient de pénalité, plus il est élevé et plus la régularisation est forte. 
                                Les x sont, la aussi centrées et réduites.",br(),
                                    "Ce qu'il a de plus de le Ridge, c'est qu'il peut faire une sélection de variable, en acceptant les coefficients nuls.",
                                    br(),
                                    br(),
                                    "L'inconvénient c'est qu'à très grandes dimensions, LASSO ne sélectionne uniquement les n 
                                variables prédictives au maximum, mécaniquement. C'est une limitation de l'algorithme. Parmi un groupe 
                                de variables corrélées, LASSO en choisit une, celle qui est la plus liée à la cible, mais qui peuvent masquer l'influence des autres.",
                                    br(),
                                    br(),
                                    "Comme dans le Ridge, nous allons utilisé la validation croisée, pour trouver le lambda''optimal''."
                                    ,style="background-color:beige;border-radius: 10px"),
                                  sidebarLayout(
                                    sidebarPanel(
                                      p("fixer le paramètre lambda ou sélectionner la validation croisée")  ,
                                      sliderInput('lamda_lasso', "choisissez la valeur du paramètre",min=0, max=20 ,step=0.5,value=0),
                                      radioButtons("CV_lasso", "Cross validation",c("Validation croisée"=1,"paramètre fixer"=2)) ),
                                    mainPanel(
                                      h3(p(strong('Model summary',style="color:salmon")),align="center"),
                                      tags$head(tags$style("#resume_lasso{height: 400px;width:auto;border: 1px solid black; background-color: lavender}")),
                                      fluidRow(column(width=2),column(verbatimTextOutput("resume_lasso"),width = 8)),
                                      
                                      
                                      h3(p(strong('Courbe ROC du Modèle final',style="color:salmon")),align="center"),
                                      tags$head(tags$style("#courbe_lasso{height: 600px;width:auto;border: 1px solid black; background-color: lavender}")),
                                      
                                      fluidRow(column(width=2),column(plotOutput("courbe_lasso"),width = 8))))),
                         
                         tabPanel("Régression Elastic-Net",
                                  br(),
                                  br(),
                                  br(),
                                  column(width=2),
                                  column(
                                    h4(p("La régression Elastic-Net",style="color:black;text-align:center")),
                                    width=8,style="background-color:beige;border-radius: 10px;text-align:center"),
                                  br(),
                                  br(),
                                  br(),
                                  p('$$ L( \\beta; \\lambda)=\\frac{1}{n} \\sum_{i=1}^n[y_i log(p(X_i,Z_i))+ (1-y_i)log(1-p(X_i,Z_i))]- \\sum_{j=1}^p (\\lambda_2 \\beta_j^2 + \\lambda|\\beta_j|)   $$',style="color:black;border:1px solid black;background-color:white"), 
                                  br(),
                                  p(" Elle est une combinaison du Lasso et du Ridge:",
                                    br(),
                                    " - Capacité de sélection de variables du LASSO conservée (coefficients nuls): exclusion des variables non pertinentes.",
                                    br(),
                                    " - Groupe de variables prédictives corrélées, partage des poids (comme Ridge) et non plus sélection arbitraire.",style="background-color:beige;border-radius: 10px"),
                                  sidebarLayout(
                                    sidebarPanel(
                                      p("fixer le paramètre lambda ou sélectionner la validation croisée")  ,
                                      sliderInput('lamda_en', "choisissez la valeur du paramètre",min=0, max=20 ,step=0.5,value=0),
                                      radioButtons("CV_en", "Cross validation",c("Validation croisée"=1,"paramètre fixer"=2)) ),
                                    mainPanel(
                                      h3(p(strong('Model summary',style="color:salmon")),align="center"),
                                      tags$head(tags$style("#resume_en{height: 400px;width:auto;border: 1px solid black; background-color: lavender}")),
                                      fluidRow(column(width=2),column(verbatimTextOutput("resume_en"),width = 8)),
                                      
                                      
                                      h3(p(strong('Courbe ROC du Model final',style="color:salmon")),align="center"),
                                      tags$head(tags$style("#courbe_en{height: 600px;width:auto;border: 1px solid black; background-color: lavender}")),
                                      
                                      fluidRow(column(width=2),column(plotOutput("courbe_en"),width = 8))))))     ),
    
    tabPanel("Méthodes d'Aggrégation",
             fluidRow(column(width=2),
                      column(
                        h4(p("Méthode d'aggrégation",style="color:black;text-align:center")),
                        width=8,style="background-color:lavender;border-radius: 10px")
             ),
             br(),
             fluidRow(column(width=2),
                      column(
                        br(),
                        p("L'amélioration des prévision peuvent se faire avec l'aggrégation de plusieurs arbres de décision.",br(),
                          br(),
                          
                          "Nous allons mettre en avant 2 méthodes d'aggrégation:",br(),
                          strong("- Random Forest"),br(),
                          strong("- Boosting"), br(),
                          br(),style="color:black;text-align:justify"), 
                        withMathJax(),
                        width=8,style="background-color:lavender;border-radius: 10px")
             ),br(),br(),
             
             tabsetPanel(
               tabPanel("Random Forest", 
                        br(),
                        br(),
                        br(),
                        column(width=2),
                        column(
                          h4(p("La méthode de Random Forest",style="color:black;text-align:center")),
                          width=8,style="background-color:beige;border-radius: 10px;text-align:center"),
                        br(),
                        br(),
                        br(),
                        fluidRow(column(width=2),column(p("C’est une méthode d’agrégation d’arbre utilisée pour pallier aux 
                                                    insuffisances des arbres de décision. Pour cela elle s’attèle principalement
                                                    à réduire la corrélation entre les hypothèses issues des arbres agrégés. 
                                                    Elle procède donc à la fois à un échantillonnage des individus et des 
                                                    prédicteurs candidats à la division à chaque itération. La prédiction 
                                                    finale pour un individu est obtenue par vote majoritaire des hypothèses 
                                                    dans le cas de la classification et est obtenue en faisant la moyenne des 
                                                    hypothèses dans le cas de la regression.",style="text-align:justify;color:black;background-color:beige;padding:15px;border-radius:10px"),
                                                        width = 8)), br(),br(),
                        sidebarLayout(
                          sidebarPanel(
                            
                            radioButtons("CV_rf", "Cross validation",c("Validation croisée"=1,"Nombre de variable à fixer"=2)),
                            sliderInput('mt', "choisissez la valeur du paramètre",min=0, max=12 ,step=1,value=6)),
                          mainPanel(
                            
                            
                            
                            h3(p(strong('Courbe ROC du Modèle',style="color:green")),align="center"),
                            tags$head(tags$style("#courbe_rf{height: 600px;width:auto;border: 1px solid black; background-color: lavender}")),
                            
                            fluidRow(column(width=2),column(plotOutput("courbe_rf"),width = 8)))))  ,
               
               
               tabPanel("Boosting",
                        br(),
                        br(),
                        br(),
                        column(width=2),
                        column(
                          h4(p("La méthode de Boosting",style="color:black;text-align:center")),
                          width=8,style="background-color:beige;border-radius: 10px;text-align:center"),
                        br(),
                        br(),
                        br(),
                        fluidRow(column(width=2),column(p("Les méthodes de Boosting repose sur une statégie adaptative pour ''booster'' les performances prédictives.
         Elle fait également partir des méthodes d’agrégation d’arbre sauf 
                                                qu’ici au lieu de construire les hypothèses de manières indépendantes, 
                                                elle va plutôt s’adapter aux résultats obtenus à l’itération précédente.
                                                Ainsi, les individus mal classés lors d’une itération seront fortement 
                                                pénalisée à l’itération suivante. De ce fait, l’hypothèse finale sera 
                                                obtenue en agrégeant l’ensemble des hypothèses tout en pénalisant f
                                                ortement les hypothèses qui ont obtenues de faibles résultats. ",
                                                          br(),
                                                          br(),
                                                          br(),
                                                          br(),
                                                          "On utilisera le parametre du RandomForest pour trouver le lambda ''optimal'' ",style="text-align:justify;color:black;background-color:beige;padding:15px;border-radius:10px"),
                                                        width = 8)),
                        h3(p(strong('Courbe ROC du model final',style="color:green")),align="center"),
                        tags$head(tags$style("#courbe_boost{height: 400px;width:auto;border: 1px solid black; background-color: lavender}")),
                        fluidRow(column(width=2),column(plotOutput("courbe_boost"),width = 8)),
                        
               )
               
             )),
    tabPanel("Comparaisons",
             fluidRow(column(width=12),
                      
                      
                      column(
                        p("Le but de cette partie est de présenter les mesures de performance des modèles utilisés dans chaque section. Nous présenterons les mesures de performance suivante :",style="color:black;text-align:justify"),
                        p("- L’AUC",style="color:black;text-align:justify"),
                        p("- L’indice de Gini (PGI)",style="color:black;text-align:justify"),
                        p("- Le taux de bonne classification (PCC)",style="color:black;text-align:justify"),
                        p("- La statistique de Kolmogorov-Smirnov (KS)",style="color:black;text-align:justify"),
                        p("Le modèle le plus performant sera celui qui a la valeur la plus élevé de l’AUC et l’indice de Gini et la valeur la plus faible du taux d’erreur.",style="color:black;text-align:justify"),
                        width=8,style="background-color:lavender;border-radius: 10px")),
             h3(p(strong('Tableau Récapitulatif',style="color:green")),align="center"),
             tags$head(tags$style("#conclusion{height: 400px;width:auto;border: 1px solid black; background-color: lavender}")),
             fluidRow(column(width=2),column(tableOutput("Conclusion"),width = 5)),
             fluidRow( column(width=2), column(textOutput("sortie"),width = 6,style="background-color:papayawhip;border-left:8px solid coral;border-top: 1px solid black;border-right:1px solid black;border-bottom: 1px solid black"),  
                       width = 5)
             
             
             
             
    )
  ))



