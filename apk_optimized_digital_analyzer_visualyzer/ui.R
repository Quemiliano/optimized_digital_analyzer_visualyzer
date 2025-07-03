#Author: Emiliano QUENUM
#Linkedin: https://www.linkedin.com/in/emiliano-quenum-50973a253


pacman::p_load(shiny, shinythemes, reactable, DT, DiagrammeR)

source("global.R", local = TRUE) # Import des variables contenu dans le fichier global.R

fluidPage(
  theme = shinytheme('sandstone'),
  style = "border : px solid silver; background-color: #F0F0F0",
  
  tags$head(
    tags$link(rel = "shortcut icon", href = "logo_apk_icone.ico"),
    
    # Balise meta pour la réactivité mobile
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    
    # Balise meta pour l'encodage des caractères
    tags$meta(charset = "UTF-8"),
    
    # Balise meta pour la description de la page
    tags$meta(name = "ANO", content = "Analyseur Numérique Optimisé par Emiliano QUENUM"),
    
    tags$style(
      HTML(
        "body{font-family: 'Times New Roman', sans-serif;font-size: 18px;color: dark}"
      )
    )
  ),
  

  
  div(class = "box", style = " border: 50px solid #F0F0F0"),
  
  tabsetPanel(
    tabPanel(
      strong(h4("Importation des données")),
      br(),
      p(
        "Chaque administration, collectivité ou établissement concerné bénéficie d'un outil crucial pour l'information et la prise de décision : le rapport social unique.
                 Ce document constitue le point de départ indispensable pour élaborer les orientations de gestion qui définissent la stratégie à long terme des ressources humaines."
      ),
      p(
        "La rédaction du Rapport sociale Unique (RSU) se basera sur l'exploitation d'une base de données. Cette base de données est issue du remplissage du
               questionnaire annuel relatif aux Ressources Humaines (RH)  de chacune des trois Directions Départementales Interministérielles (DDI) de l'ille-et-vilaine par les personnels du service RH du SGCD35.
               Notamment, pour la Direction Départementale de l'Emploi du Travail et des Solidarités (DDETS35), la Direction Départementale des Territoires et de la Mer (DDTM35) et la Direction Départementale de la Protection des Populations (DDPP35)."
      ),
      p(""),
      
      p(
        "Zone d'importation réservée aux bases de données relatives aux RSU:"
      ),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          style = "border : px solid silver; background-color: #191970;color: white",
          fileInput(
            "rsu_initiale_file",
            label = "Choisir un fichier",
            accept = c(".xlsx", ".xls", ".csv", ".ods"),
            capture = "environment",
            buttonLabel = "Parcourir",
            placeholder = "Aucun fichier sélectionné"
          ),
          actionButton("valider_import", "Valider", style = "background-color: green")
        ),
        
        mainPanel(
          width = 9,
          style = "border : 5px solid gray;",
          reactableOutput("current_table")
        )
      ),
      br(),
      p(
        "Zone d'importation dédiée aux informations concernant les partitions de la base de données:"
      ),
      
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          style = "border : px solid silver; background-color: #7A90A4;color: white",
          fileInput(
            "rsu_recap_file",
            label = "Choisir un fichier",
            accept = c(".xlsx", ".xls", ".csv", ".ods"),
            capture = "environment",
            buttonLabel = "Parcourir",
            placeholder = "Aucun fichier sélectionné"
          ),
          actionButton("valider_import_recap", "Valider", style = "background-color: green")
        ),
        
        mainPanel(
          width = 9,
          style = "border : 5px solid gray;",
          reactableOutput("recap_current_table")
        )
      )
      
    ),
    
    
    tabPanel(
      strong(h4("Traitement et visualisation")),
      selectInput(
        inputId = "DDI",
        label = h3("Sélection de la DDI à exploiter"),
        choices = NULL
      ),
      
      ###-----------------------------------Action social
      
      
      tabsetPanel(
        tabPanel("Action sociale", br(), fluidRow(
          column(
            width = 3,
            style = "border : px solid silver; background-color: #191970;color: white",
            h3("Partition de table:"),
            
            selectInput(
              width = "100%",
              inputId = "select_start_act_soc",
              label = "Colonne de début:",
              choices = NULL,
              multiple = F
            ),
            
            selectInput(
              width = "100%",
              inputId = "select_end_act_soc",
              label = "Colonne de fin:",
              choices = NULL,
              multiple = F
            ),
            
            actionButton("valider_act_soc", "Valider", style = "background-color: green;"),
            downloadButton("btn_telecharger_act_soc", "Télécharger la table"),
            
            checkboxGroupInput(
              inputId = "btn_ajout_act_soc_col",
              label = "",
              choices = "Ajouter des colonnes"
            ),
            
            textInput(
              width = "100%",
              inputId = "act_soc_col",
              label = "Nom de la colonne",
              value = "ex: nom_col ou nouveau_col1;nouveau_col2;nouveau_col3;.."
            ),
            
            textInput(
              width = "100%",
              inputId = "ajout_act_soc_col_val",
              label = "Valeur",
              value = "ex: 2 ou 3;4;5;3;2;4;....."
            ),
            
            checkboxGroupInput(
              inputId = "btn_del_act_soc_col",
              label = "",
              choices = "Supprimer des colonnes"
            ),
            selectInput(
              width = "100%",
              inputId = "act_soc_del_col",
              label = "Choix des colonnes",
              choices = NULL,
              multiple = T
            ),
            actionButton("valider_act_soc_modif", "Valider modification", style = "background-color: green;")
          ),
          column(
            width = 9,
            style = "border : 1px solid silver;",
            mainPanel(
              width = 12,
              style = "border : 1px solid silver;",
              reactableOutput("table_act_soc")
            ),
            div(class = "box", style = "width: 100%; height:150px"),
            
            sidebarPanel(
              style = "background-color: #191970;color: white",
              h3("Présentation des données:"),
              br(),
              
              
              selectInput(
                inputId = "sous_theme_act_soc",
                label = "Choix du sous-thème",
                choices =
                  c("A- Préstations sociales", "Informations supplémentaires")
              ),
              
              checkboxGroupInput(
                inputId = "vent_crois_btn_act_soc",
                label = "",
                choices = "Tableau ventilé ou croisé par défaut"
              ),
              
              selectInput(
                inputId = "vent_crois_act_soc",
                label = "",
                choices =
                  NULL
              ),
              
              checkboxGroupInput(
                inputId = "manuelle_btn_act_soc",
                label = "",
                choices = "Tableau composé manuellement"
              ),
              
              
              selectInput(
                inputId = "select_manuelle_vent_act_soc",
                label = "",
                choices =
                  NULL,
                multiple = T
              ),
              
              h3("Visualisations graphiques"),
              
              selectInput(
                inputId = "plot_act_soc",
                label = "",
                choices = c(
                  "Camembert",
                  "Histogramme",
                  "Histogramme superposé (tableau croisé uniquement)"
                )
              ),
              fluidRow(column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_title_act_soc_1",
                  label = "Titre du graphique n°1",
                  value =
                    "ex:  Graphique 1....."
                )
              ), column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_title_act_soc_2",
                  label = "Titre du graphique n°2",
                  value =
                    "ex: Graphique 2..."
                )
              )),
              
              fluidRow(column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_act_soc_1_1",
                  label = "Axe 1 graph 1",
                  value =
                    "ex: Intitulés..."
                )
              ), column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_act_soc_2_1",
                  label = "Axe 1 graph 2",
                  value =
                    "ex: Intitulés..."
                )
              )),
              
              
              fluidRow(column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_act_soc_1_2",
                  label = "Axe 2 graph 1",
                  value =
                    "ex:  Effectif.."
                )
              ), column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_act_soc_2_2",
                  label = "Axe 2 graph 2",
                  value =
                    "ex: Effectif.."
                )
              ))
            ),
            
            mainPanel(
              h4(
                "Voici les visualisations relatives aux sous-thèmes abordés dans la thématique étudiée et issues du décret."
              ),
              
              fluidRow(
                style = "border : 1px solid silver;",
                column(
                  width = 5,
                  style = "border : px solid silver;",
                  p(""),
                  div(
                    class = "box",
                    style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;",
                    p("Ventilation & croisement de table")
                  ),
                  br(),
                  DT::DTOutput("vent_crois_act_soc"),
                  p("Cases vides = données manquantes", style = "color: orange")
                  
                  
                ),
                
                column(
                  width = 7,
                  style = "border : 1px solid silver;",
                  p(""),
                  div(class = "box", style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;", p("Graphique associé")),
                  plotlyOutput("plot_vent_crois_act_soc"),
                  fluidRow(column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "val_type_plot_act_soc_1",
                      label = "Types de valeurs",
                      choices =
                        c('Valeur', "Pourcentage", "Valeur & Pourcentage"),
                      multiple = F
                    )
                  ), column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "colors_plot_act_soc_1",
                      label = "Choix des couleurs",
                      choices =
                        ma_palette_de_couleur,
                      multiple = T
                    )
                  ))
                  
                )
              ),
              br(),
              
              fluidRow(
                style = "border : 1px solid silver;",
                column(
                  width = 5,
                  style = "border : px solid silver",
                  p(""),
                  div(
                    class = "box",
                    style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;",
                    p("Manipulation manuelle")
                  ),
                  br(),
                  DT::DTOutput("manuelle_vent_act_soc")
                ),
                column(
                  width = 7,
                  style = "border : 1px solid silver;",
                  p(""),
                  div(class = "box", style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;", p("Graphique associé")),
                  plotlyOutput("plot_act_soc_manuelle"),
                  textInput(
                    width = "100%",
                    inputId = "legend_plot_act_soc",
                    label = "Nouvelles légendes",
                    value =
                      NULL
                  ),
                  
                  fluidRow(column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "val_type_plot_act_soc_2",
                      label = "Types de valeurs",
                      choices =
                        c('Valeur', "Pourcentage", "Valeur & Pourcentage"),
                      multiple = F
                    )
                  ), column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "colors_plot_act_soc_2",
                      label = "Choix des couleurs",
                      choices =
                        ma_palette_de_couleur,
                      multiple = T
                    )
                  ))
                )
              )
              
            )
          )
          
          
          
        )),
        
        ###------------------------------------------> Dialogue social
        
        tabPanel("Dialogue social", br(), fluidRow(
          column(
            width = 3,
            style = "border : px solid silver; background-color: #191970;color: white",
            h3("Partition de table:"),
            selectInput(
              width = "100%",
              inputId = "tranche_select_dial_soc",
              label = "Choix de la tranche",
              choices = c("Tranche 1", "Tranche 2", "Tranche 3")
            ),
            
            selectInput(
              width = "100%",
              inputId = "select_start_dial_soc",
              label = "Colonne de début:",
              choices = NULL,
              multiple = F
            ),
            
            selectInput(
              width = "100%",
              inputId = "select_end_dial_soc",
              label = "Colonne de fin:",
              choices = NULL,
              multiple = F
            ),
            
            actionButton("valider_dial_soc", "Valider", style = "background-color: green;"),
            downloadButton("btn_telecharger_dial_soc", "Télécharger la table"),
            
            checkboxGroupInput(
              inputId = "btn_ajout_dial_soc_col",
              label = "",
              choices = "Ajouter des colonnes"
            ),
            
            textInput(
              width = "100%",
              inputId = "dial_soc_col",
              label = "Nom de la colonne",
              value = "ex: nom_col ou nouveau_col1;nouveau_col2;nouveau_col3;.."
            ),
            
            textInput(
              width = "100%",
              inputId = "ajout_dial_soc_col_val",
              label = "Valeur",
              value = "ex: 2 ou 3;4;5;3;2;4;....."
            ),
            
            checkboxGroupInput(
              inputId = "btn_del_dial_soc_col",
              label = "",
              choices = "Supprimer des colonnes"
            ),
            
            selectInput(
              width = "100%",
              inputId = "dial_soc_del_col",
              label = "Choix des colonnes",
              choices = NULL,
              multiple = T
            ),
            
            actionButton("valider_dial_soc_modif", "Valider modification", style = "background-color: green;")
          ),
          
          column(
            width = 9,
            style = "border : 1px solid silver;",
            mainPanel(
              width = 12,
              style = "border : 1px solid silver;",
              reactableOutput("table_dial_soc")
            ),
            div(class = "box", style = "width: 100%; height:150px"),
            sidebarPanel(
              style = "background-color: #191970;color: white",
              h3("Présentation des données:"),
              br(),
              
              selectInput(
                inputId = "sous_theme_dial_soc",
                label = "Choix du sous-thème",
                choices =
                  NULL
              ),
              
              checkboxGroupInput(
                inputId = "vent_crois_btn_dial_soc",
                label = "",
                choices = "Tableau ventilé ou croisé par défaut"
              ),
              
              selectInput(
                inputId = "vent_crois_dial_soc",
                label = "",
                choices =
                  NULL
              ),
              
              checkboxGroupInput(
                inputId = "manuelle_btn_dial_soc",
                label = "",
                choices = "Tableau composé manuellement"
              ),
              
              
              selectInput(
                inputId = "select_manuelle_vent_dial_soc",
                label = "",
                choices =
                  NULL,
                multiple = T
              ),
              
              h3("Visualisations graphiques"),
              
              selectInput(
                inputId = "plot_dial_soc",
                label = "",
                choices = c(
                  "Camembert",
                  "Histogramme",
                  "Histogramme superposé (tableau croisé uniquement)"
                )
              ),
              fluidRow(column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_title_dial_soc_1",
                  label = "Titre du graphique n°1",
                  value =
                    "ex:  Graphique 1....."
                )
              ), column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_title_dial_soc_2",
                  label = "Titre du graphique n°2",
                  value =
                    "ex: Graphique 2..."
                )
              )),
              
              fluidRow(column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_dial_soc_1_1",
                  label = "Axe 1 graph 1",
                  value =
                    "ex: Intitulés..."
                )
              ), column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_dial_soc_2_1",
                  label = "Axe 1 graph 2",
                  value =
                    "ex: Intitulés..."
                )
              )),
              
              
              fluidRow(column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_dial_soc_1_2",
                  label = "Axe 2 graph 1",
                  value =
                    "ex:  Effectif.."
                )
              ), column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_dial_soc_2_2",
                  label = "Axe 2 graph 2",
                  value =
                    "ex: Effectif.."
                )
              ))
            ),
            
            mainPanel(
              h4(
                "Voici les visualisations relatives aux sous-thèmes abordés dans la thématique étudiée et issues du décret."
              ),
              
              fluidRow(
                style = "border : 1px solid silver;",
                column(
                  width = 5,
                  style = "border : px solid silver",
                  p(""),
                  div(
                    class = "box",
                    style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;",
                    p("Ventilation & croisement de table")
                  ),
                  br(),
                  DT::DTOutput("vent_crois_dial_soc"),
                  
                  p("Cases vides = données manquantes", style = "color: orange"),
                  p("AC*= Année Concernée", style = "color: green"),
                  p("AT*= Accident du Travail", style = "color: green"),
                  p("MP*= Maladie Professionnelle", style = "color: green"),
                  p("CSS*= Condition de Santé et de Sécurité", style = "color: green"),
                  p("CT*= Condition de Travail", style = "color: green"),
                  p("INT*= Introduction de Nouvelles Technologies", style = "color: green")
                  
                  
                  
                  
                ),
                
                column(
                  width = 7,
                  style = "border : 1px solid silver;",
                  p(""),
                  div(class = "box", style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;", p("Graphique associé")),
                  plotlyOutput("plot_vent_crois_dial_soc"),
                  fluidRow(column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "val_type_plot_dial_soc_1",
                      label = "Types de valeurs",
                      choices =
                        c('Valeur', "Pourcentage", "Valeur & Pourcentage"),
                      multiple = F
                    )
                  ), column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "colors_plot_dial_soc_1",
                      label = "Choix des couleurs",
                      choices =
                        ma_palette_de_couleur,
                      multiple = T
                    )
                  ))
                  
                )
              ),
              br(),
              
              fluidRow(
                style = "border : 1px solid silver;",
                column(
                  width = 5,
                  style = "border : px solid silver",
                  p(""),
                  div(
                    class = "box",
                    style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;",
                    p("Manipulation manuelle")
                  ),
                  br(),
                  DT::DTOutput("manuelle_vent_dial_soc")
                ),
                column(
                  width = 7,
                  style = "border : 1px solid silver;",
                  p(""),
                  div(class = "box", style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;", p("Graphique associé")),
                  plotlyOutput("plot_dial_soc_manuelle"),
                  textInput(
                    width = "100%",
                    inputId = "legend_plot_dial_soc",
                    label = "Nouvelles légendes",
                    value =
                      NULL
                  ),
                  
                  fluidRow(column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "val_type_plot_dial_soc_2",
                      label = "Types de valeurs",
                      choices =
                        c('Valeur', "Pourcentage", "Valeur & Pourcentage"),
                      multiple = F
                    )
                  ), column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "colors_plot_dial_soc_2",
                      label = "Choix des couleurs",
                      choices =
                        ma_palette_de_couleur,
                      multiple = T
                    )
                  ))
                )
              )
            )
          )
          
          
          
          
          
        )),
        
        tabPanel("Emploi", br(), fluidRow(
          column(
            width = 3,
            style = "border : px solid silver; background-color: #191970;color: white",
            h3("Importation des données"),
            fileInput(
              width = "100%",
              "effectif_file",
              label = "Choisir un fichier",
              accept = c(".xlsx", ".xls", ".csv", ".ods"),
              capture = "environment",
              buttonLabel = "Parcourir",
              multiple = T,
              placeholder = "Aucun fichier sélectionné"
            ),
            actionButton("valider_import_emploi", "Valider", style = "background-color: green"),
            h3("Présentation des données:"),
            br(),
            
            selectInput(
              width = "100%",
              inputId = "sous_theme_emploi",
              label = "Choix du sous-thème",
              choices = c("A- Photographie de la DDI", "B- Organisation du temps de travail")
            ),
            checkboxGroupInput(
              inputId = "vent_crois_btn_emploi",
              label = "",
              choices = "Tableau ventilé ou croisé par défaut"
            ),
            
            selectInput(
              width = "100%",
              inputId = "vent_crois_emploi",
              label = "",
              choices = NULL
            ),
            checkboxGroupInput(
              inputId = "manuelle_btn_emploi",
              label = "",
              choices = "Agrégation manuelle"
            ),
            fluidRow(
              column(
                width = 4,
                selectInput(
                  width = "100%",
                  inputId = "aggregate_key_emploi",
                  label = "Clé ",
                  choices =
                    NULL,
                  multiple = T
                )
              ),
              column(
                width = 4,
                selectInput(
                  width = "100%",
                  inputId = "aggregate_funct_emploi",
                  label = "Fonction",
                  choices =
                    c("Comptage", "Somme", "Moyenne")
                )
              ),
              column(
                width = 4,
                selectInput(
                  width = "100%",
                  inputId = "aggregate_critere_emploi",
                  label = "Critère",
                  choices =
                    NULL
                )
              )
            ),
            h3("Visualisations graphiques"),
            
            selectInput(
              width = "100%",
              inputId = "plot_emploi",
              label = "",
              choices = c(
                "Camembert",
                "Histogramme",
                "Histogramme superposé (tableau croisé uniquement)",
                "Pyramide"
              )
            ),
            fluidRow(column(
              width = 6,
              textInput(
                width = "100%",
                inputId = "plot_title_emploi_1",
                label = "Titre du graphique n°1",
                value = "ex:  Graphique 1....."
              )
            ), column(
              width = 6,
              textInput(
                width = "100%",
                inputId = "plot_title_emploi_2",
                label = "Titre du graphique n°2",
                value =
                  "ex: Graphique 2..."
              )
            )),
            
            fluidRow(column(
              width = 6,
              textInput(
                width = "100%",
                inputId = "plot_axe_emploi_1_1",
                label = "Axe 1 graph 1",
                value =
                  "ex: Intitulés..."
              )
            ), column(
              width = 6,
              textInput(
                width = "100%",
                inputId = "plot_axe_emploi_2_1",
                label = "Axe 1 graph 2",
                value =
                  "ex: Intitulés..."
              )
            )),
            
            
            fluidRow(column(
              width = 6,
              textInput(
                width = "100%",
                inputId = "plot_axe_emploi_1_2",
                label = "Axe 2 graph 1",
                value =
                  "ex:  Effectif.."
              )
            ), column(
              width = 6,
              textInput(
                width = "100%",
                inputId = "plot_axe_emploi_2_2",
                label = "Axe 2 graph 2",
                value =
                  "ex: Effectif.."
              )
            ))
          ),
          
          
          column(
            width = 9,
            style = "border : 1px solid silver;",
            mainPanel(
              width = 12,
              style = "border : 1px solid silver;",
              reactableOutput("current_table_emploi")
            ),
            div(class = "box", style = "width: 100%; height:150px"),
            
            mainPanel(
              width = 12,
              h4(
                "Voici les visualisations relatives aux sous-thèmes abordés dans la thématique étudiée et issues du décret."
              ),
              
              fluidRow(
                style = "border : 1px solid silver;",
                column(
                  width = 5,
                  style = "border : px solid silver;",
                  p(""),
                  div(
                    class = "box",
                    style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;",
                    p("Ventilation & croisement de table")
                  ),
                  br(),
                  DT::DTOutput("vent_crois_emploi"),
                  p("Cases vides = données manquantes", style = "color: orange")
                  
                  
                ),
                
                column(
                  width = 7,
                  style = "border : 1px solid silver;",
                  p(""),
                  div(class = "box", style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;", p("Graphique associé")),
                  plotlyOutput("plot_vent_crois_emploi"),
                  fluidRow(column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "val_type_plot_emploi_1",
                      label = "Types de valeurs",
                      choices =
                        c('Valeur', "Pourcentage", "Valeur & Pourcentage"),
                      multiple = F
                    )
                  ), column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "colors_plot_emploi_1",
                      label = "Choix des couleurs",
                      choices =
                        ma_palette_de_couleur,
                      multiple = T
                    )
                  ))
                  
                )
              ),
              br(),
              
              fluidRow(
                style = "border : 1px solid silver;",
                column(
                  width = 5,
                  style = "border : px solid silver",
                  p(""),
                  div(
                    class = "box",
                    style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;",
                    p("Manipulation manuelle")
                  ),
                  br(),
                  DT::DTOutput("manuelle_vent_emploi")
                ),
                column(
                  width = 7,
                  style = "border : 1px solid silver;",
                  p(""),
                  div(class = "box", style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;", p("Graphique associé")),
                  plotlyOutput("plot_emploi_manuelle"),
                  
                  fluidRow(column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "val_type_plot_emploi_2",
                      label = "Types de valeurs",
                      choices =
                        c('Valeur', "Pourcentage", "Valeur & Pourcentage"),
                      multiple = F
                    )
                  ), column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "colors_plot_emploi_2",
                      label = "Choix des couleurs",
                      choices =
                        ma_palette_de_couleur,
                      multiple = T
                    )
                  ))
                )
              )
              
            )
          )
          
          
        )),
        
        ###-----------------------------------------------------Formation------------------------------------
        
        tabPanel("Formation", br(), fluidRow(
          column(
            width = 3,
            style = "border : px solid silver; background-color: #191970;color: white",
            h3("Partition de table:"),
            
            selectInput(
              width = "100%",
              inputId = "select_start_formation",
              label = "Colonne de début:",
              choices = NULL,
              multiple = F
            ),
            
            selectInput(
              width = "100%",
              inputId = "select_end_formation",
              label = "Colonne de fin:",
              choices = NULL,
              multiple = F
            ),
            actionButton("valider_formation", "Valider", style = "background-color: green;"),
            downloadButton("btn_telecharger_formation", "Télécharger la table"),
            
            checkboxGroupInput(
              inputId = "btn_ajout_formation_col",
              label = "",
              choices = "Ajouter des colonnes"
            ),
            
            textInput(
              width = "100%",
              inputId = "formation_col",
              label = "Nom de la colonne",
              value = "ex: nom_col ou nouveau_col1;nouveau_col2;nouveau_col3;.."
            ),
            
            textInput(
              width = "100%",
              inputId = "ajout_formation_col_val",
              label = "Valeur",
              value = "ex: 2 ou 3;4;5;3;2;4;....."
            ),
            
            checkboxGroupInput(
              inputId = "btn_del_formation_col",
              label = "",
              choices = "Supprimer des colonnes"
            ),
            selectInput(
              width = "100%",
              inputId = "formation_del_col",
              label = "Choix des colonnes",
              choices = NULL,
              multiple = T
            ),
            
            actionButton("valider_formation_modif", "Valider modification", style = "background-color: green;")
          ),
          column(
            width = 9,
            style = "border : 1px solid silver;",
            mainPanel(
              width = 12,
              style = "border : 1px solid silver;",
              reactableOutput("table_formation")
            ),
            div(class = "box", style = "width: 100%; height:150px"),
            
            sidebarPanel(
              style = "background-color: #191970;color: white",
              h3("Présentation des données:"),
              br(),
              
              
              selectInput(
                inputId = "sous_theme_formation",
                label = "Choix du sous-thème",
                choices =
                  c(
                    "A- Agents formés et Nbre de jours de formation",
                    "B- Congés de formation et encadrement intermédiaire",
                    "Informations supplémentaires"
                  )
              ),
              
              checkboxGroupInput(
                inputId = "vent_crois_btn_formation",
                label = "",
                choices = "Tableau ventilé ou croisé par défaut"
              ),
              
              selectInput(
                inputId = "vent_crois_formation",
                label = "",
                choices =
                  NULL
              ),
              
              checkboxGroupInput(
                inputId = "manuelle_btn_formation",
                label = "",
                choices = "Tableau composé manuellement"
              ),
              
              
              selectInput(
                inputId = "select_manuelle_vent_formation",
                label = "",
                choices =
                  NULL,
                multiple = T
              ),
              
              h3("Visualisations graphiques"),
              
              selectInput(
                inputId = "plot_formation",
                label = "",
                choices = c(
                  "Camembert",
                  "Histogramme",
                  "Histogramme superposé (tableau croisé uniquement)"
                )
              ),
              fluidRow(column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_title_formation_1",
                  label = "Titre du graphique n°1",
                  value =
                    "ex:  Graphique 1....."
                )
              ), column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_title_formation_2",
                  label = "Titre du graphique n°2",
                  value =
                    "ex: Graphique 2..."
                )
              )),
              
              fluidRow(column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_formation_1_1",
                  label = "Axe 1 graph 1",
                  value =
                    "ex: Intitulés..."
                )
              ), column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_formation_2_1",
                  label = "Axe 1 graph 2",
                  value =
                    "ex: Intitulés..."
                )
              )),
              
              
              fluidRow(column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_formation_1_2",
                  label = "Axe 2 graph 1",
                  value =
                    "ex:  Effectif.."
                )
              ), column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_formation_2_2",
                  label = "Axe 2 graph 2",
                  value =
                    "ex: Effectif.."
                )
              ))
            ),
            
            mainPanel(
              h4(
                "Voici les visualisations relatives aux sous-thèmes abordés dans la thématique étudiée et issues du décret."
              ),
              
              fluidRow(
                style = "border : 1px solid silver;",
                column(
                  width = 5,
                  style = "border : px solid silver;",
                  p(""),
                  div(
                    class = "box",
                    style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;",
                    p("Ventilation & croisement de table")
                  ),
                  br(),
                  DT::DTOutput("vent_crois_formation"),
                  p("Cases vides = données manquantes", style = "color: orange")
                  
                  
                ),
                
                column(
                  width = 7,
                  style = "border : 1px solid silver;",
                  p(""),
                  div(class = "box", style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;", p("Graphique associé")),
                  plotlyOutput("plot_vent_crois_formation"),
                  fluidRow(column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "val_type_plot_formation_1",
                      label = "Types de valeurs",
                      choices =
                        c('Valeur', "Pourcentage", "Valeur & Pourcentage"),
                      multiple = F
                    )
                  ), column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "colors_plot_formation_1",
                      label = "Choix des couleurs",
                      choices =
                        ma_palette_de_couleur,
                      multiple = T
                    )
                  ))
                  
                )
              ),
              br(),
              
              fluidRow(
                style = "border : 1px solid silver;",
                column(
                  width = 5,
                  style = "border : px solid silver",
                  p(""),
                  div(
                    class = "box",
                    style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;",
                    p("Manipulation manuelle")
                  ),
                  br(),
                  DT::DTOutput("manuelle_vent_formation")
                ),
                column(
                  width = 7,
                  style = "border : 1px solid silver;",
                  p(""),
                  div(class = "box", style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;", p("Graphique associé")),
                  plotlyOutput("plot_formation_manuelle"),
                  textInput(
                    width = "100%",
                    inputId = "legend_plot_formation",
                    label = "Nouvelles légendes",
                    value =
                      NULL
                  ),
                  
                  fluidRow(column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "val_type_plot_formation_2",
                      label = "Types de valeurs",
                      choices =
                        c('Valeur', "Pourcentage", "Valeur & Pourcentage"),
                      multiple = F
                    )
                  ), column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "colors_plot_formation_2",
                      label = "Choix des couleurs",
                      choices =
                        ma_palette_de_couleur,
                      multiple = T
                    )
                  ))
                )
              )
              
            )
          )
        )),
        ###------------------------------------------Mouvement du personnel----------------------------------------------------
        tabPanel("Mouvement du personnel", br(), fluidRow(
          column(
            width = 3,
            style = "border : px solid silver; background-color: #191970;color: white",
            h3("Partition de table:"),
            
            selectInput(
              width = "100%",
              inputId = "select_start_mouv_perso",
              label = "Colonne de début:",
              choices = NULL,
              multiple = F
            ),
            
            selectInput(
              width = "100%",
              inputId = "select_end_mouv_perso",
              label = "Colonne de fin:",
              choices = NULL,
              multiple = F
            ),
            
            actionButton("valider_mouv_perso", "Valider la tranche", style = "background-color: green;"),
            downloadButton("btn_telecharger_mouv_perso", "Télécharger la table"),
            
            checkboxGroupInput(
              inputId = "btn_ajout_mouv_perso_col",
              label = "",
              choices = "Ajouter des colonnes"
            ),
            
            textInput(
              width = "100%",
              inputId = "mouv_perso_col",
              label = "Nom de la colonne",
              value = "ex: nom_col ou nouveau_col1;nouveau_col2;nouveau_col3;.."
            ),
            
            textInput(
              width = "100%",
              inputId = "ajout_mouv_perso_col_val",
              label = "Valeur",
              value = "ex: 2 ou 3;4;5;3;2;4;....."
            ),
            
            checkboxGroupInput(
              inputId = "btn_del_mouv_perso_col",
              label = "",
              choices = "Supprimer des colonnes"
            ),
            selectInput(
              width = "100%",
              inputId = "mouv_perso_del_col",
              label = "Choix des colonnes",
              choices = NULL,
              multiple = T
            ),
            
            actionButton("valider_mouv_perso_modif", "Valider modification", style = "background-color: green;")
          ),
          
          column(
            width = 9,
            style = "border : 1px solid silver;",
            mainPanel(
              width = 12,
              style = "border : 1px solid silver;",
              reactableOutput("table_mouv_perso")
            ),
            div(class = "box", style = "width: 100%; height:150px"),
            
            sidebarPanel(
              style = "background-color: #191970;color: white",
              h3("Présentation des données:"),
              br(),
              
              
              selectInput(
                inputId = "sous_theme_mouv_perso",
                label = "Choix du sous-thème",
                choices =
                  c("A- Arrivées", "B- Départs", "Informations supplémentaires")
              ),
              
              checkboxGroupInput(
                inputId = "vent_crois_btn_mouv_perso",
                label = "",
                choices = "Tableau ventilé ou croisé par défaut"
              ),
              
              selectInput(
                inputId = "vent_crois_mouv_perso",
                label = "",
                choices =
                  NULL
              ),
              
              checkboxGroupInput(
                inputId = "manuelle_btn_mouv_perso",
                label = "",
                choices = "Tableau composé manuellement"
              ),
              
              
              selectInput(
                inputId = "select_manuelle_vent_mouv_perso",
                label = "",
                choices =
                  NULL,
                multiple = T
              ),
              
              h3("Visualisations graphiques"),
              
              selectInput(
                inputId = "plot_mouv_perso",
                label = "",
                choices =
                  c(
                    "Camembert",
                    "Histogramme",
                    "Histogramme superposé (tableau croisé uniquement)"
                  )
              ),
              fluidRow(column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_title_mouv_perso_1",
                  label = "Titre du graphique n°1",
                  value =
                    "ex:  Graphique 1....."
                )
              ), column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_title_mouv_perso_2",
                  label = "Titre du graphique n°2",
                  value =
                    "ex: Graphique 2..."
                )
              )),
              
              fluidRow(column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_mouv_perso_1_1",
                  label = "Axe 1 graph 1",
                  value =
                    "ex: Intitulés..."
                )
              ), column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_mouv_perso_2_1",
                  label = "Axe 1 graph 2",
                  value =
                    "ex: Intitulés..."
                )
              )),
              
              
              fluidRow(column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_mouv_perso_1_2",
                  label = "Axe 2 graph 1",
                  value =
                    "ex:  Effectif.."
                )
              ), column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_mouv_perso_2_2",
                  label = "Axe 2 graph 2",
                  value =
                    "ex: Effectif.."
                )
              ))
            ),
            
            mainPanel(
              h4(
                "Voici les visualisations relatives aux sous-thèmes abordés dans la thématique étudiée et issues du décret."
              ),
              
              fluidRow(
                style = "border : 1px solid silver;",
                column(
                  width = 5,
                  style = "border : px solid silver;",
                  p(""),
                  div(
                    class = "box",
                    style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;",
                    p("Ventilation & croisement de table")
                  ),
                  br(),
                  DT::DTOutput("vent_crois_mouv_perso"),
                  p("Cases vides = données manquantes", style = "color: orange"),
                  
                  p(width = "2%", "bp* = besoins permanents", style = "color: green"),
                  p("bnp* = besoins non permanents", style = "color: green")
                  
                ),
                
                column(
                  width = 7,
                  style = "border : 1px solid silver;",
                  p(""),
                  div(class = "box", style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;", p("Graphique associé")),
                  plotlyOutput("plots_mouv_perso"),
                  fluidRow(column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "val_type_plot_mouv_perso_1",
                      label = "Types de valeurs",
                      choices =
                        c('Valeur', "Pourcentage", "Valeur & Pourcentage"),
                      multiple = F
                    )
                  ), column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "colors_plot_mouv_perso_1",
                      label = "Choix des couleurs",
                      choices =
                        ma_palette_de_couleur,
                      multiple = T
                    )
                  ))
                  
                )
              ),
              br(),
              
              fluidRow(
                style = "border : 1px solid silver;",
                column(
                  width = 5,
                  style = "border : px solid silver",
                  p(""),
                  div(
                    class = "box",
                    style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;",
                    p("Manipulation manuelle")
                  ),
                  br(),
                  DT::DTOutput("manuelle_vent_mouv_perso")
                ),
                column(
                  width = 7,
                  style = "border : 1px solid silver;",
                  p(""),
                  div(class = "box", style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;", p("Graphique associé")),
                  plotlyOutput("plot_mouv_perso_manuelle"),
                  textInput(
                    width = "100%",
                    inputId = "legend_plot_mouv_perso",
                    label = "Nouvelles légendes",
                    value =
                      NULL
                  ),
                  
                  fluidRow(column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "val_type_plot_mouv_perso_2",
                      label = "Types de valeurs",
                      choices =
                        c('Valeur', "Pourcentage", "Valeur & Pourcentage"),
                      multiple = F
                    )
                  ), column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "colors_plot_mouv_perso_2",
                      label = "Choix des couleurs",
                      choices =
                        ma_palette_de_couleur,
                      multiple = T
                    )
                  ))
                )
              )
              
            )
          )
          
        )),
        ###-----------------------------------------------Fin mouvement du personnel-----------------------------------------------------------
        
        ###----------------------------------------->Organisation du travail
        tabPanel("Organisation du travail", br(), fluidRow(
          column(
            width = 3,
            style = "border : px solid silver; background-color: #191970;color: white",
            h3("Partition de table:"),
            selectInput(
              width = "100%",
              inputId = "tranche_select_orga_trav",
              label = "Choix de la tranche",
              choices = c("Tranche 1", "Tranche 2", "Tranche 3")
            ),
            
            selectInput(
              width = "100%",
              inputId = "select_start_orga_trav",
              label = "Colonne de début:",
              choices = NULL,
              multiple = F
            ),
            
            selectInput(
              width = "100%",
              inputId = "select_end_orga_trav",
              label = "Colonne de fin:",
              choices = NULL,
              multiple = F
            ),
            
            actionButton("valider_orga_trav", "Valider", style = "background-color: green;"),
            downloadButton("btn_telecharger_orga_trav", "Télécharger la table"),
            
            checkboxGroupInput(
              inputId = "btn_ajout_orga_trav",
              label = "",
              choices = "Ajouter des colonnes"
            ),
            
            textInput(
              width = "100%",
              inputId = "orga_trav_col",
              label = "Nom de la colonne",
              value = "ex: nom_col ou nouveau_col1;nouveau_col2;nouveau_col3;.."
            ),
            
            textInput(
              width = "100%",
              inputId = "ajout_orga_trav_col_val",
              label = "Valeur",
              value = "ex: 2 ou 3;4;5;3;2;4;....."
            ),
            
            checkboxGroupInput(
              inputId = "btn_del_orga_trav",
              label = "",
              choices = "Supprimer des colonnes"
            ),
            
            selectInput(
              width = "100%",
              inputId = "orga_trav_del_col",
              label = "Choix des colonnes",
              choices = NULL,
              multiple = T
            ),
            
            actionButton("valider_orga_trav_modif", "Valider modification", style = "background-color: green;")
          ),
          column(
            width = 9,
            style = "border : 1px solid silver;",
            mainPanel(
              width = 12,
              style = "border : 1px solid silver;",
              reactableOutput("table_orga_trav")
            ),
            div(class = "box", style = "width: 100%; height:150px"),
            sidebarPanel(
              style = "background-color: #191970;color: white",
              h3("Présentation des données:"),
              br(),
              
              selectInput(
                inputId = "sous_theme_orga_trav",
                label = "Choix du sous-thème",
                choices =
                  NULL
              ),
              
              checkboxGroupInput(
                inputId = "vent_crois_btn_orga_trav",
                label = "",
                choices = "Tableau ventilé ou croisé par défaut"
              ),
              
              selectInput(
                inputId = "vent_crois_orga_trav",
                label = "",
                choices =
                  NULL
              ),
              
              checkboxGroupInput(
                inputId = "manuelle_btn_orga_trav",
                label = "",
                choices = "Tableau composé manuellement"
              ),
              
              
              selectInput(
                inputId = "select_manuelle_vent_orga_trav",
                label = "",
                choices =
                  NULL,
                multiple = T
              ),
              
              h3("Visualisations graphiques"),
              
              selectInput(
                inputId = "plot_orga_trav",
                label = "",
                choices = c(
                  "Camembert",
                  "Histogramme",
                  "Histogramme superposé (tableau croisé uniquement)"
                )
              ),
              fluidRow(column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_title_orga_trav_1",
                  label = "Titre du graphique n°1",
                  value =
                    "ex:  Graphique 1....."
                )
              ), column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_title_orga_trav_2",
                  label = "Titre du graphique n°2",
                  value =
                    "ex: Graphique 2..."
                )
              )),
              
              fluidRow(column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_orga_trav_1_1",
                  label = "Axe 1 graph 1",
                  value =
                    "ex: Intitulés..."
                )
              ), column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_orga_trav_2_1",
                  label = "Axe 1 graph 2",
                  value =
                    "ex: Intitulés..."
                )
              )),
              
              
              fluidRow(column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_orga_trav_1_2",
                  label = "Axe 2 graph 1",
                  value =
                    "ex:  Effectif.."
                )
              ), column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_orga_trav_2_2",
                  label = "Axe 2 graph 2",
                  value =
                    "ex: Effectif.."
                )
              ))
            ),
            mainPanel(
              h4(
                "Voici les visualisations relatives aux sous-thèmes abordés dans la thématique étudiée et issues du décret."
              ),
              
              fluidRow(
                style = "border : 1px solid silver;",
                column(
                  width = 5,
                  style = "border : px solid silver",
                  p(""),
                  div(
                    class = "box",
                    style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;",
                    p("Ventilation & croisement de table")
                  ),
                  br(),
                  DT::DTOutput("vent_crois_orga_trav"),
                  
                  p("Cases vides = données manquantes", style = "color: orange"),
                  p("Agent* = Agent en fonction au 31 Décembre", style = "color: green"),
                  p("TT* = Télétravail", style = "color: green"),
                  p(
                    "MAPAE*= Maternité,Adoption, Paternité, Acceuil de l'Enfant",
                    style = "color: green"
                  ),
                  p("OJEP= Organisation de Jeunesse et d'Education populaire", style = "color: green"),
                  p("AC*= Année Concernée", style = "color: green"),
                  p("ACAEC*= Au Cours de l'Année En Cours", style = "color: green"),
                  p("APS*= Accompagnement des Personnes en Situation", style = "color: green")
                ),
                
                column(
                  width = 7,
                  style = "border : 1px solid silver;",
                  p(""),
                  div(class = "box", style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;", p("Graphique associé")),
                  plotlyOutput("plot_vent_crois_orga_trav"),
                  fluidRow(column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "val_type_plot_orga_trav_1",
                      label = "Types de valeurs",
                      choices =
                        c('Valeur', "Pourcentage", "Valeur & Pourcentage"),
                      multiple = F
                    )
                  ), column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "colors_plot_orga_trav_1",
                      label = "Choix des couleurs",
                      choices =
                        ma_palette_de_couleur,
                      multiple = T
                    )
                  ))
                  
                )
              ),
              br(),
              
              fluidRow(
                style = "border : 1px solid silver;",
                column(
                  width = 5,
                  style = "border : px solid silver",
                  p(""),
                  div(
                    class = "box",
                    style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;",
                    p("Manipulation manuelle")
                  ),
                  br(),
                  DT::DTOutput("manuelle_vent_orga_trav")
                ),
                column(
                  width = 7,
                  style = "border : 1px solid silver;",
                  p(""),
                  div(class = "box", style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;", p("Graphique associé")),
                  plotlyOutput("plot_orga_trav_manuelle"),
                  textInput(
                    width = "100%",
                    inputId = "legend_plot_orga_trav",
                    label = "Nouvelles légendes",
                    value =
                      NULL
                  ),
                  
                  fluidRow(column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "val_type_plot_orga_trav_2",
                      label = "Types de valeurs",
                      choices =
                        c('Valeur', "Pourcentage", "Valeur & Pourcentage"),
                      multiple = F
                    )
                  ), column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "colors_plot_orga_trav_2",
                      label = "Choix des couleurs",
                      choices =
                        ma_palette_de_couleur,
                      multiple = T
                    )
                  ))
                )
              )
            )
          )
        )),
        
        
        ####--------------------------------------------------------------> Parcours pro <-------------------------------------------------------------------
        
        
        tabPanel("Parcours professionnels", br(), fluidRow(
          column(
            width = 3,
            style = "border : px solid silver; background-color: #191970;color: white",
            h3("Partition de table:"),
            
            selectInput(
              width = "100%",
              inputId = "select_start_parc_pro",
              label = "Colonne de début:",
              choices = NULL,
              multiple = F
            ),
            
            selectInput(
              width = "100%",
              inputId = "select_end_parc_pro",
              label = "Colonne de fin:",
              choices = NULL,
              multiple = F
            ),
            
            actionButton("valider_parc_pro", "Valider la tranche", style = "background-color: green;"),
            downloadButton("btn_telecharger_parc_pro", "Télécharger la table"),
            
            
            checkboxGroupInput(
              inputId = "btn_ajout_parc_pro_col",
              label = "",
              choices = "Ajouter des colonnes"
            ),
            
            textInput(
              width = "100%",
              inputId = "parc_pro_col",
              label = "Nom de la colonne",
              value = "ex: nom_col ou nouveau_col1;nouveau_col2;nouveau_col3;.."
            ),
            
            textInput(
              width = "100%",
              inputId = "ajout_parc_pro_col_val",
              label = "Valeur",
              value = "ex: 2 ou 3;4;5;3;2;4;....."
            ),
            
            checkboxGroupInput(
              inputId = "btn_del_parc_pro_col",
              label = "",
              choices = "Supprimer des colonnes"
            ),
            
            selectInput(
              width = "100%",
              inputId = "parc_pro_del_col",
              label = "Choix des colonnes",
              choices = NULL,
              multiple = T
            ),
            
            actionButton("valider_parc_pro_modif", "Valider modification", style = "background-color: green;")
            
          ),
          
          column(
            width = 9,
            style = "border : 1px solid silver;",
            mainPanel(
              width = 12,
              style = "border : 1px solid silver;",
              reactableOutput("table_parc_pro")
            ),
            div(class = "box", style = "width: 100%; height:150px"),
            
            sidebarPanel(
              style = "background-color: #191970;color: white",
              h3("Présentation des données:"),
              br(),
              
              selectInput(
                inputId = "sous_theme_parc_pro",
                label = "Choix du sous-thème",
                choices =
                  c(
                    "A- Postes pourvus",
                    "B- Avancement de grade et promotion interne",
                    "Informations supplémentaires"
                  )
              ),
              
              checkboxGroupInput(
                inputId = "vent_crois_btn_parc_pro",
                label = "",
                choices = "Tableau ventilé ou croisé par défaut"
              ),
              
              selectInput(
                inputId = "tables_select_parc_pro",
                label = "",
                choices =
                  NULL
              ),
              
              checkboxGroupInput(
                inputId = "manuelle_btn_parc_pro",
                label = "",
                choices = "Tableau composé manuellement"
              ),
              
              
              selectInput(
                inputId = "select_manuelle_vent_parc_pro",
                label = "",
                choices =
                  NULL,
                multiple = T
              ),
              
              h3("Visualisations graphiques"),
              
              selectInput(
                inputId = "plot_parc_pro",
                label = "",
                choices = c(
                  "Camembert",
                  "Histogramme",
                  "Histogramme superposé (tableau croisé uniquement)"
                )
              ),
              fluidRow(column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_title_parc_pro_1",
                  label = "Titre du graphique n°1",
                  value =
                    "ex:  Graphique 1....."
                )
              ), column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_title_parc_pro_2",
                  label = "Titre du graphique n°2",
                  value =
                    "ex: Graphique 2..."
                )
              )),
              
              fluidRow(column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_parc_pro_1_1",
                  label = "Axe 1 graph 1",
                  value =
                    "ex: Intitulés..."
                )
              ), column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_parc_pro_2_1",
                  label = "Axe 1 graph 2",
                  value =
                    "ex: Intitulés..."
                )
              )),
              
              
              fluidRow(column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_parc_pro_1_2",
                  label = "Axe 2 graph 1",
                  value =
                    "ex:  Effectif.."
                )
              ), column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_parc_pro_2_2",
                  label = "Axe 2 graph 2",
                  value =
                    "ex: Effectif.."
                )
              ))
            ),
            
            mainPanel(
              h4(
                "Voici les visualisations relatives aux sous-thèmes abordés dans la thématique étudiée et issues du décret."
              ),
              
              fluidRow(
                style = "border : 1px solid silver;",
                column(
                  width = 5,
                  style = "border : px solid silver;",
                  p(""),
                  div(
                    class = "box",
                    style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;",
                    p("Ventilation & croisement de table")
                  ),
                  br(),
                  DT::DTOutput("vent_crois_parc_pro"),
                  p("Cases vides = données manquantes", style = "color: orange")
                ),
                
                column(
                  width = 7,
                  style = "border : 1px solid silver;",
                  p(""),
                  div(class = "box", style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;", p("Graphique associé")),
                  plotlyOutput("plot_vent_crois_parc_pro"),
                  fluidRow(column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "val_type_plot_parc_pro_1",
                      label = "Types de valeurs",
                      choices =
                        c('Valeur', "Pourcentage", "Valeur & Pourcentage"),
                      multiple = F
                    )
                  ), column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "colors_plot_parc_pro_1",
                      label = "Choix des couleurs",
                      choices =
                        ma_palette_de_couleur,
                      multiple = T
                    )
                  ))
                  
                )
              ),
              br(),
              
              fluidRow(
                style = "border : 1px solid silver;",
                column(
                  width = 5,
                  style = "border : px solid silver",
                  p(""),
                  div(
                    class = "box",
                    style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;",
                    p("Manipulation manuelle")
                  ),
                  br(),
                  DT::DTOutput("manuelle_vent_parc_pro")
                ),
                column(
                  width = 7,
                  style = "border : 1px solid silver;",
                  p(""),
                  div(class = "box", style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;", p("Graphique associé")),
                  plotlyOutput("plot_parc_pro_manuelle"),
                  textInput(
                    width = "100%",
                    inputId = "legend_plot_parc_pro",
                    label = "Nouvelles légendes",
                    value =
                      NULL
                  ),
                  
                  fluidRow(column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "val_type_plot_parc_pro_2",
                      label = "Types de valeurs",
                      choices =
                        c('Valeur', "Pourcentage", "Valeur & Pourcentage"),
                      multiple = F
                    )
                  ), column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "colors_plot_parc_pro_2",
                      label = "Choix des couleurs",
                      choices =
                        ma_palette_de_couleur,
                      multiple = T
                    )
                  ))
                )
              )
              
            )
            
            
          )
          
          
          
        )),
        ###----------------------------------------------------> Fin parcours pro <-----------------------------------------------------------------------
        
        
        
        ###-------------------------------------------------------->Sanctions disciplinires
        tabPanel("Sanction disciplinaires", br(), fluidRow(
          column(
            width = 3,
            style = "border : px solid silver; background-color: #191970;color: white",
            h3("Partition de table:"),
            
            selectInput(
              width = "100%",
              inputId = "select_start_sanc_discip",
              label = "Colonne de début:",
              choices = NULL,
              multiple = F
            ),
            
            selectInput(
              width = "100%",
              inputId = "select_end_sanc_discip",
              label = "Colonne de fin:",
              choices = NULL,
              multiple = F
            ),
            actionButton("valider_sanc_discip", "Valider", style = "background-color: green;"),
            downloadButton("btn_telecharger_sanc_discip", "Télécharger la table"),
            
            checkboxGroupInput(
              inputId = "btn_ajout_sanc_discip_col",
              label = "",
              choices = "Ajouter des colonnes"
            ),
            
            textInput(
              width = "100%",
              inputId = "sanc_discip_col",
              label = "Nom de la colonne",
              value = "ex: nom_col ou nouveau_col1;nouveau_col2;nouveau_col3;.."
            ),
            
            textInput(
              width = "100%",
              inputId = "ajout_sanc_discip_col_val",
              label = "Valeur",
              value = "ex: 2 ou 3;4;5;3;2;4;....."
            ),
            
            checkboxGroupInput(
              inputId = "btn_del_sanc_discip_col",
              label = "",
              choices = "Supprimer des colonnes"
            ),
            selectInput(
              width = "100%",
              inputId = "sanc_discip_del_col",
              label = "Choix des colonnes",
              choices = NULL,
              multiple = T
            ),
            actionButton("valider_sanc_discip_modif", "Valider modification", style = "background-color: green;")
          ),
          
          column(
            width = 9,
            style = "border : 1px solid silver;",
            mainPanel(
              width = 12,
              style = "border : 1px solid silver;",
              reactableOutput("table_sanc_discip")
            ),
            div(class = "box", style = "width: 100%; height:150px"),
            
            sidebarPanel(
              style = "background-color: #191970;color: white",
              h3("Présentation des données:"),
              br(),
              
              
              selectInput(
                inputId = "sous_theme_sanc_discip",
                label = "Choix du sous-thème",
                choices =
                  c(
                    "A- Sanctions disciplinaires notifiés au cours de l'année en cours",
                    "Informations supplémentaires"
                  )
              ),
              
              checkboxGroupInput(
                inputId = "vent_crois_btn_sanc_discip",
                label = "",
                choices = "Tableau ventilé ou croisé par défaut"
              ),
              
              selectInput(
                inputId = "vent_crois_sanc_discip",
                label = "",
                choices =
                  NULL
              ),
              
              checkboxGroupInput(
                inputId = "manuelle_btn_sanc_discip",
                label = "",
                choices = "Tableau composé manuellement"
              ),
              
              
              selectInput(
                inputId = "select_manuelle_vent_sanc_discip",
                label = "",
                choices =
                  NULL,
                multiple = T
              ),
              
              h3("Visualisations graphiques"),
              
              selectInput(
                inputId = "plot_sanc_discip",
                label = "",
                choices = c(
                  "Camembert",
                  "Histogramme",
                  "Histogramme superposé (tableau croisé uniquement)"
                )
              ),
              fluidRow(column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_title_sanc_discip_1",
                  label = "Titre du graphique n°1",
                  value =
                    "ex:  Graphique 1....."
                )
              ), column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_title_sanc_discip_2",
                  label = "Titre du graphique n°2",
                  value =
                    "ex: Graphique 2..."
                )
              )),
              
              fluidRow(column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_sanc_discip_1_1",
                  label = "Axe 1 graph 1",
                  value =
                    "ex: Intitulés..."
                )
              ), column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_sanc_discip_2_1",
                  label = "Axe 1 graph 2",
                  value =
                    "ex: Intitulés..."
                )
              )),
              
              
              fluidRow(column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_sanc_discip_1_2",
                  label = "Axe 2 graph 1",
                  value =
                    "ex:  Effectif.."
                )
              ), column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_sanc_discip_2_2",
                  label = "Axe 2 graph 2",
                  value =
                    "ex: Effectif.."
                )
              ))
            ),
            
            mainPanel(
              h4(
                "Voici les visualisations relatives aux sous-thèmes abordés dans la thématique étudiée et issues du décret."
              ),
              
              fluidRow(
                style = "border : 1px solid silver;",
                column(
                  width = 5,
                  style = "border : px solid silver;",
                  p(""),
                  div(
                    class = "box",
                    style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;",
                    p("Ventilation & croisement de table")
                  ),
                  br(),
                  DT::DTOutput("vent_crois_sanc_discip"),
                  p("Cases vides = données manquantes", style = "color: orange")
                  
                  
                ),
                
                column(
                  width = 7,
                  style = "border : 1px solid silver;",
                  p(""),
                  div(class = "box", style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;", p("Graphique associé")),
                  plotlyOutput("plot_vent_crois_sanc_discip"),
                  fluidRow(column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "val_type_plot_sanc_discip_1",
                      label = "Types de valeurs",
                      choices =
                        c('Valeur', "Pourcentage", "Valeur & Pourcentage"),
                      multiple = F
                    )
                  ), column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "colors_plot_sanc_discip_1",
                      label = "Choix des couleurs",
                      choices =
                        ma_palette_de_couleur,
                      multiple = T
                    )
                  ))
                  
                )
              ),
              br(),
              
              fluidRow(
                style = "border : 1px solid silver;",
                column(
                  width = 5,
                  style = "border : px solid silver",
                  p(""),
                  div(
                    class = "box",
                    style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;",
                    p("Manipulation manuelle")
                  ),
                  br(),
                  DT::DTOutput("manuelle_vent_sanc_discip")
                ),
                column(
                  width = 7,
                  style = "border : 1px solid silver;",
                  p(""),
                  div(class = "box", style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;", p("Graphique associé")),
                  plotlyOutput("plot_sanc_discip_manuelle"),
                  textInput(
                    width = "100%",
                    inputId = "legend_plot_sanc_discip",
                    label = "Nouvelles légendes",
                    value =
                      NULL
                  ),
                  
                  fluidRow(column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "val_type_plot_sanc_discip_2",
                      label = "Types de valeurs",
                      choices =
                        c('Valeur', "Pourcentage", "Valeur & Pourcentage"),
                      multiple = F
                    )
                  ), column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "colors_plot_sanc_discip_2",
                      label = "Choix des couleurs",
                      choices =
                        ma_palette_de_couleur,
                      multiple = T
                    )
                  ))
                )
              )
              
            )
            
            
            
            
            
            
          )
        )),
        
        tabPanel("Santé et sécurité au travail", br(), fluidRow(
          column(
            width = 3,
            style = "border : px solid silver; background-color: #191970;color: white",
            h3("Partition de table:"),
            selectInput(
              width = "100%",
              inputId = "tranche_select_sante_secu",
              label = "Choix de la tranche",
              choices = c("Tranche 1", "Tranche 2", "Tranche 3")
            ),
            
            selectInput(
              width = "100%",
              inputId = "select_start_sante_secu",
              label = "Colonne de début:",
              choices = NULL,
              multiple = F
            ),
            
            selectInput(
              width = "100%",
              inputId = "select_end_sante_secu",
              label = "Colonne de fin:",
              choices = NULL,
              multiple = F
            ),
            
            actionButton("valider_sante_secu", "Valider", style = "background-color: green;"),
            downloadButton("btn_telecharger_sante_secu", "Télécharger la table"),
            
            checkboxGroupInput(
              inputId = "btn_ajout_sante_secu_col",
              label = "",
              choices = "Ajouter des colonnes"
            ),
            
            textInput(
              width = "100%",
              inputId = "sante_secu_col",
              label = "Nom de la colonne",
              value = "ex: nom_col ou nouveau_col1;nouveau_col2;nouveau_col3;.."
            ),
            
            textInput(
              width = "100%",
              inputId = "ajout_sante_secu_col_val",
              label = "Valeur",
              value = "ex: 2 ou 3;4;5;3;2;4;....."
            ),
            
            checkboxGroupInput(
              inputId = "btn_del_sante_secu_col",
              label = "",
              choices = "Supprimer des colonnes"
            ),
            selectInput(
              width = "100%",
              inputId = "sante_secu_del_col",
              label = "Choix des colonnes",
              choices = NULL,
              multiple = T
            ),
            
            actionButton("valider_sante_secu_modif", "Valider modification", style = "background-color: green;")
          ),
          column(
            width = 9,
            style = "border : 1px solid silver;",
            mainPanel(
              width = 12,
              style = "border : 1px solid silver;",
              reactableOutput("table_sante_secu")
            ),
            div(class = "box", style = "width: 100%; height:150px"),
            sidebarPanel(
              style = "background-color: #191970;color: white",
              h3("Présentation des données:"),
              br(),
              
              selectInput(
                inputId = "sous_theme_sante_secu",
                label = "Choix du sous-thème",
                choices =
                  NULL
              ),
              
              checkboxGroupInput(
                inputId = "vent_crois_btn_sante_secu",
                label = "",
                choices = "Tableau ventilé ou croisé par défaut"
              ),
              
              selectInput(
                inputId = "vent_crois_sante_secu",
                label = "",
                choices =
                  NULL
              ),
              
              checkboxGroupInput(
                inputId = "manuelle_btn_sante_secu",
                label = "",
                choices = "Tableau composé manuellement"
              ),
              
              
              selectInput(
                inputId = "select_manuelle_vent_sante_secu",
                label = "",
                choices =
                  NULL,
                multiple = T
              ),
              
              h3("Visualisations graphiques"),
              
              selectInput(
                inputId = "plot_sante_secu",
                label = "",
                choices = c(
                  "Camembert",
                  "Histogramme",
                  "Histogramme superposé (tableau croisé uniquement)"
                )
              ),
              fluidRow(column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_title_sante_secu_1",
                  label = "Titre du graphique n°1",
                  value =
                    "ex:  Graphique 1....."
                )
              ), column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_title_sante_secu_2",
                  label = "Titre du graphique n°2",
                  value =
                    "ex: Graphique 2..."
                )
              )),
              
              fluidRow(column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_sante_secu_1_1",
                  label = "Axe 1 graph 1",
                  value =
                    "ex: Intitulés..."
                )
              ), column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_sante_secu_2_1",
                  label = "Axe 1 graph 2",
                  value =
                    "ex: Intitulés..."
                )
              )),
              
              
              fluidRow(column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_sante_secu_1_2",
                  label = "Axe 2 graph 1",
                  value =
                    "ex:  Effectif.."
                )
              ), column(
                width = 6,
                textInput(
                  width = "100%",
                  inputId = "plot_axe_sante_secu_2_2",
                  label = "Axe 2 graph 2",
                  value =
                    "ex: Effectif.."
                )
              ))
            ),
            mainPanel(
              h4(
                "Voici les visualisations relatives aux sous-thèmes abordés dans la thématique étudiée et issues du décret."
              ),
              
              fluidRow(
                style = "border : 1px solid silver;",
                column(
                  width = 5,
                  style = "border : px solid silver",
                  p(""),
                  div(
                    class = "box",
                    style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;",
                    p("Ventilation & croisement de table")
                  ),
                  br(),
                  DT::DTOutput("vent_crois_sante_secu"),
                  
                  p("Cases vides = données manquantes", style = "color: orange"),
                  p("ISST=Inspections Santé et Sécurité au Travail", style = "color: green"),
                  p("SSG= Sécrétaire Général du Gouvernement", style = "color: green"),
                  p("Fonction*= Fonction au 31 décembre de l'année concernée", style = "color: green"),
                  p("MPFF*= Mesures de Protection Fonctionnelles Formulées", style = "color: green"),
                  p("MP* = Médecine de Prévention", style = "color: green"),
                  p("PPR* = Période de Préparation au Reclassement", style = "color: green"),
                  p("CM*= Conseil Médical", style = "color: green"),
                  p("HAT*= Hors Accident de Trajet", style = "color: green"),
                  p("AC*= Année Concernée", style = "color: green"),
                  p("AP*= Assistant de Prévention", style = "color: green"),
                  p("CP*= Conseiller de Prévention", style = "color: green"),
                  
                ),
                
                column(
                  width = 7,
                  style = "border : 1px solid silver;",
                  p(""),
                  div(class = "box", style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;", p("Graphique associé")),
                  plotlyOutput("plot_vent_crois_sante_secu"),
                  fluidRow(column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "val_type_plot_sante_secu_1",
                      label = "Types de valeurs",
                      choices =
                        c('Valeur', "Pourcentage", "Valeur & Pourcentage"),
                      multiple = F
                    )
                  ), column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "colors_plot_sante_secu_1",
                      label = "Choix des couleurs",
                      choices =
                        ma_palette_de_couleur,
                      multiple = T
                    )
                  ))
                  
                )
              ),
              br(),
              
              fluidRow(
                style = "border : 1px solid silver;",
                column(
                  width = 5,
                  style = "border : px solid silver",
                  p(""),
                  div(
                    class = "box",
                    style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;",
                    p("Manipulation manuelle")
                  ),
                  br(),
                  DT::DTOutput("manuelle_vent_sante_secu")
                ),
                column(
                  width = 7,
                  style = "border : 1px solid silver;",
                  p(""),
                  div(class = "box", style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;", p("Graphique associé")),
                  plotlyOutput("plot_sante_secu_manuelle"),
                  textInput(
                    width = "100%",
                    inputId = "legend_plot_sante_secu",
                    label = "Nouvelles légendes",
                    value =
                      NULL
                  ),
                  
                  fluidRow(column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "val_type_plot_sante_secu_2",
                      label = "Types de valeurs",
                      choices =
                        c('Valeur', "Pourcentage", "Valeur & Pourcentage"),
                      multiple = F
                    )
                  ), column(
                    width = 6,
                    selectInput(
                      width = "100%",
                      inputId = "colors_plot_sante_secu_2",
                      label = "Choix des couleurs",
                      choices =
                        ma_palette_de_couleur,
                      multiple = T
                    )
                  ))
                )
              )
            )
          )
          
          
          
        ))
        
      )
      
    ),
    
    tabPanel(strong(h4("Annexes")), br(), fluidRow(column(
      width = 12, tabsetPanel(
        tabPanel(
          strong(p("Définition")),
          
          
          p(
            "D'après l'étude du décret n° 2020-1493 du 30 novembre 2020 relatif à la base de données sociales et au rapport social unique dans la fonction publique, dix thèmes ont été relevés."
          ),
          
          
          br(),
          column(
            width = 12,
            fluidRow(
              column(
                width = 3,
                style = "border : 1px solid silver; background-color: #7A90A4;color: white",
                p(""),
                div(class = "box", style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;", p("Action sociale")),
                
                p(
                  "L'action sociale rend compte de la gestion financière relative aux prestations fournies telles que le logement, le nombre de bénéficiaires et leurs caractéristiques."
                ),
                br()
              ),
              
              column(width = 1, style = "border : px solid silver;"),
              
              column(
                width = 3,
                style = "border : 1px solid silver; background-color: #7A90A4;color: white",
                p(""),
                div(class = "box", style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;", p("Dialogue social")),
                
                p(
                  "Cette rubrique renseigne sur les instances du dialogue social, les représentants du personnel, les ressources syndicales, les négociations en cours, les accords conclus, les recours auprès des commissions administratives paritaires, les jours de grève et les réunions."
                )
              ),
              
              column(width = 1, style = "border : px solid silver;"),
              
              column(
                width = 3,
                style = "border : 1px solid silver; background-color: #7A90A4;color: white",
                p(""),
                div(class = "box", style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;", p("Emploi")),
                
                p(
                  "La rubrique emploi présente les effectifs physiques et équivalents temps plein, les caractéristiques des agents, leurs statuts variés, ainsi que les postes à pourvoir et occupés. Cette approche globale permet de comprendre et de gérer efficacement les RH au sein de chaque DDI."
                )
              )
            ),
            
            br(),
            
            fluidRow(
              column(
                width = 3,
                style = "border : 1px solid silver; background-color: #7A90A4;color: white",
                p(""),
                div(class = "box", style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;", p("Formation")),
                p(
                  "La formation des agents de DDI joue un rôle fondamental dans le pilotage stratégique et opérationnel des DDI, ainsi que dans le maintien de leur bon fonctionnement. Elle est d'une importance capitale, surtout dans le cadre de la modernisation de l'action publique et des réformes territoriales de l'État."
                )
              ),
              
              column(width = 1, style = "border : px solid silver;"),
              
              
              column(
                width = 3,
                style = "border : 1px solid silver; background-color: #7A90A4;color: white",
                p(""),
                div(
                  class = "box",
                  style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;",
                  p("Mouvement du personnel")
                ),
                
                p(
                  "Cette rubrique retrace les entrées et les sorties au sein de l'effectif durant l'année concernée. Ces données sont essentielles pour évaluer la gestion des RH, identifier les tendances et ajuster les stratégies de recrutement et de maintien dans un environnement de travail efficace et productif."
                ),
                br()
              ),
              
              column(width = 1, style = "border : px solid silver;"),
              
              column(
                width = 3,
                style = "border : 1px solid silver; background-color: #7A90A4;color: white",
                p(""),
                div(
                  class = "box",
                  style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;",
                  p("Organisation du temps de travail")
                ),
                
                p(
                  "Cette thématique permet de visualiser les cycles et l'organisation des missions, incluant les heures supplémentaires, le télétravail, et les congés. Elle est essentielle à la flexibilité organisationnelle, ainsi qu'au  bien-être des agents."
                ),
                br()
              )
            ),
            
            br(),
            
            fluidRow(
              column(
                width = 3,
                style = "border : 1px solid silver; background-color: #7A90A4;color: white",
                p(""),
                div(
                  class = "box",
                  style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;",
                  p("Parcours professionnels")
                ),
                
                p(
                  "Cette thématique  informe sur les mutations, les avancements de grade, les promotions et les examens professionnels. Elle est cruciale pour suivre et soutenir le développement de carrière des agents au sein de l'organisation."
                ),
                br()
              ),
              
              column(width = 1, style = "border : px solid silver;"),
              
              column(
                width = 3,
                style = "border : 1px solid silver; background-color: #7A90A4;color: white",
                p(""),
                div(
                  class = "box",
                  style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;",
                  p("Sanctions disciplinaires")
                ),
                
                p(
                  "Cette rubrique informe sur les fautes commises et les différentes les sanctions appliquées."
                ),
                br(br())
              ),
              
              column(width = 1, style = "border : px solid silver;"),
              
              column(
                width = 3,
                style = "border : 1px solid silver; background-color: #7A90A4;color: white",
                p(""),
                div(
                  class = "box",
                  style = " border: 5px solid #424340;background-color: #424340;color: white; text-align: center;",
                  p("Santé et sécurité au travail")
                ),
                
                p(
                  "Cette thématique traite des risques professionnels liés aux conditions de travail, ayant un impact sur la santé des agents (maladies, accidents, RPS), des décisions du conseil médical, des acteurs de la prévention et des différents registres."
                )
              )
            )
          )
        )
        ,
        tabPanel(
          strong(p("Organigramme")),
          tabsetPanel(
            tabPanel(
              strong(p("Action social")),
              br(),
              grVizOutput("dendo_act_soc", height = "600px", width = "100%")
            ),
            
            tabPanel(
              strong(p("Dialogue social")),
              br(),
              grVizOutput("dendo_dial_soc", height = "600px", width = "100%")
            ),
            
            tabPanel(
              strong(p("Emploi")),
              br(),
              grVizOutput("dendo_emploi", height = "600px", width = "100%")
            ),
            tabPanel(
              strong(p("Formation")),
              br(),
              grVizOutput("dendo_formation", height = "600px", width = "100%")
            ),
            
            tabPanel(
              strong(p("Mouvement du personnel")),
              br(),
              grVizOutput("dendo_mouv_perso", height = "600px", width = "100%")
            ),
            
            tabPanel(
              strong(p("Organisation du travail")),
              br(),
              grVizOutput("dendo_orga_trav", height = "600px", width = "100%")
            ),
            
            tabPanel(
              strong(p("Parcours professionnels")),
              br(),
              grVizOutput("dendo_parc_pro", height = "600px", width = "100%")
            ),
            
            tabPanel(
              strong(p("Sanction disciplinaire")),
              br(),
              grVizOutput("dendo_sanc_discip", height = "600px", width = "100%")
            ),
            
            tabPanel(
              strong(p("Santé et sécurité au travail")),
              br(),
              grVizOutput("dendo_sante_secu", height = "600px", width = "100%")
            )
            
            
          )
        )
        
        
        
      )
    )))
    
  )
)
