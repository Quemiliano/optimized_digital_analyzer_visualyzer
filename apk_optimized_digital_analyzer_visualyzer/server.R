#Author: Emiliano QUENUM
#Linkedin: https://www.linkedin.com/in/emiliano-quenum-50973a253


#Importation packages
pacman::p_load(
  shiny,
  shinyjs,
  dplyr,
  tidyr,
  readr,
  openxlsx,
  DT,
  shiny,
  reactable,
  shinythemes,
  ggplot2,
  stringi,
  stringr,
  readODS
)

source("global.R", local = TRUE) # Import des variables contenu dans le fichier global.R


function(input, output, session) {
  # Déclaration de la fonction initialisatrice du SERVER SHINY
  
  
  #Création de variable réactive utilisable que lorque l'application est ouverte, elles se supprime à sa fermeture
  data_rsu = reactiveValues(data = NULL, col_names = NULL)
  data_rsu_recap = reactiveValues(data = NULL)
  
  # Réaction au clic sur le bouton de chargement
  observeEvent(input$valider_import, {
    req(input$rsu_initiale_file)
    
    tryCatch({
      #trycatch permet de contourner les éventuelles erreur à l'affichage ou créant des bugs dans le code
      #initialisation de la zone d'importation du fichier relative à la BDD du RSU
      data_rsu$data <- tryCatch({
        import_file(my_path = input$rsu_initiale_file$datapath)
      }, error = function(e) {
        
      })
      
      showNotification("Le fichier a été importé avec succès.",
                       type = "message",
                       duration = 10)
      colnames(data_rsu$data) <-  gsub("\\.", " ", sub("\\..", "- ", gsub("\\-", " ", names(
        data_rsu$data
      ))))
      data_rsu$col_names = colnames(data_rsu$data)
      
      
    }, error = function(e) {
      showNotification(
        #affichage d'une notification si il y as des erreurs
        "Erreur: Le fichier importé n'est pas au bon format !!!",
        type = "error",
        duration = 5
      )
    })
  })
  
  
  observeEvent(input$valider_import_recap, {
    req(input$rsu_recap_file)
    #initialisation de la zone d'importation du fichier relative aux partitions
    tryCatch({
      data_rsu_recap$data <- tryCatch({
        import_file(my_path = input$rsu_recap_file$datapath)
      }, error = function(e) {
        
      })
      
      showNotification("Le fichier a été importé avec succès.",
                       type = "message",
                       duration = 5)
    }, error = function(e) {
      showNotification(
        "Erreur: Le fichier importé n'est pas au bon format !!!",
        type = "error",
        duration = 5
      )
    })
  })
  
  
  output$current_table <- renderReactable({
    #Création sous reactable de la table à utilisé durant tout l'étude
    tryCatch({
      reactable(
        data_rsu$data,
        resizable = TRUE,
        wrap = FALSE,
        bordered = TRUE,
        searchable = TRUE,
        highlight = TRUE,
        defaultColDef = colDef(
          na = "-",
          align = "center",
          minWidth = 200,
          headerStyle = list(background = '#424340', color = '#fff')
        )
      )
    }, error = function(e) {
      
    }) # tryCach enlève les erreurs d'affichage si ele ne gène pas au bon dérouler du programme
    
  })
  
  output$recap_current_table <- renderReactable({
    #Création sous reactable de la table à utilisé durant tout l'étude
    tryCatch({
      reactable(
        data_rsu_recap$data,
        resizable = TRUE,
        defaultPageSize = 5,
        # nombre de ligne à l'affichage
        wrap = T,
        bordered = TRUE,
        searchable = TRUE,
        highlight = TRUE,
        defaultColDef = colDef(
          na = "-",
          #affichage des données manquantes
          align = "center",
          minWidth = 50,
          headerStyle = list(background = '#424340', color = '#fff') # couleur de l'entête du tableau
        )
      )
    }, error = function(e) {
      
    }) # tryCach enlève les erreurs d'affichage si elle ne gène pas au bon dérouler du programme
    
  })
  
  
  observeEvent(data_rsu$data, {
    tryCatch({
      data_rsu$posit_ddi_col = names(data_rsu$data) %>% grepl("DDI", ., fixed = T) %>% which() %>% first() #Recherche de la position de la colonne renseignant les différents DDI
      
      data_rsu$posit_dept_col = names(data_rsu$data) %>% grepl("DEPARTEMENT", ., fixed = T) %>% which() %>% first() #Recherche de la position de la colonne renseignant les différents département
      
      updateSelectInput(session,
                        "DDI",
                        choices = paste(data_rsu$data[, data_rsu$posit_ddi_col], sep = "", data_rsu$data[, data_rsu$posit_dept_col]))  # Mise à jour des différentes options en rapport avec le choix de la DDI à étudier
      
      
      # Mise a jour des valeurs possible de la liste déroulante select_satrt et select_end en fonction des colonnes de la BDD
      
      
      updateSelectInput(session, "select_start_act_soc", choices = data_rsu$col_names)  # Mise a jour des colonnes de séléction dans le menu déroulant des colonne de début pour l'action social
      updateSelectInput(session, "select_end_act_soc", choices = data_rsu$col_names)    #"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""" de fin pour l'action social
      
      updateSelectInput(session, "select_start_dial_soc", choices = data_rsu$col_names)  # Mise a jour des colonnes de séléction dans le menu déroulant des colonne de début pour le dialogue social
      updateSelectInput(session, "select_end_dial_soc", choices = data_rsu$col_names)    #"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""" de fin pour le dialogue social
      
      updateSelectInput(session, "select_start_formation", choices = data_rsu$col_names)  # Mise a jour des colonnes de séléction dans le menu déroulant des colonne de début pour la formation
      updateSelectInput(session, "select_end_formation", choices = data_rsu$col_names)    #"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""" de fin pour la formation
      
      
      updateSelectInput(session, "select_start_mouv_perso", choices = data_rsu$col_names)  # Mise a jour des colonnes de séléction dans le menu déroulant des colonne de début pour le mouvement du personnel
      updateSelectInput(session, "select_end_mouv_perso", choices = data_rsu$col_names)    #"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""" de fin pour le mouvement du personnel
      
      updateSelectInput(session, "select_start_orga_trav", choices = data_rsu$col_names)  # Mise a jour des colonnes de séléction dans le menu déroulant des colonne de début pour l'organisation du travail
      updateSelectInput(session, "select_end_orga_trav", choices = data_rsu$col_names)    #"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""" de fin pour l'organisation du travail
      
      
      updateSelectInput(session, "select_start_parc_pro", choices = data_rsu$col_names)  # Mise a jour des colonnes de séléction dans le menu déroulant des colonne de début pour le parcours pro
      updateSelectInput(session, "select_end_parc_pro", choices = data_rsu$col_names)    #"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""" de fin pour le parcours pro
      
      updateSelectInput(session,
                        "select_start_sanc_discip",
                        choices = data_rsu$col_names)  # Mise a jour des colonnes de séléction dans le menu déroulant des colonne de début pour la santé et la sécurité au travail
      updateSelectInput(session, "select_end_sanc_discip", choices = data_rsu$col_names)
      
      updateSelectInput(session, "select_start_sante_secu", choices = data_rsu$col_names)  # Mise a jour des colonnes de séléction dans le menu déroulant des colonne de début pour la santé et la sécurité au travail
      updateSelectInput(session, "select_end_sante_secu", choices = data_rsu$col_names)
      
      # Initialisation des tranches  et réinitialisation en cas de changement de la base de données via nouvelle importation
      
      data_rsu$tranche_act_soc = NULL
      data_rsu$tranche_dial_soc = NULL
      data_rsu$tranche_formation = NULL
      data_rsu$tranche_mouv_perso = NULL
      data_rsu$tranche_orga_trav = NULL
      data_rsu$tranche_parc_pro = NULL
      data_rsu$tranche_sanc_discip = NULL
      data_rsu$tranche_sante_secu = NULL
      
    }, error = function(e) {
      
    })
    
    observeEvent(data_rsu_recap$data, {
      tryCatch({
        #Fonction de mise a jour automatique des noms de colonne à séléctionner dans le cas d"un ou de plusieurs intervalles abordant une même thématique, MAJ= Mise à Jour
        #Récupération à partir de la base de donnée des partitions des intervalle d'appartenance au thématique en retrouvant la colonne grace à une recherche de caractère
        
        
        MAJ_col = function(thematique,
                           ID_input_start,
                           ID_input_end,
                           ID_ntranche = "Tranche 1") {
          observeEvent(ID_ntranche, {
            if (ID_ntranche == "Tranche 1") {
              start_extraction_1 = str_extract((data_rsu_recap$data %>%
                                                  filter(.[, 1] == thematique))[1, 2], "^[0-9]+-")
              end_extraction_1 = str_extract((data_rsu_recap$data %>%
                                                filter(.[, 1] == thematique))[1, 3], "^[0-9]+-")
              #Mise à jour des colones de début et de fin constituant les bornes de l'intervalle en raport avec la thématique abordé
              updateSelectInput(session,
                                ID_input_start,
                                selected = str_subset(
                                  data_rsu$col_names,
                                  paste("^", sep = "", start_extraction_1)
                                ))
              updateSelectInput(session,
                                ID_input_end,
                                selected = str_subset(
                                  data_rsu$col_names,
                                  paste("^", sep = "", end_extraction_1)
                                ))
            } else{
              if (ID_ntranche == "Tranche 2") {
                start_extraction_2 = str_extract((
                  data_rsu_recap$data %>%
                    filter(.[, 1] == thematique)
                )[2, 2],
                "^[0-9]+-")
                end_extraction_2 = str_extract((
                  data_rsu_recap$data %>%
                    filter(.[, 1] == thematique)
                )[2, 3],
                "^[0-9]+-")
                
                updateSelectInput(session,
                                  ID_input_start,
                                  selected = str_subset(
                                    data_rsu$col_names,
                                    paste("^", sep = "", start_extraction_2)
                                  ))
                updateSelectInput(session,
                                  ID_input_end,
                                  selected = str_subset(
                                    data_rsu$col_names,
                                    paste("^", sep = "", end_extraction_2)
                                  ))
                
              } else{
                start_extraction_3 = str_extract((
                  data_rsu_recap$data %>%
                    filter(.[, 1] == thematique)
                )[3, 2],
                "^[0-9]+-")
                end_extraction_3 = str_extract((
                  data_rsu_recap$data %>%
                    filter(.[, 1] == thematique)
                )[3, 3],
                "^[0-9]+-")
                
                updateSelectInput(session,
                                  ID_input_start,
                                  selected = str_subset(
                                    data_rsu$col_names,
                                    paste("^", sep = "", start_extraction_3)
                                  ))
                updateSelectInput(session,
                                  ID_input_end,
                                  selected = str_subset(
                                    data_rsu$col_names,
                                    paste("^", sep = "", end_extraction_3)
                                  ))
              }
              
            }
            
            
          })
          
          
        }
        
        #Séléction automatique des noms de colonne
        
        MAJ_col(
          thematique = "Action sociale" ,
          ID_input_start = "select_start_act_soc",
          ID_input_end = "select_end_act_soc"
        )
        
        MAJ_col(
          thematique = "Formation" ,
          ID_input_start = "select_start_formation",
          ID_input_end = "select_end_formation"
        )
        
        MAJ_col(
          thematique = "Mouvement du personnel" ,
          ID_input_start = "select_start_mouv_perso",
          ID_input_end = "select_end_mouv_perso"
        )
        
        MAJ_col(
          thematique = "Sanction disciplinaire" ,
          ID_input_start = "select_start_sanc_discip",
          ID_input_end = "select_end_sanc_discip"
        )
        
        MAJ_col(
          thematique = "Parcours professionnels" ,
          ID_input_start = "select_start_parc_pro",
          ID_input_end = "select_end_parc_pro"
        )
        
        MAJ_col(
          thematique = "Sanction disciplinaires" ,
          ID_input_start = "select_start_sanc_discip",
          ID_input_end = "select_end_sanc_discip"
        )
        
        
        observeEvent(input$tranche_select_dial_soc, {
          MAJ_col(
            thematique = "Dialogue social" ,
            ID_input_start = "select_start_dial_soc",
            ID_input_end = "select_end_dial_soc",
            ID_ntranch = input$tranche_select_dial_soc
          )
        })
        
        observeEvent(input$tranche_select_orga_trav, {
          MAJ_col(
            thematique = "Organisation du temps de travail" ,
            ID_input_start = "select_start_orga_trav",
            ID_input_end = "select_end_orga_trav",
            ID_ntranche = input$tranche_select_orga_trav
          )
        })
        observeEvent(input$tranche_select_sante_secu, {
          MAJ_col(
            thematique = "Santé et sécurité au travail" ,
            ID_input_start = "select_start_sante_secu",
            ID_input_end = "select_end_sante_secu",
            ID_ntranche = input$tranche_select_sante_secu
          )
        })
        
      }, error = function(e) {
        
      })
      
    })
    
  })
  
  
  
  
  data_rsu$ctn_select_ddi = 0 #Compteur utilisé dans le futur pour des message d'alerte de validation de tranche
  
  observeEvent(is.null(input$DDI) != T, {
    # NB: Cette observation est commune à tout les rubriques
    
    tryCatch({
      data_rsu$ctn_select_ddi = data_rsu$ctn_select_ddi + 1
      data_rsu$data_DDI = data_rsu$data[data_rsu$data[, names(data_rsu$data)[data_rsu$posit_ddi_col]] ==
                                          str_extract(input$DDI, "^[^0-9]+"), ] # Mise à jour des différentes DDI dans la liste déroulant relatif au choix de la DDI étudié
      
      # Décochage des cases de visualisation des tableaux  si on change de DDI
      updateCheckboxGroupInput(session, "vent_crois_btn_act_soc", selected = character(0))
      updateCheckboxGroupInput(session, "manuelle_btn_act_soc", selected = character(0))
      
      
      updateCheckboxGroupInput(session, "vent_crois_btn_dial_soc", selected = character(0))
      updateCheckboxGroupInput(session, "manuelle_btn_dial_soc", selected = character(0))
      
      updateCheckboxGroupInput(session, "vent_crois_btn_formation", selected = character(0))
      updateCheckboxGroupInput(session, "manuelle_btn_formation", selected = character(0))
      
      
      updateCheckboxGroupInput(session, "vent_crois_btn_mouv_perso", selected = character(0))
      updateCheckboxGroupInput(session, "manuelle_btn_mouv_perso", selected = character(0))
      
      
      updateCheckboxGroupInput(session, "vent_crois_btn_orga_trav", selected = character(0))
      updateCheckboxGroupInput(session, "manuelle_btn_orga_trav", selected = character(0))
      
      updateCheckboxGroupInput(session, "vent_crois_btn_parc_pro", selected = character(0))
      updateCheckboxGroupInput(session, "manuelle_btn_parc_pro", selected = character(0))
      
      updateCheckboxGroupInput(session, "vent_crois_btn_sanc_discip", selected = character(0))
      updateCheckboxGroupInput(session, "manuelle_btn_sanc_discip", selected = character(0))
      
      updateCheckboxGroupInput(session, "vent_crois_btn_sante_secu", selected = character(0))
      updateCheckboxGroupInput(session, "manuelle_btn_sante_secu", selected = character(0))
      
      
      if (data_rsu$ctn_select_ddi > 2) {
        showNotification("Tranche reinitialiser!!!",
                         type = "warning",
                         duration = 5)
        showNotification("N'oubliez pas de valider la tranche",
                         type = "warning",
                         duration = 5)
      }
    }, error = function(e) {
      
    })
    
  })
  
  
  ###------------------------------------------------>   Partie création de la table principale de chaque rubrique
  
  #----> Création de la partition relative à l'action sociale
  
  observeEvent(input$valider_act_soc, {
    tryCatch({
      data_rsu$tranche_act_soc = data_rsu$data_DDI %>%
        select(input$select_start_act_soc:input$select_end_act_soc)
      
      
      showNotification("Table découpé avec succès.",
                       type = "message",
                       duration = 5)
      
      updateSelectInput(session, "act_soc_del_col", choices = names(clean_dt(data_rsu$tranche_act_soc))) # mise à jour des colonnes à supprimé dans action sociale
      updateSelectInput(session,
                        "select_manuelle_vent_act_soc",
                        choices = names(clean_dt(data_rsu$tranche_act_soc))) # mise à jour des colonnes à séléctionner pour constitué le tableau ventiler  dans action sociale
      
      table_act_soc =   reactable(
        clean_dt(data_rsu$tranche_act_soc),
        wrap = FALSE,
        bordered = TRUE,
        resizable = TRUE,
        highlight = TRUE,
        defaultColDef = colDef(
          na = "-",
          align = "center",
          minWidth = 200,
          headerStyle = list(background = '#424340', color = '#fff')
        )
      )
      
      output$table_act_soc <- renderReactable({
        table_act_soc
      }) # Rendu de la table complet relatif l'action sociale
      updateCheckboxGroupInput(session, "vent_crois_btn_act_soc", selected = character(0)) # Décochage automatique de la case
      
      
    }, error = function(e) {
      showNotification(
        "Erreur: La tranche sélectionné ne contient rien.",
        type = "error",
        duration = 5
      )
    })
    
    output$btn_telecharger_act_soc <- downloadHandler(
      filename = function() {
        paste("table_action_sociale", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(clean_dt(data_rsu$tranche_act_soc), file, row.names = FALSE)
      }
    )
    
  })
  
  #----> Création de la partition relative au dialogue social
  
  observeEvent(input$valider_dial_soc, {
    tryCatch({
      data_rsu$tranche_dial_soc = data_rsu$data_DDI %>%
        select(input$select_start_dial_soc:input$select_end_dial_soc)
      
      
      showNotification("Table découpé avec succès.",
                       type = "message",
                       duration = 5)
      
      updateSelectInput(session, "dial_soc_del_col", choices = names(clean_dt(data_rsu$tranche_dial_soc))) # mise à jour des colonnes à supprimé dans dialogue social
      updateSelectInput(session,
                        "select_manuelle_vent_dial_soc",
                        choices = names(clean_dt(data_rsu$tranche_dial_soc))) # mise à jour des colonnes à séléctionner pour constitué le tableau ventiler  dans dialogue social
      
      table_dial_soc =   reactable(
        clean_dt(data_rsu$tranche_dial_soc),
        wrap = FALSE,
        bordered = TRUE,
        resizable = TRUE,
        highlight = TRUE,
        defaultColDef = colDef(
          na = "-",
          align = "center",
          minWidth = 200,
          headerStyle = list(background = '#424340', color = '#fff')
        )
      )
      
      output$table_dial_soc <- renderReactable({
        table_dial_soc
      }) # Rendu de la table complet relatif au dialogue social
      updateCheckboxGroupInput(session, "vent_crois_btn_dial_soc", selected = character(0)) # Décochage automatique de la case
      
      
    }, error = function(e) {
      showNotification(
        "Erreur: La tranche sélectionné ne contient rien.",
        type = "error",
        duration = 5
      )
    })
    
    output$btn_telecharger_dial_soc <- downloadHandler(
      filename = function() {
        paste("table_dialogue_social", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(clean_dt(data_rsu$tranche_dial_soc), file, row.names = FALSE)
      }
    )
    
  })
  #----> Création de la partition relative à la formation
  
  
  observeEvent(input$valider_formation, {
    tryCatch({
      data_rsu$tranche_formation = data_rsu$data_DDI %>%
        select(input$select_start_formation:input$select_end_formation)
      
      
      showNotification("Table découpé avec succès.",
                       type = "message",
                       duration = 5)
      
      updateSelectInput(session, "formation_del_col", choices = names(clean_dt(data_rsu$tranche_formation))) # mise à jour des colonnes à supprimé dans formation
      updateSelectInput(session,
                        "select_manuelle_vent_formation",
                        choices = names(clean_dt(data_rsu$tranche_formation))) # mise à jour des colonnes à séléctionner pour constitué le tableau ventiler  dans mouvement de la formation
      
      table_formation =   reactable(
        clean_dt(data_rsu$tranche_formation),
        wrap = FALSE,
        bordered = TRUE,
        resizable = TRUE,
        highlight = TRUE,
        defaultColDef = colDef(
          na = "-",
          align = "center",
          minWidth = 200,
          headerStyle = list(background = '#424340', color = '#fff')
        )
      )
      
      output$table_formation <- renderReactable({
        table_formation
      }) # Rendu de la table complet relatif à la formation
      updateCheckboxGroupInput(session, "vent_crois_btn_formation", selected = character(0)) # Décochage automatique de la case
      
      
    }, error = function(e) {
      showNotification(
        "Erreur: La tranche sélectionné ne contient rien.",
        type = "error",
        duration = 5
      )
    })
    
    output$btn_telecharger_formation <- downloadHandler(
      filename = function() {
        paste("table_formation", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(clean_dt(data_rsu$tranche_formation), file, row.names = FALSE)
      }
    )
    
  })
  
  #----> Création de la partition relative au mouvement du personnel
  
  
  observeEvent(input$valider_mouv_perso, {
    tryCatch({
      data_rsu$tranche_mouv_perso = data_rsu$data_DDI %>%
        select(input$select_start_mouv_perso:input$select_end_mouv_perso)
      
      
      showNotification("Table découpé avec succès.",
                       type = "message",
                       duration = 5)
      
      updateSelectInput(session, "mouv_perso_del_col", choices = names(clean_dt(data_rsu$tranche_mouv_perso))) # mise à jour des colonnes à supprimé dans mouvement du perso
      updateSelectInput(session,
                        "select_manuelle_vent_mouv_perso",
                        choices = names(clean_dt(data_rsu$tranche_mouv_perso))) # mise à jour des colonnes à séléctionner pour constitué le tableau ventiler  dans mouvement du perso
      
      table_mouv_perso =   reactable(
        clean_dt(data_rsu$tranche_mouv_perso),
        wrap = FALSE,
        bordered = TRUE,
        resizable = TRUE,
        highlight = TRUE,
        defaultColDef = colDef(
          na = "-",
          align = "center",
          minWidth = 200,
          headerStyle = list(background = '#424340', color = '#fff')
        )
      )
      
      output$table_mouv_perso <- renderReactable({
        table_mouv_perso
      }) # Rendu de la table complet relatif au mouvement du perso
      updateCheckboxGroupInput(session, "vent_crois_btn_mouv_perso", selected = character(0)) # Décochage automatique de la case
      
      
    }, error = function(e) {
      showNotification(
        "Erreur: La tranche sélectionné ne contient rien.",
        type = "error",
        duration = 5
      )
    })
    
    output$btn_telecharger_mouv_perso <- downloadHandler(
      filename = function() {
        paste("table_mouvement_du_personnel",
              Sys.Date(),
              ".csv",
              sep = "")
      },
      content = function(file) {
        write.csv(clean_dt(data_rsu$tranche_mouv_perso),
                  file,
                  row.names = FALSE)
      }
    )
    
  })
  
  #----> Création de la partition relative à l'organisation du travail
  
  
  observeEvent(input$valider_orga_trav, {
    tryCatch({
      data_rsu$tranche_orga_trav = data_rsu$data_DDI %>%
        select(input$select_start_orga_trav:input$select_end_orga_trav)
      
      
      showNotification("Table découpé avec succès.",
                       type = "message",
                       duration = 5)
      
      updateSelectInput(session, "orga_trav_del_col", choices = names(clean_dt(data_rsu$tranche_orga_trav))) # mise à jour des colonnes à supprimé dans l' organisation du travail
      updateSelectInput(session,
                        "select_manuelle_vent_orga_trav",
                        choices = names(clean_dt(data_rsu$tranche_orga_trav))) # mise à jour des colonnes à séléctionner pour constitué le tableau ventiler l' organisation du travail
      
      table_orga_trav =   reactable(
        clean_dt(data_rsu$tranche_orga_trav),
        wrap = FALSE,
        bordered = TRUE,
        resizable = TRUE,
        highlight = TRUE,
        defaultColDef = colDef(
          na = "-",
          align = "center",
          minWidth = 200,
          headerStyle = list(background = '#424340', color = '#fff')
        )
      )
      
      output$table_orga_trav <- renderReactable({
        table_orga_trav
      }) # Rendu de la table complet relatif à l' organisation du travail
      updateCheckboxGroupInput(session, "vent_crois_btn_orga_trav", selected = character(0)) # Décochage automatique de la case
      
      
    }, error = function(e) {
      showNotification(
        "Erreur: La tranche sélectionné ne contient rien.",
        type = "error",
        duration = 5
      )
    })
    
    output$btn_telecharger_orga_trav <- downloadHandler(
      filename = function() {
        paste("table_organisation_du_travail",
              Sys.Date(),
              ".csv",
              sep = "")
      },
      content = function(file) {
        write.csv(clean_dt(data_rsu$tranche_orga_trav), file, row.names = FALSE)
      }
    )
    
  })
  
  #----> Création de la partition relative au parcours professionnel
  
  
  observeEvent(input$valider_parc_pro, {
    tryCatch({
      data_rsu$tranche_parc_pro = data_rsu$data_DDI %>% select(input$select_start_parc_pro:input$select_end_parc_pro)
      
      showNotification("Table découpé avec succès.",
                       type = "message",
                       duration = 5)
      
      updateSelectInput(session, "parc_pro_del_col", choices = names(clean_dt(data_rsu$tranche_parc_pro))) # mise à jour des colonnes à supprimé dans la mobilité; parc pro
      updateSelectInput(session,
                        "select_manuelle_vent_parc_pro",
                        choices = names(clean_dt(data_rsu$tranche_parc_pro))) # mise à jour des colonnes à séléctionner pour constitué le tableau ventiler dans la mobilité; parc pro
      
      table_parc_pro =   reactable(
        clean_dt(data_rsu$tranche_parc_pro),
        wrap = FALSE,
        bordered = TRUE,
        resizable = TRUE,
        highlight = TRUE,
        defaultColDef = colDef(
          na = "-",
          align = "center",
          minWidth = 200,
          headerStyle = list(background = '#424340', color = '#fff')
        )
      )
      
      output$table_parc_pro <- renderReactable({
        table_parc_pro
      }) # Rendu de la table complet relatif au parcours professionnel
      
      updateCheckboxGroupInput(session, "vent_crois_btn_parc_pro", selected = character(0)) # Décochage automatique de la case
      
      
    }, error = function(e) {
      showNotification(
        "Erreur: La tranche sélectionné ne contient rien.",
        type = "error",
        duration = 5
      )
    })
    
    output$btn_telecharger_parc_pro <- downloadHandler(
      filename = function() {
        paste("table_parcours_pro", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(clean_dt(data_rsu$tranche_parc_pro), file, row.names = FALSE)
      }
    )
    
  })
  
  #----> Création de la partition relative à la sanction disciplinaire
  
  
  observeEvent(input$valider_sanc_discip, {
    tryCatch({
      data_rsu$tranche_sanc_discip = data_rsu$data_DDI %>%
        select(input$select_start_sanc_discip:input$select_end_sanc_discip)
      
      
      showNotification("Table découpé avec succès.",
                       type = "message",
                       duration = 5)
      
      updateSelectInput(session, "sanc_discip_del_col", choices = names(clean_dt(data_rsu$tranche_sanc_discip))) # mise à jour des colonnes à supprimé dans sanction disciplinaire
      updateSelectInput(session,
                        "select_manuelle_vent_sanc_discip",
                        choices = names(clean_dt(data_rsu$tranche_sanc_discip))) # mise à jour des colonnes à séléctionner pour constitué le tableau ventiler  dans sanction disciplinaire
      
      table_sanc_discip =   reactable(
        clean_dt(data_rsu$tranche_sanc_discip),
        wrap = FALSE,
        bordered = TRUE,
        resizable = TRUE,
        highlight = TRUE,
        defaultColDef = colDef(
          na = "-",
          align = "center",
          minWidth = 200,
          headerStyle = list(background = '#424340', color = '#fff')
        )
      )
      
      output$table_sanc_discip <- renderReactable({
        table_sanc_discip
      }) # Rendu de la table complet relatif aux sanctions disciplinaires
      updateCheckboxGroupInput(session, "vent_crois_btn_sanc_discip", selected = character(0)) # Décochage automatique de la case
      
      
    }, error = function(e) {
      showNotification(
        "Erreur: La tranche sélectionné ne contient rien.",
        type = "error",
        duration = 5
      )
    })
    
    output$btn_telecharger_sanc_discip <- downloadHandler(
      filename = function() {
        paste("table_action_sociale", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(clean_dt(data_rsu$tranche_sanc_discip),
                  file,
                  row.names = FALSE)
      }
    )
    
  })
  
  
  #----> Création de la partition relative à la santé et au sécurité au travail
  
  observeEvent(input$valider_sante_secu, {
    tryCatch({
      data_rsu$tranche_sante_secu = data_rsu$data_DDI %>%
        select(input$select_start_sante_secu:input$select_end_sante_secu)
      
      
      showNotification("Table découpé avec succès.",
                       type = "message",
                       duration = 5)
      
      updateSelectInput(session, "sante_secu_del_col", choices = names(clean_dt(data_rsu$tranche_sante_secu))) # mise à jour des colonnes à supprimé dans sante et sécurité au travail
      updateSelectInput(session,
                        "select_manuelle_vent_sante_secu",
                        choices = names(clean_dt(data_rsu$tranche_sante_secu))) # mise à jour des colonnes à séléctionner pour constitué le tableau ventiler  dans sante et sécurité au travail
      
      table_sante_secu =   reactable(
        clean_dt(data_rsu$tranche_sante_secu),
        wrap = FALSE,
        bordered = TRUE,
        resizable = TRUE,
        highlight = TRUE,
        defaultColDef = colDef(
          na = "-",
          align = "center",
          minWidth = 200,
          headerStyle = list(background = '#424340', color = '#fff')
        )
      )
      
      output$table_sante_secu <- renderReactable({
        table_sante_secu
      }) # Rendu de la table complet relatif au santé et sécurité au travail
      updateCheckboxGroupInput(session, "vent_crois_btn_sante_secu", selected = character(0)) # Décochage automatique de la case
      
      
    }, error = function(e) {
      showNotification(
        "Erreur: La tranche sélectionné ne contient rien.",
        type = "error",
        duration = 5
      )
    })
    
    output$btn_telecharger_sante_secu <- downloadHandler(
      filename = function() {
        paste("table_sante_et_securite_au_travail",
              Sys.Date(),
              ".csv",
              sep = "")
      },
      content = function(file) {
        write.csv(clean_dt(data_rsu$tranche_sante_secu),
                  file,
                  row.names = FALSE)
      }
    )
    
  })
  
  
  #-----------------------------------------------------> Fin création  de table
  
  
  ###------------------------------------------------>   Partie modification de la table principale de chaque rubrique
  #Implémentation du script permettant la modification de la partition : ajout de colonne ou suppression d'un ou de plusieurs colonnes pour toutes les thématiques sauf l'emploi
  
  
  #----> Modification de la partition relative à l'action sociale
  
  observeEvent(input$valider_act_soc_modif, {
    tryCatch({
      if (is.null(input$btn_ajout_act_soc_col) != TRUE) {
        tryCatch({
          data_rsu$tranche_act_soc[unlist(strsplit(input$act_soc_col, split = ";"))] = c(unlist(strsplit(
            input$ajout_act_soc_col_val, split = ";"
          ))) # ajout des colonnes en tranches
          
          updateSelectInput(session, "act_soc_del_col", choices = names(clean_dt(data_rsu$tranche_act_soc))) # mise à jour des colonnes à supprimé
          updateSelectInput(session,
                            "select_manuelle_vent_act_soc",
                            choices = names(clean_dt(data_rsu$tranche_act_soc))) # mise à jour des colonnes à séléctionner pour constitué le tableau ventiler
          showNotification(
            "Colonne(s) ajouté(e)(s) avec succès.",
            type = "message",
            duration = 5
          )
        }, error = function(e) {
          
        })
      }
      
      
      else{
        if (is.null(input$btn_del_act_soc_col) != TRUE) {
          tryCatch({
            data_rsu$tranche_act_soc = data_rsu$tranche_act_soc %>% select(-input$act_soc_del_col)
            updateSelectInput(session, "act_soc_del_col", choices = names(clean_dt(data_rsu$tranche_act_soc))) # mise à jour des colonnes à supprimé dans action social
            updateSelectInput(session,
                              "select_manuelle_vent_act_soc",
                              choices = names(clean_dt(data_rsu$tranche_act_soc))) # mise à jour des colonnes à séléctionner pour constitué le tableau ventiler dans action social
            showNotification("Suppression effective.",
                             type = "message",
                             duration = 5)
            
          }, error = function(e) {
            
          })
        }
        
      }
      
      
      table_act_soc =   reactable(
        clean_dt(data_rsu$tranche_act_soc),
        wrap = FALSE,
        bordered = TRUE,
        resizable = TRUE,
        highlight = TRUE,
        defaultColDef = colDef(
          na = "-",
          align = "center",
          minWidth = 200,
          headerStyle = list(background = '#424340', color = '#fff')
        )
      )
      
      output$table_act_soc <- renderReactable({
        table_act_soc
      }) # Rendu de la table complet relatif à l'action social
      
      updateCheckboxGroupInput(session, "btn_ajout_act_soc_col", selected = character(0)) # Décochage de la case après validation d'ajout de colonne précédent
      updateCheckboxGroupInput(session, "btn_del_act_soc_col", selected = character(0)) # Décochage de la case "Tableau constitué... après suppression de colonne précédent
      updateTextInput(session, "act_soc_col", value = "")
      updateTextInput(session, "ajout_act_soc_col_val", value = "")
      
    }, error = function(e) {
      showNotification("Valider d'abord votre partition",
                       type = "error",
                       duration = 5)
    })
    
  })
  
  
  
  #----> Modification de la partition relative au dialogue social
  
  
  observeEvent(input$valider_dial_soc_modif, {
    tryCatch({
      if (is.null(input$btn_ajout_dial_soc_col) != TRUE) {
        tryCatch({
          data_rsu$tranche_dial_soc[unlist(strsplit(input$dial_soc_col, split = ";"))] = c(unlist(strsplit(
            input$ajout_dial_soc_col_val, split = ";"
          ))) # ajout des colonnes en tranches
          
          updateSelectInput(session, "dial_soc_del_col", choices = names(clean_dt(data_rsu$tranche_dial_soc))) # mise à jour des colonnes à supprimé
          updateSelectInput(session,
                            "select_manuelle_vent_dial_soc",
                            choices = names(clean_dt(data_rsu$tranche_dial_soc))) # mise à jour des colonnes à séléctionner pour constitué le tableau ventiler
          
          showNotification(
            "Colonne(s) ajouté(e)(s) avec succès.",
            type = "message",
            duration = 5
          )
        }, error = function(e) {
          
        })
      }
      
      else{
        if (is.null(input$btn_del_dial_soc_col) != TRUE) {
          tryCatch({
            data_rsu$tranche_dial_soc = data_rsu$tranche_dial_soc %>% select(-input$dial_soc_del_col)
            updateSelectInput(session, "dial_soc_del_col", choices = names(clean_dt(data_rsu$tranche_dial_soc))) # mise à jour des colonnes à supprimé dans la mobilité; parc pro
            updateSelectInput(session,
                              "select_manuelle_vent_dial_soc",
                              choices = names(clean_dt(data_rsu$tranche_dial_soc))) # mise à jour des colonnes à séléctionner pour constitué le tableau ventiler du dialogue social
            showNotification("Suppression effective.",
                             type = "message",
                             duration = 5)
            
          }, error = function(e) {
            
          })
        }
        
      }
      
      
      table_dial_soc =   reactable(
        clean_dt(data_rsu$tranche_dial_soc),
        wrap = FALSE,
        bordered = TRUE,
        resizable = TRUE,
        highlight = TRUE,
        defaultColDef = colDef(
          na = "-",
          align = "center",
          minWidth = 200,
          headerStyle = list(background = '#424340', color = '#fff')
        )
      )
      
      output$table_dial_soc <- renderReactable({
        table_dial_soc
      }) # Rendu de la table complet relatif au parcours professionnel
      
      updateCheckboxGroupInput(session, "btn_ajout_dial_soc_col", selected = character(0)) # Décochage de la case après validation d'ajout de colonne précédent
      updateCheckboxGroupInput(session, "btn_del_dial_soc_col", selected = character(0)) # Décochage de la case "Tableau constitué... après suppression de colonne précédent
      updateTextInput(session, "dial_soc_col", value = "")
      updateTextInput(session, "ajout_dial_soc_col_val", value = "")
      
    }, error = function(e) {
      showNotification("Valider d'abord votre partition",
                       type = "error",
                       duration = 5)
    })
    
  })
  
  #----> Modification de la partition relative à la formation
  
  
  observeEvent(input$valider_formation_modif, {
    tryCatch({
      if (is.null(input$btn_ajout_formation_col) != TRUE) {
        tryCatch({
          data_rsu$tranche_formation[unlist(strsplit(input$formation_col, split = ";"))] = c(unlist(
            strsplit(input$ajout_formation_col_val, split = ";")
          )) # ajout des colonnes en tranches
          
          updateSelectInput(session, "formation_del_col", choices = names(clean_dt(data_rsu$tranche_formation))) # mise à jour des colonnes à supprimé dans la formation
          updateSelectInput(session,
                            "select_manuelle_vent_formation",
                            choices = names(clean_dt(data_rsu$tranche_formation))) # mise à jour des colonnes à séléctionner pour constitué le tableau ventiler dans la formation
          showNotification(
            "Colonne(s) ajouté(e)(s) avec succès.",
            type = "message",
            duration = 5
          )
        }, error = function(e) {
          
        })
      }
      
      
      else{
        if (is.null(input$btn_del_formation_col) != TRUE) {
          tryCatch({
            data_rsu$tranche_formation = data_rsu$tranche_formation %>% select(-input$formation_del_col)
            updateSelectInput(session,
                              "formation_del_col",
                              choices = names(clean_dt(data_rsu$tranche_formation))) # mise à jour des colonnes à supprimé dans la formation
            updateSelectInput(session,
                              "select_manuelle_vent_formation",
                              choices = names(clean_dt(data_rsu$tranche_formation))) # mise à jour des colonnes à séléctionner pour constitué le tableau ventiler dans la formation
            showNotification("Suppression effective.",
                             type = "message",
                             duration = 5)
            
          }, error = function(e) {
            
          })
        }
        
      }
      
      
      table_formation =   reactable(
        clean_dt(data_rsu$tranche_formation),
        wrap = FALSE,
        bordered = TRUE,
        resizable = TRUE,
        highlight = TRUE,
        defaultColDef = colDef(
          na = "-",
          align = "center",
          minWidth = 200,
          headerStyle = list(background = '#424340', color = '#fff')
        )
      )
      
      output$table_formation <- renderReactable({
        table_formation
      }) # Rendu de la table complet relatif à la formation
      
      updateCheckboxGroupInput(session, "btn_ajout_formation_col", selected = character(0)) # Décochage de la case  après validation d'ajout de colonne précédent
      updateCheckboxGroupInput(session, "btn_del_formation_col", selected = character(0)) # Décochage de la case "Tableau constitué... après suppression de colonne précédent
      updateTextInput(session, "formation_col", value = "")
      updateTextInput(session, "ajout_formation_col_val", value = "")
      
    }, error = function(e) {
      showNotification("Valider d'abord votre partition",
                       type = "error",
                       duration = 5)
    })
    
  })
  
  #----> Modification de la partition relative au mouvement du personnel
  
  
  
  observeEvent(input$valider_mouv_perso_modif, {
    tryCatch({
      if (is.null(input$btn_ajout_mouv_perso_col) != TRUE) {
        tryCatch({
          data_rsu$tranche_mouv_perso[unlist(strsplit(input$mouv_perso_col, split = ";"))] = c(unlist(
            strsplit(input$ajout_mouv_perso_col_val, split = ";")
          )) # ajout des colonnes en tranches
          
          updateSelectInput(session, "mouv_perso_del_col", choices = names(clean_dt(data_rsu$tranche_mouv_perso))) # mise à jour des colonnes à supprimé
          updateSelectInput(session,
                            "select_manuelle_vent_mouv_perso",
                            choices = names(clean_dt(data_rsu$tranche_mouv_perso))) # mise à jour des colonnes à séléctionner pour constitué le tableau ventiler
          showNotification(
            "Colonne(s) ajouté(e)(s) avec succès.",
            type = "message",
            duration = 5
          )
        }, error = function(e) {
          
        })
      }
      
      
      else{
        if (is.null(input$btn_del_mouv_perso_col) != TRUE) {
          tryCatch({
            data_rsu$tranche_mouv_perso = data_rsu$tranche_mouv_perso %>% select(-input$mouv_perso_del_col)
            updateSelectInput(session,
                              "mouv_perso_del_col",
                              choices = names(clean_dt(
                                data_rsu$tranche_mouv_perso
                              ))) # mise à jour des colonnes à supprimé dans les mouvements du perso
            updateSelectInput(session,
                              "select_manuelle_vent_mouv_perso",
                              choices = names(clean_dt(
                                data_rsu$tranche_mouv_perso
                              ))) # mise à jour des colonnes à séléctionner pour constitué le tableau ventiler dans les mouvements du perso
            showNotification("Suppression effective.",
                             type = "message",
                             duration = 5)
            
          }, error = function(e) {
            
          })
        }
        
      }
      
      
      table_mouv_perso =   reactable(
        clean_dt(data_rsu$tranche_mouv_perso),
        wrap = FALSE,
        bordered = TRUE,
        resizable = TRUE,
        highlight = TRUE,
        defaultColDef = colDef(
          na = "-",
          align = "center",
          minWidth = 200,
          headerStyle = list(background = '#424340', color = '#fff')
        )
      )
      
      output$table_mouv_perso <- renderReactable({
        table_mouv_perso
      }) # Rendu de la table complet relatif au mouvements personnels
      
      updateCheckboxGroupInput(session, "btn_ajout_mouv_perso_col", selected = character(0)) # Décochage de la case après validation d'ajout de colonne précédent
      updateCheckboxGroupInput(session, "btn_del_mouv_perso_col", selected = character(0)) # Décochage de la case "Tableau constitué... après suppression de colonne précédent
      updateTextInput(session, "mouv_perso_col", value = "")
      updateTextInput(session, "ajout_mouv_perso_col_val", value = "")
      
    }, error = function(e) {
      showNotification("Valider d'abord votre partition",
                       type = "error",
                       duration = 5)
    })
    
  })
  
  #----> Modification de la partition relative à l'a formation'organisation du travail
  
  
  observeEvent(input$valider_orga_trav_modif, {
    tryCatch({
      if (is.null(input$btn_ajout_orga_trav) != TRUE) {
        tryCatch({
          data_rsu$tranche_orga_trav[unlist(strsplit(input$orga_trav_col, split = ";"))] = c(unlist(
            strsplit(input$ajout_orga_trav_col_val, split = ";")
          )) # ajout des colonnes en tranches
          
          updateSelectInput(session, "orga_trav_del_col", choices = names(clean_dt(data_rsu$tranche_orga_trav))) # mise à jour des colonnes à supprimé
          
          updateSelectInput(session,
                            "select_manuelle_vent_orga_trav",
                            choices = names(clean_dt(data_rsu$tranche_orga_trav))) # mise à jour des colonnes à séléctionner pour constitué le tableau ventiler
          
          showNotification(
            "Colonne(s) ajouté(e)(s) avec succès.",
            type = "message",
            duration = 5
          )
        }, error = function(e) {
          
        })
      }
      
      
      else{
        if (is.null(input$btn_del_orga_trav) != TRUE) {
          tryCatch({
            data_rsu$tranche_orga_trav = data_rsu$tranche_orga_trav %>% select(-input$orga_trav_del_col)
            updateSelectInput(session,
                              "orga_trav_del_col",
                              choices = names(clean_dt(data_rsu$tranche_orga_trav))) # mise à jour des colonnes à supprimé dans organisation du travail
            updateSelectInput(session,
                              "select_manuelle_vent_orga_trav",
                              choices = names(clean_dt(data_rsu$tranche_orga_trav))) # mise à jour des colonnes à séléctionner pour constitué le tableau ventiler dans  organisation du travail
            showNotification("Suppression effective.",
                             type = "message",
                             duration = 5)
            
          }, error = function(e) {
            
          })
        }
        
      }
      
      
      table_orga_trav =   reactable(
        clean_dt(data_rsu$tranche_orga_trav),
        wrap = FALSE,
        bordered = TRUE,
        resizable = TRUE,
        highlight = TRUE,
        defaultColDef = colDef(
          na = "-",
          align = "center",
          minWidth = 200,
          headerStyle = list(background = '#424340', color = '#fff')
        )
      )
      
      output$table_orga_trav <- renderReactable({
        table_orga_trav
      }) # Rendu de la table complet relatif au parcours professionnel
      
      updateCheckboxGroupInput(session, "btn_ajout_orga_trav", selected = character(0)) # Décochage de la case après validation d'ajout de colonne précédent
      updateCheckboxGroupInput(session, "btn_del_orga_trav", selected = character(0)) # Décochage de la case "Tableau constitué... après suppression de colonne précédent
      updateTextInput(session, "orga_trav_col", value = "")
      updateTextInput(session, "ajout_orga_trav_col_val", value = "")
      
    }, error = function(e) {
      showNotification("Valider d'abord votre partition",
                       type = "error",
                       duration = 5)
    })
    
  })
  
  #----> Modification de la partition relative au parcours professionnel
  
  
  observeEvent(input$valider_parc_pro_modif, {
    tryCatch({
      if (is.null(input$btn_ajout_parc_pro_col) != TRUE) {
        tryCatch({
          data_rsu$tranche_parc_pro[unlist(strsplit(input$parc_pro_col, split = ";"))] = c(unlist(strsplit(
            input$ajout_parc_pro_col_val, split = ";"
          ))) # ajout des colonnes en tranches
          
          updateSelectInput(session, "parc_pro_del_col", choices = names(clean_dt(data_rsu$tranche_parc_pro))) # mise à jour des colonnes à supprimé dans la mobilité; parc pro
          updateSelectInput(session,
                            "select_manuelle_vent_parc_pro",
                            choices = names(clean_dt(data_rsu$tranche_parc_pro))) # mise à jour des colonnes à séléctionner pour constitué le tableau ventiler dans la mobilité; parc pro
          showNotification(
            "Colonne(s) ajouté(e)(s) avec succès.",
            type = "message",
            duration = 5
          )
        }, error = function(e) {
          
        })
      }
      
      
      else{
        if (is.null(input$btn_del_parc_pro_col) != TRUE) {
          tryCatch({
            data_rsu$tranche_parc_pro = data_rsu$tranche_parc_pro %>% select(-input$parc_pro_del_col)
            updateSelectInput(session, "parc_pro_del_col", choices = names(clean_dt(data_rsu$tranche_parc_pro))) # mise à jour des colonnes à supprimé dans la mobilité; parc pro
            updateSelectInput(session,
                              "select_manuelle_vent_parc_pro",
                              choices = names(clean_dt(data_rsu$tranche_parc_pro))) # mise à jour des colonnes à séléctionner pour constitué le tableau ventiler dans la mobilité; parc pro
            showNotification("Suppression effective.",
                             type = "message",
                             duration = 5)
            
          }, error = function(e) {
            
          })
        }
        
      }
      
      
      table_parc_pro =   reactable(
        clean_dt(data_rsu$tranche_parc_pro),
        wrap = FALSE,
        bordered = TRUE,
        resizable = TRUE,
        highlight = TRUE,
        defaultColDef = colDef(
          na = "-",
          align = "center",
          minWidth = 200,
          headerStyle = list(background = '#424340', color = '#fff')
        )
      )
      
      output$table_parc_pro <- renderReactable({
        table_parc_pro
      }) # Rendu de la table complet relatif au parcours professionnel
      
      updateCheckboxGroupInput(session, "btn_ajout_parc_pro_col", selected = character(0)) # Décochage de la case  après validation d'ajout de colonne précédent
      updateCheckboxGroupInput(session, "btn_del_parc_pro_col", selected = character(0)) # Décochage de la case "Tableau constitué... après suppression de colonne précédent
      updateTextInput(session, "parc_pro_col", value = "")
      updateTextInput(session, "ajout_parc_pro_col_val", value = "")
      
    }, error = function(e) {
      showNotification("Valider d'abord votre partition",
                       type = "error",
                       duration = 5)
    })
    
  })
  
  #----> Modification de la partition relative à la anction disciplinaire
  
  
  observeEvent(input$valider_sanc_discip_modif, {
    tryCatch({
      if (is.null(input$btn_ajout_sanc_discip_col) != TRUE) {
        tryCatch({
          data_rsu$tranche_sanc_discip[unlist(strsplit(input$sanc_discip_col, split = ";"))] = c(unlist(
            strsplit(input$ajout_sanc_discip_col_val, split = ";")
          )) # ajout des colonnes en tranches
          
          updateSelectInput(session,
                            "sanc_discip_del_col",
                            choices = names(clean_dt(data_rsu$tranche_sanc_discip))) # mise à jour des colonnes à supprimé
          updateSelectInput(session,
                            "select_manuelle_vent_sanc_discip",
                            choices = names(clean_dt(data_rsu$tranche_sanc_discip))) # mise à jour des colonnes à séléctionner pour constitué le tableau ventiler
          showNotification(
            "Colonne(s) ajouté(e)(s) avec succès.",
            type = "message",
            duration = 5
          )
        }, error = function(e) {
          
        })
      }
      
      
      else{
        if (is.null(input$btn_del_sanc_discip_col) != TRUE) {
          tryCatch({
            data_rsu$tranche_sanc_discip = data_rsu$tranche_sanc_discip %>% select(-input$sanc_discip_del_col)
            updateSelectInput(session,
                              "sanc_discip_del_col",
                              choices = names(clean_dt(
                                data_rsu$tranche_sanc_discip
                              ))) # mise à jour des colonnes à supprimé dans sanction disciplinaire
            updateSelectInput(session,
                              "select_manuelle_vent_sanc_discip",
                              choices = names(clean_dt(
                                data_rsu$tranche_sanc_discip
                              ))) # mise à jour des colonnes à séléctionner pour constitué le tableau ventiler dans sanction disciplinaire
            showNotification("Suppression effective.",
                             type = "message",
                             duration = 5)
            
          }, error = function(e) {
            
          })
        }
        
      }
      
      
      table_sanc_discip =   reactable(
        clean_dt(data_rsu$tranche_sanc_discip),
        wrap = FALSE,
        bordered = TRUE,
        resizable = TRUE,
        highlight = TRUE,
        defaultColDef = colDef(
          na = "-",
          align = "center",
          minWidth = 200,
          headerStyle = list(background = '#424340', color = '#fff')
        )
      )
      
      output$table_sanc_discip <- renderReactable({
        table_sanc_discip
      }) # Rendu de la table complet relatif aux sanctions disciplinaires
      
      updateCheckboxGroupInput(session, "btn_ajout_sanc_discip_col", selected = character(0)) # Décochage de la case après validation d'ajout de colonne précédent
      updateCheckboxGroupInput(session, "btn_del_sanc_discip_col", selected = character(0)) # Décochage de la case "Tableau constitué... après suppression de colonne précédent
      updateTextInput(session, "sanc_discip_col", value = "")
      updateTextInput(session, "ajout_sanc_discip_col_val", value = "")
      
    }, error = function(e) {
      showNotification("Valider d'abord votre partition",
                       type = "error",
                       duration = 5)
    })
    
  })
  
  #----> Modification de la partition relative à la santé et à la sécurité au travail
  
  observeEvent(input$valider_sante_secu_modif, {
    tryCatch({
      if (is.null(input$btn_ajout_sante_secu_col) != TRUE) {
        tryCatch({
          data_rsu$tranche_sante_secu[unlist(strsplit(input$sante_secu_col, split = ";"))] = c(unlist(
            strsplit(input$ajout_sante_secu_col_val, split = ";")
          )) # ajout des colonnes en tranches
          
          updateSelectInput(session, "sante_secu_del_col", choices = names(clean_dt(data_rsu$tranche_sante_secu))) # mise à jour des colonnes à supprimé
          
          updateSelectInput(session,
                            "select_manuelle_vent_sante_secu",
                            choices = names(clean_dt(data_rsu$tranche_sante_secu))) # mise à jour des colonnes à séléctionner pour constitué le tableau ventiler
          
          showNotification(
            "Colonne(s) ajouté(e)(s) avec succès.",
            type = "message",
            duration = 5
          )
        }, error = function(e) {
          
        })
      }
      
      
      else{
        if (is.null(input$btn_del_sante_secu_col) != TRUE) {
          tryCatch({
            data_rsu$tranche_sante_secu = data_rsu$tranche_sante_secu %>% select(-input$sante_secu_del_col)
            updateSelectInput(session,
                              "sante_secu_del_col",
                              choices = names(clean_dt(
                                data_rsu$tranche_sante_secu
                              ))) # mise à jour des colonnes à supprimé dans santé et sécurité au travail
            updateSelectInput(session,
                              "select_manuelle_vent_sante_secu",
                              choices = names(clean_dt(
                                data_rsu$tranche_sante_secu
                              ))) # mise à jour des colonnes à séléctionner pour constitué le tableau ventiler dans la mobilité; parc pro
            showNotification("Suppression effective.",
                             type = "message",
                             duration = 5)
            
          }, error = function(e) {
            
          })
        }
        
      }
      
      
      table_sante_secu =   reactable(
        clean_dt(data_rsu$tranche_sante_secu),
        wrap = FALSE,
        bordered = TRUE,
        resizable = TRUE,
        highlight = TRUE,
        defaultColDef = colDef(
          na = "-",
          align = "center",
          minWidth = 200,
          headerStyle = list(background = '#424340', color = '#fff')
        )
      )
      
      output$table_sante_secu <- renderReactable({
        table_sante_secu
      }) # Rendu de la table complet relatif au parcours professionnel
      
      updateCheckboxGroupInput(session, "btn_ajout_sante_secu_col", selected = character(0)) # Décochage de la case après validation d'ajout de colonne précédent
      updateCheckboxGroupInput(session, "btn_del_sante_secu_col", selected = character(0)) # Décochage de la case "Tableau constitué... après suppression de colonne précédent
      updateTextInput(session, "sante_secu_col", value = "")
      updateTextInput(session, "ajout_sante_secu_col_val", value = "")
      
    }, error = function(e) {
      showNotification("Valider d'abord votre partition",
                       type = "error",
                       duration = 5)
    })
    
  })
  
  
  #-----------------------------------------------------> Fin modification de la table
  
  
  
  ## -------------------------------------------Script de conception des tables & graphes par défaut de l'action sociale---------------------------------------------------
  
  ##---------------------> Conception tableau par défaut de l'action social
  
  sous_theme_act_soc_dataset = reactive({
    #Initialisation des sous-thèmes abordés
    switch(
      input$sous_theme_act_soc,
      "A- Préstations sociales" = c(
        "Agents bénéficiaires de prestations sociales: par type de prestation",
        "Agents bénéficiaires de prestations sociales: type de prestation & genre",
        "Agents bénéficiaires de prestations sociales: type de prestation & catégorie",
        "Agents bénéficiaires de prestations sociales: catégorie & genre"
      ),
      "Informations supplémentaires" = c("Réponses qualitatives")
    )
  })
  
  observe({
    updateSelectInput(session, "vent_crois_act_soc", choices = sous_theme_act_soc_dataset()) # mise à jour des colonnes à séléctionner pour sélectionner le sous thème étudier dans l'action sociale
  })
  
  #Recherche des colonnes par recherche sur les mots clées
  
  
  observeEvent(input$vent_crois_btn_act_soc, {
    tryCatch({
      cat_A_grd_enfant_act_soc = colonne_match(
        data = data_rsu$tranche_act_soc,
        vecteurs_mots =  c("Catégorie A", "Garde", "enfant"),
        somme = F
      )
      cat_B_grd_enfant_act_soc = colonne_match(
        data = data_rsu$tranche_act_soc,
        vecteurs_mots =  c("Catégorie B", "Garde", "enfant"),
        somme = F
      )
      cat_C_grd_enfant_act_soc = colonne_match(
        data = data_rsu$tranche_act_soc,
        vecteurs_mots =  c("Catégorie C", "Garde", "enfant"),
        somme = F
      )
      
      
      cat_A_CESU_act_soc = colonne_match(
        data = data_rsu$tranche_act_soc,
        vecteurs_mots =  c("Catégorie A", "CESU"),
        somme = F
      )
      cat_B_CESU_act_soc = colonne_match(
        data = data_rsu$tranche_act_soc,
        vecteurs_mots =  c("Catégorie B", "CESU"),
        somme = F
      )
      cat_C_CESU_act_soc = colonne_match(
        data = data_rsu$tranche_act_soc,
        vecteurs_mots =  c("Catégorie C", "CESU"),
        somme = F
      )
      
      cat_A_creche_act_soc = colonne_match(
        data = data_rsu$tranche_act_soc,
        vecteurs_mots =  c("Catégorie A", "Place", "crèche"),
        somme = F
      )
      cat_B_creche_act_soc = colonne_match(
        data = data_rsu$tranche_act_soc,
        vecteurs_mots =  c("Catégorie B", "Place", "crèche"),
        somme = F
      )
      cat_C_creche_act_soc = colonne_match(
        data = data_rsu$tranche_act_soc,
        vecteurs_mots =  c("Catégorie C", "Place", "crèche"),
        somme = F
      )
      
      cat_A_act_para_act_soc = colonne_match(
        data = data_rsu$tranche_act_soc,
        vecteurs_mots =  c("Catégorie A", "Activité", "parascolaire"),
        somme = F
      )
      cat_B_act_para_act_soc = colonne_match(
        data = data_rsu$tranche_act_soc,
        vecteurs_mots =  c("Catégorie B", "Activité", "parascolaire"),
        somme = F
      )
      cat_C_act_para_act_soc = colonne_match(
        data = data_rsu$tranche_act_soc,
        vecteurs_mots =  c("Catégorie C", "Activité", "parascolaire"),
        somme = F
      )
      
      cat_A_logement_act_soc = colonne_match(
        data = data_rsu$tranche_act_soc,
        vecteurs_mots =  c("Catégorie A", "Logement"),
        somme = F
      )
      cat_B_logement_act_soc = colonne_match(
        data = data_rsu$tranche_act_soc,
        vecteurs_mots =  c("Catégorie B", "Logement"),
        somme = F
      )
      cat_C_logement_act_soc = colonne_match(
        data = data_rsu$tranche_act_soc,
        vecteurs_mots =  c("Catégorie C", "Logement"),
        somme = F
      )
      
      
      cat_A_resto_act_soc = colonne_match(
        data = data_rsu$tranche_act_soc,
        vecteurs_mots =  c("Catégorie A", "Restauration"),
        somme = F
      )
      cat_B_resto_act_soc = colonne_match(
        data = data_rsu$tranche_act_soc,
        vecteurs_mots =  c("Catégorie B", "Restauration"),
        somme = F
      )
      cat_C_resto_act_soc = colonne_match(
        data = data_rsu$tranche_act_soc,
        vecteurs_mots =  c("Catégorie C", "Restauration"),
        somme = F
      )
      
      
      cat_A_aide_finance_act_soc = colonne_match(
        data = data_rsu$tranche_act_soc,
        vecteurs_mots =  c("Catégorie A", "Aide", "financière"),
        somme = F
      )
      cat_B_aide_finance_act_soc = colonne_match(
        data = data_rsu$tranche_act_soc,
        vecteurs_mots =  c("Catégorie B", "Aide", "financière"),
        somme = F
      )
      cat_C_aide_finance_act_soc = colonne_match(
        data = data_rsu$tranche_act_soc,
        vecteurs_mots =  c("Catégorie C", "Aide", "financière"),
        somme = F
      )
      
      cat_A_loisir_act_soc = colonne_match(
        data = data_rsu$tranche_act_soc,
        vecteurs_mots =  c("Catégorie A", "Loisirs"),
        somme = F
      )
      cat_B_loisir_act_soc = colonne_match(
        data = data_rsu$tranche_act_soc,
        vecteurs_mots =  c("Catégorie B", "Loisirs"),
        somme = F
      )
      cat_C_loisir_act_soc = colonne_match(
        data = data_rsu$tranche_act_soc,
        vecteurs_mots =  c("Catégorie C", "Loisirs"),
        somme = F
      )
      
      
      cat_A_autre_act_soc = colonne_match(
        data = data_rsu$tranche_act_soc,
        vecteurs_mots =  c("Catégorie A", "Autre"),
        somme = F
      )
      cat_B_autre_act_soc = colonne_match(
        data = data_rsu$tranche_act_soc,
        vecteurs_mots =  c("Catégorie B", "Autre"),
        somme = F
      )
      cat_C_autre_act_soc = colonne_match(
        data = data_rsu$tranche_act_soc,
        vecteurs_mots =  c("Catégorie C", "Autre"),
        somme = F
      )
      
      # Conception des objets tableau par défauts
      
      tables_act_soc_defaut = reactive({
        tryCatch({
          switch(
            input$vent_crois_act_soc,
            "Agents bénéficiaires de prestations sociales: par type de prestation" = data.frame(
              row.names = c(
                "Garde d'enfant",
                "ACESU",
                "Place en crèche",
                "Activité parascolaire",
                "Logement",
                "Restauration",
                "Aides financière",
                "Loisirs",
                "Autres"
              ),
              "Effectif" = c(
                sum(
                  cat_A_grd_enfant_act_soc + cat_B_grd_enfant_act_soc + cat_C_grd_enfant_act_soc
                ),
                sum(
                  cat_A_CESU_act_soc + cat_B_CESU_act_soc + cat_C_CESU_act_soc
                ),
                sum(
                  cat_A_creche_act_soc + cat_B_creche_act_soc + cat_C_creche_act_soc
                ),
                sum(
                  cat_A_act_para_act_soc + cat_B_act_para_act_soc + cat_C_act_para_act_soc
                ),
                sum(
                  cat_A_logement_act_soc + cat_B_logement_act_soc + cat_C_logement_act_soc
                ),
                sum(
                  cat_A_resto_act_soc + cat_B_resto_act_soc + cat_C_resto_act_soc
                ),
                sum(
                  cat_A_aide_finance_act_soc + cat_B_aide_finance_act_soc + cat_C_aide_finance_act_soc
                ),
                sum(
                  cat_A_loisir_act_soc + cat_B_loisir_act_soc + cat_C_loisir_act_soc
                ),
                sum(
                  cat_A_autre_act_soc + cat_B_autre_act_soc + cat_C_autre_act_soc
                )
              )
            ) %>%
              bind_rows(
                data.frame(row.names = "Ensemble", "Effectif" = colSums(.)[1])
              ),
            
            "Agents bénéficiaires de prestations sociales: type de prestation & genre" = data.frame(
              row.names = c(
                "Garde d'enfant",
                "ACESU",
                "Place en crèche",
                "Activité parascolaire",
                "Logement",
                "Restauration",
                "Aides financière",
                "Loisirs",
                "Autres"
              ),
              "Femmes" =
                c(
                  sum(
                    cat_A_grd_enfant_act_soc[1, 1],
                    cat_B_grd_enfant_act_soc[1, 1],
                    cat_C_grd_enfant_act_soc[1, 1]
                  ),
                  sum(
                    cat_A_CESU_act_soc[1, 1],
                    cat_B_CESU_act_soc[1, 1],
                    cat_C_CESU_act_soc[1, 1]
                  ),
                  sum(
                    cat_A_creche_act_soc[1, 1],
                    cat_B_creche_act_soc[1, 1],
                    cat_C_creche_act_soc[1, 1]
                  ),
                  sum(
                    cat_A_act_para_act_soc[1, 1],
                    cat_B_act_para_act_soc[1, 1],
                    cat_C_act_para_act_soc[1, 1]
                  ),
                  sum(
                    cat_A_logement_act_soc[1, 1],
                    cat_B_logement_act_soc[1, 1],
                    cat_C_logement_act_soc[1, 1]
                  ),
                  sum(
                    cat_A_resto_act_soc[1, 1],
                    cat_B_resto_act_soc[1, 1],
                    cat_C_resto_act_soc[1, 1]
                  ),
                  sum(
                    cat_A_aide_finance_act_soc[1, 1],
                    cat_B_aide_finance_act_soc[1, 1],
                    cat_C_aide_finance_act_soc[1, 1]
                  ),
                  sum(
                    cat_A_loisir_act_soc[1, 1],
                    cat_B_loisir_act_soc[1, 1],
                    cat_C_loisir_act_soc[1, 1]
                  ),
                  sum(
                    cat_A_autre_act_soc[1, 1],
                    cat_B_autre_act_soc[1, 1],
                    cat_C_autre_act_soc[1, 1]
                  )
                ),
              
              "Hommes" = c(
                sum(
                  cat_A_grd_enfant_act_soc[1, 2],
                  cat_B_grd_enfant_act_soc[1, 2],
                  cat_C_grd_enfant_act_soc[1, 2]
                ),
                sum(
                  cat_A_CESU_act_soc[1, 2],
                  cat_B_CESU_act_soc[1, 2],
                  cat_C_CESU_act_soc[1, 2]
                ),
                sum(
                  cat_A_creche_act_soc[1, 2],
                  cat_B_creche_act_soc[1, 2],
                  cat_C_creche_act_soc[1, 2]
                ),
                sum(
                  cat_A_act_para_act_soc[1, 2],
                  cat_B_act_para_act_soc[1, 2],
                  cat_C_act_para_act_soc[1, 2]
                ),
                sum(
                  cat_A_logement_act_soc[1, 2],
                  cat_B_logement_act_soc[1, 2],
                  cat_C_logement_act_soc[1, 2]
                ),
                sum(
                  cat_A_resto_act_soc[1, 2],
                  cat_B_resto_act_soc[1, 2],
                  cat_C_resto_act_soc[1, 2]
                ),
                sum(
                  cat_A_aide_finance_act_soc[1, 2],
                  cat_B_aide_finance_act_soc[1, 2],
                  cat_C_aide_finance_act_soc[1, 2]
                ),
                sum(
                  cat_A_loisir_act_soc[1, 2],
                  cat_B_loisir_act_soc[1, 2],
                  cat_C_loisir_act_soc[1, 2]
                ),
                sum(
                  cat_A_autre_act_soc[1, 2],
                  cat_B_autre_act_soc[1, 2],
                  cat_C_autre_act_soc[1, 2]
                )
              )
            ) %>%
              bind_rows(
                data.frame(
                  row.names = "Ensemble",
                  Femmes = colSums(.)[1],
                  Hommes = colSums(.)[2]
                )
              ) %>% mutate(Total =  rowSums(.)),
            
            "Agents bénéficiaires de prestations sociales: catégorie & genre" = data.frame(
              row.names = c("Catégorie A", "Catégorie B", "Catégorie C"),
              "Femmes" =
                c(
                  sum(
                    cat_A_grd_enfant_act_soc[1, 1],
                    cat_A_CESU_act_soc[1, 1],
                    cat_A_creche_act_soc[1, 1],
                    cat_A_act_para_act_soc[1, 1],
                    cat_A_logement_act_soc[1, 1],
                    cat_A_resto_act_soc[1, 1],
                    cat_A_aide_finance_act_soc[1, 1],
                    cat_A_loisir_act_soc[1, 1],
                    cat_A_autre_act_soc[1, 1]
                  ),
                  
                  sum(
                    cat_B_grd_enfant_act_soc[1, 1],
                    cat_B_CESU_act_soc[1, 1],
                    cat_B_creche_act_soc[1, 1],
                    cat_B_act_para_act_soc[1, 1],
                    cat_B_logement_act_soc[1, 1],
                    cat_B_resto_act_soc[1, 1],
                    cat_B_aide_finance_act_soc[1, 1],
                    cat_B_loisir_act_soc[1, 1],
                    cat_B_autre_act_soc[1, 1]
                  ),
                  
                  sum(
                    cat_C_grd_enfant_act_soc[1, 1],
                    cat_C_CESU_act_soc[1, 1],
                    cat_C_creche_act_soc[1, 1],
                    cat_C_act_para_act_soc[1, 1],
                    cat_C_logement_act_soc[1, 1],
                    cat_C_resto_act_soc[1, 1],
                    cat_C_aide_finance_act_soc[1, 1],
                    cat_C_loisir_act_soc[1, 1],
                    cat_C_autre_act_soc[1, 1]
                  )
                ),
              
              "Hommes" = c(
                sum(
                  cat_A_grd_enfant_act_soc[1, 2],
                  cat_A_CESU_act_soc[1, 2],
                  cat_A_creche_act_soc[1, 2],
                  cat_A_act_para_act_soc[1, 2],
                  cat_A_logement_act_soc[1, 2],
                  cat_A_resto_act_soc[1, 2],
                  cat_A_aide_finance_act_soc[1, 2],
                  cat_A_loisir_act_soc[1, 2],
                  cat_A_autre_act_soc[1, 2]
                ),
                
                sum(
                  cat_B_grd_enfant_act_soc[1, 2],
                  cat_B_CESU_act_soc[1, 2],
                  cat_B_creche_act_soc[1, 2],
                  cat_B_act_para_act_soc[1, 2],
                  cat_B_logement_act_soc[1, 2],
                  cat_B_resto_act_soc[1, 2],
                  cat_B_aide_finance_act_soc[1, 2],
                  cat_B_loisir_act_soc[1, 2],
                  cat_B_autre_act_soc[1, 2]
                ),
                
                sum(
                  cat_C_grd_enfant_act_soc[1, 2],
                  cat_C_CESU_act_soc[1, 2],
                  cat_C_creche_act_soc[1, 2],
                  cat_C_act_para_act_soc[1, 2],
                  cat_C_logement_act_soc[1, 2],
                  cat_C_resto_act_soc[1, 2],
                  cat_C_aide_finance_act_soc[1, 2],
                  cat_C_loisir_act_soc[1, 2],
                  cat_C_autre_act_soc[1, 2]
                )
              )
            ) %>%
              bind_rows(
                data.frame(
                  row.names = "Ensemble",
                  Femmes = colSums(.)[1],
                  Hommes = colSums(.)[2]
                )
              ) %>% mutate(Total =  rowSums(.)),
            
            
            "Agents bénéficiaires de prestations sociales: type de prestation & catégorie" = data.frame(
              row.names = c(
                "Garde d'enfant",
                "ACESU",
                "Place en crèche",
                "Activité parascolaire",
                "Logement",
                "Restauration",
                "Aides financière",
                "Loisirs",
                "Autres"
              ),
              "Catégorie_A" =
                c(
                  sum(cat_A_grd_enfant_act_soc),
                  sum(cat_A_CESU_act_soc),
                  sum(cat_A_creche_act_soc),
                  sum(cat_A_act_para_act_soc),
                  sum(cat_A_logement_act_soc),
                  sum(cat_A_resto_act_soc),
                  sum(cat_A_aide_finance_act_soc),
                  sum(cat_A_loisir_act_soc),
                  sum(cat_A_autre_act_soc)
                ),
              
              "Catégorie_B" = c(
                sum(cat_B_grd_enfant_act_soc),
                sum(cat_B_CESU_act_soc),
                sum(cat_B_creche_act_soc),
                sum(cat_B_act_para_act_soc),
                sum(cat_B_logement_act_soc),
                sum(cat_B_resto_act_soc),
                sum(cat_B_aide_finance_act_soc),
                sum(cat_B_loisir_act_soc),
                sum(cat_B_autre_act_soc)
              ),
              
              "Catégorie_C" = c(
                sum(cat_C_grd_enfant_act_soc),
                sum(cat_C_CESU_act_soc),
                sum(cat_C_creche_act_soc),
                sum(cat_C_act_para_act_soc),
                sum(cat_C_logement_act_soc),
                sum(cat_C_resto_act_soc),
                sum(cat_C_aide_finance_act_soc),
                sum(cat_C_loisir_act_soc),
                sum(cat_C_autre_act_soc)
              )
            ) %>%
              bind_rows(
                data.frame(
                  row.names = "Ensemble",
                  "Catégorie_A" = colSums(.)[1],
                  "Catégorie_B" = colSums(.)[2],
                  "Catégorie_C" = colSums(.)[3]
                )
              ) %>% mutate(Total =  rowSums(.)),
            
            "Réponses qualitatives" = data_rsu$tranche_act_soc %>% select_if( ~ is.character(.) &
                                                                                !(is.na(.))) %>% t() %>%
              data.frame() %>% rename(Reponse = names(.))
            
          )
        }, error = function(e) {
          showNotification(
            "Pas de données disponible pour cette tranche!!!",
            type = "warning",
            duration = 5
          )
        })
        
      })
      
      
      output$vent_crois_act_soc = DT::renderDT({
        #Affichage de la tranche de table si l'utilisateur coche la case "Tableau ... défaut"
        
        tryCatch({
          if (is.null(input$vent_crois_btn_act_soc) != T &
              input$sous_theme_act_soc != "Informations supplémentaires") {
            table_classic(ma_table = tables_act_soc_defaut())
          } else{
            if (input$vent_crois_btn_act_soc != T) {
              table_manuelle(ma_table = tables_act_soc_defaut())
            }
          }
          
        }, error = function(e) {
          
        })
        
      })
      
      
      
      ##---------------------> Fin de conception des tableaux par défauts de l'action social
      
      
      ##-----------------------------------> Affichage graphique de action sociale
      
      plot_act_soc_dataset = reactive({
        if (input$vent_crois_act_soc == "Agents bénéficiaires de prestations sociales: par type de prestation") {
          if (input$plot_act_soc == "Histogramme superposé (tableau croisé uniquement)") {
            showNotification(
              "NB: L'histogramme superposé n'offre pas une visualisation adapté aux tableaux ventilés !!!",
              type = "error",
              duration = 5
            )
          }
          
          
          data_rsu$tables_act_soc_hist = tables_act_soc_defaut() %>% slice(-nrow(.)) %>% mutate(value = Effectif, names = rownames(.)) %>% filter(Effectif !=
                                                                                                                                                    0)    # Création de la table à utilisé pour représenter l'histogramme conformément aux attendus de plotly
          data_rsu$tables_act_soc_cam = tables_act_soc_defaut() %>%  slice(-nrow(.)) %>% mutate(value = Effectif, names = rownames(.)) %>% filter(Effectif !=
                                                                                                                                                    0)   # Création de la table à utilisé pour représenter le camembert conformément aux attendus de plotly
          
          data_rsu$val_type_plot_act_soc_1 = input$val_type_plot_act_soc_1
          
          return(switch(
            input$plot_act_soc,
            "Camembert" = cam(
              titre = input$plot_title_act_soc_1,
              ma_data = data_rsu$tables_act_soc_cam,
              type_aff = data_rsu$val_type_plot_act_soc_1,
              couleur = input$colors_plot_act_soc_1
            ),
            
            "Histogramme" = histo(
              titre = input$plot_title_act_soc_1,
              ma_data = data_rsu$tables_act_soc_hist,
              type_aff =  data_rsu$val_type_plot_act_soc_1,
              couleur = input$colors_plot_act_soc_1,
              axe_1 = input$plot_axe_act_soc_1_1,
              axe_2 = input$plot_axe_act_soc_1_2
            )
          ))
        } else{
          if (input$vent_crois_act_soc == "Agents bénéficiaires de prestations sociales: type de prestation & catégorie") {
            if (input$plot_act_soc == "Camembert") {
              showNotification(
                "NB: Le camembert n'offre pas une visualisation adapté aux tableaux croisés !!!",
                type = "error",
                duration = 5
              )
            }
            
            data_rsu$tables_act_soc_hist = tables_act_soc_defaut() %>% slice(-nrow(.)) %>% mutate(names = rownames(.)) %>% filter(Total !=
                                                                                                                                    0)
            names(data_rsu$tables_act_soc_hist) = gsub("\\.", " ", names(data_rsu$tables_act_soc_hist))
            
            data_rsu$val_type_plot_act_soc_1 = input$val_type_plot_act_soc_1
            
            return(switch(
              input$plot_act_soc,
              "Histogramme" = histo_crois_3(
                titre = input$plot_title_act_soc_1,
                ma_data = data_rsu$tables_act_soc_hist,
                type_aff =  data_rsu$val_type_plot_act_soc_1,
                couleur = input$colors_plot_act_soc_1,
                axe_1 = input$plot_axe_act_soc_1_1,
                axe_2 = input$plot_axe_act_soc_1_2
              ),
              
              "Histogramme superposé (tableau croisé uniquement)" = histo_crois_3(
                titre = input$plot_title_act_soc_1,
                ma_data = data_rsu$tables_act_soc_hist,
                type_aff =  data_rsu$val_type_plot_act_soc_1,
                couleur = input$colors_plot_act_soc_1,
                axe_1 = input$plot_axe_act_soc_1_1,
                axe_2 = input$plot_axe_act_soc_1_2,
                sup = 'stack'
              )
            ))
          } else{
            if (input$plot_act_soc == "Camembert") {
              showNotification(
                "NB: Le camembert n'offre pas une visualisation adapté aux tableaux croisés !!!",
                type = "error",
                duration = 5
              )
            }
            
            data_rsu$tables_act_soc_hist = tables_act_soc_defaut() %>% slice(-nrow(.)) %>% mutate(names = rownames(.)) %>% filter(Total !=
                                                                                                                                    0)
            names(data_rsu$tables_act_soc_hist) = gsub("\\.", " ", names(data_rsu$tables_act_soc_hist))
            
            data_rsu$val_type_plot_act_soc_1 = input$val_type_plot_act_soc_1
            
            return(switch(
              input$plot_act_soc,
              "Histogramme" = histo_crois_2(
                titre = input$plot_title_act_soc_1,
                ma_data = data_rsu$tables_act_soc_hist,
                type_aff =  data_rsu$val_type_plot_act_soc_1,
                couleur = input$colors_plot_act_soc_1,
                axe_1 = input$plot_axe_act_soc_1_1,
                axe_2 = input$plot_axe_act_soc_1_2
              ),
              
              "Histogramme superposé (tableau croisé uniquement)" = histo_crois_2(
                titre = input$plot_title_act_soc_1,
                ma_data = data_rsu$tables_act_soc_hist,
                type_aff =  data_rsu$val_type_plot_act_soc_1,
                couleur = input$colors_plot_act_soc_1,
                axe_1 = input$plot_axe_act_soc_1_1,
                axe_2 = input$plot_axe_act_soc_1_2,
                sup = 'stack'
              )
            ))
          }
        }
        
      })
      
      
      
      
      #------> Sorties affichage des graphiques
      output$plot_vent_crois_act_soc = renderPlotly({
        if (is.null(input$vent_crois_btn_act_soc) != T) {
          tryCatch({
            plot_act_soc_dataset()
          }, error = function(e) {
            
          })
        }
      })
      
      
      
    }, error = function(e) {
      showNotification("Valider d'abord votre partition",
                       type = "error",
                       duration = 5)
    })
    
    
  })
  
  ## -------------------------------------------Fin code de conception des tables & graphes par défaut de action sociale---------------------------------------------------
  
  
  
  
  
  ## ------------------------------------------Code de conception des tables & graphes manuelles de l'action sociale---------------------------------------------------
  
  output$manuelle_vent_act_soc = DT::renderDT({
    if (is.null(input$manuelle_btn_act_soc) != T) {
      # Affichage de la table si la case  " Tableau composé manuellement" est coché
      
      tryCatch({
        tranche_manuelle_act_soc = data_rsu$tranche_act_soc %>%    # Constitution de la table issue de la séléction manuelle
          select(input$select_manuelle_vent_act_soc) %>% t() %>%
          data.frame() %>%
          rename(Effectif = names(.))
        
        table_manuelle(ma_table = tranche_manuelle_act_soc)
        
      }, error = function(e) {
        
      })
    }
  })
  
  ##--------> représentation des tableaux ventilés
  
  plot_act_soc_manuelle_dataset = reactive({
    tranche_manuelle_act_soc = data_rsu$tranche_act_soc %>%
      select(input$select_manuelle_vent_act_soc) %>% t() %>% data.frame() %>%
      rename(Effectif = names(.)) %>%
      mutate(names = gsub("\\.", " ", sub("[0-9]*- ", "", rownames(.))))
    
    tranche_manuelle_act_soc_plot = tranche_manuelle_act_soc %>% filter(Effectif !=
                                                                          0)
    
    #--------> fonction switch permettant d'avoir des rendu au relatif aux choix graphiques faites par l'utilisateur
    observeEvent(input$select_manuelle_vent_act_soc, {
      updateTextInput(session, "legend_plot_act_soc", value = paste(gsub(
        "\\.", " ", sub("[0-9]*- ", "", rownames(tranche_manuelle_act_soc_plot))
      ), sep = ",")) # mise à jour des colonnes à séléctionner pour constitué les légendes
    })
    
    rownames(tranche_manuelle_act_soc_plot) = input$legend_plot_act_soc %>% strsplit(., split = ",") %>% unlist() %>% as.vector() # Mise à jour de la légende à travers le nom des lignes
    data_rsu$val_type_plot_act_soc_2 = input$val_type_plot_act_soc_2
    
    
    if (is.null(c(
      input$select_manuelle_vent_act_soc,
      input$manuelle_btn_act_soc
    )) != T   &
    input$plot_act_soc == "Histogramme superposé (tableau croisé uniquement)") {
      showNotification(
        "NB: L'histogramme superposé n'offre pas une visualisation adapté aux tableaux ventilés !!!",
        type = "error",
        duration = 5
      )
    }
    
    switch(
      input$plot_act_soc,
      "Camembert" = cam(
        titre = input$plot_title_act_soc_2,
        ma_data = tranche_manuelle_act_soc_plot,
        type_aff = data_rsu$val_type_plot_act_soc_2,
        couleur = input$colors_plot_act_soc_2,
        noms_legende = input$legend_plot_act_soc %>% strsplit(., split = ",") %>% unlist() %>% as.vector()
      ),
      "Histogramme" = histo(
        titre = input$plot_title_act_soc_2,
        ma_data = tranche_manuelle_act_soc_plot,
        type_aff = data_rsu$val_type_plot_act_soc_2,
        couleur = input$colors_plot_act_soc_2,
        noms_legende = input$legend_plot_act_soc %>% strsplit(., split = ",") %>% unlist() %>% as.vector()
        ,
        axe_1 = input$plot_axe_act_soc_2_1,
        axe_2 = input$plot_axe_act_soc_2_2
      )
      
    )
  })
  
  #--------->  Graphiques associés aux tables manuelles
  
  output$plot_act_soc_manuelle =  renderPlotly({
    if (is.null(input$manuelle_btn_act_soc) != T) {
      tryCatch({
        plot_act_soc_manuelle_dataset()
      }, error = function(e) {
        
      })
    }
  })
  
  ## -------------------------------------------Fin Code de conception des tables & graphes manuelles  de l'action sociale---------------------------------------------------
  
  
  
  ## -------------------------------------------Code de conception des tables & graphes par défaut  dialogue social---------------------------------------------------
  
  tranche_dial_soc_dataset = reactive({
    switch(
      input$tranche_select_dial_soc,
      "Tranche 1" = c("A- Grève", "Informations supplémentaires"),
      
      "Tranche 2" = c(
        "B- Organismes consultatifs",
        "C- Formation des membres de la FS",
        "D- Activité de la FS hors réunions plénière de l'instance: les enquêtes menées",
        "E- Consultation de la FS pour des projets au cours de l'AC*",
        "Informations supplémentaires"
      ),
      
      "Tranche 3" = c(
        "F- Formation Spécialisé (FS): recours à un expert certifié, avis rendus et mesures proposées",
        "Informations supplémentaires"
      )
      
    )
  })
  
  
  observeEvent(input$tranche_select_dial_soc, {
    updateSelectInput(session, "sous_theme_dial_soc", choices = tranche_dial_soc_dataset())
  })
  
  
  sous_theme_dial_soc_dataset = reactive({
    #Initialisation des sous-thèmes abordés
    switch(
      input$sous_theme_dial_soc,
      
      "A- Grève" = c(
        "Mot d'ordre national & local: par nombre de jours non travaillés"
      ),
      
      "B- Organismes consultatifs" = c(
        "Représentants du personnel de la FS au 31 décembre de l'AC: par fonction",
        "Actions engagées par les instances au cours de l'AC*: action & instance",
        "Actions de la Formation Spécialisée (FS) au cours de l'AC*",
        "Saisines de la FS par le Comité Social: par type initiateur",
        "Représentants du personnel des instances au 31 décembre de l'AC: fonction & instance",
        "Représentants du personnel de la CS au 31 décembre de l'AC: fonction & genre",
        "Représentants du personnel de la FS au 31 décembre de l'AC: fonction & genre",
        "Réunions de la FS (hors période de circonstance exceptionnelle) tenues l'AC*: par motifs",
        "Réunions de la FS sans représentants du personnel (y compris les annulations faute de quorum): par type de participant"
      ),
      
      "C- Formation des membres de la FS" = c(
        "Membres de la FS en fonction au 31 décembre de  l'AC*: état & type de formation",
        "Membres de la FS en fonction au 31 décembre de  l'AC*: état de la formation & genre",
        "Membres de la FS en fonction au 31 décembre de  l'AC* ayant bénéficié d'une formation dans son intégralité:  type de formation & genre",
        "Membres de la FS en fonction au 31 décembre de  l'AC* ayant bénéficié d'une formation dans son intégralité:  type de formation & Nbr de jours"
      ),
      
      "D- Activité de la FS hors réunions plénière de l'instance: les enquêtes menées" = c(
        "Enquêtes menées suite à un AT* ou  MP*: AT*,MP* & état de l'enquête",
        "Groupes de travail en lien avec les travaux de la FS & Nbre de visites de site réaliséés  au cours de l'AC*"
      ),
      
      "E- Consultation de la FS pour des projets au cours de l'AC*" =
        c(
          "Projets aménagement et nouvelle technologie, règlement/consignes: type de projet & action de la FS"
        ),
      
      "F- Formation Spécialisé (FS): recours à un expert certifié, avis rendus et mesures proposées" = c(
        "Actions engagées par la FS au cours de l'AC*",
        "Actions engagées par la FS au cours de l'AC*: type d'action & décision de l'administration",
        "Mesures proposées par la FS au cours de l'AC*:par type de mesure",
        "Demandes de recours à une expertise & coût globale au cours de l'AC*"
      ),
      
      "Informations supplémentaires" = c("Réponses qualitatives")
      
      
    )
  })
  
  
  observe({
    updateSelectInput(session, "vent_crois_dial_soc", choices = sous_theme_dial_soc_dataset()) # mise à jour des colonnes à séléctionner pour sélectionner le sous thème étudier dans la santé et sécurité au travail
  })
  
  
  #Recherche des colonnes par recherche sur les mots clées
  
  observeEvent(input$vent_crois_btn_dial_soc, {
    tryCatch({
      mot_ordre_nat_tranche_1_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c("national", "jours", "non", "travaillé")
      )
      mot_ordre_loc_tranche_1_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c("local", "jours", "non", "travaillé")
      )
      
      #---Actioins engagés
      
      dmd_recours_tranche_3_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c("total", "demande", "recours", "formulé")
      )
      avs_rendu_tranche_3_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c("total", "d'avis", "rendu")
      )
      msr_prop_tranche_3_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c("total", "mesure", "proposé"),
        somme = F
      )
      
      cout_recours_tranche_3_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c("Coût", "global", "taxe")
      )
      
      #--------------les recours
      
      dmd_rcs_acpt_tranche_3_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c("total", "demande", "recours", "accepté")
      )
      dmd_rcs_rfs_tranche_3_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c("total", "demande", "recours", "refusé")
      )
      
      msr_prop_acpt_tranche_3_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c("total", "mesure", "accepté")
      )
      msr_prop_rfs_tranche_3_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c("total", "mesure", "refusé")
      )
      
      msr_prop_CMR_tranche_3_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c(
          "total",
          "mesure",
          "proposé",
          "substances",
          "cancérogène",
          "mutagène",
          "CMR"
        )
      )
      msr_prop_TMS_tranche_3_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c("total", "mesure", "proposé", "trouble", "musculo", "TMS")
      )
      msr_prop_RPS_tranche_3_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c(
          "total",
          "mesure",
          "proposé",
          "risque",
          "psychosociaux",
          "RPS"
        )
      )
      
      #--------------------Composition du CS/FS
      
      repr_perso_tit_f_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c(
          "représentant",
          "personnel",
          "titulaire",
          "féminin",
          "31 décembre"
        ),
        somme = F
      )
      repr_perso_tit_h_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c(
          "représentant",
          "personnel",
          "titulaire",
          "masculin",
          "31 décembre"
        ),
        somme = F
      )
      
      repr_perso_supl_f_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c(
          "représentant",
          "personnel",
          "suppléant",
          "féminin",
          "31 décembre"
        ),
        somme = F
      )
      repr_perso_supl_h_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c(
          "représentant",
          "personnel",
          "suppléant",
          "masculin",
          "31 décembre"
        ),
        somme = F
      )
      
      repr_perso_new_tot_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c(
          "représentant",
          "personnel",
          "F et H",
          "confondu",
          "nouvellement",
          "nommé"
        )
      )
      
      #----------------Réunions de la FS
      
      reun_president_FS_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c(
          "réunion",
          "proposition",
          "Président",
          "instance",
          "tenue"
        )
      )
      
      reun_representant_FS_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c(
          "réunion",
          "proposition",
          "représentant",
          "personnel",
          "tenue",
          "membre"
        )
      )
      
      reun_accd_grv_FS_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c("réunion", "accident", "grave", "tenue")
      )
      
      reun_divergeance_FS_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c(
          "réunion",
          "divergence",
          "signalement",
          "danger",
          "imminent",
          "tenue"
        )
      )
      
      
      
      #------------------------------------------
      
      tot_reun_FS_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c("total", "de réunion")
      )
      tot_conv_FS_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c("total", "convocation", "tenir")
      )
      tot_reconv_FS_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c(
          "total",
          "reconvocation",
          "tenir",
          "incluant",
          "faute",
          "quorum"
        )
      )
      tot_consult_subst_FS_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c("cas", "consultation", "substitué", "obligatoire")
      )
      
      #--------------------------------------------
      
      saisine_adm_FS_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c("Nombre", "saisine", "l'initiative", "l'administration")
      )
      
      saisine_mbr_FS_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c("Nombre", "saisine", "l'initiative", "membre")
      )
      
      ##------------------------------
      
      total_reun_CT_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c("CT_REUNION_NBRE")
      )
      total_reconv_CT_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c("CT_RECONVOC_NBRE")
      )
      
      #----------------------------Formation
      mbr_ay_ben_frmt_SST_3j_FS_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c(
          "membre",
          "ayant bénéficié",
          "3 jours",
          "intégral",
          "formation",
          "SST"
        ),
        somme = F
      )
      mbr_nay_ben_frmt_SST_3j_FS_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c(
          "membre",
          " pas bénéficié",
          "3 jours",
          "intégral",
          "formation",
          "SST"
        ),
        somme = F
      )
      
      mbr_ay_ben_frmt_SST_2j_FS_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c(
          "membre",
          "ayant bénéficié",
          "2 jours",
          "intégral",
          "formation",
          "SST"
        ),
        somme = F
      )
      mbr_nay_ben_frmt_SST_2j_FS_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c(
          "membre",
          " pas bénéficié",
          "2 jours",
          "intégral",
          "formation",
          "SST"
        ),
        somme = F
      )
      
      mbr_ay_ben_frmt_RPS_2j_FS_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c(
          "membre",
          "ayant bénéficié",
          "2 jours",
          "intégral",
          "formation",
          "RPS"
        ),
        somme = F
      )
      mbr_nay_ben_frmt_RPS_2j_FS_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c(
          "membre",
          " pas bénéficié",
          "2 jours",
          "intégral",
          "formation",
          "RPS"
        ),
        somme = F
      )
      
      
      #--------------------------------------- Accident & maladie pro
      
      acd_real_FS_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c("enquête", "accident", "service", "annéee", "concerné")
      )
      acd_rslt_FS_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c(
          "enquête",
          "accident",
          "résultat",
          "communiqué",
          "membre",
          "année",
          "concerné"
        )
      )
      
      mld_pro_real_FS_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c(
          "enquête",
          "maladie",
          "professionnelle",
          "annéee",
          "concerné"
        )
      )
      mld_pro_rslt_FS_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c(
          "enquête",
          "maladie",
          "professionnelle",
          "résultat",
          "communiqué",
          "membre",
          "année",
          "concerné"
        )
      )
      
      
      nbr_visites_FS_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c("CHSCT_VISITE_SITE_NBRE")
      )
      
      grp_trav_FS_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c("groupe", "travail", "lien", "travaux", "en place")
      )
      
      
      secret_prop_inscr_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c("CHSCT_SECR_PROP_ODJ")
      )
      
      #-------------------Autres participants réunion
      
      
      
      reun_inspect_ISST_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c(
          "réunion",
          "présence",
          "ISST",
          "Santé",
          "Sécurité",
          "Travail",
          "quorum"
        )
      )
      
      reun_cons_prev_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c(
          "réunion",
          "présence",
          "l'assistant",
          "conseiller",
          "prévention",
          "quorum"
        )
      )
      
      reun_mdcn_prev_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c(
          "réunion",
          "présence",
          "médecin",
          "prévention",
          "Sécurité",
          "quorum"
        )
      )
      
      
      #----------------------------------------Aménagement et nv tech
      
      
      amng_cons_CSS_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c(
          "projet",
          "d'aménagement",
          "modifiant",
          "condition",
          "santé",
          "sécurité",
          "consulté"
        )
      )
      
      amng_avis_CSS_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c(
          "projet",
          "d'aménagement",
          "modifiant",
          "condition",
          "santé",
          "sécurité",
          "avis",
          "l'objet"
        )
      )
      
      amng_cons_NT_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c(
          "projet",
          "d'aménagement",
          "d'introduction",
          "nouvelle",
          "technologie",
          "consulté"
        )
      )
      
      amng_avis_NT_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c(
          "projet",
          "d'aménagement",
          "d'introduction",
          "nouvelle",
          "technologie",
          "avis",
          "l'objet"
        )
      )
      
      
      #--------------------------------------------Règlement | consignes
      
      rglm_cons_NT_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c("projet", "règlement", "consigne", "consulté")
      )
      
      rglm_avis_NT_tranche_2_dial_soc = colonne_match(
        data = data_rsu$tranche_dial_soc,
        vecteurs_mots =  c("projet", "règlement", "consigne", "avis", "l'objet")
      )
      
      # Conception des objets tableau par défauts
      
      tables_dial_soc_defaut = reactive({
        switch(
          input$vent_crois_dial_soc,
          "Mot d'ordre national & local: par nombre de jours non travaillés" = data.frame(
            row.names = c("Echelle national", "Echelle local"),
            "Effectif" =
              c(
                mot_ordre_nat_tranche_1_dial_soc,
                mot_ordre_loc_tranche_1_dial_soc
              )
          ) %>%
            bind_rows(
              data.frame(row.names = "Ensemble", "Effectif" = colSums(.)[1])
            ),
          
          "Actions engagées par la FS au cours de l'AC*" = data.frame(
            row.names = c(
              "Demandes de recours",
              "Avis rendus",
              "Mesures proposées"
            ),
            "Effectif" = c(
              dmd_recours_tranche_3_dial_soc,
              avs_rendu_tranche_3_dial_soc,
              msr_prop_tranche_3_dial_soc[1, 1]
            )
          ) %>%
            bind_rows(
              data.frame(row.names = "Ensemble", "Effectif" = colSums(.)[1])
            ) ,
          
          "Actions engagées par la FS au cours de l'AC*: type d'action & décision de l'administration" = data.frame(
            row.names = c("Demandes de recours", "Mesures"),
            "Accepté" =
              c(
                dmd_rcs_acpt_tranche_3_dial_soc,
                msr_prop_acpt_tranche_3_dial_soc
              ),
            "Refusé" = c(
              dmd_rcs_rfs_tranche_3_dial_soc,
              msr_prop_rfs_tranche_3_dial_soc
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                "Accepté" = colSums(.)[1],
                "Refusé" = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "Mesures proposées par la FS au cours de l'AC*:par type de mesure" = data.frame(
            row.names = c("Mesures pour CMR", "Mesure pour TMS", "Mesure pour RPS"),
            "Effectif" =
              c(
                msr_prop_CMR_tranche_3_dial_soc,
                msr_prop_TMS_tranche_3_dial_soc,
                msr_prop_RPS_tranche_3_dial_soc
              )
          ) %>%
            bind_rows(
              data.frame(row.names = "Ensemble", "Effectif" = colSums(.)[1])
            ) ,
          
          "Demandes de recours à une expertise & coût globale au cours de l'AC*" = data.frame(
            row.names = c("Appréciation"),
            "Demandes_de_recours" =
              c(dmd_recours_tranche_3_dial_soc),
            "Coût_globale_hors_taxe" = c(paste(
              cout_recours_tranche_3_dial_soc, "€"
            ))
          ),
          
          "Représentants du personnel de la FS au 31 décembre de l'AC: fonction & genre" = data.frame(
            row.names = c("Titulaires", "Suppléants"),
            "Femmes" =
              c(
                repr_perso_tit_f_tranche_2_dial_soc[1, 2],
                repr_perso_supl_f_tranche_2_dial_soc[1, 2]
              ),
            "Hommes" = c(
              repr_perso_tit_h_tranche_2_dial_soc[1, 2],
              repr_perso_supl_h_tranche_2_dial_soc[1, 2]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "Représentants du personnel de la CS au 31 décembre de l'AC: fonction & genre" = data.frame(
            row.names = c("Titulaires", "Suppléants"),
            "Femmes" =
              c(
                repr_perso_tit_f_tranche_2_dial_soc[1, 1],
                repr_perso_supl_f_tranche_2_dial_soc[1, 1]
              ),
            "Hommes" = c(
              repr_perso_tit_h_tranche_2_dial_soc[1, 1],
              repr_perso_supl_h_tranche_2_dial_soc[1, 1]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          
          "Représentants du personnel des instances au 31 décembre de l'AC: fonction & instance" = data.frame(
            row.names = c("Titulaires", "Suppléants"),
            "CS" =
              c(
                repr_perso_tit_f_tranche_2_dial_soc[1, 1] + repr_perso_tit_h_tranche_2_dial_soc[1, 1],
                repr_perso_supl_f_tranche_2_dial_soc[1, 1] + repr_perso_supl_h_tranche_2_dial_soc[1, 1]
              ),
            
            "FS" = c(
              repr_perso_tit_f_tranche_2_dial_soc[1, 2] + repr_perso_tit_h_tranche_2_dial_soc[1, 2],
              repr_perso_supl_f_tranche_2_dial_soc[1, 2] + repr_perso_supl_h_tranche_2_dial_soc[1, 2]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                CS = colSums(.)[1],
                FS = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "Représentants du personnel de la FS au 31 décembre de l'AC: par fonction" =  data.frame(
            row.names = c(
              "Titulaires",
              "Suppléants",
              "Nouvellement nommés (toutes fonctions confondus)"
            ),
            "Effectif" = c(
              sum(
                repr_perso_tit_f_tranche_2_dial_soc  + repr_perso_tit_h_tranche_2_dial_soc
              ),
              sum(
                repr_perso_supl_f_tranche_2_dial_soc + repr_perso_supl_h_tranche_2_dial_soc
              ),
              repr_perso_new_tot_tranche_2_dial_soc
            )
          ),
          
          "Réunions de la FS (hors période de circonstance exceptionnelle) tenues l'AC*: par motifs" = data.frame(
            row.names = c(
              "Sur proposition de Président de l'instance ",
              "Sur proposition de représentants du personnel",
              "Suite à un accident grave",
              "Suite à une divergence d'appréciation sur un signalement d'un danger grave et imminent"
            ),
            "Effectif" = c(
              reun_president_FS_tranche_2_dial_soc,
              reun_representant_FS_tranche_2_dial_soc,
              reun_accd_grv_FS_tranche_2_dial_soc,
              reun_divergeance_FS_tranche_2_dial_soc
            )
          ) %>%
            bind_rows(
              data.frame(row.names = "Ensemble", "Effectif" = colSums(.)[1])
            ) ,
          
          "Actions de la Formation Spécialisée (FS) au cours de l'AC*" = data.frame(
            row.names = c(
              "Réunions",
              "Convocations",
              "Reconvocation",
              "Consultation du CS substituant la consultation obligatoire de la FS"
            ),
            "Effectif" = c(
              tot_reun_FS_tranche_2_dial_soc,
              tot_conv_FS_tranche_2_dial_soc,
              tot_reconv_FS_tranche_2_dial_soc,
              tot_consult_subst_FS_tranche_2_dial_soc
            )
          ),
          
          
          "Saisines de la FS par le Comité Social: par type initiateur" = data.frame(
            row.names = c(
              "Initiative de l'administration",
              "Initiative des membres du CS"
            ),
            "Effectif" = c(
              saisine_adm_FS_tranche_2_dial_soc,
              saisine_mbr_FS_tranche_2_dial_soc
            )
          ) %>%
            bind_rows(
              data.frame(row.names = "Ensemble", "Effectif" = colSums(.)[1])
            ) ,
          
          
          "Actions engagées par les instances au cours de l'AC*: action & instance" = data.frame(
            row.names = c("Réunions", "Reconvocation"),
            "CS" =
              c(
                total_reun_CT_tranche_2_dial_soc,
                total_reconv_CT_tranche_2_dial_soc
              ),
            
            "FS" = c(
              tot_reun_FS_tranche_2_dial_soc,
              tot_reconv_FS_tranche_2_dial_soc
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                CS = colSums(.)[1],
                FS = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "Membres de la FS en fonction au 31 décembre de  l'AC*: état & type de formation" =  data.frame(
            row.names = c("Ayant bénéficié", "N'ayant pas bénéficié"),
            "Formation_SST" =
              c(
                sum(
                  mbr_ay_ben_frmt_SST_3j_FS_tranche_2_dial_soc + mbr_ay_ben_frmt_SST_2j_FS_tranche_2_dial_soc
                ),
                sum(
                  mbr_nay_ben_frmt_SST_3j_FS_tranche_2_dial_soc + mbr_nay_ben_frmt_SST_2j_FS_tranche_2_dial_soc
                )
              ),
            
            "Formation_RPS" = c(
              sum(mbr_ay_ben_frmt_RPS_2j_FS_tranche_2_dial_soc),
              sum(mbr_nay_ben_frmt_RPS_2j_FS_tranche_2_dial_soc)
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Formation_SST = colSums(.)[1],
                Formation_RPS = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          
          "Membres de la FS en fonction au 31 décembre de  l'AC*: état de la formation & genre" = data.frame(
            row.names = c("Ayant bénéficié", "N'ayant pas bénéficié"),
            "Femmes" =
              c(
                sum(
                  mbr_ay_ben_frmt_SST_3j_FS_tranche_2_dial_soc[1, 1],
                  mbr_ay_ben_frmt_SST_2j_FS_tranche_2_dial_soc[1, 1],
                  mbr_ay_ben_frmt_RPS_2j_FS_tranche_2_dial_soc[1, 1]
                ),
                sum(
                  mbr_nay_ben_frmt_SST_3j_FS_tranche_2_dial_soc[1, 1],
                  mbr_nay_ben_frmt_SST_2j_FS_tranche_2_dial_soc[1, 1],
                  mbr_nay_ben_frmt_RPS_2j_FS_tranche_2_dial_soc[1, 1]
                )
              ),
            
            "Hommes" = c(
              sum(
                mbr_ay_ben_frmt_SST_3j_FS_tranche_2_dial_soc[1, 2],
                mbr_ay_ben_frmt_SST_2j_FS_tranche_2_dial_soc[1, 2],
                mbr_ay_ben_frmt_RPS_2j_FS_tranche_2_dial_soc[1, 2]
              ),
              sum(
                mbr_nay_ben_frmt_SST_3j_FS_tranche_2_dial_soc[1, 2],
                mbr_nay_ben_frmt_SST_2j_FS_tranche_2_dial_soc[1, 2],
                mbr_nay_ben_frmt_RPS_2j_FS_tranche_2_dial_soc[1, 2]
              )
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          
          
          
          "Membres de la FS en fonction au 31 décembre de  l'AC* ayant bénéficié d'une formation dans son intégralité:  type de formation & genre" =  data.frame(
            row.names = c("Formation SST", "Formation RPS"),
            "Femmes" =
              c(
                mbr_ay_ben_frmt_SST_3j_FS_tranche_2_dial_soc[1, 1] + mbr_ay_ben_frmt_SST_2j_FS_tranche_2_dial_soc[1, 1],
                mbr_ay_ben_frmt_RPS_2j_FS_tranche_2_dial_soc[1, 1]
              ),
            
            "Hommes" = c(
              mbr_ay_ben_frmt_SST_3j_FS_tranche_2_dial_soc[1, 2] + mbr_ay_ben_frmt_SST_2j_FS_tranche_2_dial_soc[1, 2],
              mbr_ay_ben_frmt_RPS_2j_FS_tranche_2_dial_soc[1, 2]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          
          
          "Membres de la FS en fonction au 31 décembre de  l'AC* ayant bénéficié d'une formation dans son intégralité:  type de formation & Nbr de jours" = data.frame(
            row.names = c("2 jours intégrales", "3 jours intégrales"),
            "Formation_SST" =
              c(
                sum(mbr_ay_ben_frmt_SST_2j_FS_tranche_2_dial_soc),
                sum(mbr_ay_ben_frmt_SST_3j_FS_tranche_2_dial_soc)
              ),
            
            "Formation_RPS" = c(
              sum(mbr_ay_ben_frmt_RPS_2j_FS_tranche_2_dial_soc),
              0
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Formation_SST = colSums(.)[1],
                Formation_RPS = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "Enquêtes menées suite à un AT* ou  MP*: AT*,MP* & état de l'enquête" = data.frame(
            row.names = c("AT*", "MP*"),
            "Enquêtes_réalisées" =
              c(
                acd_real_FS_tranche_2_dial_soc,
                mld_pro_real_FS_tranche_2_dial_soc
              ),
            
            "Résultats_Enquêtes" = c(
              acd_rslt_FS_tranche_2_dial_soc,
              mld_pro_rslt_FS_tranche_2_dial_soc
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                "Enquêtes_réalisées" = colSums(.)[1],
                "Résultats_Enquêtes" = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "Groupes de travail en lien avec les travaux de la FS & Nbre de visites de site réaliséés  au cours de l'AC*" = data.frame(
            row.names = c(
              "Visite de sites réalisées l'AC*",
              "Groupe de travail en lien avec les travaux de la FS l'AC*",
              "FS pour lequel le sécrétaire à proposé l'inscription de points à l'ordre du jour"
            ),
            "Effectif" = c(
              nbr_visites_FS_tranche_2_dial_soc,
              grp_trav_FS_tranche_2_dial_soc,
              secret_prop_inscr_tranche_2_dial_soc
            )
          ),
          
          
          
          
          
          "Réunions de la FS sans représentants du personnel (y compris les annulations faute de quorum): par type de participant" = data.frame(
            row.names = c("Présence de ISST", "Présence d'AP ou CP", "Présence de MP"),
            "Effectif" =
              c(
                reun_inspect_ISST_tranche_2_dial_soc,
                reun_cons_prev_tranche_2_dial_soc,
                reun_mdcn_prev_tranche_2_dial_soc
              )
          ) %>%
            bind_rows(
              data.frame(row.names = "Ensemble", "Effectif" = colSums(.)[1])
            ),
          
          
          
          "Projets aménagement et nouvelle technologie, règlement/consignes: type de projet & action de la FS" = data.frame(
            row.names = c("Consulté par la FS", "Avec avis de la FS"),
            "CSS_ou_CT" =
              c(
                amng_cons_CSS_tranche_2_dial_soc,
                amng_avis_CSS_tranche_2_dial_soc
              ),
            "INT" = c(
              amng_cons_NT_tranche_2_dial_soc,
              amng_avis_NT_tranche_2_dial_soc
            ),
            "Règlement_et_consigne" =
              c(
                rglm_cons_NT_tranche_2_dial_soc,
                rglm_avis_NT_tranche_2_dial_soc
              )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                "CSS_ou_CT" = colSums(.)[1],
                "INT" = colSums(.)[2],
                "Règlement_et_consigne" = colSums(.)[3]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "Réponses qualitatives" =  data_rsu$tranche_dial_soc %>% select_if( ~ is.character(.) &
                                                                                !(is.na(.))) %>% t() %>%
            data.frame() %>% rename(Reponse = names(.))
        )
      })
      
      #------------------->Création des objets représentant la table datatable
      
      output$vent_crois_dial_soc = DT::renderDT({
        #Affichage de la tranche de table si l'utilisateur coche la case "Tableau ... défaut"
        tryCatch({
          if (is.null(input$vent_crois_btn_dial_soc) != TRUE &
              input$sous_theme_dial_soc != "Informations supplémentaires") {
            table_classic(ma_table = tables_dial_soc_defaut())
          } else{
            if (is.null(input$vent_crois_btn_dial_soc) != TRUE) {
              table_manuelle(ma_table = tables_dial_soc_defaut())
            }
          }
        }, error = function(e) {
          
        })
      })
      
      
      ##---------------------> Fin de conception des tableaux par défauts du dialogue social
      
      
      
      ##-----------------------------------> Affichage graphique santé et sécurité au travail
      
      plot_dial_soc_dataset = reactive({
        if (input$vent_crois_dial_soc %in% c(
          "Mot d'ordre national & local: par nombre de jours non travaillés",
          "Représentants du personnel de la FS au 31 décembre de l'AC: par fonction",
          "Actions de la Formation Spécialisée (FS) au cours de l'AC*",
          "Saisines de la FS par le Comité Social: par type initiateur",
          "Réunions de la FS (hors période de circonstance exceptionnelle) tenues l'AC*: par motifs",
          "Réunions de la FS sans représentants du personnel (y compris les annulations faute de quorum): par type de participant",
          "Groupes de travail en lien avec les travaux de la FS & Nbre de visites de site réaliséés au cours de l'AC*",
          "Actions engagées par la FS au cours de l'AC*",
          "Mesures proposées par la FS au cours de l'AC*:par type de mesure"
        )) {
          if (input$plot_dial_soc == "Histogramme superposé (tableau croisé uniquement)") {
            showNotification(
              "NB: L'histogramme superposé n'offre pas une visualisation adapté aux tableaux ventilés !!!",
              type = "error",
              duration = 5
            )
          }
          
          data_rsu$tables_dial_soc_hist = tables_dial_soc_defaut() %>% slice(-nrow(.)) %>% mutate(value = Effectif, names = rownames(.)) %>% filter(Effectif !=
                                                                                                                                                      0)    # Création de la table à utilisé pour représenter l'histogramme conformément aux attendus de plotly
          data_rsu$tables_dial_soc_cam = tables_dial_soc_defaut() %>%  slice(-nrow(.)) %>% mutate(value = Effectif, names = rownames(.)) %>% filter(Effectif !=
                                                                                                                                                      0)   # Création de la table à utilisé pour représenter le camembert conformément aux attendus de plotly
          
          data_rsu$val_type_plot_dial_soc_1 = input$val_type_plot_dial_soc_1
          
          return(switch(
            input$plot_dial_soc,
            "Camembert" = cam(
              titre = input$plot_title_dial_soc_1,
              ma_data = data_rsu$tables_dial_soc_cam,
              type_aff = data_rsu$val_type_plot_dial_soc_1,
              couleur = input$colors_plot_dial_soc_1
            ),
            
            "Histogramme" = histo(
              titre = input$plot_title_dial_soc_1,
              ma_data = data_rsu$tables_dial_soc_hist,
              type_aff =  data_rsu$val_type_plot_dial_soc_1,
              couleur = input$colors_plot_dial_soc_1,
              axe_1 = input$plot_axe_dial_soc_1_1,
              axe_2 = input$plot_axe_dial_soc_1_2
            )
          ))
        } else{
          if (input$vent_crois_dial_soc == "Projets aménagement et nouvelle technologie, règlement/consignes: type de projet & action de la FS") {
            if (input$plot_dial_soc == "Camembert") {
              showNotification(
                "NB: Le camembert n'offre pas une visualisation adapté aux tableaux croisés !!!",
                type = "error",
                duration = 5
              )
            }
            
            data_rsu$tables_dial_soc_hist = tables_dial_soc_defaut() %>% slice(-nrow(.)) %>% mutate(names = rownames(.)) %>% filter(Total !=
                                                                                                                                      0)
            names(data_rsu$tables_dial_soc_hist) = gsub("\\.", " ", names(data_rsu$tables_dial_soc_hist))
            
            data_rsu$val_type_plot_dial_soc_1 = input$val_type_plot_dial_soc_1
            
            return(switch(
              input$plot_dial_soc,
              "Histogramme" = histo_crois_3(
                titre = input$plot_title_dial_soc_1,
                ma_data = data_rsu$tables_dial_soc_hist,
                type_aff =  data_rsu$val_type_plot_dial_soc_1,
                couleur = input$colors_plot_dial_soc_1,
                axe_1 = input$plot_axe_dial_soc_1_1,
                axe_2 = input$plot_axe_dial_soc_1_2
              ),
              
              "Histogramme superposé (tableau croisé uniquement)" = histo_crois_3(
                titre = input$plot_title_dial_soc_1,
                ma_data = data_rsu$tables_dial_soc_hist,
                type_aff =  data_rsu$val_type_plot_dial_soc_1,
                couleur = input$colors_plot_dial_soc_1,
                axe_1 = input$plot_axe_dial_soc_1_1,
                axe_2 = input$plot_axe_dial_soc_1_2,
                sup = 'stack'
              )
            ))
            
          }
          
          
          else{
            if (input$plot_dial_soc == "Camembert") {
              showNotification(
                "NB: Le camembert n'offre pas une visualisation adapté aux tableaux croisés !!!",
                type = "error",
                duration = 5
              )
            }
            
            
            data_rsu$tables_dial_soc_hist = tables_dial_soc_defaut() %>% slice(-nrow(.)) %>% mutate(names = rownames(.)) %>% filter(Total !=
                                                                                                                                      0)
            names(data_rsu$tables_dial_soc_hist) = gsub("\\.", " ", names(data_rsu$tables_dial_soc_hist))
            
            data_rsu$val_type_plot_dial_soc_1 = input$val_type_plot_dial_soc_1
            
            return(switch(
              input$plot_dial_soc,
              "Histogramme" = histo_crois_2(
                titre = input$plot_title_dial_soc_1,
                ma_data = data_rsu$tables_dial_soc_hist,
                type_aff =  data_rsu$val_type_plot_dial_soc_1,
                couleur = input$colors_plot_dial_soc_1,
                axe_1 = input$plot_axe_dial_soc_1_1,
                axe_2 = input$plot_axe_dial_soc_1_2
              ),
              
              "Histogramme superposé (tableau croisé uniquement)" = histo_crois_2(
                titre = input$plot_title_dial_soc_1,
                ma_data = data_rsu$tables_dial_soc_hist,
                type_aff =  data_rsu$val_type_plot_dial_soc_1,
                couleur = input$colors_plot_dial_soc_1,
                axe_1 = input$plot_axe_dial_soc_1_1,
                axe_2 = input$plot_axe_dial_soc_1_2,
                sup = 'stack'
              )
            ))
          }
        }
        
      })
      
      
      
      
      #------> Sorties des affichages des graphiques
      
      output$plot_vent_crois_dial_soc = renderPlotly({
        if (is.null(input$vent_crois_btn_dial_soc) != T) {
          tryCatch({
            plot_dial_soc_dataset()
          }, error = function(e) {
            
          })
        }
      })
      
      
      
      
      
    }, error = function(e) {
      showNotification("Valider d'abord votre partition",
                       type = "error",
                       duration = 5)
    })
  })
  
  ## ------------------------------------------Code de conception des tables & graphes manuelles de dialogue social---------------------------------------------------
  
  output$manuelle_vent_dial_soc = DT::renderDT({
    if (is.null(input$manuelle_btn_dial_soc) != T) {
      # Affichage de la table si la case  " Tableau composé manuellement" est coché
      
      tryCatch({
        tranche_manuelle_dial_soc = data_rsu$tranche_dial_soc %>%    # Constitution de la table issue de la séléction manuelle
          select(input$select_manuelle_vent_dial_soc) %>% t() %>%
          data.frame() %>%
          rename(Effectif = names(.))
        
        table_manuelle(ma_table = tranche_manuelle_dial_soc)
        
      }, error = function(e) {
        
      })
    }
  })
  
  ##--------> représentation des tableaux manuellement ventilés: graphes 2 sous table  mouvement du perso crée manuellement
  
  plot_dial_soc_manuelle_dataset = reactive({
    tranche_manuelle_dial_soc = data_rsu$tranche_dial_soc %>%
      select(input$select_manuelle_vent_dial_soc) %>% t() %>% data.frame() %>%
      rename(Effectif = names(.)) %>%
      mutate(names = gsub("\\.", " ", sub("[0-9]*- ", "", rownames(.))))
    
    tranche_manuelle_dial_soc_plot = tranche_manuelle_dial_soc %>% filter(Effectif !=
                                                                            0)
    
    #--------> fonction switch permettant d'avoir des rendu au relatif aux choix graphiques faites par l'utilisateur
    observeEvent(input$select_manuelle_vent_dial_soc, {
      updateTextInput(session, "legend_plot_dial_soc", value = paste(gsub(
        "\\.", " ", sub(
          "[0-9]*- ",
          "",
          rownames(tranche_manuelle_dial_soc_plot)
        )
      ), sep = ",")) # mise à jour des colonnes à séléctionner pour constitué les légendes
    })
    
    rownames(tranche_manuelle_dial_soc_plot) = input$legend_plot_dial_soc %>% strsplit(., split = ",") %>% unlist() %>% as.vector() # Mise à jour de la légende à travers le nom des lignes
    data_rsu$val_type_plot_dial_soc_2 = input$val_type_plot_dial_soc_2
    
    
    if (is.null(c(
      input$select_manuelle_vent_dial_soc,
      input$manuelle_btn_dial_soc
    )) != T   &
    input$plot_dial_soc == "Histogramme superposé (tableau croisé uniquement)") {
      showNotification(
        "NB: L'histogramme superposé n'offre pas une visualisation adapté aux tableaux ventilés !!!",
        type = "error",
        duration = 5
      )
    }
    
    switch(
      input$plot_dial_soc,
      "Camembert" = cam(
        titre = input$plot_title_dial_soc_2,
        ma_data = tranche_manuelle_dial_soc_plot,
        type_aff = data_rsu$val_type_plot_dial_soc_2,
        couleur = input$colors_plot_dial_soc_2,
        noms_legende = input$legend_plot_dial_soc %>% strsplit(., split = ",") %>% unlist() %>% as.vector()
      ),
      "Histogramme" = histo(
        titre = input$plot_title_dial_soc_2,
        ma_data = tranche_manuelle_dial_soc_plot,
        type_aff = data_rsu$val_type_plot_dial_soc_2,
        couleur = input$colors_plot_dial_soc_2,
        noms_legende = input$legend_plot_dial_soc %>% strsplit(., split = ",") %>% unlist() %>% as.vector()
        ,
        axe_1 = input$plot_axe_dial_soc_2_1,
        axe_2 = input$plot_axe_dial_soc_2_2
      )
      
    )
  })
  
  #--------->  Graphiques associés aux tables manuelles
  
  output$plot_dial_soc_manuelle =  renderPlotly({
    if (is.null(input$manuelle_btn_dial_soc) != T) {
      tryCatch({
        plot_dial_soc_manuelle_dataset()
      }, error = function(e) {
        
      })
    }
  })
  
  ## -------------------------------------------Fin Code de conception des tables & graphes manuelles  dialogue social---------------------------------------------------
  
  
  
  
  ## ------------------------------------------Code de conception des tables & graphes par  de l'emploi---------------------------------------------------
  
  observeEvent(is.null(input$DDI) != T, {
    updateCheckboxGroupInput(session, "vent_crois_btn_emploi", selected = character(0))
    updateCheckboxGroupInput(session, "manuelle_btn_emploi", selected = character(0))
    
  })
  
  
  
  
  observeEvent(input$valider_import_emploi, {
    data_rsu$tranche_emploi = NULL
    updateCheckboxGroupInput(session, "vent_crois_btn_emploi", selected = character(0))
    
    
    tryCatch({
      # req(input$effectif_file)
      
      observeEvent(is.null(input$DDI) != T, {
        DDI = tryCatch({
          gsub("[^a-zA-Z]", "", input$DDI)
        }, error = function(e) {
          
        })
        
        
        YEAR = tryCatch({
          as.integer(str_extract(input$effectif_file$name, "[0-9]{4}"))
        }, error = function(e) {
          
        })
        
        file_path_effectif = tryCatch({
          name_feuille_match(
            my_vector = input$effectif_file$name ,
            mot_cle = DDI,
            name_or_no = F
          )
        }, error = function(e) {
          
        })
        
        
        
        if (length(file_path_effectif) != 0) {
          data_rsu$tranche_emploi =
            tryCatch({
              BDD_effectif_creator(
                file_path_effectif = input$effectif_file$datapath[file_path_effectif[[1]]] ,
                colnames_extract = c(
                  "Ministère",
                  "Sexe",
                  "Handicap",
                  "Catégorie",
                  "Filière",
                  "ETP",
                  "Physique"
                ),
                DDI = DDI,
                current_year = YEAR
              )
            }, error = function(e) {
              
            })
          
        }
        
        output$current_table_emploi <- renderReactable({
          #Création sous reactable de la table à utilisé durant tout l'étude
          
          tryCatch({
            reactable(
              data_rsu$tranche_emploi,
              resizable = TRUE,
              defaultPageSize = 5,
              wrap = FALSE,
              bordered = TRUE,
              searchable = TRUE,
              highlight = TRUE,
              defaultColDef = colDef(
                na = "-",
                align = "center",
                minWidth = 200,
                headerStyle = list(
                  background = '#424340',
                  color = '#fff'
                )
              )
            )
            
          }, error = function(e) {
            
          })
          
        })
      })
      
      showNotification("Le fichier a été importé avec succès.",
                       type = "message",
                       duration = 5)
      
      
      
      
    }, error = function(e) {
      showNotification(
        "Erreur: Le fichier importé n'est pas au bon format !!!",
        type = "error",
        duration = 5
      )
    })
    
    
  })
  
  sous_theme_emploi_dataset = reactive({
    #Initialisation des sous-thèmes abordés
    switch(
      input$sous_theme_emploi,
      "A- Photographie de la DDI" = c(
        "Répartitions des effectifs physiques au 31 Décembre: par origine ministérielle",
        "Répartitions des effectifs physiques au 31 Décembre: par catégorie",
        "Répartitions des effectifs physiques au 31 Décembre: par genre",
        "Répartitions des effectifs physiques au 31 Décembre: par filière",
        "Moyenne d'âge des agents aux 31 Décembre: par genre",
        "Moyenne d'âge des agents aux 31 Décembre: par ministère",
        "Moyenne d'âge des agents aux 31 Décembre: par catégorie",
        "Répartitions des âges des agents: tranche d'âge & genre",
        "Répartitions des âges des agents de catégorie A: tranche d'âge & genre",
        "Répartitions des âges des agents de catégorie B: tranche d'âge & genre",
        "Répartitions des âges des agents de catégorie C: tranche d'âge & genre",
        "Répartitions des âges des agents aux 31 Décembre: par catégorie"
      ),
      "B- Organisation du temps de travail" = c(
        "Répartitions des effectifs physiques au 31 Décembre: par ETP",
        "Moyenne des ETP des agents aux 31 Décembre: par ministère"
      )
    )
  })
  
  observe({
    updateSelectInput(session, "vent_crois_emploi", choices = sous_theme_emploi_dataset()) # mise à jour des colonnes à séléctionner pour sélectionner le sous thème étudier dans le parcours professionnel
  })
  
  
  #Recherche des colonnes par recherche sur les mots clées
  # Conception des objets tableau par défauts
  
  observeEvent(input$vent_crois_btn_emploi, {
    tryCatch({
      tables_emploi_defaut = reactive({
        tryCatch({
          switch(
            input$vent_crois_emploi,
            
            "Répartitions des effectifs physiques au 31 Décembre: par origine ministérielle" = groupage_vars(
              data = data_rsu$tranche_emploi ,
              key = "Ministère" ,
              agg_func = "count",
              func_name = "Effectif"
            ) %>% ungroup() %>%
              bind_rows(data.frame(
                "Ministère" = "Ensemble", Effectif = sum(.[["Effectif"]])
              )) %>% as.data.frame(),
            
            "Répartitions des effectifs physiques au 31 Décembre: par catégorie" = groupage_vars(
              data = data_rsu$tranche_emploi ,
              key = "Catégorie" ,
              agg_func = "count",
              func_name = "Effectif"
            ) %>%   ungroup() %>%
              bind_rows(
                filter(
                  .,
                  Catégorie %in% c("A", "A+", "Catégorie A", "Catégorie A+")
                ) %>%         # Filtrer les lignes
                  summarise(
                    Catégorie = "A et A+",
                    Effectif = sum(Effectif)
                  )
              ) %>%
              filter(!Catégorie %in% c("A" , "A+"))  %>%
              mutate(Catégorie = ifelse(
                Catégorie == "A et A+", "A", Catégorie
              )) %>%
              arrange(Catégorie) %>%
              bind_rows(data.frame(
                "Catégorie" = "Ensemble", Effectif = sum(.[["Effectif"]])
              )) %>% as.data.frame(),
            "Répartitions des effectifs physiques au 31 Décembre: par genre" = groupage_vars(
              data = data_rsu$tranche_emploi ,
              key = "Sexe" ,
              agg_func = "count",
              func_name = "Effectif"
            ) %>% ungroup() %>%
              bind_rows(data.frame(
                "Sexe" = "Ensemble", Effectif = sum(.[["Effectif"]])
              )) %>% as.data.frame(),
            
            "Répartitions des effectifs physiques au 31 Décembre: par filière" = groupage_vars(
              data = data_rsu$tranche_emploi ,
              key = "Filière" ,
              agg_func = "count",
              func_name = "Effectif"
            ) %>% ungroup() %>%
              bind_rows(data.frame(
                "Filière" = "Ensemble", Effectif = sum(.[["Effectif"]])
              )) %>% as.data.frame(),
            
            "Répartitions des effectifs physiques au 31 Décembre: par ETP" = groupage_vars(
              data = data_rsu$tranche_emploi ,
              key = "ETP" ,
              vecteur_vars = "Ministère",
              agg_func = "count",
              func_name = "Effectif"
            )  %>%  ungroup(),
            
            "Moyenne d'âge des agents aux 31 Décembre: par ministère" = groupage_vars(
              data = data_rsu$tranche_emploi ,
              key = "Ministère" ,
              vecteur_vars = "Age",
              agg_func = "mean",
              func_name = "Moyenne_age"
            ) %>%  ungroup() %>% mutate(Moyenne_age = round(Moyenne_age, 1)),
            
            "Moyenne d'âge des agents aux 31 Décembre: par catégorie" = groupage_vars(
              data = data_rsu$tranche_emploi ,
              key = "Catégorie" ,
              vecteur_vars = "Age",
              agg_func = "mean",
              func_name = "Moyenne_age"
            ) %>%   ungroup() %>%
              bind_rows(
                filter(
                  .,
                  Catégorie %in% c("A", "A+", "Catégorie A", "Catégorie A+")
                ) %>%         # Filtrer les lignes
                  summarise(
                    Catégorie = "A et A+",
                    Moyenne_age = sum(Moyenne_age) / 2
                  )
              ) %>%
              filter(!Catégorie %in% c("A" , "A+"))  %>%
              mutate(Catégorie = ifelse(
                Catégorie == "A et A+", "A", Catégorie
              )) %>%
              arrange(Catégorie) %>% mutate(Moyenne_age = round(Moyenne_age, 1)),
            
            "Moyenne d'âge des agents aux 31 Décembre: par genre" = groupage_vars(
              data = data_rsu$tranche_emploi ,
              key = "Sexe" ,
              vecteur_vars = "Age",
              agg_func = "mean",
              func_name = "Moyenne_age"
            ) %>%  ungroup() %>% mutate(Moyenne_age = round(Moyenne_age, 1)),
            
            "Moyenne des ETP des agents aux 31 Décembre: par ministère" = groupage_vars(
              data = data_rsu$tranche_emploi ,
              key = "Ministère" ,
              vecteur_vars = "ETP",
              agg_func = "mean",
              func_name = "Moyenne_ETP"
            ) %>%  ungroup() %>% mutate(Moyenne_ETP = round(Moyenne_ETP, 2)),
            
            
            "Répartitions des âges des agents: tranche d'âge & genre" = data_rsu$tranche_emploi %>%
              
              mutate(
                Tranche_age = case_when(
                  Age >= 18 & Age <= 24 ~ "18-24",
                  Age >= 25 & Age <= 29 ~ "25-29",
                  Age >= 30 & Age <= 34 ~ "30-34",
                  Age >= 35 & Age <= 39 ~ "35-39",
                  Age >= 40 & Age <= 44 ~ "40-44",
                  Age >= 45 & Age <= 49 ~ "45-49",
                  Age >= 50 & Age <= 54 ~ "50-54",
                  Age >= 55 & Age <= 59 ~ "55-59",
                  Age >= 60 & Age <= 64 ~ "60-64",
                  Age >= 65 ~ "65 et plus",
                  TRUE ~ NA_character_
                )
              ) %>%
              groupage_vars(
                data = . ,
                key = c("Tranche_age", "Sexe") ,
                func_name = "Effectif"
              ) %>%
              ungroup() %>%
              pivot_wider(
                names_from = "Sexe",
                values_from = Effectif,
                values_fill = list(Effectif = 0)
              ) %>% mutate(Total =  rowSums(.[, -1])),
            
            
            "Répartitions des âges des agents de catégorie A: tranche d'âge & genre" = data_rsu$tranche_emploi %>%
              filter(
                Catégorie %in% c("A+", "A", "catégorie A", "Catégorie A")
              ) %>%
              
              mutate(
                Tranche_age = case_when(
                  Age >= 18 & Age <= 24 ~ "18-24",
                  Age >= 25 &
                    Age <= 29 ~ "25-29",
                  Age >= 30 &
                    Age <= 34 ~ "30-34",
                  Age >= 35 &
                    Age <= 39 ~ "35-39",
                  Age >= 40 &
                    Age <= 44 ~ "40-44",
                  Age >= 45 &
                    Age <= 49 ~ "45-49",
                  Age >= 50 &
                    Age <= 54 ~ "50-54",
                  Age >= 55 &
                    Age <= 59 ~ "55-59",
                  Age >= 60 &
                    Age <= 64 ~ "60-64",
                  Age >= 65 ~ "65 et plus",
                  TRUE ~ NA_character_
                )
              ) %>%
              groupage_vars(
                data = . ,
                key = c("Tranche_age", "Sexe") ,
                func_name = "Effectif"
              ) %>%
              ungroup() %>%
              pivot_wider(
                names_from = "Sexe",
                values_from = Effectif,
                values_fill = list(Effectif = 0)
              ) %>% mutate(Total =  rowSums(.[, -1])),
            
            
            
            
            "Répartitions des âges des agents de catégorie B: tranche d'âge & genre" = data_rsu$tranche_emploi %>%
              filter(Catégorie %in% c(
                "B", "catégorie B", "Catégorie B"
              )) %>%
              
              mutate(
                Tranche_age = case_when(
                  Age >= 18 & Age <= 24 ~ "18-24",
                  Age >= 25 &
                    Age <= 29 ~ "25-29",
                  Age >= 30 &
                    Age <= 34 ~ "30-34",
                  Age >= 35 &
                    Age <= 39 ~ "35-39",
                  Age >= 40 &
                    Age <= 44 ~ "40-44",
                  Age >= 45 &
                    Age <= 49 ~ "45-49",
                  Age >= 50 &
                    Age <= 54 ~ "50-54",
                  Age >= 55 &
                    Age <= 59 ~ "55-59",
                  Age >= 60 &
                    Age <= 64 ~ "60-64",
                  Age >= 65 ~ "65 et plus",
                  TRUE ~ NA_character_
                )
              ) %>%
              groupage_vars(
                data = . ,
                key = c("Tranche_age", "Sexe") ,
                func_name = "Effectif"
              ) %>%
              ungroup() %>%
              pivot_wider(
                names_from = "Sexe",
                values_from = Effectif,
                values_fill = list(Effectif = 0)
              ) %>% mutate(Total =  rowSums(.[, -1])) ,
            
            
            "Répartitions des âges des agents de catégorie C: tranche d'âge & genre" = data_rsu$tranche_emploi %>%
              filter(Catégorie %in% c(
                "C", "catégorie C", "Catégorie C"
              )) %>%
              
              mutate(
                Tranche_age = case_when(
                  Age >= 18 & Age <= 24 ~ "18-24",
                  Age >= 25 &
                    Age <= 29 ~ "25-29",
                  Age >= 30 &
                    Age <= 34 ~ "30-34",
                  Age >= 35 &
                    Age <= 39 ~ "35-39",
                  Age >= 40 &
                    Age <= 44 ~ "40-44",
                  Age >= 45 &
                    Age <= 49 ~ "45-49",
                  Age >= 50 &
                    Age <= 54 ~ "50-54",
                  Age >= 55 &
                    Age <= 59 ~ "55-59",
                  Age >= 60 &
                    Age <= 64 ~ "60-64",
                  Age >= 65 ~ "65 et plus",
                  TRUE ~ NA_character_
                )
              ) %>%
              groupage_vars(
                data = . ,
                key = c("Tranche_age", "Sexe") ,
                func_name = "Effectif"
              ) %>%
              ungroup() %>%
              pivot_wider(
                names_from = "Sexe",
                values_from = Effectif,
                values_fill = list(Effectif = 0)
              ) %>% mutate(Total =  rowSums(.[, -1])) ,
            
            
            
            "Répartitions des âges des agents aux 31 Décembre: par catégorie" = data_rsu$tranche_emploi %>%
              
              mutate(
                Tranche_age = case_when(
                  Age >= 18 & Age <= 24 ~ "18-24",
                  Age >= 25 &
                    Age <= 29 ~ "25-29",
                  Age >= 30 &
                    Age <= 34 ~ "30-34",
                  Age >= 35 &
                    Age <= 39 ~ "35-39",
                  Age >= 40 &
                    Age <= 44 ~ "40-44",
                  Age >= 45 &
                    Age <= 49 ~ "45-49",
                  Age >= 50 &
                    Age <= 54 ~ "50-54",
                  Age >= 55 &
                    Age <= 59 ~ "55-59",
                  Age >= 60 &
                    Age <= 64 ~ "60-64",
                  Age >= 65 ~ "65 et plus",
                  TRUE ~ NA_character_
                )
              ) %>%
              groupage_vars(
                data = . ,
                key = c("Tranche_age", "Catégorie") ,
                func_name = "Effectif"
              ) %>%
              ungroup() %>%
              pivot_wider(
                names_from = "Catégorie",
                values_from = Effectif,
                values_fill = list(Effectif = 0)
              ) %>% mutate(Total =  rowSums(.[, -1]))
            
            
          )
        }, error = function(e) {
          showNotification(
            "Pas de visualisation disponible!!!",
            type = "error",
            duration = 5
          )
          showNotification(
            "Il est recommendé de passer à la composition manuelle!!!",
            type = "warning",
            duration = 5
          )
        })
        
      })
      
      
      output$vent_crois_emploi = DT::renderDT({
        #Affichage de la tranche de table si l'utilisateur coche la case "Tableau ... défaut"
        
        tryCatch({
          if (is.null(input$vent_crois_btn_emploi) != T) {
            table_classic(ma_table = tables_emploi_defaut(), off_name = T)
          }
          
        }, error = function(e) {
          
        })
        
      })
      
      #------------------------------> Représentation graphique tableau par défaut
      
      plot_emploi_dataset = reactive({
        if (input$vent_crois_emploi %in% c(
          "Répartitions des effectifs physiques au 31 Décembre: par origine ministérielle",
          "Répartitions des effectifs physiques au 31 Décembre: par catégorie",
          "Répartitions des effectifs physiques au 31 Décembre: par genre",
          "Répartitions des effectifs physiques au 31 Décembre: par filière"
        )) {
          if (input$plot_emploi == "Histogramme superposé (tableau croisé uniquement)") {
            showNotification(
              "NB: L'histogramme superposé n'offre pas une visualisation adapté aux tableaux ventilés !!!",
              type = "error",
              duration = 5
            )
          }
          if (input$plot_emploi == "Pyramide") {
            showNotification(
              "NB: La pyramide n'offre pas une visualisation adapté aux tableaux ventilés ou croisés !!!",
              type = "error",
              duration = 5
            )
          }
          
          names = c((tables_emploi_defaut())[[1]])
          value = c((tables_emploi_defaut())[[2]])
          
          data_rsu$tables_emploi_hist = data.frame(names = names,
                                                   value = value,
                                                   Effectif = value) %>% mutate(names = factor(names, exclude = NA)) %>% slice(-nrow(.))    # Création de la table à utilisé pour représenter l'histogramme conformément aux attendus de plotly
          data_rsu$tables_emploi_cam = data.frame(names = names,
                                                  value = value,
                                                  Effectif = value) %>% mutate(names = factor(names, exclude = NA)) %>% slice(-nrow(.))    # Création de la table à utilisé pour représenter le camembert conformément aux attendus de plotly
          
          data_rsu$val_type_plot_emploi_1 = input$val_type_plot_emploi_1
          
          return(switch(
            input$plot_emploi,
            "Camembert" = cam(
              titre = input$plot_title_emploi_1,
              ma_data = data_rsu$tables_emploi_cam,
              type_aff = data_rsu$val_type_plot_emploi_1,
              couleur = input$colors_plot_emploi_1
            ),
            
            "Histogramme" = histo(
              titre = input$plot_title_emploi_1,
              ma_data = data_rsu$tables_emploi_hist,
              type_aff =  data_rsu$val_type_plot_emploi_1,
              couleur = input$colors_plot_emploi_1,
              axe_1 = input$plot_axe_emploi_1_1,
              axe_2 = input$plot_axe_emploi_1_2
            )
          ))
        } else{
          if (input$vent_crois_emploi %in% c(
            "Moyenne d'âge des agents aux 31 Décembre: par genre",
            "Moyenne d'âge des agents aux 31 Décembre: par ministère",
            "Moyenne d'âge des agents aux 31 Décembre: par catégorie",
            "Moyenne des ETP des agents aux 31 Décembre: par ministère",
            "Répartitions des effectifs physiques au 31 Décembre: par ETP"
          )) {
            if (input$plot_emploi == "Histogramme superposé (tableau croisé uniquement)") {
              showNotification(
                "NB: L'histogramme superposé n'offre pas une visualisation adapté aux tableaux ventilés !!!",
                type = "error",
                duration = 5
              )
            }
            if (input$plot_emploi == "Pyramide") {
              showNotification(
                "NB: La pyramide n'offre pas une visualisation adapté aux tableaux ventilés ou croisés !!!",
                type = "error",
                duration = 5
              )
            }
            
            
            names = c((tables_emploi_defaut())[[1]])
            value = round(c((tables_emploi_defaut())[[2]]), 2)
            
            data_rsu$tables_emploi_hist = data.frame(names = names,
                                                     value = value,
                                                     Effectif = value) %>% mutate(names = factor(names, exclude = NA))    # Création de la table à utilisé pour représenter l'histogramme conformément aux attendus de plotly
            data_rsu$tables_emploi_cam = data.frame(names = names,
                                                    value = value,
                                                    Effectif = value) %>% mutate(names = factor(names, exclude = NA))    # Création de la table à utilisé pour représenter le camembert conformément aux attendus de plotly
            
            data_rsu$val_type_plot_emploi_1 = input$val_type_plot_emploi_1
            
            return(switch(
              input$plot_emploi,
              "Camembert" = cam(
                titre = input$plot_title_emploi_1,
                ma_data = data_rsu$tables_emploi_cam,
                type_aff = data_rsu$val_type_plot_emploi_1,
                couleur = input$colors_plot_emploi_1
              ),
              
              "Histogramme" = histo(
                titre = input$plot_title_emploi_1,
                ma_data = data_rsu$tables_emploi_hist,
                type_aff =  data_rsu$val_type_plot_emploi_1,
                couleur = input$colors_plot_emploi_1,
                axe_1 = input$plot_axe_emploi_1_1,
                axe_2 = input$plot_axe_emploi_1_2
              )
            ))
            
            
          } else{
            if (input$vent_crois_emploi %in% c(
              "Répartitions des âges des agents: tranche d'âge & genre",
              "Répartitions des âges des agents de catégorie A: tranche d'âge & genre",
              "Répartitions des âges des agents de catégorie B: tranche d'âge & genre",
              "Répartitions des âges des agents de catégorie C: tranche d'âge & genre",
              "Répartitions des âges des agents aux 31 Décembre: par catégorie"
            ) & input$plot_emploi == "Pyramide") {
              if (input$plot_emploi == "Camembert") {
                showNotification(
                  "NB: Le camembert n'offre pas une visualisation adapté aux tableaux croisés !!!",
                  type = "error",
                  duration = 5
                )
              }
              
              pyra_age = tables_emploi_defaut() %>%
                pivot_longer(
                  cols = if ("M" %in% names(.)) {
                    c("F", "M")
                  } else{
                    c("F", "H")
                  },
                  names_to = "Sexe",
                  values_to = "Effectif"
                ) %>%
                mutate_at(vars(Effectif), as.double) %>%
                
                mutate(Effectif = ifelse(Sexe %in% c("M", "H"), -Effectif, Effectif))
              
              
              return(switch(
                input$plot_emploi,
                "Pyramide" =  plot_ly(
                  pyra_age,
                  x = if (input$val_type_plot_emploi_1 == "Valeur") {
                    ~ Effectif
                  } else{
                    ~ (round((
                      pyra_age$Effectif / sum(pyra_age$Total)
                    ) * 100, 1))
                  },
                  # Utiliser les valeurs originales pour l'affichage des barres
                  y = ~ Tranche_age,
                  color = ~ Sexe,
                  colors = input$colors_plot_emploi_1,
                  type = 'bar',
                  orientation = 'h',
                  text = if (input$val_type_plot_emploi_1 == "Valeur") {
                    ~ abs(Effectif)
                  } else{
                    ~ paste0(abs(round((pyra_age$Effectif * 100 / sum(abs(
                      pyra_age$Effectif
                    )))
                    )), "%")
                  },
                  # Afficher les valeurs absolues sur les barres
                  textposition = "outside"       # Placer les valeurs au bout des barres
                ) %>%
                  layout(
                    title = input$plot_title_emploi_1,
                    barmode = 'overlay',
                    xaxis = list(
                      title = input$plot_axe_emploi_1_1,
                      tickvals = seq(
                        -max(pyra_age$Effectif),
                        max(pyra_age$Effectif),
                        by = 2
                      ),
                      showticklabels = ifelse(
                        input$val_type_plot_emploi_1 != "Valeur & Pourcentage",
                        FALSE,
                        TRUE
                      )
                    ),
                    yaxis = list(title = input$plot_axe_emploi_1_2)
                  ) %>%
                  config(displaylogo = FALSE) %>%
                  config(displayModeBar = TRUE) %>%
                  config(
                    modeBarButtonsToRemove = list(
                      'zoom2d',
                      'resetScale2d',
                      'pan2d',
                      'select2d',
                      'lasso2d',
                      'zoomIn2d',
                      'zoomOut2d',
                      'autoScale2d',
                      'hoverClosestCartesian',
                      'hoverCompareCartesian'
                    )
                  ) %>%
                  config(toImageButtonOptions = list(format = "png"))
                
                
              ))
            } else{
              if (input$vent_crois_emploi %in% c(
                "Répartitions des âges des agents: tranche d'âge & genre",
                "Répartitions des âges des agents de catégorie A: tranche d'âge & genre",
                "Répartitions des âges des agents de catégorie B: tranche d'âge & genre",
                "Répartitions des âges des agents de catégorie C: tranche d'âge & genre"
              ) &
              input$plot_emploi %in% c("Histogramme",
                                       "Histogramme superposé (tableau croisé uniquement)")) {
                if (input$plot_emploi == "Camembert") {
                  showNotification(
                    "NB: Le camembert n'offre pas une visualisation adapté aux tableaux croisés !!!",
                    type = "error",
                    duration = 5
                  )
                }
                
                data_rsu$tables_emploi_hist = tables_emploi_defaut()   %>% filter(!(is.na(Tranche_age))) %>%
                  mutate(across(c("F", "M"), as.numeric)) %>%
                  column_to_rownames(var = "Tranche_age") %>% select(c("M", "F", "Total")) %>%
                  mutate(names = rownames(.))
                
                data_rsu$val_type_plot_emploi_1 = input$val_type_plot_emploi_1
                
                return(switch(
                  input$plot_emploi,
                  "Histogramme" = histo_crois_2(
                    titre = input$plot_title_emploi_1,
                    ma_data = data_rsu$tables_emploi_hist,
                    type_aff =  data_rsu$val_type_plot_emploi_1,
                    couleur = input$colors_plot_emploi_1,
                    axe_1 = input$plot_axe_emploi_1_1,
                    axe_2 = input$plot_axe_emploi_1_2
                  ),
                  
                  "Histogramme superposé (tableau croisé uniquement)" = histo_crois_2(
                    titre = input$plot_title_emploi_1,
                    ma_data = data_rsu$tables_emploi_hist,
                    type_aff =  data_rsu$val_type_plot_emploi_1,
                    couleur = input$colors_plot_emploi_1,
                    axe_1 = input$plot_axe_emploi_1_1,
                    axe_2 = input$plot_axe_emploi_1_2,
                    sup = 'stack'
                  )
                ))
                
              } else{
                if (input$vent_crois_emploi == "Répartitions des âges des agents aux 31 Décembre: par catégorie") {
                  if (input$plot_emploi == "Histogramme superposé (tableau croisé uniquement)") {
                    showNotification(
                      "NB: L'histogramme superposé n'offre pas une visualisation adapté aux tableaux ventilés !!!",
                      type = "error",
                      duration = 5
                    )
                  }
                  if (input$plot_emploi == "Pyramide") {
                    showNotification(
                      "NB: La pyramide n'offre pas une visualisation adapté aux tableaux ventilés ou croisés !!!",
                      type = "error",
                      duration = 5
                    )
                  }
                  
                  
                  names = c((tables_emploi_defaut())[[1]])
                  value = c((tables_emploi_defaut())[["Total"]])
                  
                  data_rsu$tables_emploi_hist = data.frame(
                    names = names,
                    value = value,
                    Effectif = value
                  ) %>% mutate(names = factor(names, exclude = NA))    # Création de la table à utilisé pour représenter l'histogramme conformément aux attendus de plotly
                  data_rsu$tables_emploi_cam = data.frame(
                    names = names,
                    value = value,
                    Effectif = value
                  ) %>% mutate(names = factor(names, exclude = NA))    # Création de la table à utilisé pour représenter le camembert conformément aux attendus de plotly
                  
                  data_rsu$val_type_plot_emploi_1 = input$val_type_plot_emploi_1
                  
                  return(switch(
                    input$plot_emploi,
                    "Camembert" = cam(
                      titre = input$plot_title_emploi_1,
                      ma_data = data_rsu$tables_emploi_cam,
                      type_aff = data_rsu$val_type_plot_emploi_1,
                      couleur = input$colors_plot_emploi_1
                    ),
                    
                    "Histogramme" = histo(
                      titre = input$plot_title_emploi_1,
                      ma_data = data_rsu$tables_emploi_hist,
                      type_aff =  data_rsu$val_type_plot_emploi_1,
                      couleur = input$colors_plot_emploi_1,
                      axe_1 = input$plot_axe_emploi_1_1,
                      axe_2 = input$plot_axe_emploi_1_2
                    )
                  ))
                }
                
              }
              
            }
            
          }
        }
      })
      
      #------> Sorties affichage des tableaux de l'emploi
      output$plot_vent_crois_emploi = renderPlotly({
        tryCatch({
          if (is.null(input$vent_crois_btn_emploi) != T) {
            plot_emploi_dataset()
          }
        }, error = function(e) {
          
        })
      })
      
      
    }, error = function(e) {
      showNotification("Valider d'abord l'importation",
                       type = "error",
                       duration = 5)
    })
  })
  
  
  ## ------------------------------------------Code de conception des tables & graphes manuelles  de emploi---------------------------------------------------
  
  observe({
    updateSelectInput(
      session,
      "aggregate_key_emploi",
      choices = colnames(data_rsu$tranche_emploi),
      selected = (colnames(data_rsu$tranche_emploi))[1]
    )
  })
  
  observeEvent(is.null(input$aggregate_key_emploi) != T, {
    updateSelectInput(
      session,
      "aggregate_critere_emploi",
      choices = setdiff(
        colnames(data_rsu$tranche_emploi),
        input$aggregate_key_emploi
      )
    )
  })
  
  fonction_manuelle = reactive({
    switch(
      input$aggregate_funct_emploi,
      "Comptage" = "count",
      "Somme" = "sum",
      "Moyenne" = "mean"
    )
  })
  
  output$manuelle_vent_emploi = DT::renderDT({
    if (is.null(input$manuelle_btn_emploi) != T) {
      # Affichage de la table si la case  " Tableau composé manuellement" est coché
      
      tryCatch({
        tranche_manuelle_agg_emploi = data_rsu$tranche_emploi %>% groupage_vars(
          data = . ,
          key = c(input$aggregate_key_emploi) ,
          vecteur_vars = c(input$aggregate_critere_emploi),
          agg_func = fonction_manuelle(),
          func_name = paste(input$aggregate_funct_emploi, sep = "_", "{col}")
        ) %>%
          mutate(across(.cols = last_col(), .fns = ~ round(., 1)))
        
        table_manuelle(ma_table = tranche_manuelle_agg_emploi)
        
      }, error = function(e) {
        
      })
    }
  })
  
  
  
  
  plot_emploi_dataset = reactive({
    tryCatch({
      tranche_manuelle_agg_emploi = data_rsu$tranche_emploi %>% groupage_vars(
        data = . ,
        key = c(input$aggregate_key_emploi) ,
        vecteur_vars = c(input$aggregate_critere_emploi),
        agg_func = fonction_manuelle(),
        func_name = paste(input$aggregate_funct_emploi, sep = "_", "{col}")
      ) %>%
        mutate(across(.cols = last_col(), .fns = ~ round(., 1)))
      
      if (dim(tranche_manuelle_agg_emploi)[2] == 2) {
        names = c(tranche_manuelle_agg_emploi[[1]])
        value = c(tranche_manuelle_agg_emploi[[2]])
        
        
        
        data_rsu$tables_manu_emploi_hist = data.frame(names = names,
                                                      value = value,
                                                      Effectif = value) %>% mutate(names = factor(names, exclude = NA))    # Création de la table à utilisé pour représenter l'histogramme conformément aux attendus de plotly
        data_rsu$tables_manu_emploi_cam = data.frame(names = names,
                                                     value = value,
                                                     Effectif = value) %>% mutate(names = factor(names, exclude = NA))    # Création de la table à utilisé pour représenter le camembert conformément aux attendus de plotly
        
        data_rsu$val_type_plot_emploi_2 = input$val_type_plot_emploi_2
        
        return(switch(
          input$plot_emploi,
          "Camembert" = cam(
            titre = input$plot_title_emploi_2,
            ma_data = data_rsu$tables_manu_emploi_cam,
            type_aff = data_rsu$val_type_plot_emploi_2,
            couleur = input$colors_plot_emploi_2
          ),
          
          "Histogramme" = histo(
            titre = input$plot_title_emploi_2,
            ma_data = data_rsu$tables_manu_emploi_hist,
            type_aff =  data_rsu$val_type_plot_emploi_2,
            couleur = input$colors_plot_emploi_2,
            axe_1 = input$plot_axe_emploi_2_1,
            axe_2 = input$plot_axe_emploi_2_2
          )
        ))
      }
      
    }, error = function(e) {
      
    })
    
  })
  
  #------> Sorties affichage des tableaux de l'emploi
  output$plot_emploi_manuelle = renderPlotly({
    if (is.null(input$manuelle_btn_emploi) != T) {
      tryCatch({
        plot_emploi_dataset()
      }, error = function(e) {
        
      })
    }
  })
  
  
  ## ------------------------------------------Fin Code de conception des tables & graphes par défaut  de l'emploi---------------------------------------------------
  
  
  
  
  
  ## -------------------------------------------Code de conception des tables & graphes par défaut de la formation---------------------------------------------------
  
  
  sous_theme_formation_dataset = reactive({
    #Initialisation des sous-thèmes abordés
    switch(
      input$sous_theme_formation,
      "A- Agents formés et Nbre de jours de formation" = c(
        "Agent formés par type de formation",
        "Agent formés: genre & catégorie",
        "Agent formés: type de formation & genre",
        "Agent formés: type de formation & catégorie",
        "Formations reçu par les agents: type de formation & Nbre agent , de jours de formation"
      ),
      "B- Congés de formation et encadrement intermédiaire" = c(
        "Demandes de congés formation: catégorie & décision prise",
        "Nbre de cadres intermédiaire issus de la DDI: genre & catégories"
      ),
      "Informations supplémentaires" = c("Réponses qualitatives")
    )
  })
  
  observe({
    updateSelectInput(session, "vent_crois_formation", choices = sous_theme_formation_dataset()) # mise à jour des colonnes à séléctionner pour sélectionner le sous thème étudier dans le parcours professionnel
  })
  
  
  #Recherche des colonnes par recherche sur les mots clées
  
  observeEvent(input$vent_crois_btn_formation, {
    tryCatch({
      cat_A_statuaire_frmt = colonne_match(
        data = data_rsu$tranche_formation,
        vecteurs_mots =  c("Catégorie A", "statuaire"),
        somme = F
      )
      cat_B_statuaire_frmt = colonne_match(
        data = data_rsu$tranche_formation,
        vecteurs_mots =  c("Catégorie B", "statuaire"),
        somme = F
      )
      cat_C_statuaire_frmt = colonne_match(
        data = data_rsu$tranche_formation,
        vecteurs_mots =  c("Catégorie C", "statuaire"),
        somme = F
      )
      
      cat_A_continue_frmt = colonne_match(
        data = data_rsu$tranche_formation,
        vecteurs_mots =  c("Catégorie A", "continue"),
        somme = F
      )
      cat_B_continue_frmt = colonne_match(
        data = data_rsu$tranche_formation,
        vecteurs_mots =  c("Catégorie B", "continue"),
        somme = F
      )
      cat_C_continue_frmt = colonne_match(
        data = data_rsu$tranche_formation,
        vecteurs_mots =  c("Catégorie C", "continue"),
        somme = F
      )
      
      cat_A_concours_frmt = colonne_match(
        data = data_rsu$tranche_formation,
        vecteurs_mots =  c("Catégorie A", "concours", "examen"),
        somme = F
      )
      cat_B_concours_frmt = colonne_match(
        data = data_rsu$tranche_formation,
        vecteurs_mots =  c("Catégorie B", "concours", "examen"),
        somme = F
      )
      cat_C_concours_frmt = colonne_match(
        data = data_rsu$tranche_formation,
        vecteurs_mots =  c("Catégorie C", "concours", "examen"),
        somme = F
      )
      
      cat_A_bilan_frmt = colonne_match(
        data = data_rsu$tranche_formation,
        vecteurs_mots =  c("Catégorie A", "bilan", "compétence"),
        somme = F
      )
      cat_B_bilan_frmt = colonne_match(
        data = data_rsu$tranche_formation,
        vecteurs_mots =  c("Catégorie B", "bilan", "compétence"),
        somme = F
      )
      cat_C_bilan_frmt = colonne_match(
        data = data_rsu$tranche_formation,
        vecteurs_mots =  c("Catégorie C", "bilan", "compétence"),
        somme = F
      )
      
      cat_A_VAE_frmt = colonne_match(
        data = data_rsu$tranche_formation,
        vecteurs_mots =  c("Catégorie A", "acquis", "l'expérience", "VAE"),
        somme = F
      )
      cat_B_VAE_frmt = colonne_match(
        data = data_rsu$tranche_formation,
        vecteurs_mots =  c("Catégorie B", "acquis", "l'expérience", "VAE"),
        somme = F
      )
      cat_C_VAE_frmt = colonne_match(
        data = data_rsu$tranche_formation,
        vecteurs_mots =  c("Catégorie C", "acquis", "l'expérience", "VAE"),
        somme = F
      )
      
      
      cat_A_form_pro_frmt = colonne_match(
        data = data_rsu$tranche_formation,
        vecteurs_mots =  c("Catégorie A", "formation", "professionnelle"),
        somme = F
      )
      cat_B_form_pro_frmt = colonne_match(
        data = data_rsu$tranche_formation,
        vecteurs_mots =  c("Catégorie B", "formation", "professionnelle"),
        somme = F
      )
      cat_C_form_pro_frmt = colonne_match(
        data = data_rsu$tranche_formation,
        vecteurs_mots =  c("Catégorie C", "formation", "professionnelle"),
        somme = F
      )
      
      
      cat_A_profess_frmt = colonne_match(
        data = data_rsu$tranche_formation,
        vecteurs_mots =  c("Catégorie A", "période", "professionnalisation"),
        somme = F
      )
      cat_B_profess_frmt = colonne_match(
        data = data_rsu$tranche_formation,
        vecteurs_mots =  c("Catégorie B", "période", "professionnalisation"),
        somme = F
      )
      cat_C_profess_frmt = colonne_match(
        data = data_rsu$tranche_formation,
        vecteurs_mots =  c("Catégorie C", "période", "professionnalisation"),
        somme = F
      )
      
      #------ congés formation
      
      tit_dem_acpt_frmt = colonne_match(
        data = data_rsu$tranche_formation,
        vecteurs_mots =  c("titulaire", "acceptée", "demande")
      )
      
      tit_dem_ref_frmt = colonne_match(
        data = data_rsu$tranche_formation,
        vecteurs_mots =  c("titulaire", "refusée", "demande")
      )
      
      contr_dem_acpt_frmt = colonne_match(
        data = data_rsu$tranche_formation,
        vecteurs_mots =  c("contractuel", "acceptée", "demande")
      )
      
      contr_dem_ref_frmt = colonne_match(
        data = data_rsu$tranche_formation,
        vecteurs_mots =  c("contractuel", "refusée", "demande")
      )
      
      
      #----------------------Encadrement
      
      
      encadr_femme_cat_A_frmt = colonne_match(
        data = data_rsu$tranche_formation,
        vecteurs_mots =  c("intermédiaire", "Femmes", "Catégorie A")
      )
      encadr_femme_cat_B_frmt = colonne_match(
        data = data_rsu$tranche_formation,
        vecteurs_mots =  c("intermédiaire", "Femmes", "Catégorie B")
      )
      
      encadr_homme_cat_A_frmt = colonne_match(
        data = data_rsu$tranche_formation,
        vecteurs_mots =  c("intermédiaire", "Hommes", "Catégorie A")
      )
      encadr_homme_cat_B_frmt = colonne_match(
        data = data_rsu$tranche_formation,
        vecteurs_mots =  c("intermédiaire", "Hommes", "Catégorie B")
      )
      
      
      
      # Conception des objets tableau par défauts
      tables_formation_defaut = reactive({
        tryCatch({
          switch(
            input$vent_crois_formation,
            "Agent formés par type de formation" = data.frame(
              row.names = c(
                "Formation statuaire",
                "Formation continue",
                "Préparation aux concours et examens",
                "Réalisation de bilan de compétence",
                "Validation d'acquis d'expérience",
                "Congé de formation professionnelle",
                "Période de professionnalisation"
              ),
              Effectif = c(
                sum(
                  cat_A_statuaire_frmt[1, 1],
                  cat_A_statuaire_frmt[1, 2] ,
                  cat_B_statuaire_frmt[1, 1] ,
                  cat_B_statuaire_frmt[1, 2],
                  cat_C_statuaire_frmt[1, 1],
                  cat_C_statuaire_frmt[1, 2]
                ),
                sum(
                  cat_A_continue_frmt[1, 1],
                  cat_A_continue_frmt[1, 2],
                  cat_B_continue_frmt[1, 1],
                  cat_B_continue_frmt[1, 2],
                  cat_C_continue_frmt[1, 1],
                  cat_C_continue_frmt[1, 2]
                ),
                sum(
                  cat_A_concours_frmt[1, 1],
                  cat_A_concours_frmt[1, 2],
                  cat_B_concours_frmt[1, 1],
                  cat_B_concours_frmt[1, 2],
                  cat_C_concours_frmt[1, 1],
                  cat_C_concours_frmt[1, 2]
                ),
                sum(
                  cat_A_bilan_frmt[1, 1],
                  cat_A_bilan_frmt[1, 2],
                  cat_B_bilan_frmt[1, 1],
                  cat_B_bilan_frmt[1, 2],
                  cat_C_bilan_frmt[1, 1],
                  cat_C_bilan_frmt[1, 2]
                ),
                sum(
                  cat_A_VAE_frmt[1, 1],
                  cat_A_VAE_frmt[1, 2],
                  cat_B_VAE_frmt[1, 1],
                  cat_B_VAE_frmt[1, 2],
                  cat_C_VAE_frmt[1, 1],
                  cat_C_VAE_frmt[1, 2]
                ),
                sum(
                  cat_A_form_pro_frmt[1, 1],
                  cat_A_form_pro_frmt[1, 2],
                  cat_B_form_pro_frmt[1, 1],
                  cat_B_form_pro_frmt[1, 2],
                  cat_C_form_pro_frmt[1, 1],
                  cat_C_form_pro_frmt[1, 2]
                ),
                sum(
                  cat_A_profess_frmt[1, 1],
                  cat_A_profess_frmt[1, 2],
                  cat_B_profess_frmt[1, 1],
                  cat_B_profess_frmt[1, 2],
                  cat_C_profess_frmt[1, 1],
                  cat_C_profess_frmt[1, 2]
                )
              )
            ) %>%
              bind_rows(data.frame(
                row.names = "Total", Effectif = colSums(.)
              )) ,
            
            "Formations reçu par les agents: type de formation & Nbre agent , de jours de formation" = data.frame(
              row.names = c(
                "Formation statuaire",
                "Formation continue",
                "Préparation aux concours et examens",
                "Réalisation de bilan de compétence",
                "Validation d'acquis d'expérience",
                "Congé de formation professionnelle",
                "Période de professionnalisation"
              ),
              "Agents" = c(
                sum(
                  cat_A_statuaire_frmt[1, 1],
                  cat_A_statuaire_frmt[1, 2],
                  cat_B_statuaire_frmt[1, 1],
                  cat_B_statuaire_frmt[1, 2],
                  cat_C_statuaire_frmt[1, 1],
                  cat_C_statuaire_frmt[1, 2]
                ),
                sum(
                  cat_A_continue_frmt[1, 1],
                  cat_A_continue_frmt[1, 2],
                  cat_B_continue_frmt[1, 1],
                  cat_B_continue_frmt[1, 2],
                  cat_C_continue_frmt[1, 1],
                  cat_C_continue_frmt[1, 2]
                ),
                sum(
                  cat_A_concours_frmt[1, 1],
                  cat_A_concours_frmt[1, 2],
                  cat_B_concours_frmt[1, 1],
                  cat_B_concours_frmt[1, 2],
                  cat_C_concours_frmt[1, 1],
                  cat_C_concours_frmt[1, 2]
                ),
                sum(
                  cat_A_bilan_frmt[1, 1],
                  cat_A_bilan_frmt[1, 2],
                  cat_B_bilan_frmt[1, 1],
                  cat_B_bilan_frmt[1, 2],
                  cat_C_bilan_frmt[1, 1],
                  cat_C_bilan_frmt[1, 2]
                ),
                sum(
                  cat_A_VAE_frmt[1, 1],
                  cat_A_VAE_frmt[1, 2],
                  cat_B_VAE_frmt[1, 1],
                  cat_B_VAE_frmt[1, 2],
                  cat_C_VAE_frmt[1, 1],
                  cat_C_VAE_frmt[1, 2]
                ),
                sum(
                  cat_A_form_pro_frmt[1, 1],
                  cat_A_form_pro_frmt[1, 2],
                  cat_B_form_pro_frmt[1, 1],
                  cat_B_form_pro_frmt[1, 2],
                  cat_C_form_pro_frmt[1, 1],
                  cat_C_form_pro_frmt[1, 2]
                ),
                sum(
                  cat_A_profess_frmt[1, 1],
                  cat_A_profess_frmt[1, 2],
                  cat_B_profess_frmt[1, 1],
                  cat_B_profess_frmt[1, 2],
                  cat_C_profess_frmt[1, 1],
                  cat_C_profess_frmt[1, 2]
                )
              ),
              
              "Nbre_de_jours" = c(
                sum(
                  cat_A_statuaire_frmt[1, 3],
                  cat_A_statuaire_frmt[1, 4],
                  cat_B_statuaire_frmt[1, 3],
                  cat_B_statuaire_frmt[1, 4],
                  cat_C_statuaire_frmt[1, 3],
                  cat_C_statuaire_frmt[1, 4]
                ),
                sum(
                  cat_A_continue_frmt[1, 3],
                  cat_A_continue_frmt[1, 4],
                  cat_B_continue_frmt[1, 3],
                  cat_B_continue_frmt[1, 4],
                  cat_C_continue_frmt[1, 3],
                  cat_C_continue_frmt[1, 4]
                ),
                sum(
                  cat_A_concours_frmt[1, 3],
                  cat_A_concours_frmt[1, 4],
                  cat_B_concours_frmt[1, 3],
                  cat_B_concours_frmt[1, 4],
                  cat_C_concours_frmt[1, 3],
                  cat_C_concours_frmt[1, 4]
                ),
                sum(
                  cat_A_bilan_frmt[1, 3],
                  cat_A_bilan_frmt[1, 4],
                  cat_B_bilan_frmt[1, 3],
                  cat_B_bilan_frmt[1, 4],
                  cat_C_bilan_frmt[1, 3],
                  cat_C_bilan_frmt[1, 4]
                ),
                sum(
                  cat_A_VAE_frmt[1, 3],
                  cat_A_VAE_frmt[1, 4],
                  cat_B_VAE_frmt[1, 3],
                  cat_B_VAE_frmt[1, 4],
                  cat_C_VAE_frmt[1, 3],
                  cat_C_VAE_frmt[1, 4]
                ),
                sum(
                  cat_A_form_pro_frmt[1, 3],
                  cat_A_form_pro_frmt[1, 4],
                  cat_B_form_pro_frmt[1, 3],
                  cat_B_form_pro_frmt[1, 4],
                  cat_C_form_pro_frmt[1, 3],
                  cat_C_form_pro_frmt[1, 4]
                ),
                sum(
                  cat_A_profess_frmt[1, 3],
                  cat_A_profess_frmt[1, 4],
                  cat_B_profess_frmt[1, 3],
                  cat_B_profess_frmt[1, 4],
                  cat_C_profess_frmt[1, 3],
                  cat_C_profess_frmt[1, 4]
                )
              )
            ) %>%
              
              bind_rows(
                data.frame(
                  row.names = "Ensemble",
                  "Agents" = colSums(.)[1],
                  "Nbre_de_jours" = colSums(.)[2]
                )
              )  ,
            
            "Agent formés: genre & catégorie" = data.frame(
              row.names = c("Catégorie A", "Catégorie B", "Catégorie C"),
              "Femmes" = c(
                sum(
                  cat_A_statuaire_frmt[1, 1],
                  cat_A_continue_frmt[1, 1],
                  cat_A_concours_frmt[1, 1],
                  cat_A_bilan_frmt[1, 1],
                  cat_A_VAE_frmt[1, 1],
                  cat_A_form_pro_frmt[1, 1],
                  cat_A_profess_frmt[1, 1]
                ),
                sum(
                  cat_B_statuaire_frmt[1, 1],
                  cat_B_continue_frmt[1, 1],
                  cat_B_concours_frmt[1, 1],
                  cat_B_bilan_frmt[1, 1],
                  cat_B_VAE_frmt[1, 1],
                  cat_B_form_pro_frmt[1, 1],
                  cat_B_profess_frmt[1, 1]
                ),
                sum(
                  cat_C_statuaire_frmt[1, 1],
                  cat_C_continue_frmt[1, 1],
                  cat_C_concours_frmt[1, 1],
                  cat_C_bilan_frmt[1, 1],
                  cat_C_VAE_frmt[1, 1],
                  cat_C_form_pro_frmt[1, 1],
                  cat_C_profess_frmt[1, 1]
                )
              ),
              
              "Hommes" = c(
                sum(
                  cat_A_statuaire_frmt[1, 2],
                  cat_A_continue_frmt[1, 2],
                  cat_A_concours_frmt[1, 2],
                  cat_A_bilan_frmt[1, 2],
                  cat_A_VAE_frmt[1, 2],
                  cat_A_form_pro_frmt[1, 2],
                  cat_A_profess_frmt[1, 2]
                ),
                sum(
                  cat_B_statuaire_frmt[1, 2],
                  cat_B_continue_frmt[1, 2],
                  cat_B_concours_frmt[1, 2],
                  cat_B_bilan_frmt[1, 2],
                  cat_B_VAE_frmt[1, 2],
                  cat_B_form_pro_frmt[1, 2],
                  cat_B_profess_frmt[1, 2]
                ),
                sum(
                  cat_C_statuaire_frmt[1, 2],
                  cat_C_continue_frmt[1, 2],
                  cat_C_concours_frmt[1, 2],
                  cat_C_bilan_frmt[1, 2],
                  cat_C_VAE_frmt[1, 2],
                  cat_C_form_pro_frmt[1, 2],
                  cat_C_profess_frmt[1, 2]
                )
              )
            ) %>%
              
              bind_rows(
                data.frame(
                  row.names = "Ensemble",
                  "Femmes" = colSums(.)[1],
                  "Hommes" = colSums(.)[2]
                )
              ) %>% mutate(Total =  rowSums(.)) ,
            
            "Agent formés: type de formation & genre" = data.frame(
              row.names = c(
                "Formation statuaire",
                "Formation continue",
                "Préparation aux concours et examens",
                "Réalisation de bilan de compétence",
                "Validation d'acquis d'expérience",
                "Congé de formation professionnelle",
                "Période de professionnalisation"
              ),
              "Femmes" = c(
                sum(
                  cat_A_statuaire_frmt[1, 1],
                  cat_B_statuaire_frmt[1, 1],
                  cat_C_statuaire_frmt[1, 1]
                ),
                sum(
                  cat_A_continue_frmt[1, 1],
                  cat_B_continue_frmt[1, 1],
                  cat_C_continue_frmt[1, 1]
                ),
                sum(
                  cat_A_concours_frmt[1, 1],
                  cat_B_concours_frmt[1, 1],
                  cat_C_concours_frmt[1, 1]
                ),
                sum(
                  cat_A_bilan_frmt[1, 1],
                  cat_B_bilan_frmt[1, 1],
                  cat_C_bilan_frmt[1, 1]
                ),
                sum(cat_A_VAE_frmt[1, 1], cat_B_VAE_frmt[1, 1], cat_C_VAE_frmt[1, 1]),
                sum(
                  cat_A_form_pro_frmt[1, 1],
                  cat_B_form_pro_frmt[1, 1],
                  cat_C_form_pro_frmt[1, 1]
                ),
                sum(
                  cat_A_profess_frmt[1, 1],
                  cat_B_profess_frmt[1, 1],
                  cat_C_profess_frmt[1, 1]
                )
              ),
              
              "Hommes" = c(
                sum(
                  cat_A_statuaire_frmt[1, 2],
                  cat_B_statuaire_frmt[1, 2],
                  cat_C_statuaire_frmt[1, 2]
                ),
                sum(
                  cat_A_continue_frmt[1, 2],
                  cat_B_continue_frmt[1, 2],
                  cat_C_continue_frmt[1, 2]
                ),
                sum(
                  cat_A_concours_frmt[1, 2],
                  cat_B_concours_frmt[1, 2],
                  cat_C_concours_frmt[1, 2]
                ),
                sum(
                  cat_A_bilan_frmt[1, 2],
                  cat_B_bilan_frmt[1, 2],
                  cat_C_bilan_frmt[1, 2]
                ),
                sum(cat_A_VAE_frmt[1, 2], cat_B_VAE_frmt[1, 2], cat_C_VAE_frmt[1, 2]),
                sum(
                  cat_A_form_pro_frmt[1, 2],
                  cat_B_form_pro_frmt[1, 2],
                  cat_C_form_pro_frmt[1, 2]
                ),
                sum(
                  cat_A_profess_frmt[1, 2],
                  cat_B_profess_frmt[1, 2],
                  cat_C_profess_frmt[1, 2]
                )
              )
            ) %>%
              
              bind_rows(
                data.frame(
                  row.names = "Ensemble",
                  "Femmes" = colSums(.)[1],
                  "Hommes" = colSums(.)[2]
                )
              ) %>% mutate(Total =  rowSums(.)),
            
            "Agent formés: type de formation & catégorie" = data.frame(
              row.names = c(
                "Formation statuaire",
                "Formation continue",
                "Préparation aux concours et examens",
                "Réalisation de bilan de compétence",
                "Validation d'acquis d'expérience",
                "Congé de formation professionnelle",
                "Période de professionnalisation"
              ),
              "Catégorie_A" = c(
                sum(cat_A_statuaire_frmt[1, 1], cat_A_statuaire_frmt[1, 2]),
                sum(cat_A_continue_frmt[1, 1], cat_A_continue_frmt[1, 2]),
                sum(cat_A_concours_frmt[1, 1], cat_A_concours_frmt[1, 2]),
                sum(cat_A_bilan_frmt[1, 1], cat_A_bilan_frmt[1, 2]),
                sum(cat_A_VAE_frmt[1, 1], cat_A_VAE_frmt[1, 2]),
                sum(cat_A_form_pro_frmt[1, 1], cat_A_form_pro_frmt[1, 2]),
                sum(cat_A_profess_frmt[1, 1], cat_A_profess_frmt[1, 2])
              ),
              
              "Catégorie_B" = c(
                sum(cat_B_statuaire_frmt[1, 1], cat_B_statuaire_frmt[1, 2]),
                sum(cat_B_continue_frmt[1, 1], cat_B_continue_frmt[1, 2]),
                sum(cat_B_concours_frmt[1, 1], cat_B_concours_frmt[1, 2]),
                sum(cat_B_bilan_frmt[1, 1], cat_B_bilan_frmt[1, 2]),
                sum(cat_B_VAE_frmt[1, 1], cat_B_VAE_frmt[1, 2]),
                sum(cat_B_form_pro_frmt[1, 1], cat_B_form_pro_frmt[1, 2]),
                sum(cat_B_profess_frmt[1, 1], cat_B_profess_frmt[1, 2])
              ),
              
              "Catégorie_C" = c(
                sum(cat_C_statuaire_frmt[1, 1], cat_C_statuaire_frmt[1, 2]),
                sum(cat_C_continue_frmt[1, 1], cat_C_continue_frmt[1, 2]),
                sum(cat_C_concours_frmt[1, 1], cat_C_concours_frmt[1, 2]),
                sum(cat_C_bilan_frmt[1, 1], cat_C_bilan_frmt[1, 2]),
                sum(cat_C_VAE_frmt[1, 1], cat_C_VAE_frmt[1, 2]),
                sum(cat_C_form_pro_frmt[1, 1], cat_C_form_pro_frmt[1, 2]),
                sum(cat_C_profess_frmt[1, 1], cat_C_profess_frmt[1, 2])
              )
            ) %>%
              
              bind_rows(
                data.frame(
                  row.names = "Ensemble",
                  "Catégorie_A" = colSums(.)[1],
                  "Catégorie_B" = colSums(.)[2],
                  "Catégorie_C" = colSums(.)[3]
                )
              ) %>% mutate(Total =  rowSums(.)) ,
            
            
            
            "Demandes de congés formation: catégorie & décision prise" = data.frame(
              row.names = c("Titulaires", "Contractuel"),
              "Acceptées" = c(tit_dem_acpt_frmt, contr_dem_acpt_frmt),
              
              "Refusées" = c(tit_dem_ref_frmt, contr_dem_ref_frmt)
            ) %>%
              
              bind_rows(
                data.frame(
                  row.names = "Ensemble",
                  "Acceptées" = colSums(.)[1],
                  "Refusées" = colSums(.)[2]
                )
              ) %>% mutate(Total =  rowSums(.)),
            
            "Nbre de cadres intermédiaire issus de la DDI: genre & catégories" = data.frame(
              row.names = c("Catégorie A", "Catégorie B"),
              "Femmes" = c(encadr_femme_cat_A_frmt, encadr_femme_cat_B_frmt),
              
              "Hommes" = c(encadr_homme_cat_A_frmt, encadr_homme_cat_B_frmt)
            ) %>%
              
              bind_rows(
                data.frame(
                  row.names = "Ensemble",
                  "Femmes" = colSums(.)[1],
                  "Hommes" = colSums(.)[2]
                )
              ) %>% mutate(Total =  rowSums(.)),
            
            
            
            "Réponses qualitatives" = data_rsu$tranche_formation %>% select_if( ~ is.character(.) &
                                                                                  !(is.na(.))) %>% t() %>%
              data.frame() %>% rename(Reponse = names(.))
            
          )
        }, error = function(e) {
          showNotification(
            "Pas de données disponible pour cette tranche!!!",
            type = "warning",
            duration = 5
          )
        })
        
      })
      
      
      
      output$vent_crois_formation = DT::renderDT({
        #Affichage de la tranche de table si l'utilisateur coche la case "Tableau ... défaut"
        
        tryCatch({
          if (is.null(input$vent_crois_btn_formation) != T &
              input$sous_theme_formation != "Informations supplémentaires") {
            table_classic(ma_table = tables_formation_defaut())
          } else{
            if (input$vent_crois_btn_formation != T) {
              table_manuelle(ma_table = tables_formation_defaut())
            }
          }
          
        }, error = function(e) {
          
        })
        
      })
      
      ##---------------------> Fin de conception des tableaux par défauts de la formation
      
      ##-----------------------------------> Affichage graphique de formation
      
      plot_formation_dataset = reactive({
        if (input$vent_crois_formation == "Agent formés par type de formation") {
          if (input$plot_formation == "Histogramme superposé (tableau croisé uniquement)") {
            showNotification(
              "NB: L'histogramme superposé n'offre pas une visualisation adapté aux tableaux ventilés !!!",
              type = "error",
              duration = 5
            )
          }
          
          data_rsu$tables_formation_hist = tables_formation_defaut() %>% slice(-nrow(.)) %>% mutate(value = Effectif, names = rownames(.)) %>% filter(Effectif !=
                                                                                                                                                        0)    # Création de la table à utilisé pour représenter l'histogramme conformément aux attendus de plotly
          data_rsu$tables_formation_cam = tables_formation_defaut() %>%  slice(-nrow(.)) %>% mutate(value = Effectif, names = rownames(.)) %>% filter(Effectif !=
                                                                                                                                                        0)   # Création de la table à utilisé pour représenter le camembert conformément aux attendus de plotly
          
          data_rsu$val_type_plot_formation_1 = input$val_type_plot_formation_1
          
          return(switch(
            input$plot_formation,
            "Camembert" = cam(
              titre = input$plot_title_formation_1,
              ma_data = data_rsu$tables_formation_cam,
              type_aff = data_rsu$val_type_plot_formation_1,
              couleur = input$colors_plot_formation_1
            ),
            
            "Histogramme" = histo(
              titre = input$plot_title_formation_1,
              ma_data = data_rsu$tables_formation_hist,
              type_aff =  data_rsu$val_type_plot_formation_1,
              couleur = input$colors_plot_formation_1,
              axe_1 = input$plot_axe_formation_1_1,
              axe_2 = input$plot_axe_formation_1_2
            )
          ))
        } else{
          if (input$vent_crois_formation == "Agent formés: type de formation & catégorie") {
            if (input$plot_emploi == "Camembert") {
              showNotification(
                "NB: Le camembert n'offre pas une visualisation adapté aux tableaux croisés !!!",
                type = "error",
                duration = 5
              )
            }
            
            data_rsu$tables_formation_hist = tables_formation_defaut() %>% slice(-nrow(.)) %>% mutate(names = rownames(.)) %>% filter(Total !=
                                                                                                                                        0)
            names(data_rsu$tables_formation_hist) = gsub("\\.", " ", names(data_rsu$tables_formation_hist))
            
            data_rsu$val_type_plot_formation_1 = input$val_type_plot_formation_1
            
            return(switch(
              input$plot_formation,
              "Histogramme" = histo_crois_3(
                titre = input$plot_title_formation_1,
                ma_data = data_rsu$tables_formation_hist,
                type_aff =  data_rsu$val_type_plot_formation_1,
                couleur = input$colors_plot_formation_1,
                axe_1 = input$plot_axe_formation_1_1,
                axe_2 = input$plot_axe_formation_1_2
              ),
              
              "Histogramme superposé (tableau croisé uniquement)" = histo_crois_3(
                titre = input$plot_title_formation_1,
                ma_data = data_rsu$tables_formation_hist,
                type_aff =  data_rsu$val_type_plot_formation_1,
                couleur = input$colors_plot_formation_1,
                axe_1 = input$plot_axe_formation_1_1,
                axe_2 = input$plot_axe_formation_1_2,
                sup = 'stack'
              )
            ))
          } else{
            if (input$plot_emploi == "Camembert") {
              showNotification(
                "NB: Le camembert n'offre pas une visualisation adapté aux tableaux croisés !!!",
                type = "error",
                duration = 5
              )
            }
            
            
            data_rsu$tables_formation_hist = tables_formation_defaut() %>% slice(-nrow(.)) %>% mutate(names = rownames(.)) %>% filter(Total !=
                                                                                                                                        0)
            names(data_rsu$tables_formation_hist) = gsub("\\.", " ", names(data_rsu$tables_formation_hist))
            
            data_rsu$val_type_plot_formation_1 = input$val_type_plot_formation_1
            
            return(switch(
              input$plot_formation,
              "Histogramme" = histo_crois_2(
                titre = input$plot_title_formation_1,
                ma_data = data_rsu$tables_formation_hist,
                type_aff =  data_rsu$val_type_plot_formation_1,
                couleur = input$colors_plot_formation_1,
                axe_1 = input$plot_axe_formation_1_1,
                axe_2 = input$plot_axe_formation_1_2
              ),
              
              "Histogramme superposé (tableau croisé uniquement)" = histo_crois_2(
                titre = input$plot_title_formation_1,
                ma_data = data_rsu$tables_formation_hist,
                type_aff =  data_rsu$val_type_plot_formation_1,
                couleur = input$colors_plot_formation_1,
                axe_1 = input$plot_axe_formation_1_1,
                axe_2 = input$plot_axe_formation_1_2,
                sup = 'stack'
              )
            ))
          }
        }
        
      })
      
      
      
      
      #------> Sorties affichage des graphiques
      output$plot_vent_crois_formation = renderPlotly({
        if (is.null(input$vent_crois_btn_formation) != T) {
          tryCatch({
            plot_formation_dataset()
          }, error = function(e) {
            
          })
        }
      })
      
      
      
    }, error = function(e) {
      showNotification("Valider d'abord votre partition",
                       type = "error",
                       duration = 5)
    })
    
    
  })
  
  
  ## ------------------------------------------Code de conception des tables & graphes manuelles  de la formation---------------------------------------------------
  
  output$manuelle_vent_formation = DT::renderDT({
    if (is.null(input$manuelle_btn_formation) != T) {
      # Affichage de la table si la case  " Tableau composé manuellement" est coché
      
      tryCatch({
        tranche_manuelle_formation = data_rsu$tranche_formation %>%    # Constitution de la table issue de la séléction manuelle
          select(input$select_manuelle_vent_formation) %>% t() %>%
          data.frame() %>%
          rename(Effectif = names(.))
        
        table_manuelle(ma_table = tranche_manuelle_formation)
        
      }, error = function(e) {
        
      })
    }
  })
  
  ##--------> représentation des tableaux ventilés: graphes 2 sous table  mouvement du perso crée manuellement
  
  plot_formation_manuelle_dataset = reactive({
    tranche_manuelle_formation = data_rsu$tranche_formation %>%
      select(input$select_manuelle_vent_formation) %>% t() %>% data.frame() %>%
      rename(Effectif = names(.)) %>%
      mutate(names = gsub("\\.", " ", sub("[0-9]*- ", "", rownames(.))))
    
    tranche_manuelle_formation_plot = tranche_manuelle_formation %>% filter(Effectif !=
                                                                              0)
    
    #--------> fonction switch permettant d'avoir des rendu au relatif aux choix graphiques faites par l'utilisateur
    observeEvent(input$select_manuelle_vent_formation, {
      updateTextInput(session,
                      "legend_plot_formation",
                      value = paste(gsub(
                        "\\.", " ", sub(
                          "[0-9]*- ",
                          "",
                          rownames(tranche_manuelle_formation_plot)
                        )
                      ), sep = ",")) # mise à jour des colonnes à séléctionner pour constitué les légendes
    })
    
    rownames(tranche_manuelle_formation_plot) = input$legend_plot_formation %>% strsplit(., split = ",") %>% unlist() %>% as.vector() # Mise à jour de la légende à travers le nom des lignes
    data_rsu$val_type_plot_formation_2 = input$val_type_plot_formation_2
    
    
    if (is.null(c(
      input$select_manuelle_vent_formation,
      input$manuelle_btn_formation
    )) != T   &
    input$plot_formation == "Histogramme superposé (tableau croisé uniquement)") {
      showNotification(
        "NB: L'histogramme superposé n'offre pas une visualisation adapté aux tableaux ventilés !!!",
        type = "error",
        duration = 5
      )
    }
    
    switch(
      input$plot_formation,
      "Camembert" = cam(
        titre = input$plot_title_formation_2,
        ma_data = tranche_manuelle_formation_plot,
        type_aff = data_rsu$val_type_plot_formation_2,
        couleur = input$colors_plot_formation_2,
        noms_legende = input$legend_plot_formation %>% strsplit(., split = ",") %>% unlist() %>% as.vector()
      ),
      "Histogramme" = histo(
        titre = input$plot_title_formation_2,
        ma_data = tranche_manuelle_formation_plot,
        type_aff = data_rsu$val_type_plot_formation_2,
        couleur = input$colors_plot_formation_2,
        noms_legende = input$legend_plot_formation %>% strsplit(., split = ",") %>% unlist() %>% as.vector()
        ,
        axe_1 = input$plot_axe_formation_2_1,
        axe_2 = input$plot_axe_formation_2_2
      )
      
    )
  })
  
  #--------->  Graphiques associés aux tables manuelles
  
  output$plot_formation_manuelle =  renderPlotly({
    if (is.null(input$manuelle_btn_formation) != T) {
      tryCatch({
        plot_formation_manuelle_dataset()
      }, error = function(e) {
        
      })
    }
  })
  
  ## -------------------------------------------Fin Code de conception des tables & graphes manuelles de la formation---------------------------------------------------
  
  
  
  
  
  
  ## -------------------------------------------Code de conception des tables & graphes du mouvement du personnel  ---------------------------------------------------
  
  
  sous_theme_mouv_perso_dataset = reactive({
    #Initialisation des sous-thèmes abordés
    switch(
      input$sous_theme_mouv_perso,
      "A- Arrivées" = c(
        "Motifs d'embauche des titulaires",
        "Motifs d'embauche des contractuels",
        "Catégories & motifs d'embauche des titulaires",
        "Catégories & motifs d'embauche des contractuels",
        "Statuts d'emploi & catégories des embauches",
        "Genres & catégories des embauches"
      ),
      
      "B- Départs" = c(
        "Motifs de départ des titulaires",
        "Motifs de départ des contractuels",
        "Catégories & motifs de départ des titulaires",
        "Catégories & motifs de départ des contractuels",
        "Statuts d'emploi & catégories des départs",
        "Genres & catégories des départs"
      ),
      "Informations supplémentaires" = c("Réponses qualitatives")
    )
  })
  
  observe({
    updateSelectInput(session, "vent_crois_mouv_perso", choices = sous_theme_mouv_perso_dataset()) # mise à jour des colonnes à séléctionner pour sélectionner le sous thème étudier dans le parcours professionnel
  })
  
  #Recherche des colonnes par recherche sur les mots clées
  
  
  observeEvent(input$vent_crois_btn_mouv_perso, {
    tryCatch({
      ### -----> cases de tableau croisé infos cases
      
      arrivee_concours_titulaire_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Titulaires",
          "Entrées",
          "Catégorie A",
          "concours",
          "école"
        )
      )
      
      arrivee_handi_titulaire_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Titulaires",
          "Entrées",
          "Catégorie A",
          "situation",
          "handicap"
        )
      )
      
      arrivee_emploi_titulaire_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Titulaires",
          "Entrées",
          "Catégorie A",
          "Emplois",
          "réservés"
        )
      )
      
      arrivee_dispo_titulaire_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Titulaires",
          "Entrées",
          "Catégorie A",
          "mise",
          "disposition"
        )
      )
      
      arrivee_detache_titulaire_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Titulaires", "Entrées", "Catégorie A", "détachement")
      )
      
      arrivee_mobilite_titulaire_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Titulaires", "Entrées", "Catégorie A", "mobilité")
      )
      
      arrivee_PACTE_titulaire_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Titulaires", "Entrées", "Catégorie A", "voie", "PACTE")
      )
      
      #-------------------------------------------------------------------------------------------------------------------
      
      arrivee_concours_titulaire_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Titulaires",
          "Entrées",
          "Catégorie B",
          "concours",
          "école"
        )
      )
      
      arrivee_handi_titulaire_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Titulaires",
          "Entrées",
          "Catégorie B",
          "situation",
          "handicap"
        )
      )
      
      arrivee_emploi_titulaire_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Titulaires",
          "Entrées",
          "Catégorie B",
          "Emplois",
          "réservés"
        )
      )
      
      arrivee_dispo_titulaire_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Titulaires",
          "Entrées",
          "Catégorie B",
          "mise",
          "disposition"
        )
      )
      
      arrivee_detache_titulaire_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Titulaires", "Entrées", "Catégorie B", "détachement")
      )
      
      arrivee_mobilite_titulaire_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Titulaires", "Entrées", "Catégorie B", "mobilité")
      )
      
      arrivee_PACTE_titulaire_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Titulaires", "Entrées", "Catégorie B", "voie", "PACTE")
      )
      
      #-------------------------------------------------------------------------------------------------------------------------
      
      arrivee_concours_titulaire_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Titulaires",
          "Entrées",
          "Catégorie C",
          "concours",
          "école"
        )
      )
      
      arrivee_handi_titulaire_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Titulaires",
          "Entrées",
          "Catégorie C",
          "situation",
          "handicap"
        )
      )
      
      arrivee_emploi_titulaire_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Titulaires",
          "Entrées",
          "Catégorie C",
          "Emplois",
          "réservés"
        )
      )
      
      arrivee_dispo_titulaire_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Titulaires",
          "Entrées",
          "Catégorie C",
          "mise",
          "disposition"
        )
      )
      
      arrivee_detache_titulaire_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Titulaires", "Entrées", "Catégorie C", "détachement")
      )
      
      arrivee_mobilite_titulaire_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Titulaires", "Entrées", "Catégorie C", "mobilité")
      )
      
      arrivee_PACTE_titulaire_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Titulaires", "Entrées", "Catégorie C", "voie", "PACTE")
      )
      
      
      #-----> cases de tableau croisé motif & catégorie contractuelle
      
      arrivee_bp_contractuel_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Contractuel",
          "Entrées",
          "Catégorie A",
          "besoin permanent"
        )
      )
      
      arrivee_bnp_contractuel_cat_A  = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Contractuel",
          "Entrées",
          "Catégorie A",
          "besoin",
          "non",
          "permanent"
        )
      )
      
      
      arrivee_bp_contractuel_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Contractuel",
          "Entrées",
          "Catégorie B",
          "besoin permanent"
        )
      )
      
      arrivee_bnp_contractuel_cat_B  = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Contractuel",
          "Entrées",
          "Catégorie B",
          "besoin",
          "non",
          "permanent"
        )
      )
      
      
      arrivee_bp_contractuel_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Contractuel",
          "Entrées",
          "Catégorie C",
          "besoin permanent"
        )
      )
      
      arrivee_bnp_contractuel_cat_C  = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Contractuel",
          "Entrées",
          "Catégorie C",
          "besoin",
          "non",
          "permanent"
        )
      )
      
      
      
      #-----> cases de tableau croisé statut & catégorie
      
      arrivee_contractuel_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Contractuel", "Entrées", "Catégorie A")
      )
      
      arrivee_contractuel_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Contractuel", "Entrées", "Catégorie B")
      )
      
      arrivee_contractuel_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Contractuel", "Entrées", "Catégorie C")
      )
      
      #--------------------------------------------------------------------------
      
      arrivee_titulaire_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Titulaires", "Entrées", "Catégorie A")
      )
      
      arrivee_titulaire_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Titulaires", "Entrées", "Catégorie B")
      )
      
      arrivee_titulaire_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Titulaires", "Entrées", "Catégorie C")
      )
      
      #----------------> case de tableau catégorie  et sexe
      
      arrivee_hommes_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Hommes", "Entrées", "Catégorie A")
      )
      
      arrivee_hommes_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Hommes", "Entrées", "Catégorie B")
      )
      
      arrivee_hommes_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Hommes", "Entrées", "Catégorie C")
      )
      
      arrivee_apprentis_hommes = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Contractuel", "Hommes", "Entrées", "apprent", "recruté")
      )
      
      
      #--------------------------------------------------------------------------
      
      arrivee_femmes_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Femmes", "Entrées", "Catégorie A")
      )
      
      arrivee_femmes_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Femmes", "Entrées", "Catégorie B")
      )
      
      arrivee_femmes_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Femmes", "Entrées", "Catégorie C")
      )
      
      arrivee_apprenties_femmes = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Contractuel", "Femmes", "Entrées", "Apprent", "recruté")
      )
      
      
      ### -----> cases de tableau croisé infos cases : départ titulaires
      
      depart_retraire_titulaire_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Titulaires", "Sorties", "Catégorie A", "retraite")
      )
      
      depart_deces_titulaire_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Titulaires", "Sorties", "Catégorie A", "décès")
      )
      
      depart_demission_abandon_titulaire_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Titulaires",
          "Sorties",
          "Catégorie A",
          "démission",
          "abandon",
          "poste"
        )
      )
      
      depart_volontaire_titulaire_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Titulaires", "Sorties", "Catégorie A", "volontaire")
      )
      
      depart_concours_titulaire_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Titulaires", "Sorties", "Catégorie A", "concours")
      )
      
      depart_fin_detach_titulaire_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Titulaires",
          "Sorties",
          "Catégorie A",
          "fin",
          "détachement"
        )
      )
      
      depart_mobilite_titulaire_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Titulaires", "Sorties", "Catégorie A", "mobilité")
      )
      
      depart_inapt_titulaire_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Titulaires",
          "Sorties",
          "Catégorie A",
          "inaptitude",
          "physique"
        )
      )
      
      #---------------------------------------------------------------------
      
      depart_retraire_titulaire_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Titulaires", "Sorties", "Catégorie B", "retraite")
      )
      
      depart_deces_titulaire_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Titulaires", "Sorties", "Catégorie B", "décès")
      )
      
      depart_demission_abandon_titulaire_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Titulaires",
          "Sorties",
          "Catégorie B",
          "démission",
          "abandon",
          "poste"
        )
      )
      
      depart_volontaire_titulaire_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Titulaires", "Sorties", "Catégorie B", "volontaire")
      )
      
      depart_concours_titulaire_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Titulaires", "Sorties", "Catégorie B", "concours")
      )
      
      depart_fin_detach_titulaire_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Titulaires",
          "Sorties",
          "Catégorie B",
          "fin",
          "détachement"
        )
      )
      
      depart_mobilite_titulaire_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Titulaires", "Sorties", "Catégorie B", "mobilité")
      )
      
      depart_inapt_titulaire_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Titulaires",
          "Sorties",
          "Catégorie B",
          "inaptitude",
          "physique"
        )
      )
      
      #--------------------------------------------------------------------
      
      depart_retraire_titulaire_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Titulaires", "Sorties", "Catégorie A", "retraite")
      )
      
      depart_deces_titulaire_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Titulaires", "Sorties", "Catégorie A", "décès")
      )
      
      depart_demission_abandon_titulaire_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Titulaires",
          "Sorties",
          "Catégorie A",
          "démission",
          "abandon",
          "poste"
        )
      )
      
      depart_volontaire_titulaire_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Titulaires", "Sorties", "Catégorie A", "volontaire")
      )
      
      depart_concours_titulaire_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Titulaires", "Sorties", "Catégorie A", "concours")
      )
      
      depart_fin_detach_titulaire_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Titulaires",
          "Sorties",
          "Catégorie A",
          "fin",
          "détachement"
        )
      )
      
      depart_mobilite_titulaire_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Titulaires", "Sorties", "Catégorie A", "mobilité")
      )
      
      depart_inapt_titulaire_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Titulaires",
          "Sorties",
          "Catégorie A",
          "inaptitude",
          "physique"
        )
      )
      
      
      
      ##--------------------------------------------Départ contractuel
      
      depart_retraire_contractuel_bp_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Contractuel", "Sorties", "Catégorie A", "retraite")
      )
      
      depart_deces_contractuel_bp_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Contractuel", "Sorties", "Catégorie A", "décès")
      )
      
      depart_demission_contractuel_bp_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Contractuel",
          "Sorties",
          "Catégorie A",
          "démission",
          "abandon",
          "poste"
        )
      )
      
      depart_volontaire_contractuel_bp_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Contractuel", "Sorties", "Catégorie A", "volontaire")
      )
      
      depart_licenciement_contractuel_bp_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Contractuel", "Sorties", "Catégorie A", "licenciement")
      )
      
      depart_concours_contractuel_bp_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Contractuel",
          "Sorties",
          "Catégorie A",
          "fin",
          "concours"
        )
      )
      
      depart_inapt_contractuel_cat_bp_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Contractuel",
          "Sorties",
          "Catégorie A",
          "inaptitude",
          "physique"
        )
      )
      
      depart_fin_contrat_contractuel_bp_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Contractuel", "Sorties", "Catégorie A", "fin", "contrat")
      )
      
      depart_fin_contrat_contractuel_bnp_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Contractuel",
          "Sorties",
          "Catégorie A",
          "Non",
          "Permanent",
          "fin",
          "contrat"
        )
      )
      
      depart_autres_causes_contractuel_bnp_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Contractuel",
          "Sorties",
          "Catégorie A",
          "Non",
          "Permanent",
          "autres",
          "causes"
        )
      )
      
      depart_fin_contrat_contractuel_bp_cat_A = depart_fin_contrat_contractuel_bp_cat_A - depart_fin_contrat_contractuel_bnp_cat_A
      
      
      ##--------------------------------------------------
      
      
      depart_retraire_contractuel_bp_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Contractuel", "Sorties", "Catégorie B", "retraite")
      )
      
      depart_deces_contractuel_bp_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Contractuel", "Sorties", "Catégorie B", "décès")
      )
      
      depart_demission_contractuel_bp_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Contractuel",
          "Sorties",
          "Catégorie B",
          "démission",
          "abandon",
          "poste"
        )
      )
      
      depart_volontaire_contractuel_bp_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Contractuel", "Sorties", "Catégorie B", "volontaire")
      )
      
      depart_licenciement_contractuel_bp_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Contractuel", "Sorties", "Catégorie B", "licenciement")
      )
      
      depart_concours_contractuel_bp_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Contractuel",
          "Sorties",
          "Catégorie B",
          "fin",
          "concours"
        )
      )
      
      depart_inapt_contractuel_bp_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Contractuel",
          "Sorties",
          "Catégorie B",
          "inaptitude",
          "physique"
        )
      )
      
      depart_fin_contrat_contractuel_bp_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Contractuel", "Sorties", "Catégorie B", "fin", "contrat")
      )
      
      depart_fin_contrat_contractuel_bnp_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Contractuel",
          "Sorties",
          "Catégorie B",
          "Non",
          "Permanent",
          "fin",
          "contrat"
        )
      )
      
      depart_autres_causes_contractuel_bnp_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Contractuel",
          "Sorties",
          "Catégorie B",
          "Non",
          "Permanent",
          "autres",
          "causes"
        )
      )
      
      depart_fin_contrat_contractuel_bp_cat_B = depart_fin_contrat_contractuel_bp_cat_B - depart_fin_contrat_contractuel_bnp_cat_B
      
      
      ##-----------------------------------------------------------------
      
      depart_retraire_contractuel_bp_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Contractuel", "Sorties", "Catégorie C", "retraite")
      )
      
      depart_deces_contractuel_bp_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Contractuel", "Sorties", "Catégorie C", "décès")
      )
      
      depart_demission_contractuel_bp_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Contractuel",
          "Sorties",
          "Catégorie C",
          "démission",
          "abandon",
          "poste"
        )
      )
      
      depart_volontaire_contractuel_bp_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Contractuel", "Sorties", "Catégorie C", "volontaire")
      )
      
      depart_licenciement_contractuel_bp_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Contractuel", "Sorties", "Catégorie C", "licenciement")
      )
      
      depart_concours_contractuel_bp_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Contractuel",
          "Sorties",
          "Catégorie C",
          "fin",
          "concours"
        )
      )
      
      depart_inapt_contractuel_bp_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Contractuel",
          "Sorties",
          "Catégorie C",
          "inaptitude",
          "physique"
        )
      )
      
      depart_fin_contrat_contractuel_bp_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Contractuel", "Sorties", "Catégorie C", "fin", "contrat")
      )
      
      depart_fin_contrat_contractuel_bnp_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Contractuel",
          "Sorties",
          "Catégorie C",
          "Non",
          "Permanent",
          "fin",
          "contrat"
        )
      )
      
      depart_autres_causes_contractuel_bnp_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c(
          "Contractuel",
          "Sorties",
          "Catégorie C",
          "Non",
          "Permanent",
          "autres",
          "causes"
        )
      )
      
      depart_fin_contrat_contractuel_bp_cat_C = depart_fin_contrat_contractuel_bp_cat_C - depart_fin_contrat_contractuel_bnp_cat_C
      
      
      
      #-----> cases de tableau croisé statut & catégorie   départs
      
      depart_contractuel_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Contractuel", "Sorties", "Catégorie A")
      )
      
      depart_contractuel_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Contractuel", "Sorties", "Catégorie B")
      )
      
      depart_contractuel_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Contractuel", "Sorties", "Catégorie C")
      )
      
      #--------------------------------------------------------------------------
      
      depart_titulaire_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Titulaires", "Sorties", "Catégorie A")
      )
      
      depart_titulaire_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Titulaires", "Sorties", "Catégorie B")
      )
      
      depart_titulaire_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Titulaires", "Sorties", "Catégorie C")
      )
      
      #----------------> case de tableau catégorie  et sexe départs
      
      depart_hommes_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Hommes", "Sorties", "Catégorie A")
      )
      
      depart_hommes_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Hommes", "Sorties", "Catégorie B")
      )
      
      depart_hommes_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Hommes", "Sorties", "Catégorie C")
      )
      
      #--------------------------------------------------------------------------
      
      depart_femmes_cat_A = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Femmes", "Sorties", "Catégorie A")
      )
      
      depart_femmes_cat_B = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Femmes", "Sorties", "Catégorie B")
      )
      
      depart_femmes_cat_C = colonne_match(
        data = data_rsu$tranche_mouv_perso,
        vecteurs_mots =  c("Femmes", "Sorties", "Catégorie C")
      )
      
      # Conception des objets tableau par défauts
      
      tables_mouv_perso_defaut = reactive({
        tryCatch({
          switch(
            input$vent_crois_mouv_perso,
            "Motifs d'embauche des titulaires" = data.frame(
              Effectif = c(
                sum(
                  arrivee_concours_titulaire_cat_A,
                  arrivee_concours_titulaire_cat_B,
                  arrivee_concours_titulaire_cat_C
                ),
                sum(
                  arrivee_handi_titulaire_cat_A,
                  arrivee_handi_titulaire_cat_B,
                  arrivee_handi_titulaire_cat_C
                ),
                sum(
                  arrivee_emploi_titulaire_cat_A,
                  arrivee_emploi_titulaire_cat_B,
                  arrivee_emploi_titulaire_cat_C
                ),
                
                sum(
                  arrivee_dispo_titulaire_cat_A,
                  arrivee_dispo_titulaire_cat_B,
                  arrivee_dispo_titulaire_cat_C
                ),
                sum(
                  arrivee_detache_titulaire_cat_A,
                  arrivee_detache_titulaire_cat_B,
                  arrivee_detache_titulaire_cat_C
                ),
                sum(
                  arrivee_mobilite_titulaire_cat_A,
                  arrivee_mobilite_titulaire_cat_B,
                  arrivee_mobilite_titulaire_cat_C
                ),
                sum(
                  arrivee_PACTE_titulaire_cat_A,
                  arrivee_PACTE_titulaire_cat_B,
                  arrivee_PACTE_titulaire_cat_C
                )
              ),
              row.names = c(
                "Concours & sortie d'école",
                "Handicap",
                "Emploi réservés",
                "Mise à disposition",
                "Détachement",
                "Mobilité",
                "PACTE"
              )
            ) %>%
              bind_rows(data.frame(
                row.names = "Total", Effectif = colSums(.)
              )),
            
            "Motifs de départ des titulaires" = data.frame(
              Effectif = c(
                sum(
                  depart_retraire_titulaire_cat_A,
                  depart_retraire_titulaire_cat_B,
                  depart_retraire_titulaire_cat_C
                ),
                sum(
                  depart_deces_titulaire_cat_A,
                  depart_deces_titulaire_cat_B,
                  depart_deces_titulaire_cat_C
                ),
                sum(
                  depart_demission_abandon_titulaire_cat_A,
                  depart_demission_abandon_titulaire_cat_B,
                  depart_demission_abandon_titulaire_cat_C
                ),
                
                sum(
                  depart_volontaire_titulaire_cat_A,
                  depart_volontaire_titulaire_cat_B,
                  depart_volontaire_titulaire_cat_C
                ),
                sum(
                  depart_concours_titulaire_cat_A,
                  depart_concours_titulaire_cat_B,
                  depart_concours_titulaire_cat_C
                ),
                sum(
                  depart_fin_detach_titulaire_cat_A,
                  depart_fin_detach_titulaire_cat_B,
                  depart_fin_detach_titulaire_cat_C
                ),
                
                sum(
                  depart_mobilite_titulaire_cat_A,
                  depart_mobilite_titulaire_cat_B,
                  depart_mobilite_titulaire_cat_C
                ),
                sum(
                  depart_inapt_titulaire_cat_A,
                  depart_inapt_titulaire_cat_B,
                  depart_inapt_titulaire_cat_C
                )
              ),
              row.names = c(
                "Retraite",
                "Décès",
                "Démission ou abandon",
                "Volontaire",
                "Concours",
                "Fin détachement",
                "Mobilité",
                "Inapt physique"
              )
            ) %>%
              bind_rows(data.frame(
                row.names = "Total", Effectif = colSums(.)
              )),
            
            
            
            "Motifs d'embauche des contractuels" = data.frame(
              Effectif = c(
                sum(
                  arrivee_bp_contractuel_cat_A,
                  arrivee_bp_contractuel_cat_B,
                  arrivee_bp_contractuel_cat_C
                ),
                sum(
                  arrivee_bnp_contractuel_cat_A,
                  arrivee_bnp_contractuel_cat_B,
                  arrivee_bnp_contractuel_cat_C
                )
              ),
              row.names = c("Besoin permanent", "Besoin non permanent")
            ) %>% bind_rows(data.frame(
              row.names = "Total", Effectif = colSums(.)
            )) ,
            
            "Motifs de départ des contractuels" = data.frame(
              Effectif = c(
                sum(
                  depart_retraire_contractuel_bp_cat_A,
                  depart_retraire_contractuel_bp_cat_B,
                  depart_retraire_contractuel_bp_cat_C
                ),
                sum(
                  depart_deces_contractuel_bp_cat_A,
                  depart_deces_contractuel_bp_cat_B,
                  depart_deces_contractuel_bp_cat_C
                ),
                sum(
                  depart_demission_contractuel_bp_cat_A,
                  depart_demission_contractuel_bp_cat_B,
                  depart_demission_contractuel_bp_cat_C
                ),
                
                sum(
                  depart_volontaire_contractuel_bp_cat_A,
                  depart_volontaire_contractuel_bp_cat_B,
                  depart_volontaire_contractuel_bp_cat_C
                ),
                sum(
                  depart_licenciement_contractuel_bp_cat_A,
                  depart_licenciement_contractuel_bp_cat_B,
                  depart_licenciement_contractuel_bp_cat_C
                ),
                sum(
                  depart_concours_contractuel_bp_cat_A,
                  depart_concours_contractuel_bp_cat_B,
                  depart_concours_contractuel_bp_cat_C
                ),
                
                sum(
                  depart_inapt_contractuel_cat_bp_A,
                  depart_inapt_contractuel_bp_cat_B,
                  depart_inapt_contractuel_bp_cat_C
                ),
                sum(
                  depart_fin_contrat_contractuel_bp_cat_A,
                  depart_fin_contrat_contractuel_bp_cat_B,
                  depart_fin_contrat_contractuel_bp_cat_C
                ),
                sum(
                  depart_fin_contrat_contractuel_bnp_cat_A,
                  depart_fin_contrat_contractuel_bnp_cat_B,
                  depart_fin_contrat_contractuel_bnp_cat_C
                ),
                sum(
                  depart_autres_causes_contractuel_bnp_cat_A,
                  depart_autres_causes_contractuel_bnp_cat_B,
                  depart_autres_causes_contractuel_bnp_cat_C
                )
              ),
              row.names = c(
                "Retraite bp",
                "Décès bp",
                "Démission ou abandon bp",
                "Volontaire bp",
                "Licenciement bp",
                "Concours bp",
                "Inapt physique bp",
                "Fin contrat bp",
                "Fin contrat bnp",
                "Autres causes bnp"
              )
            ) %>%
              bind_rows(data.frame(
                row.names = "Total", Effectif = colSums(.)
              )) ,
            
            
            
            "Catégories & motifs d'embauche des titulaires" = data.frame(
              row.names = c(
                "Concours & sortie d'école",
                "Handicap",
                "Emploi réservés",
                "Mise à disposition",
                "Détachement",
                "Mobilité",
                "PACTE"
              ),
              "Catégorie_A" = c(
                arrivee_concours_titulaire_cat_A,
                arrivee_handi_titulaire_cat_A,
                arrivee_emploi_titulaire_cat_A,
                arrivee_dispo_titulaire_cat_A,
                arrivee_detache_titulaire_cat_A,
                arrivee_mobilite_titulaire_cat_A,
                arrivee_PACTE_titulaire_cat_A
              ),
              
              "Catégorie_B" = c(
                arrivee_concours_titulaire_cat_B,
                arrivee_handi_titulaire_cat_B,
                arrivee_emploi_titulaire_cat_B,
                arrivee_dispo_titulaire_cat_B,
                arrivee_detache_titulaire_cat_B,
                arrivee_mobilite_titulaire_cat_B,
                arrivee_PACTE_titulaire_cat_B
              ),
              
              "Catégorie_C" = c(
                arrivee_concours_titulaire_cat_C,
                arrivee_handi_titulaire_cat_C,
                arrivee_emploi_titulaire_cat_C,
                arrivee_dispo_titulaire_cat_C,
                arrivee_detache_titulaire_cat_C,
                arrivee_mobilite_titulaire_cat_C,
                arrivee_PACTE_titulaire_cat_C
              )
            ) %>% bind_rows(
              data.frame(
                row.names = "Ensemble",
                "Catégorie_A" = colSums(.)[1],
                "Catégorie_B" = colSums(.)[2],
                "Catégorie_C" = colSums(.)[3]
              )
            ) %>% mutate(Total =  rowSums(.)) ,
            
            
            "Catégories & motifs de départ des titulaires" = data.frame(
              row.names = c(
                "Retraite",
                "Décès",
                "Démission ou abandon",
                "Volontaire",
                "Concours",
                "Fin détachement",
                "Mobilité",
                "Inapt physique"
              ),
              "Catégorie_A" = c(
                depart_retraire_titulaire_cat_A,
                depart_deces_titulaire_cat_A,
                depart_demission_abandon_titulaire_cat_A,
                depart_volontaire_titulaire_cat_A,
                depart_concours_titulaire_cat_A,
                depart_fin_detach_titulaire_cat_A,
                depart_mobilite_titulaire_cat_A,
                depart_inapt_titulaire_cat_A
              ),
              
              "Catégorie_B" = c(
                depart_retraire_titulaire_cat_B,
                depart_deces_titulaire_cat_B,
                depart_demission_abandon_titulaire_cat_B,
                depart_volontaire_titulaire_cat_B,
                depart_concours_titulaire_cat_B,
                depart_fin_detach_titulaire_cat_B,
                depart_mobilite_titulaire_cat_B,
                depart_inapt_titulaire_cat_B
              ),
              
              "Catégorie_C" = c(
                depart_retraire_titulaire_cat_C,
                depart_deces_titulaire_cat_C,
                depart_demission_abandon_titulaire_cat_C,
                depart_volontaire_titulaire_cat_C,
                depart_concours_titulaire_cat_C,
                depart_fin_detach_titulaire_cat_C,
                depart_mobilite_titulaire_cat_C,
                depart_inapt_titulaire_cat_C
              )
            ) %>%
              bind_rows(
                data.frame(
                  row.names = "Ensemble",
                  "Catégorie_A" = colSums(.)[1],
                  "Catégorie_B" = colSums(.)[2],
                  "Catégorie_C" = colSums(.)[3]
                )
              ) %>% mutate(Total =  rowSums(.)) ,
            
            
            
            "Catégories & motifs d'embauche des contractuels" = data.frame(
              row.names = c("Besoins permanents", "Besoins non permanents"),
              "Catégorie_A" = c(
                arrivee_bp_contractuel_cat_A,
                arrivee_bnp_contractuel_cat_A
              ),
              "Catégorie_B" = c(
                arrivee_bp_contractuel_cat_B,
                arrivee_bnp_contractuel_cat_B
              ),
              "Catégorie_C" = c(
                arrivee_bp_contractuel_cat_C,
                arrivee_bnp_contractuel_cat_C
              )
            ) %>%
              bind_rows(
                data.frame(
                  row.names = "Ensemble",
                  "Catégorie_A" = colSums(.)[1],
                  "Catégorie_B" = colSums(.)[2],
                  "Catégorie_C" = colSums(.)[3]
                )
              ) %>% mutate(Total =  rowSums(.)) ,
            
            
            "Catégories & motifs de départ des contractuels" = data.frame(
              row.names = c(
                "Retraites bp",
                "Décès bp*",
                "Démissions ou abandons bp*",
                "Volontaires bp*",
                "Licenciements bp",
                "Concours bp",
                "Inapts physiques bp",
                "Fin de contrat bp*",
                "Fin de contrat bnp*",
                "Autres causes bnp*"
              ),
              "Catégorie_A" = c(
                depart_retraire_contractuel_bp_cat_A,
                depart_deces_contractuel_bp_cat_A,
                depart_demission_contractuel_bp_cat_A,
                depart_volontaire_contractuel_bp_cat_A,
                depart_licenciement_contractuel_bp_cat_A,
                depart_concours_contractuel_bp_cat_A,
                depart_inapt_contractuel_cat_bp_A,
                depart_fin_contrat_contractuel_bp_cat_A,
                depart_fin_contrat_contractuel_bnp_cat_A,
                depart_autres_causes_contractuel_bnp_cat_A
              ),
              
              "Catégorie_B" = c(
                depart_retraire_contractuel_bp_cat_B,
                depart_deces_contractuel_bp_cat_B,
                depart_demission_contractuel_bp_cat_B,
                depart_volontaire_contractuel_bp_cat_B,
                depart_licenciement_contractuel_bp_cat_B,
                depart_concours_contractuel_bp_cat_B,
                depart_inapt_contractuel_cat_bp_A,
                depart_fin_contrat_contractuel_bp_cat_B,
                depart_fin_contrat_contractuel_bnp_cat_B,
                depart_autres_causes_contractuel_bnp_cat_B
              ),
              
              "Catégorie_C" = c(
                depart_retraire_contractuel_bp_cat_C,
                depart_deces_contractuel_bp_cat_C,
                depart_demission_contractuel_bp_cat_C,
                depart_volontaire_contractuel_bp_cat_C,
                depart_licenciement_contractuel_bp_cat_C,
                depart_concours_contractuel_bp_cat_C,
                depart_inapt_contractuel_cat_bp_A,
                depart_fin_contrat_contractuel_bp_cat_C,
                depart_fin_contrat_contractuel_bnp_cat_C,
                depart_autres_causes_contractuel_bnp_cat_C
              )
            ) %>% bind_rows(
              data.frame(
                row.names = "Ensemble",
                "Catégorie_A" = colSums(.)[1],
                "Catégorie_B" = colSums(.)[2],
                "Catégorie_C" = colSums(.)[3]
              )
            ) %>% mutate(Total =  rowSums(.)) ,
            
            
            
            "Statuts d'emploi & catégories des embauches" = data.frame(
              row.names = c("Contractuels", "Titulaires"),
              "Catégorie_A" = c(
                arrivee_contractuel_cat_A,
                arrivee_titulaire_cat_A
              ),
              "Catégorie_B" = c(
                arrivee_contractuel_cat_B,
                arrivee_titulaire_cat_B
              ),
              "Catégorie_C" = c(
                arrivee_contractuel_cat_C,
                arrivee_titulaire_cat_C
              )
            ) %>% bind_rows(
              data.frame(
                row.names = "Ensemble",
                "Catégorie_A" = colSums(.)[1],
                "Catégorie_B" = colSums(.)[2],
                "Catégorie_C" = colSums(.)[3]
              )
            ) %>% mutate(Total =  rowSums(.)),
            
            "Statuts d'emploi & catégories des départs" = data.frame(
              row.names = c("Contractuels", "Titulaires"),
              "Catégorie_A" = c(depart_contractuel_cat_A, depart_titulaire_cat_A),
              "Catégorie_B" = c(depart_contractuel_cat_B, depart_titulaire_cat_B),
              "Catégorie_C" = c(depart_contractuel_cat_C, depart_titulaire_cat_C)
            ) %>%
              bind_rows(
                data.frame(
                  row.names = "Ensemble",
                  "Catégorie_A" = colSums(.)[1],
                  "Catégorie_B" = colSums(.)[2],
                  "Catégorie_C" = colSums(.)[3]
                )
              ) %>% mutate(Total =  rowSums(.)),
            
            "Genres & catégories des embauches" = data.frame(
              row.names = c(
                "Catégorie A",
                "Catégorie B",
                "Catégorie C",
                "Apprentissage"
              ),
              "Hommes" = c(
                arrivee_hommes_cat_A,
                arrivee_hommes_cat_B,
                arrivee_hommes_cat_C,
                arrivee_apprentis_hommes
              ),
              "Femmes" = c(
                arrivee_femmes_cat_A,
                arrivee_femmes_cat_B,
                arrivee_femmes_cat_C,
                arrivee_apprenties_femmes
              )
            ) %>% bind_rows(
              data.frame(
                row.names = "Ensemble",
                "Hommes" = colSums(.)[1],
                "Femmes" = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
            
            "Genres & catégories des départs" = data.frame(
              row.names = c("Catégorie A", "Catégorie B", "Catégorie C"),
              "Hommes" = c(
                depart_hommes_cat_A,
                depart_hommes_cat_B,
                depart_hommes_cat_C
              ),
              "Femmes" = c(
                depart_femmes_cat_A,
                depart_femmes_cat_B,
                depart_femmes_cat_C
              )
            ) %>%
              bind_rows(
                data.frame(
                  row.names = "Ensemble",
                  "Hommes" = colSums(.)[1],
                  "Femmes" = colSums(.)[2]
                )
              ) %>% mutate(Total =  rowSums(.)),
            
            "Réponses qualitatives" = data_rsu$tranche_mouv_perso %>% select_if( ~ is.character(.) &
                                                                                   !(is.na(.))) %>% t() %>%
              data.frame() %>% rename(Reponse = names(.))
            
          )
        }, error = function(e) {
          showNotification(
            "Pas de données disponible pour cette tranche!!!",
            type = "warning",
            duration = 5
          )
        })
        
      })
      
      
      output$vent_crois_mouv_perso = DT::renderDT({
        #Affichage de la tranche de table si l'utilisateur coche la case "Tableau ... défaut"
        
        tryCatch({
          if (is.null(input$vent_crois_btn_mouv_perso) != T &
              input$sous_theme_mouv_perso != "Informations supplémentaires") {
            table_classic(ma_table = tables_mouv_perso_defaut())
          } else{
            if (input$vent_crois_btn_mouv_perso != T) {
              table_manuelle(ma_table = tables_mouv_perso_defaut())
            }
          }
          
        }, error = function(e) {
          
        })
        
      })
      
      ##---------------------> Fin de conception des tableaux par défauts du mouvement du personnel
      
      
      ##-----------------------------------> Affichage graphique mouvement du personnel
      
      plot_mouv_perso_dataset = reactive({
        if (input$vent_crois_mouv_perso %in% c(
          "Motifs d'embauche des titulaires",
          "Motifs d'embauche des contractuels",
          "Motifs de départ des titulaires",
          "Motifs de départ des contractuels"
        )) {
          if (input$plot_mouv_perso == "Histogramme superposé (tableau croisé uniquement)") {
            showNotification(
              "NB: L'histogramme superposé n'offre pas une visualisation adapté aux tableaux ventilés !!!",
              type = "error",
              duration = 5
            )
          }
          
          data_rsu$tables_mouv_perso_hist = tables_mouv_perso_defaut() %>% slice(-nrow(.)) %>% mutate(value = Effectif, names = rownames(.)) %>% filter(Effectif !=
                                                                                                                                                          0)     # Création de la table à utilisé pour représenter l'histogramme conformément aux attendus de plotly
          data_rsu$tables_mouv_perso_cam = tables_mouv_perso_defaut() %>%  slice(-nrow(.)) %>% mutate(value = Effectif, names = rownames(.)) %>% filter(Effectif !=
                                                                                                                                                          0)    # Création de la table à utilisé pour représenter le camembert conformément aux attendus de plotly
          
          data_rsu$val_type_plot_mouv_perso_1 = input$val_type_plot_mouv_perso_1
          
          return(switch(
            input$plot_mouv_perso,
            "Camembert" = cam(
              titre = input$plot_title_mouv_perso_1,
              ma_data = data_rsu$tables_mouv_perso_cam,
              type_aff = data_rsu$val_type_plot_mouv_perso_1,
              couleur = input$colors_plot_mouv_perso_1
            ),
            
            "Histogramme" = histo(
              titre = input$plot_title_mouv_perso_1,
              ma_data = data_rsu$tables_mouv_perso_hist,
              type_aff =  data_rsu$val_type_plot_mouv_perso_1,
              couleur = input$colors_plot_mouv_perso_1,
              axe_1 = input$plot_axe_mouv_perso_1_1,
              axe_2 = input$plot_axe_mouv_perso_1_2
            )
          ))
        } else{
          if (input$vent_crois_mouv_perso %in% c("Genres & catégories des embauches",
                                                 "Genres & catégories des départs")) {
            if (input$plot_mouv_perso == "Camembert") {
              showNotification(
                "NB: Le camembert n'offre pas une visualisation adapté aux tableaux croisés !!!",
                type = "error",
                duration = 5
              )
            }
            
            
            data_rsu$tables_mouv_perso_hist = tables_mouv_perso_defaut() %>% slice(-nrow(.)) %>% mutate(names = rownames(.)) %>% filter(Total !=
                                                                                                                                          0)
            names(data_rsu$tables_mouv_perso_hist) = gsub("\\.",
                                                          " ",
                                                          names(data_rsu$tables_mouv_perso_hist))
            
            data_rsu$val_type_plot_mouv_perso_1 = input$val_type_plot_mouv_perso_1
            
            return(switch(
              input$plot_mouv_perso,
              "Histogramme" = histo_crois_2(
                titre = input$plot_title_mouv_perso_1,
                ma_data = data_rsu$tables_mouv_perso_hist,
                type_aff =  data_rsu$val_type_plot_mouv_perso_1,
                couleur = input$colors_plot_mouv_perso_1,
                axe_1 = input$plot_axe_mouv_perso_1_1,
                axe_2 = input$plot_axe_mouv_perso_1_2
              ),
              
              "Histogramme superposé (tableau croisé uniquement)" = histo_crois_2(
                titre = input$plot_title_mouv_perso_1,
                ma_data = data_rsu$tables_mouv_perso_hist,
                type_aff =  data_rsu$val_type_plot_mouv_perso_1,
                couleur = input$colors_plot_mouv_perso_1,
                axe_1 = input$plot_axe_mouv_perso_1_1,
                axe_2 = input$plot_axe_mouv_perso_1_2,
                sup = 'stack'
              )
            ))
          } else{
            if (input$plot_mouv_perso == "Camembert") {
              showNotification(
                "NB: Le camembert n'offre pas une visualisation adapté aux tableaux croisés !!!",
                type = "error",
                duration = 5
              )
            }
            
            
            data_rsu$tables_mouv_perso_hist =  tables_mouv_perso_defaut() %>% slice(-nrow(.)) %>% mutate(names = rownames(.)) %>% filter(Total !=
                                                                                                                                           0)
            data_rsu$val_type_plot_mouv_perso_1 = input$val_type_plot_mouv_perso_1
            names(data_rsu$tables_mouv_perso_hist) = gsub("\\.",
                                                          " ",
                                                          names(data_rsu$tables_mouv_perso_hist))
            
            
            return(switch(
              input$plot_mouv_perso,
              "Histogramme" = histo_crois_3(
                titre = input$plot_title_mouv_perso_1,
                ma_data = data_rsu$tables_mouv_perso_hist,
                type_aff =  data_rsu$val_type_plot_mouv_perso_1,
                couleur = input$colors_plot_mouv_perso_1,
                axe_1 = input$plot_axe_mouv_perso_1_1,
                axe_2 = input$plot_axe_mouv_perso_1_2
              ),
              
              "Histogramme superposé (tableau croisé uniquement)" = histo_crois_3(
                titre = input$plot_title_mouv_perso_1,
                ma_data = data_rsu$tables_mouv_perso_hist,
                type_aff =  data_rsu$val_type_plot_mouv_perso_1,
                couleur = input$colors_plot_mouv_perso_1,
                axe_1 = input$plot_axe_mouv_perso_1_1,
                axe_2 = input$plot_axe_mouv_perso_1_2,
                sup = 'stack'
              )
            ))
          }
        }
        
      })
      
      #------> Sorties affichage des tableaux du mouvement du perso
      output$plots_mouv_perso = renderPlotly({
        if (is.null(input$vent_crois_btn_mouv_perso) != T) {
          tryCatch({
            plot_mouv_perso_dataset()
          }, error = function(e) {
            
          })
        }
      })
      
    }, error = function(e) {
      showNotification("Valider d'abord votre partition",
                       type = "error",
                       duration = 5)
    })
  })
  
  
  
  
  
  
  ## ------------------------------------------Code de conception des tables & graphes manuelles  mouvement du personnel---------------------------------------------------
  
  output$manuelle_vent_mouv_perso = DT::renderDT({
    if (is.null(input$manuelle_btn_mouv_perso) != T) {
      # Affichage de la table si la case  " Tableau composé manuellement" est coché
      
      tryCatch({
        tranche_manuelle_mouv_perso = data_rsu$tranche_mouv_perso %>%    # Constitution de la table issue de la séléction manuelle
          select(input$select_manuelle_vent_mouv_perso)  %>% t() %>%
          data.frame() %>%
          rename(Effectif = names(.))
        
        
        table_manuelle(ma_table = tranche_manuelle_mouv_perso)
      }, error = function(e) {
        
      })
    }
  })
  
  ##--------> graphes2 sous table mouv_perso crée manuellement
  
  plot_mouv_perso_manuelle_dataset = reactive({
    tranche_manuelle_mouv_perso = data_rsu$tranche_mouv_perso %>%
      select(input$select_manuelle_vent_mouv_perso)  %>% t() %>% data.frame() %>%
      rename(Effectif = names(.)) %>%
      mutate(names = gsub("\\.", " ", sub("[0-9]*- ", "", rownames(.))))
    
    tranche_manuelle_mouv_perso_plot = tranche_manuelle_mouv_perso %>% filter(Effectif !=
                                                                                0)
    
    #--------> fonction switch permettant d'avoir des rendu au relatif aux choix faites par l'utilisateur
    observeEvent(input$select_manuelle_vent_mouv_perso, {
      updateTextInput(session,
                      "legend_plot_mouv_perso",
                      value = paste(gsub(
                        "\\.", " ", sub(
                          "[0-9]*- ",
                          "",
                          rownames(tranche_manuelle_mouv_perso_plot)
                        )
                      ), sep = ",")) # mise à jour des colonnes à séléctionner pour constitué les légendes
    })
    
    rownames(tranche_manuelle_mouv_perso_plot) = input$legend_plot_mouv_perso %>% strsplit(., split = ",") %>% unlist() %>% as.vector() # Mise à jour de la légende à travers le nom des lignes
    data_rsu$val_type_plot_mouv_perso_2 = input$val_type_plot_mouv_perso_2
    
    
    if (is.null(c(
      input$select_manuelle_vent_mouv_perso,
      input$manuelle_btn_mouv_perso
    )) != T   &
    input$plot_mouv_perso == "Histogramme superposé (tableau croisé uniquement)") {
      showNotification(
        "NB: L'histogramme superposé n'offre pas une visualisation adapté aux tableaux ventilés !!!",
        type = "error",
        duration = 5
      )
    }
    
    switch(
      input$plot_mouv_perso,
      "Camembert" = cam(
        titre = input$plot_title_mouv_perso_2,
        ma_data = tranche_manuelle_mouv_perso_plot,
        type_aff = data_rsu$val_type_plot_mouv_perso_2,
        couleur = input$colors_plot_mouv_perso_2,
        noms_legende = input$legend_plot_mouv_perso %>% strsplit(., split = ",") %>% unlist() %>% as.vector()
      ),
      "Histogramme" = histo(
        titre = input$plot_title_mouv_perso_2,
        ma_data = tranche_manuelle_mouv_perso_plot,
        type_aff = data_rsu$val_type_plot_mouv_perso_2,
        couleur = input$colors_plot_mouv_perso_2,
        noms_legende = input$legend_plot_mouv_perso %>% strsplit(., split = ",") %>% unlist() %>% as.vector()
        ,
        axe_1 = input$plot_axe_mouv_perso_2_1,
        axe_2 = input$plot_axe_mouv_perso_2_2
      )
      
    )
  })
  
  #--------->  Graphiques associés aux table 2_1 et 2_2
  
  output$plot_mouv_perso_manuelle =  renderPlotly({
    if (is.null(input$manuelle_btn_mouv_perso) != T) {
      tryCatch({
        plot_mouv_perso_manuelle_dataset()
      }, error = function(e) {
        
      })
    }
  })
  
  ## -------------------------------------------Fin Code de conception des tables & graphes manuelles  mouvement du personnel---------------------------------------------------
  
  
  
  
  
  ###-------------------------------------------Code de conception des tables & graphes  organisation du travail
  
  
  tranche_orga_trav_dataset = reactive({
    switch(
      input$tranche_select_orga_trav,
      "Tranche 1" = c(
        "A- Astreintes et interventions",
        "B- Télétravail et travail à distance",
        "C- Heures supplémentaire",
        "D- Temps complet/incomplet ou non complet- Temps plein & partiel",
        "E- Congés",
        "F- Abscences au travail hors raison de santé",
        "Informations supplémentaires"
      ),
      
      "Tranche 2" = c("G- CET", "Informations supplémentaires"),
      
      "Tranche 3" = c(
        "H- Absences au travail pour raisons de santé",
        "I- Jours de carence",
        "Informations supplémentaires"
      )
      
    )
  })
  
  
  observeEvent(input$tranche_select_orga_trav, {
    updateSelectInput(session, "sous_theme_orga_trav", choices = tranche_orga_trav_dataset())
  })
  
  
  sous_theme_orga_trav_dataset = reactive({
    #Initialisation des sous-thèmes abordés
    switch(
      input$sous_theme_orga_trav,
      "A- Astreintes et interventions" = c(
        "Agent* soumis à des astreintes: catégorie & sexe",
        "Agent* ayant bénéficié d'un repos compensateur: par catégorie & sexe"
      ),
      
      "B- Télétravail et travail à distance" = c(
        "Agent* exerçant en télétravail : par nombre de jour de TT*",
        "Agent* exerçant en télétravail: nombre de jour TT* & catégorie",
        "Agent* exerçant en télétravail: nombre de jour TT* & genre",
        "Demandes de TT* exprimé au fil de l'eau : catégorie & genre",
        "Refus de TT* exprimé au fil de l'eau : catégorie & genre",
        "Demandes de TT* pour campagne de recensement : catégorie & genre",
        "Refus de TT* pour campagne de recensement : catégorie & genre"
      ),
      
      "C- Heures supplémentaire" = c(
        "Agent* par: ayant effectué des heures supplémentaires l'année concernée: catégorie & genre",
        "Autres types d'heures; écrêtés, celui du dispositif de l'horaire variable"
      ),
      
      "D- Temps complet/incomplet ou non complet- Temps plein & partiel" = "Demandes de temps partiels au cours de l'année concernée: catégorie & genre",
      
      "E- Congés" = c(
        "Nombre de dons de jours enregistrés pour aide à la santé*: catégorie & genre",
        "Nombre de dons de jours reçus pour aide à la santé*: catégorie & genre"
      ),
      
      "F- Abscences au travail hors raison de santé" = c(
        "Nbre de jours de congés bénéficiés sur une durée égale ou superieur à 6 mois: motif & genre",
        "Nbre d'agents en congés, abscent au cours de l'année concernée : motif & genre",
        "Nbre de jours d'abscence pour congés hors raison de santé l'année concernée: motif & genre"
      ),
      
      "G- CET" = c(
        "Agent possédant ou ayant ouvert un CET: motif & genre",
        "Agent ayant déposé ou ayant utilisé des jours de CET: motif & genre",
        "Jours de CET déposés ou utilisés: motif & genre"
      ),
      
      "H- Absences au travail pour raisons de santé" = c(
        "Agent ayant pris un/des congés maladie au cours de l'année concernée: catégorie & genre",
        "Jours cumulés d'arrêt de congé maladie au cours de l'année concernée: catégorie & sexe",
        "Nombre cumulé de jours d'arrêt CMO au cours de l'AC*: par genre",
        "Nombre cumulé d'arrêt CMO: période & genre",
        "Nombre cumulé de jours de congés de CMO: période & genre",
        "Nombre cumulé de jours d'arrêt déclaré/reconnu au cours de l'AC* pour maladies professionnelles: état & genre",
        "Nombre cumulé de jours d'arrêt l'AC* reconnu par l'administration: type d'accident & genre",
        "Maladies professionnelles reconnues ayant débuté au cours de l'année étudiée: Imputabilité & genre",
        "Maladies professionnelles reconnues ayant débuté au cours de l'année étudiée: aptitude & genre",
        "Maladies professionnelles entrainant une incapacité: type d'incapacité & genre"
      ),
      
      "I- Jours de carence" = "Nbre de jours de carence imputés au cours de l'année concernée: catégorie & genre",
      
      "Informations supplémentaires" = c("Réponses qualitatives")
      
      
      
    )
  })
  
  observe({
    updateSelectInput(session, "vent_crois_orga_trav", choices = sous_theme_orga_trav_dataset()) # mise à jour des colonnes à séléctionner pour sélectionner le sous thème étudier dans le parcours professionnel
  })
  
  #Recherche des colonnes par recherche sur les mots clées
  
  
  observeEvent(input$vent_crois_btn_orga_trav, {
    tryCatch({
      TT1_cat_A_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("catégorie A", "d'1", "jour")
      )
      TT1_cat_B_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("catégorie B", "d'1", "jour")
      )
      TT1_cat_C_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("catégorie C", "d'1", "jour")
      )
      
      TT2_cat_A_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("catégorie A", "de 2", "jours")
      )
      TT2_cat_B_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("catégorie B", "de 2", "jours")
      )
      TT2_cat_C_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("catégorie C", "de 2", "jours")
      )
      
      TT3_cat_A_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("catégorie A", "de 3", "jours")
      )
      TT3_cat_B_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("catégorie B", "de 3", "jours")
      )
      TT3_cat_C_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("catégorie C", "de 3", "jours")
      )
      
      #table  jours de TT & genre
      TT1_Femmes_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Femmes", "d'1", "jour")
      )
      TT1_Hommes_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Hommes", "d'1", "jour")
      )
      
      TT2_Femmes_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Femmes", "de 2", "jours")
      )
      TT2_Hommes_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Hommes", "de 2", "jours")
      )
      
      TT3_Femmes_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Femmes", "de 3", "jours")
      )
      TT3_Hommes_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Hommes", "de 3", "jours")
      )
      
      # Ensemble  Agent categorie & genre dans la tranche 1
      
      
      Femmes_cat_A_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Femmes", "Catégorie A"),
        somme = F
      )
      Femmes_cat_B_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Femmes", "Catégorie B"),
        somme = F
      )
      Femmes_cat_C_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Femmes", "Catégorie C"),
        somme = F
      )
      
      
      Hommes_cat_A_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Hommes", "Catégorie A"),
        somme = F
      )
      Hommes_cat_B_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Hommes", "Catégorie B"),
        somme = F
      )
      Hommes_cat_C_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Hommes", "Catégorie C"),
        somme = F
      )
      
      ##---------------------------------------------Abscence hors raison de santé_1
      
      cong_parental_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Nbre", "jours", "Congé", "parental"),
        somme = F
      )
      cong_adoption_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Nbre", "jours", "Congé", "d'adoption"),
        somme = F
      )
      cong_famille_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Nbre", "jours", "congé", "Autre", "famille"),
        somme = F
      )
      cong_accomp_fin_vie_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Nbre", "jours", "personnes", "fin", "vie"),
        somme = F
      )
      cong_accomp_depend_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Nbre", "jours", "personnes", "situation", "dépendance"),
        somme = F
      )
      cong_conve_perso_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Nbre", "jours", "convenances", "personnelles"),
        somme = F
      )
      
      ##---------------------------------------------Abscence hors raison de santé_2
      
      cong_mat_adp_pat_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c(
          "Congé",
          "maternité",
          "d'adoption",
          "paternité",
          "enfant"
        ),
        somme = F
      )
      cong_forma_pro_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c(
          "Congé",
          "formation",
          "professionnelle",
          "expérience",
          "compétence"
        ),
        somme = F
      )
      cong_forma_syndic_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Congé", "formation", "syndicale"),
        somme = F
      )
      
      cong_perso_CHSCT_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c(
          "Congé",
          "accordé",
          "représentant"
          ,
          "suivre",
          "formation",
          "hygiène",
          "sécurité"
        ),
        somme = F
      )
      cong_jeune_educ_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c(
          "Congé",
          "activité",
          "jeunesse",
          "éducation",
          "populaire"
        ),
        somme = F
      )
      cong_benev_asso_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Congé", "bénévole", "d'association"),
        somme = F
      )
      
      cong_solida_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Congé", "solidarité", "familiale"),
        somme = F
      )
      cong_asso_mut_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Congé", "siéger", "association", "mutuelle"),
        somme = F
      )
      cong_act_opera_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Congé", "période", "réserve", "opérationnelle"),
        somme = F
      )
      
      cong_pres_parent_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Congé", "présence", "parentale"),
        somme = F
      )
      cong_proche_aid_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Congé", "proche", "aidant"),
        somme = F
      )
      
      #------------------------------------------------------------------
      
      Femmes_agent_cet_tranche_2_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("féminin", "ayant un", "CET")
      )
      Femmes_agent_open_cet_tranche_2_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("féminin", "ouvert", "CET")
      )
      Hommes_agent_agent_cet_tranche_2_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("masculin", "ayant un", "CET")
      )
      Hommes_agent_open_cet_tranche_2_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("masculin", "ouvert", "CET")
      )
      
      Femmes_agent_depo_cet_tranche_2_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("féminin", "déposé", "des jours", "CET")
      )
      Femmes_agent_use_cet_tranche_2_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("féminin", "utilisé", "des jours", "CET")
      )
      Hommes_agent_depo_cet_tranche_2_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("masculin", "déposé", "des jours", "CET")
      )
      Hommes_agent_use_cet_tranche_2_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("masculin", "utilisé", "des jours", "CET")
      )
      
      
      #-----------------------------------------------------------------------
      
      Femmes_jours_depo_cet_tranche_2_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("féminin", "déposé", "31", "décembre", "jours", "CET")
      )
      Femmes_jours_use_cet_tranche_2_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("féminin", "utilisé", "31", "décembre", "jours", "CET")
      )
      Femmes_jours_no_use_depo_cet_tranche_2_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("féminin", "non pris", "31", "décembre", "jours")
      )
      
      Hommes_jours_depo_cet_tranche_2_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("masculin", "déposé", "31", "décembre", "jours", "CET")
      )
      Hommes_jours_use_cet_tranche_2_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("masculin", "utilisé", "31", "décembre", "jours", "CET")
      )
      Hommes_jours_no_use_depo_cet_tranche_2_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("masculin", "non pris", "31", "décembre", "jours")
      )
      
      # Ensemble  Agent categorie & genre dans la tranche 1
      
      
      Femmes_cat_A_tranche_2_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Femmes", "Catégorie A")
      )
      Femmes_cat_B_tranche_2_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Femmes", "Catégorie B")
      )
      Femmes_cat_C_tranche_2_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Femmes", "Catégorie C")
      )
      
      
      Hommes_cat_A_tranche_2_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Hommes", "Catégorie A")
      )
      Hommes_cat_B_tranche_2_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Hommes", "Catégorie B")
      )
      Hommes_cat_C_tranche_2_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Hommes", "Catégorie C")
      )
      
      
      # Les congés maladies
      #les agents
      Femmes_agent_cmo_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("agents", "Femmes", "CMO")
      )
      Femmes_agent_clm_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("agents", "Femmes", "CLM")
      )
      Femmes_agent_cld_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("agents", "Femmes", "CLD")
      )
      
      Hommes_agent_cmo_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("agents", "Hommes", "CMO")
      )
      Hommes_agent_clm_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("agents", "Hommes", "CLM")
      )
      Hommes_agent_cld_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("agents", "Hommes", "CLD")
      )
      
      # cumule jours d'arrêts
      
      Femmes_jours_cmo_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("cumulé", "jours d'arrêt", "Femmes", "CMO", "concernée")
      )
      Femmes_jours_clm_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("cumulé", "jours d'arrêt", "Femmes", "CLM", "concernée")
      )
      Femmes_jours_cld_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("cumulé", "jours d'arrêt", "Femmes", "CLD", "concernée")
      )
      
      Hommes_jours_cmo_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("cumulé", "jours d'arrêt", "Hommes", "CMO", "concernée")
      )
      Hommes_jours_clm_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("cumulé", "jours d'arrêt", "Hommes", "CLM", "concernée")
      )
      Hommes_jours_cld_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("cumulé", "jours d'arrêt", "Hommes", "CLD", "concernée")
      )
      
      #cumulé arrêt cmo
      
      Femmes_arret_type_1_cmo_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("cumulé d'arrêt", "Femmes", "CMO", "concernée")
      )
      Femmes_arret_type_2_cmo_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c(
          "cumulé d'arrêt",
          "Femmes",
          "CMO",
          "moins",
          "8 jours",
          "en cours"
        )
      )
      Femmes_arret_type_3_cmo_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c(
          "cumulé d'arrêt",
          "Femmes",
          "CMO",
          "plus",
          "8 jours",
          "en cours"
        )
      )
      
      
      Hommes_arret_type_1_cmo_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("cumulé d'arrêt", "Hommes", "CMO", "concernée")
      )
      Hommes_arret_type_2_cmo_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c(
          "cumulé d'arrêt",
          "Hommes",
          "CMO",
          "moins",
          "8 jours",
          "en cours"
        )
      )
      Hommes_arret_type_3_cmo_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c(
          "cumulé d'arrêt",
          "Hommes",
          "CMO",
          "plus",
          "8 jours",
          "en cours"
        )
      )
      
      #cumulé jours d'arrêts cmo
      
      Femmes_jours_arret_type_1_cmo_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c(
          "cumulé",
          "jours de congé",
          "Femmes",
          "CMO",
          "moins",
          "8 jours",
          "en cours"
        )
      )
      Femmes_jours_arret_type_2_cmo_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c(
          "cumulé",
          "jours de congé",
          "Femmes",
          "CMO",
          "plus",
          "8 jours",
          "en cours"
        )
      )
      
      Hommes_jours_arret_type_1_cmo_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c(
          "cumulé",
          "jours de congé",
          "Hommes",
          "CMO",
          "moins",
          "8 jours",
          "en cours"
        )
      )
      Hommes_jours_arret_type_2_cmo_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c(
          "cumulé",
          "jours de congé",
          "Hommes",
          "CMO",
          "plus",
          "8 jours",
          "en cours"
        )
      )
      
      
      
      #Maladie pro aynt débuté au cours de l'année traité
      
      
      Femmes_jours_arret_mpni_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c(
          "maladie",
          "professionnelle",
          "Femmes",
          "non imputable",
          "service",
          "reconnue",
          "concerné"
        )
      )
      Femmes_jours_arret_mpi_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c(
          "maladie",
          "professionnelle",
          "Femmes",
          "imputable",
          "service",
          "reconnue",
          "concerné"
        )
      )
      Femmes_jours_arret_apte_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c(
          "maladie",
          "professionnelle",
          "Femmes",
          "sans",
          "incapacité",
          "reconnue",
          "concerné"
        )
      )
      Femmes_jours_arret_inapte_temp_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c(
          "maladie",
          "professionnelle",
          "Femmes",
          "avec",
          "incapacité",
          "temporaire",
          "reconnue",
          "concerné"
        )
      )
      Femmes_jours_arret_inapte_perma_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c(
          "maladie",
          "professionnelle",
          "Femmes",
          "avec",
          "incapacité",
          "permanente",
          "reconnue",
          "concerné"
        )
      )
      Femmes_jours_arret_deces_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c(
          "maladie",
          "professionnelle",
          "Femmes",
          "provoqué",
          "décès",
          "reconnue",
          "concerné"
        )
      )
      
      Hommes_jours_arret_mpni_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c(
          "maladie",
          "professionnelle",
          "Hommes",
          "non imputable",
          "service",
          "reconnue",
          "concerné"
        )
      )
      Hommes_jours_arret_mpi_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c(
          "maladie",
          "professionnelle",
          "Hommes",
          "imputable",
          "service",
          "reconnue",
          "concerné"
        )
      )
      Hommes_jours_arret_apte_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c(
          "maladie",
          "professionnelle",
          "Hommes",
          "sans",
          "incapacité",
          "reconnue",
          "concerné"
        )
      )
      Hommes_jours_arret_inapte_temp_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c(
          "maladie",
          "professionnelle",
          "Hommes",
          "avec",
          "incapacité",
          "temporaire",
          "reconnue",
          "concerné"
        )
      )
      Hommes_jours_arret_inapte_perma_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c(
          "maladie",
          "professionnelle",
          "Hommes",
          "avec",
          "incapacité",
          "permanente",
          "reconnue",
          "concerné"
        )
      )
      Hommes_jours_arret_deces_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c(
          "maladie",
          "professionnelle",
          "Hommes",
          "provoqué",
          "décès",
          "reconnue",
          "concerné"
        )
      )
      
      
      
      
      #-------------------------------------------Jours d'arrêt année concernée
      
      Femmes_jours_arret_declare_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c(
          "Femmes",
          "jours",
          "cumulé",
          "maladie",
          "professionnelle",
          "déclaré",
          "avant"
        )
      )
      Femmes_jours_arret_acc_serv_reconnu_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c(
          "Femmes",
          "jours",
          "cumulé",
          "accident",
          "service",
          "reconnu",
          "avant"
        )
      )
      Femmes_jours_arret_acc_trajet_reconnu_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c(
          "Femmes",
          "jours",
          "cumulé",
          "accident",
          "trajet",
          "reconnu",
          "avant"
        )
      )
      
      
      Hommes_jours_arret_declare_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c(
          "Hommes",
          "jours",
          "cumulé",
          "maladie",
          "professionnelle",
          "déclaré",
          "avant"
        )
      )
      Hommes_jours_arret_acc_serv_reconnu_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c(
          "Hommes",
          "jours",
          "cumulé",
          "accident",
          "service",
          "reconnu",
          "avant"
        )
      )
      Hommes_jours_arret_acc_trajet_reconnu_tranche_3_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c(
          "Hommes",
          "jours",
          "cumulé",
          "accident",
          "trajet",
          "reconnu",
          "avant"
        )
      )
      
      #------------------------------Autre  type d'heure
      
      agent_hvar_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Horaire", "variable", "Nombre", "d'agent")
      )
      
      agent_ecrete_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("Nbre", "d'agent", "heure", "écrêté")
      )
      
      heure_ecrete_tranche_1_orga_trav = colonne_match(
        data = data_rsu$tranche_orga_trav,
        vecteurs_mots =  c("total", "d'heure", "écrêté")
      )
      
      # Conception des objets tableau par défauts
      
      tables_orga_trav_defaut = reactive({
        switch(
          input$vent_crois_orga_trav,
          
          "Agent* soumis à des astreintes: catégorie & sexe" = data.frame(
            row.names = c("Catégorie A", "Catégorie B", "Catégorie C"),
            "Femmes" =
              c(
                Femmes_cat_A_tranche_1_orga_trav[1, 6],
                Femmes_cat_B_tranche_1_orga_trav[1, 6],
                Femmes_cat_C_tranche_1_orga_trav[1, 6]
              ),
            "Hommes" = c(
              Hommes_cat_A_tranche_1_orga_trav[1, 6],
              Hommes_cat_B_tranche_1_orga_trav[1, 6],
              Hommes_cat_C_tranche_1_orga_trav[1, 6]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "Agent* ayant bénéficié d'un repos compensateur: par catégorie & sexe" = data.frame(
            row.names = c("Catégorie A", "Catégorie B", "Catégorie C"),
            "Femmes" =
              c(
                Femmes_cat_A_tranche_1_orga_trav[1, 7],
                Femmes_cat_B_tranche_1_orga_trav[1, 7],
                Femmes_cat_C_tranche_1_orga_trav[1, 7]
              ),
            "Hommes" = c(
              Hommes_cat_A_tranche_1_orga_trav[1, 7],
              Hommes_cat_B_tranche_1_orga_trav[1, 7],
              Hommes_cat_C_tranche_1_orga_trav[1, 7]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "Agent* exerçant en télétravail : par nombre de jour de TT*" = data.frame(
            row.names = c(
              "1 jour de TT* hebdo",
              "2 jours de TT* hebdo",
              "3 jours de TT* hebdo"
            ),
            "Effectif" = c(
              sum(
                TT1_Femmes_tranche_1_orga_trav,
                TT1_Hommes_tranche_1_orga_trav
              ),
              sum(
                TT2_Femmes_tranche_1_orga_trav,
                TT2_Hommes_tranche_1_orga_trav
              ),
              sum(
                TT3_Femmes_tranche_1_orga_trav,
                TT3_Hommes_tranche_1_orga_trav
              )
            )
          ) %>%
            bind_rows(data.frame(
              row.names = "Total", Effectif = colSums(.)[1]
            )),
          
          "Agent* exerçant en télétravail: nombre de jour TT* & catégorie" = data.frame(
            row.names = c(
              "1 jour de TT* hebdo",
              "2 jours de TT* hebdo",
              "3 jours de TT* hebdo"
            ),
            "Cat_A" = c(
              TT1_cat_A_tranche_1_orga_trav,
              TT2_cat_A_tranche_1_orga_trav,
              TT3_cat_A_tranche_1_orga_trav
            ),
            "Cat_B" = c(
              TT1_cat_B_tranche_1_orga_trav,
              TT2_cat_B_tranche_1_orga_trav,
              TT3_cat_B_tranche_1_orga_trav
            ),
            "Cat_C" = c(
              TT1_cat_C_tranche_1_orga_trav,
              TT2_cat_C_tranche_1_orga_trav,
              TT3_cat_C_tranche_1_orga_trav
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                "Cat_A" = colSums(.)[1],
                "Cat_B" = colSums(.)[2],
                "Cat_C" = colSums(.)[3]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "Agent* exerçant en télétravail: nombre de jour TT* & genre" = data.frame(
            row.names = c(
              "1 jour de TT* hebdo",
              "2 jours de TT* hebdo",
              "3 jours de TT* hebdo"
            ),
            "Femmes" = c(
              TT1_Femmes_tranche_1_orga_trav,
              TT2_Femmes_tranche_1_orga_trav,
              TT3_Femmes_tranche_1_orga_trav
            ),
            "Hommes" = c(
              TT1_Hommes_tranche_1_orga_trav,
              TT2_Hommes_tranche_1_orga_trav,
              TT3_Hommes_tranche_1_orga_trav
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                "Femmes" = colSums(.)[1],
                "Hommes" = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "Agent* par: ayant effectué des heures supplémentaires l'année concernée: catégorie & genre" =  data.frame(
            row.names = c("Catégorie A", "Catégorie B", "Catégorie C"),
            "Femmes" =
              c(
                Femmes_cat_A_tranche_1_orga_trav[1, 8],
                Femmes_cat_B_tranche_1_orga_trav[1, 8],
                Femmes_cat_C_tranche_1_orga_trav[1, 8]
              ),
            "Hommes" = c(
              Hommes_cat_A_tranche_1_orga_trav[1, 8],
              Hommes_cat_B_tranche_1_orga_trav[1, 8],
              Hommes_cat_C_tranche_1_orga_trav[1, 8]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "Autres types d'heures; écrêtés, celui du dispositif de l'horaire variable" = data.frame(
            row.names = c(
              "Agents bénéficiant du dispositif de l'horaire variable l'AC*",
              "Agents ayants des heures écrêtées",
              "Total d'heures écrêtées durant l'AC*"
            ),
            "Effectif" = c(
              agent_hvar_tranche_1_orga_trav,
              agent_ecrete_tranche_1_orga_trav,
              heure_ecrete_tranche_1_orga_trav
            )
          ),
          
          
          
          
          "Demandes de TT* exprimé au fil de l'eau : catégorie & genre" =   data.frame(
            row.names = c("Catégorie A", "Catégorie B", "Catégorie C"),
            "Femmes" =
              c(
                Femmes_cat_A_tranche_1_orga_trav[1, 2],
                Femmes_cat_B_tranche_1_orga_trav[1, 2],
                Femmes_cat_C_tranche_1_orga_trav[1, 2]
              ),
            "Hommes" = c(
              Hommes_cat_A_tranche_1_orga_trav[1, 2],
              Hommes_cat_B_tranche_1_orga_trav[1, 2],
              Hommes_cat_C_tranche_1_orga_trav[1, 2]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "Refus de TT* exprimé au fil de l'eau : catégorie & genre"            =     data.frame(
            row.names = c("Catégorie A", "Catégorie B", "Catégorie C"),
            "Femmes" =
              c(
                Femmes_cat_A_tranche_1_orga_trav[1, 3],
                Femmes_cat_B_tranche_1_orga_trav[1, 3],
                Femmes_cat_C_tranche_1_orga_trav[1, 3]
              ),
            "Hommes" = c(
              Hommes_cat_A_tranche_1_orga_trav[1, 3],
              Hommes_cat_B_tranche_1_orga_trav[1, 3],
              Hommes_cat_C_tranche_1_orga_trav[1, 3]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          
          "Demandes de TT* pour campagne de recensement : catégorie & genre" =   data.frame(
            row.names = c("Catégorie A", "Catégorie B", "Catégorie C"),
            "Femmes" =
              c(
                Femmes_cat_A_tranche_1_orga_trav[1, 4],
                Femmes_cat_B_tranche_1_orga_trav[1, 4],
                Femmes_cat_C_tranche_1_orga_trav[1, 4]
              ),
            "Hommes" = c(
              Hommes_cat_A_tranche_1_orga_trav[1, 4],
              Hommes_cat_B_tranche_1_orga_trav[1, 4],
              Hommes_cat_C_tranche_1_orga_trav[1, 4]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          
          
          "Refus de TT* pour campagne de recensement : catégorie & genre" =    data.frame(
            row.names = c("Catégorie A", "Catégorie B", "Catégorie C"),
            "Femmes" =
              c(
                Femmes_cat_A_tranche_1_orga_trav[1, 5],
                Femmes_cat_B_tranche_1_orga_trav[1, 5],
                Femmes_cat_C_tranche_1_orga_trav[1, 5]
              ),
            "Hommes" = c(
              Hommes_cat_A_tranche_1_orga_trav[1, 5],
              Hommes_cat_B_tranche_1_orga_trav[1, 5],
              Hommes_cat_C_tranche_1_orga_trav[1, 5]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          
          "Demandes de temps partiels au cours de l'année concernée: catégorie & genre" =  data.frame(
            row.names = c("Catégorie A", "Catégorie B", "Catégorie C"),
            "Femmes" =
              c(
                Femmes_cat_A_tranche_1_orga_trav[1, 1],
                Femmes_cat_B_tranche_1_orga_trav[1, 1],
                Femmes_cat_C_tranche_1_orga_trav[1, 1]
              ),
            "Hommes" = c(
              Hommes_cat_A_tranche_1_orga_trav[1, 1],
              Hommes_cat_B_tranche_1_orga_trav[1, 1],
              Hommes_cat_C_tranche_1_orga_trav[1, 1]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          
          "Nombre de dons de jours enregistrés pour aide à la santé*: catégorie & genre" = data.frame(
            row.names = c("Catégorie A", "Catégorie B", "Catégorie C"),
            "Femmes" =
              c(
                Femmes_cat_A_tranche_1_orga_trav[1, 9],
                Femmes_cat_B_tranche_1_orga_trav[1, 9],
                Femmes_cat_C_tranche_1_orga_trav[1, 9]
              ),
            "Hommes" = c(
              Hommes_cat_A_tranche_1_orga_trav[1, 9],
              Hommes_cat_B_tranche_1_orga_trav[1, 9],
              Hommes_cat_C_tranche_1_orga_trav[1, 9]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          
          "Nombre de dons de jours reçus pour aide à la santé*: catégorie & genre" =  data.frame(
            row.names = c("Catégorie A", "Catégorie B", "Catégorie C"),
            "Femmes" =
              c(
                Femmes_cat_A_tranche_1_orga_trav[1, 10],
                Femmes_cat_B_tranche_1_orga_trav[1, 10],
                Femmes_cat_C_tranche_1_orga_trav[1, 10]
              ),
            "Hommes" = c(
              Hommes_cat_A_tranche_1_orga_trav[1, 10],
              Hommes_cat_B_tranche_1_orga_trav[1, 10],
              Hommes_cat_C_tranche_1_orga_trav[1, 10]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "Nbre de jours de congés bénéficiés sur une durée égale ou superieur à 6 mois: motif & genre" =  data.frame(
            row.names = c(
              "Parental",
              "Adoption",
              "familiale",
              "APS* de fin de vie",
              "APS* de dépendance ",
              "Convenances personnelles"
            ),
            "Femmes" =
              c(
                cong_parental_tranche_1_orga_trav[1, 1],
                cong_adoption_tranche_1_orga_trav[1, 1],
                cong_famille_tranche_1_orga_trav[1, 1],
                cong_accomp_fin_vie_tranche_1_orga_trav[1, 1],
                cong_accomp_depend_tranche_1_orga_trav[1, 1],
                cong_conve_perso_tranche_1_orga_trav[1, 1]
              ),
            
            "Hommes" = c(
              cong_parental_tranche_1_orga_trav[1, 2],
              cong_adoption_tranche_1_orga_trav[1, 2],
              cong_famille_tranche_1_orga_trav[1, 2],
              cong_accomp_fin_vie_tranche_1_orga_trav[1, 2],
              cong_accomp_depend_tranche_1_orga_trav[1, 2],
              cong_conve_perso_tranche_1_orga_trav[1, 2]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "Nbre d'agents en congés, abscent au cours de l'année concernée : motif & genre" = data.frame(
            row.names = c(
              "Congés de MAPAE*",
              "Formation pro",
              "Formation syndicale",
              "Formation du perso CHSCT",
              "Activités OJEP*",
              "Résponsable bénévole d'asso",
              "Solidarité familiale",
              "Siége asssociation mutuele",
              "Pour réserve opérationnelle",
              "Présence Parental",
              "Proche aidant"
            ),
            "Femmes" =
              c(
                cong_mat_adp_pat_tranche_1_orga_trav[1, 1],
                cong_forma_pro_tranche_1_orga_trav[1, 1],
                cong_forma_syndic_tranche_1_orga_trav[1, 1],
                cong_perso_CHSCT_tranche_1_orga_trav[1, 1],
                cong_jeune_educ_tranche_1_orga_trav[1, 1],
                cong_benev_asso_tranche_1_orga_trav[1, 1],
                cong_solida_tranche_1_orga_trav[1, 1],
                cong_asso_mut_tranche_1_orga_trav[1, 1],
                cong_act_opera_tranche_1_orga_trav[1, 1],
                cong_pres_parent_tranche_1_orga_trav[1, 1],
                cong_proche_aid_tranche_1_orga_trav[1, 1]
              ),
            
            "Hommes" = c(
              cong_mat_adp_pat_tranche_1_orga_trav[1, 2],
              cong_forma_pro_tranche_1_orga_trav[1, 2],
              cong_forma_syndic_tranche_1_orga_trav[1, 2],
              cong_perso_CHSCT_tranche_1_orga_trav[1, 2],
              cong_jeune_educ_tranche_1_orga_trav[1, 2],
              cong_benev_asso_tranche_1_orga_trav[1, 2],
              cong_solida_tranche_1_orga_trav[1, 2],
              cong_asso_mut_tranche_1_orga_trav[1, 2],
              cong_act_opera_tranche_1_orga_trav[1, 2],
              cong_pres_parent_tranche_1_orga_trav[1, 2],
              cong_proche_aid_tranche_1_orga_trav[1, 2]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          
          "Nbre de jours d'abscence pour congés hors raison de santé l'année concernée: motif & genre" = data.frame(
            row.names = c(
              "Congés de MAPAE*",
              "Formations pro",
              "Formations syndicales",
              "Formations du perso CHSCT",
              "Activités OJEP*",
              "Résponsables bénévole d'asso",
              "Solidarités familiales",
              "Siéges associations mutuelles",
              "Pour réserves opérationnelles",
              "Présence Parental",
              "Proche aidant"
            ),
            "Femmes" =
              c(
                cong_mat_adp_pat_tranche_1_orga_trav[1, 3],
                cong_forma_pro_tranche_1_orga_trav[1, 3],
                cong_forma_syndic_tranche_1_orga_trav[1, 3],
                cong_perso_CHSCT_tranche_1_orga_trav[1, 3],
                cong_jeune_educ_tranche_1_orga_trav[1, 3],
                cong_benev_asso_tranche_1_orga_trav[1, 3],
                cong_solida_tranche_1_orga_trav[1, 3],
                cong_asso_mut_tranche_1_orga_trav[1, 3],
                cong_act_opera_tranche_1_orga_trav[1, 3],
                cong_pres_parent_tranche_1_orga_trav[1, 3],
                cong_proche_aid_tranche_1_orga_trav[1, 3]
              ),
            
            "Hommes" = c(
              cong_mat_adp_pat_tranche_1_orga_trav[1, 4],
              cong_forma_pro_tranche_1_orga_trav[1, 4],
              cong_forma_syndic_tranche_1_orga_trav[1, 4],
              cong_perso_CHSCT_tranche_1_orga_trav[1, 4],
              cong_jeune_educ_tranche_1_orga_trav[1, 4],
              cong_benev_asso_tranche_1_orga_trav[1, 4],
              cong_solida_tranche_1_orga_trav[1, 4],
              cong_asso_mut_tranche_1_orga_trav[1, 4],
              cong_act_opera_tranche_1_orga_trav[1, 4],
              cong_pres_parent_tranche_1_orga_trav[1, 4],
              cong_proche_aid_tranche_1_orga_trav[1, 4]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          
          "Agent possédant ou ayant ouvert un CET: motif & genre" = data.frame(
            row.names = c("Possédant un CET", "Ayant ouvert un CET l'AC*"),
            "Femmes" =
              c(
                Femmes_agent_cet_tranche_2_orga_trav,
                Femmes_agent_open_cet_tranche_2_orga_trav
              ),
            "Hommes" = c(
              Hommes_agent_agent_cet_tranche_2_orga_trav,
              Hommes_agent_open_cet_tranche_2_orga_trav
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "Agent ayant déposé ou ayant utilisé des jours de CET: motif & genre" = data.frame(
            row.names = c("Ayant déposé un CET", "Ayant utilisé un CET"),
            "Femmes" =
              c(
                Femmes_agent_depo_cet_tranche_2_orga_trav,
                Femmes_agent_use_cet_tranche_2_orga_trav
              ),
            "Hommes" = c(
              Hommes_agent_depo_cet_tranche_2_orga_trav,
              Hommes_agent_use_cet_tranche_2_orga_trav
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "Jours de CET déposés ou utilisés: motif & genre" = data.frame(
            row.names = c(
              "Déposant un CET",
              "Ayant utilisé un CET",
              "N'ayant pas pris/utilisé un CET"
            ),
            "Femmes" =
              c(
                Femmes_jours_depo_cet_tranche_2_orga_trav,
                Femmes_jours_use_cet_tranche_2_orga_trav,
                Femmes_jours_no_use_depo_cet_tranche_2_orga_trav
              ),
            "Hommes" = c(
              Hommes_jours_depo_cet_tranche_2_orga_trav,
              Hommes_jours_use_cet_tranche_2_orga_trav,
              Hommes_jours_no_use_depo_cet_tranche_2_orga_trav
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          
          "Nbre de jours de carence imputés au cours de l'année concernée: catégorie & genre" =   data.frame(
            row.names = c("Catégorie A", "Catégorie B", "Catégorie C"),
            "Femmes" =
              c(
                Femmes_cat_A_tranche_2_orga_trav,
                Femmes_cat_B_tranche_2_orga_trav,
                Femmes_cat_C_tranche_2_orga_trav
              ),
            "Hommes" = c(
              Hommes_cat_A_tranche_2_orga_trav,
              Hommes_cat_B_tranche_2_orga_trav,
              Hommes_cat_C_tranche_2_orga_trav
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          
          
          "Agent ayant pris un/des congés maladie au cours de l'année concernée: catégorie & genre" =   data.frame(
            row.names = c(
              "Congés maladie ordinaires(CMO)",
              "Congés longue maladie(CLM)",
              "Congés longue durée(CLD)"
            ),
            "Femmes" =
              c(
                Femmes_agent_cmo_tranche_3_orga_trav,
                Femmes_agent_clm_tranche_3_orga_trav,
                Femmes_agent_cld_tranche_3_orga_trav
              ),
            "Hommes" = c(
              Hommes_agent_cmo_tranche_3_orga_trav,
              Hommes_agent_clm_tranche_3_orga_trav,
              Hommes_agent_cld_tranche_3_orga_trav
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          
          "Jours cumulés d'arrêt de congé maladie au cours de l'année concernée: catégorie & sexe" =   data.frame(
            row.names = c(
              "Congés maladie ordinaires(CMO)",
              "Congés longue maladie(CLM)",
              "Congés longue durée(CLD)"
            ),
            "Femmes" =
              c(
                Femmes_jours_cmo_tranche_3_orga_trav,
                Femmes_jours_clm_tranche_3_orga_trav,
                Femmes_jours_cld_tranche_3_orga_trav
              ),
            "Hommes" = c(
              Hommes_jours_cmo_tranche_3_orga_trav,
              Hommes_jours_clm_tranche_3_orga_trav,
              Hommes_jours_cld_tranche_3_orga_trav
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          
          "Nombre cumulé de jours d'arrêt CMO au cours de l'AC*: par genre" = data.frame(
            row.names = c(
              "Cumules jours d'arrêt de CMO au cours de l'année concernée"
            ),
            "Femmes" =
              c(Femmes_jours_cmo_tranche_3_orga_trav),
            "Hommes" = c(Hommes_jours_cmo_tranche_3_orga_trav)
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          
          
          "Nombre cumulé d'arrêt CMO: période & genre" =  data.frame(
            row.names = c(
              "Au cours de l'AC*",
              "Moins de 8 jours consécutif ACAEC*",
              "8 jours consécutifs ou plus ACAEC*"
            ),
            "Femmes" =
              c(
                Femmes_arret_type_1_cmo_tranche_3_orga_trav,
                Femmes_arret_type_2_cmo_tranche_3_orga_trav,
                Femmes_arret_type_3_cmo_tranche_3_orga_trav
              ),
            "Hommes" = c(
              Hommes_arret_type_1_cmo_tranche_3_orga_trav,
              Hommes_arret_type_2_cmo_tranche_3_orga_trav,
              Hommes_arret_type_3_cmo_tranche_3_orga_trav
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "Nombre cumulé de jours de congés de CMO: période & genre" =  data.frame(
            row.names = c(
              "Moins de 8 jours consécutif ACAEC*",
              "8 jours consécutifs ou plus ACAEC*"
            ),
            "Femmes" =
              c(
                Femmes_jours_arret_type_1_cmo_tranche_3_orga_trav,
                Femmes_jours_arret_type_2_cmo_tranche_3_orga_trav
              ),
            "Hommes" = c(
              Hommes_jours_arret_type_1_cmo_tranche_3_orga_trav,
              Hommes_jours_arret_type_2_cmo_tranche_3_orga_trav
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          
          "Nombre cumulé de jours d'arrêt déclaré/reconnu au cours de l'AC* pour maladies professionnelles: état & genre" = data.frame(
            row.names = c(
              "Déclarés avant l'AC*",
              "Reconnus par l'administration avant l'AC*"
            ),
            "Femmes" =
              c(
                Femmes_jours_arret_declare_tranche_3_orga_trav,
                Femmes_jours_arret_acc_serv_reconnu_tranche_3_orga_trav + Femmes_jours_arret_acc_trajet_reconnu_tranche_3_orga_trav
              ),
            "Hommes" = c(
              Hommes_jours_arret_declare_tranche_3_orga_trav,
              Hommes_jours_arret_acc_serv_reconnu_tranche_3_orga_trav + Hommes_jours_arret_acc_trajet_reconnu_tranche_3_orga_trav
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          
          "Nombre cumulé de jours d'arrêt l'AC* reconnu par l'administration: type d'accident & genre" = data.frame(
            row.names = c("Accidents de service", "Accidents de trajet"),
            "Femmes" =
              c(
                Femmes_jours_arret_acc_serv_reconnu_tranche_3_orga_trav,
                Femmes_jours_arret_acc_trajet_reconnu_tranche_3_orga_trav
              ),
            "Hommes" = c(
              Hommes_jours_arret_acc_serv_reconnu_tranche_3_orga_trav,
              Hommes_jours_arret_acc_trajet_reconnu_tranche_3_orga_trav
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "Maladies professionnelles reconnues ayant débuté au cours de l'année étudiée: Imputabilité & genre" =  data.frame(
            row.names = c("Imputables au service", "Non imputables au service"),
            "Femmes" =
              c(
                Femmes_jours_arret_mpi_tranche_3_orga_trav - Femmes_jours_arret_mpni_tranche_3_orga_trav,
                Femmes_jours_arret_mpni_tranche_3_orga_trav
              ),
            "Hommes" = c(
              Hommes_jours_arret_mpi_tranche_3_orga_trav - Hommes_jours_arret_mpni_tranche_3_orga_trav,
              Hommes_jours_arret_mpni_tranche_3_orga_trav
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "Maladies professionnelles reconnues ayant débuté au cours de l'année étudiée: aptitude & genre" =  data.frame(
            row.names = c(
              "Reconnues sans incapacité",
              "Reconnues avec incapacité"
            ),
            "Femmes" =
              c(
                Femmes_jours_arret_apte_tranche_3_orga_trav,
                Femmes_jours_arret_inapte_temp_tranche_3_orga_trav + Femmes_jours_arret_inapte_perma_tranche_3_orga_trav
              ),
            "Hommes" = c(
              Hommes_jours_arret_apte_tranche_3_orga_trav,
              Hommes_jours_arret_inapte_temp_tranche_3_orga_trav + Hommes_jours_arret_inapte_perma_tranche_3_orga_trav
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "Maladies professionnelles entrainant une incapacité: type d'incapacité & genre" = data.frame(
            row.names = c("Temporaires", "Permanents", "Provoquant un décès"),
            "Femmes" =
              c(
                Femmes_jours_arret_inapte_temp_tranche_3_orga_trav,
                Femmes_jours_arret_inapte_perma_tranche_3_orga_trav ,
                Femmes_jours_arret_deces_tranche_3_orga_trav
              ),
            "Hommes" = c(
              Hommes_jours_arret_inapte_temp_tranche_3_orga_trav,
              Hommes_jours_arret_inapte_perma_tranche_3_orga_trav ,
              Hommes_jours_arret_deces_tranche_3_orga_trav
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "Réponses qualitatives" = data_rsu$tranche_orga_trav %>% select_if( ~ is.character(.) &
                                                                                !(is.na(.))) %>% t() %>%
            data.frame() %>% rename(Reponse = names(.))
          
        )
      })
      
      
      output$vent_crois_orga_trav = DT::renderDT({
        #Affichage de la tranche de table si l'utilisateur coche la case "Tableau ... défaut"
        tryCatch({
          if (is.null(input$vent_crois_btn_orga_trav) != TRUE &
              input$sous_theme_orga_trav != "Informations supplémentaires") {
            table_classic(ma_table = tables_orga_trav_defaut())
          } else{
            if (is.null(input$vent_crois_btn_orga_trav) != TRUE) {
              table_manuelle(ma_table = tables_orga_trav_defaut())
            }
          }
        }, error = function(e) {
          
        })
      })
      
      ##-----------------------------------> Affichage graphique organisation du travail
      
      plot_orga_trav_dataset = reactive({
        if (input$vent_crois_orga_trav %in% c(
          "Agent* exerçant en télétravail : par nombre de jour de TT*",
          "Nbre de jours de carence imputés au cours de l'année concernée",
          "Autres types d'heures; écrêtés, celui du dispositif de l'horaire variable"
        )) {
          if (input$plot_orga_trav == "Histogramme superposé (tableau croisé uniquement)") {
            showNotification(
              "NB: L'histogramme superposé n'offre pas une visualisation adapté aux tableaux ventilés !!!",
              type = "error",
              duration = 5
            )
          }
          
          data_rsu$tables_orga_trav_hist = tables_orga_trav_defaut() %>% slice(-nrow(.)) %>% mutate(value = Effectif, names = rownames(.)) %>% filter(Effectif !=
                                                                                                                                                        0)    # Création de la table à utilisé pour représenter l'histogramme conformément aux attendus de plotly
          data_rsu$tables_orga_trav_cam = tables_orga_trav_defaut() %>%  slice(-nrow(.)) %>% mutate(value = Effectif, names = rownames(.)) %>% filter(Effectif !=
                                                                                                                                                        0)   # Création de la table à utilisé pour représenter le camembert conformément aux attendus de plotly
          
          data_rsu$val_type_plot_orga_trav_1 = input$val_type_plot_orga_trav_1
          
          return(switch(
            input$plot_orga_trav,
            "Camembert" = cam(
              titre = input$plot_title_orga_trav_1,
              ma_data = data_rsu$tables_orga_trav_cam,
              type_aff = data_rsu$val_type_plot_orga_trav_1,
              couleur = input$colors_plot_orga_trav_1
            ),
            
            "Histogramme" = histo(
              titre = input$plot_title_orga_trav_1,
              ma_data = data_rsu$tables_orga_trav_hist,
              type_aff =  data_rsu$val_type_plot_orga_trav_1,
              couleur = input$colors_plot_orga_trav_1,
              axe_1 = input$plot_axe_orga_trav_1_1,
              axe_2 = input$plot_axe_orga_trav_1_2
            )
          ))
        } else{
          if (input$vent_crois_orga_trav == "Agent* exerçant en télétravail: nombre de jour TT* & catégorie") {
            if (input$plot_orga_trav == "Camembert") {
              showNotification(
                "NB: Le camembert n'offre pas une visualisation adapté aux tableaux croisés !!!",
                type = "error",
                duration = 5
              )
            }
            
            data_rsu$tables_orga_trav_hist = tables_orga_trav_defaut() %>% slice(-nrow(.)) %>% mutate(names = rownames(.)) %>% filter(Total !=
                                                                                                                                        0)
            names(data_rsu$tables_orga_trav_hist) = gsub("\\.", " ", names(data_rsu$tables_orga_trav_hist))
            
            data_rsu$val_type_plot_orga_trav_1 = input$val_type_plot_orga_trav_1
            
            return(switch(
              input$plot_orga_trav,
              "Histogramme" = histo_crois_3(
                titre = input$plot_title_orga_trav_1,
                ma_data = data_rsu$tables_orga_trav_hist,
                type_aff =  data_rsu$val_type_plot_orga_trav_1,
                couleur = input$colors_plot_orga_trav_1,
                axe_1 = input$plot_axe_orga_trav_1_1,
                axe_2 = input$plot_axe_orga_trav_1_2
              ),
              
              "Histogramme superposé (tableau croisé uniquement)" = histo_crois_3(
                titre = input$plot_title_orga_trav_1,
                ma_data = data_rsu$tables_orga_trav_hist,
                type_aff =  data_rsu$val_type_plot_orga_trav_1,
                couleur = input$colors_plot_orga_trav_1,
                axe_1 = input$plot_axe_orga_trav_1_1,
                axe_2 = input$plot_axe_orga_trav_1_2,
                sup = 'stack'
              )
            ))
          } else{
            if (input$plot_orga_trav == "Camembert") {
              showNotification(
                "NB: Le camembert n'offre pas une visualisation adapté aux tableaux croisés !!!",
                type = "error",
                duration = 5
              )
            }
            
            
            
            data_rsu$tables_orga_trav_hist = tables_orga_trav_defaut() %>% slice(-nrow(.)) %>% mutate(names = rownames(.)) %>% filter(Total !=
                                                                                                                                        0)
            names(data_rsu$tables_orga_trav_hist) = gsub("\\.", " ", names(data_rsu$tables_orga_trav_hist))
            
            data_rsu$val_type_plot_orga_trav_1 = input$val_type_plot_orga_trav_1
            
            return(switch(
              input$plot_orga_trav,
              "Histogramme" = histo_crois_2(
                titre = input$plot_title_orga_trav_1,
                ma_data = data_rsu$tables_orga_trav_hist,
                type_aff =  data_rsu$val_type_plot_orga_trav_1,
                couleur = input$colors_plot_orga_trav_1,
                axe_1 = input$plot_axe_orga_trav_1_1,
                axe_2 = input$plot_axe_orga_trav_1_2
              ),
              
              "Histogramme superposé (tableau croisé uniquement)" = histo_crois_2(
                titre = input$plot_title_orga_trav_1,
                ma_data = data_rsu$tables_orga_trav_hist,
                type_aff =  data_rsu$val_type_plot_orga_trav_1,
                couleur = input$colors_plot_orga_trav_1,
                axe_1 = input$plot_axe_orga_trav_1_1,
                axe_2 = input$plot_axe_orga_trav_1_2,
                sup = 'stack'
              )
            ))
          }
        }
        
      })
      
      
      
      
      #------> Sorties affichage des graphiques
      output$plot_vent_crois_orga_trav = renderPlotly({
        if (is.null(input$vent_crois_btn_orga_trav) != T) {
          tryCatch({
            plot_orga_trav_dataset()
          }, error = function(e) {
            
          })
        }
      })
      
    }, error = function(e) {
      showNotification("Valider d'abord votre partition",
                       type = "error",
                       duration = 5)
    })
  })
  
  
  ###-------------------------------------------Fin code de conception des tables & graphes  organisation du travail
  
  
  
  
  ## ------------------------------------------Code de conception des tables & graphes manuelles  orgnaisation du personnel---------------------------------------------------
  
  output$manuelle_vent_orga_trav = DT::renderDT({
    if (is.null(input$manuelle_btn_orga_trav) != T) {
      # Affichage de la table si la case  " Tableau composé manuellement" est coché
      
      tryCatch({
        tranche_manuelle_orga_trav = data_rsu$tranche_orga_trav %>%    # Constitution de la table issue de la séléction manuelle
          select(input$select_manuelle_vent_orga_trav)  %>% t() %>%
          data.frame() %>%
          rename(Effectif = names(.))
        
        table_manuelle(ma_table = tranche_manuelle_orga_trav)
        
      }, error = function(e) {
        
      })
    }
  })
  
  ##--------> représentation des tableaux ventilés: graphes 2 sous table  mouvement du perso crée manuellement
  
  plot_orga_trav_manuelle_dataset = reactive({
    tranche_manuelle_orga_trav = data_rsu$tranche_orga_trav %>%
      select(input$select_manuelle_vent_orga_trav) %>% t() %>% data.frame() %>%
      rename(Effectif = names(.)) %>%
      mutate(names = gsub("\\.", " ", sub("[0-9]*- ", "", rownames(.))))
    
    tranche_manuelle_orga_trav_plot = tranche_manuelle_orga_trav %>% filter(Effectif !=
                                                                              0)
    
    #--------> fonction switch permettant d'avoir des rendu au relatif aux choix graphiques faites par l'utilisateur
    observeEvent(input$select_manuelle_vent_orga_trav, {
      updateTextInput(session,
                      "legend_plot_orga_trav",
                      value = paste(gsub(
                        "\\.", " ", sub(
                          "[0-9]*- ",
                          "",
                          rownames(tranche_manuelle_orga_trav_plot)
                        )
                      ), sep = ",")) # mise à jour des colonnes à séléctionner pour constitué les légendes
    })
    
    rownames(tranche_manuelle_orga_trav_plot) = input$legend_plot_orga_trav %>% strsplit(., split = ",") %>% unlist() %>% as.vector() # Mise à jour de la légende à travers le nom des lignes
    data_rsu$val_type_plot_orga_trav_2 = input$val_type_plot_orga_trav_2
    
    
    if (is.null(c(
      input$select_manuelle_vent_orga_trav,
      input$manuelle_btn_orga_trav
    )) != T   &
    input$plot_orga_trav == "Histogramme superposé (tableau croisé uniquement)") {
      showNotification(
        "NB: L'histogramme superposé n'offre pas une visualisation adapté aux tableaux ventilés !!!",
        type = "error",
        duration = 5
      )
    }
    
    switch(
      input$plot_orga_trav,
      "Camembert" = cam(
        titre = input$plot_title_orga_trav_2,
        ma_data = tranche_manuelle_orga_trav_plot,
        type_aff = data_rsu$val_type_plot_orga_trav_2,
        couleur = input$colors_plot_orga_trav_2,
        noms_legende = input$legend_plot_orga_trav %>% strsplit(., split = ",") %>% unlist() %>% as.vector()
      ),
      "Histogramme" = histo(
        titre = input$plot_title_orga_trav_2,
        ma_data = tranche_manuelle_orga_trav_plot,
        type_aff = data_rsu$val_type_plot_orga_trav_2,
        couleur = input$colors_plot_orga_trav_2,
        noms_legende = input$legend_plot_orga_trav %>% strsplit(., split = ",") %>% unlist() %>% as.vector()
        ,
        axe_1 = input$plot_axe_orga_trav_2_1,
        axe_2 = input$plot_axe_orga_trav_2_2
      )
      
    )
  })
  
  #--------->  Graphiques associés aux table 2_1 et 2_2
  
  output$plot_orga_trav_manuelle =  renderPlotly({
    if (is.null(input$manuelle_btn_orga_trav) != T) {
      tryCatch({
        plot_orga_trav_manuelle_dataset()
      }, error = function(e) {
        
      })
    }
  })
  
  ## -------------------------------------------Fin Code de conception des tables & graphes manuelles  orgnaisation du personnel---------------------------------------------------
  
  
  
  
  
  
  ## -------------------------------------------Code de conception des tables & graphes par défaut parcours_proffessionnel---------------------------------------------------
  
  
  sous_theme_parc_pro_dataset = reactive({
    #Initialisation des sous-thèmes abordés
    switch(
      input$sous_theme_parc_pro,
      "A- Postes pourvus" = c(
        "Statut d’emploi du candidat retenu",
        "Pourvu par des candidats externes ou internes",
        "Postes pourvus: Sexe & périmètre de provenance",
        "Postes pourvus: Sexe & Statut d'emploi"
      ),
      "B- Avancement de grade et promotion interne" = c(
        "Postes promus & promouvable: en fonction du sexe",
        "Postes promus: croisé par sexe & catégorie",
        "Postes promouvable: croisé par sexe & catégorie",
        "Réussites aux concours et examen pro: par catégorie/statuts & sexe"
      ),
      "Informations supplémentaires" = c("Réponses qualitatives")
    )
  })
  
  observe({
    updateSelectInput(session, "tables_select_parc_pro", choices = sous_theme_parc_pro_dataset()) # mise à jour des colonnes à séléctionner pour sélectionner le sous thème étudier dans le parcours professionnel
  })
  
  #------> constitution table ventilé 1_1 et 1_2 mobilité
  
  #Recherche des colonnes par recherche sur les mots clées
  
  
  observeEvent(input$vent_crois_btn_parc_pro, {
    tryCatch({
      pourvus_exterieurs = colonne_match(
        data = data_rsu$tranche_parc_pro,
        vecteurs_mots = c("pourvu", "extérieur", "périmètre")
      )
      
      pourvus_internes = colonne_match(
        data = data_rsu$tranche_parc_pro,
        vecteurs_mots = c("pourvu", "interne")
      )
      
      pourvus_contactuels = colonne_match(
        data = data_rsu$tranche_parc_pro,
        vecteurs_mots = c("pourvu", "contractuel")
      )  # vecteur constitué de la somme des colonnes de poste pourvu contractuelle
      
      pourvus_titulaires = colonne_match(
        data = data_rsu$tranche_parc_pro,
        vecteurs_mots = c("pourvu", "Titulaires")
      )
      
      #------> constitution table croisé 1_3 et 1_4 mobilité------------------------------------------------------------------------------
      
      femmes_contra_poste_1 = colonne_match(
        data = data_rsu$tranche_parc_pro,
        vecteurs_mots =  c("pourvu", "femmes", "contractuel")
      )
      
      femmes_titu_poste_1 = colonne_match(
        data = data_rsu$tranche_parc_pro,
        vecteurs_mots =  c("pourvu", "femmes", "titulaires")
      )
      
      hommes_contra_poste_1 = colonne_match(
        data = data_rsu$tranche_parc_pro,
        vecteurs_mots =  c("pourvu", "hommes", "contractuel")
      )
      
      hommes_titu_poste_1 = colonne_match(
        data = data_rsu$tranche_parc_pro,
        vecteurs_mots =  c("pourvu", "hommes", "titulaires")
      )
      
      femmes_ext_poste_2 = colonne_match(
        data = data_rsu$tranche_parc_pro,
        vecteurs_mots =   c("extérieur", "femmes")
      )
      
      femmes_int_poste_2 = colonne_match(
        data = data_rsu$tranche_parc_pro,
        vecteurs_mots =   c("interne", "femmes")
      )
      
      hommes_ext_poste_2 = colonne_match(
        data = data_rsu$tranche_parc_pro,
        vecteurs_mots = c("extérieur", "hommes")
      )
      
      hommes_int_poste_2 = colonne_match(
        data = data_rsu$tranche_parc_pro,
        vecteurs_mots = c("interne", "hommes")
      )
      
      #----------------------------------------------------------------------------------------------------------------------------------
      
      tables_parc_pro_postes = reactive({
        switch(
          input$tables_select_parc_pro,
          "Pourvu par des candidats externes ou internes" = data.frame(
            "Candidats externes" = pourvus_exterieurs,
            "Candidats internes" = pourvus_internes
          ) %>%
            mutate(
              "Total des postes pourvus au cours de l'année" = sum(pourvus_exterieurs, pourvus_internes)
            )  %>%
            t() %>% data.frame() %>% mutate_all(as.double) %>% rename(Effectif = names(.)),
          
          "Statut d’emploi du candidat retenu" = data.frame(
            "Statut de contractuel" = pourvus_contactuels,
            "Statut de titulaire" = pourvus_titulaires
          )  %>%
            mutate(
              "Total des statuts d'emploi ayant pourvu un poste cours de l'année" = sum(pourvus_contactuels, pourvus_titulaires)
            ) %>%
            t() %>% data.frame() %>% mutate_all(as.double) %>% rename(Effectif = names(.)),
          
          "Postes pourvus: Sexe & périmètre de provenance" = data.frame(
            "Femmes" = c(
              femmes_int_poste_2,
              femmes_ext_poste_2,
              femmes_ext_poste_2 + femmes_int_poste_2
            ),
            # Constitution de la table issue du croisement  sexe et périmètre de provenance
            "Hommes" = c(
              hommes_int_poste_2,
              hommes_ext_poste_2,
              hommes_ext_poste_2 + hommes_int_poste_2
            ),
            row.names = c("Interne à la DDI", "Externe à la DDI", "Ensemble")
          ) %>% mutate(Total =  rowSums(.)),
          
          "Postes pourvus: Sexe & Statut d'emploi" = data.frame(
            "Femmes" = c(
              femmes_contra_poste_1,
              femmes_titu_poste_1,
              femmes_contra_poste_1 + femmes_titu_poste_1
            ),
            ## Constitution de la table issus du croisement  sexe et statut d'emploi de provenance
            "Hommes" = c(
              hommes_contra_poste_1,
              hommes_titu_poste_1,
              hommes_contra_poste_1 + hommes_titu_poste_1
            ),
            row.names = c("Contractuel(le)", "Titulaires", "Ensemble")
          ) %>% mutate(Total =  rowSums(.))
        )
      })
      
      ##-------------------------------------------------Avancement de grade-----------------------------------------
      
      avance_femmes_cat_A = colonne_match(
        data = data_rsu$tranche_parc_pro,
        vecteurs_mots =  c("Femme", "Catégorie A"),
        somme = FALSE
      )
      
      avance_femmes_cat_B = colonne_match(
        data = data_rsu$tranche_parc_pro,
        vecteurs_mots =  c("Femme", "Catégorie B"),
        somme = FALSE
      )
      
      avance_femmes_cat_C = colonne_match(
        data = data_rsu$tranche_parc_pro,
        vecteurs_mots =  c("Femme", "Catégorie C"),
        somme = FALSE
      )
      
      avance_hommes_cat_A = colonne_match(
        data = data_rsu$tranche_parc_pro,
        vecteurs_mots =  c("Homme", "Catégorie A"),
        somme = FALSE
      )
      
      avance_hommes_cat_B = colonne_match(
        data = data_rsu$tranche_parc_pro,
        vecteurs_mots =  c("Homme", "Catégorie B"),
        somme = FALSE
      )
      
      avance_hommes_cat_C = colonne_match(
        data = data_rsu$tranche_parc_pro,
        vecteurs_mots =  c("Homme", "Catégorie C"),
        somme = FALSE
      )
      
      #--------------------------------------------------------------------------------------
      
      tables_parc_pro_avance = reactive({
        tryCatch({
          switch(
            input$tables_select_parc_pro,
            "Postes promus & promouvable: en fonction du sexe" = data.frame(
              "Hommes" = c(
                sum(
                  avance_hommes_cat_A[1],
                  avance_hommes_cat_B[1],
                  avance_hommes_cat_C[1]
                ),
                sum(
                  avance_hommes_cat_A[2],
                  avance_hommes_cat_B[2],
                  avance_hommes_cat_C[2]
                ),
                round(100 * (
                  sum(
                    avance_hommes_cat_A[1],
                    avance_hommes_cat_B[1],
                    avance_hommes_cat_C[1]
                  ) / sum(
                    avance_hommes_cat_A[2],
                    avance_hommes_cat_B[2],
                    avance_hommes_cat_C[2]
                  )
                ), 1)
              ) ,
              
              "Femmes" = c(
                sum(
                  avance_femmes_cat_A[1],
                  avance_femmes_cat_B[1],
                  avance_femmes_cat_C[1]
                ),
                sum(
                  avance_femmes_cat_A[2],
                  avance_femmes_cat_B[2],
                  avance_femmes_cat_C[2]
                ),
                round(100 * (
                  sum(
                    avance_femmes_cat_A[1],
                    avance_femmes_cat_B[1],
                    avance_femmes_cat_C[1]
                  ) / sum(
                    avance_femmes_cat_A[2],
                    avance_femmes_cat_B[2],
                    avance_femmes_cat_C[2]
                  )
                ), 1)
              ),
              row.names = c("Promus", "Promouvable", "Taux de promotion en %")
            ) %>% mutate(Total =  rowSums(.)) ,
            
            "Postes promus: croisé par sexe & catégorie" = data.frame(
              "H_promus" = c(
                avance_hommes_cat_A[1, 1],
                avance_hommes_cat_B[1, 1],
                avance_hommes_cat_C[1, 1]
              ),
              "F_promus" =
                c(
                  avance_femmes_cat_A[1, 1],
                  avance_femmes_cat_B[1, 1],
                  avance_femmes_cat_C[1, 1]
                ),
              row.names = c("Catégorie A", "Catégorie B", "Catégorie C")
            ) %>%
              bind_rows(
                data.frame(
                  row.names = "Ensemble",
                  H_promus = colSums(.)[1],
                  F_promus = colSums(.)[2]
                )
              ) %>% mutate(Total =  rowSums(.)),
            
            "Postes promouvable: croisé par sexe & catégorie" = data.frame(
              "H_promouvable" = c(
                avance_hommes_cat_A[1, 2],
                avance_hommes_cat_B[1, 2],
                avance_hommes_cat_C[1, 2]
              ),
              "F_promouvable" =
                c(
                  avance_femmes_cat_A[1, 2],
                  avance_femmes_cat_B[1, 2],
                  avance_femmes_cat_C[1, 2]
                ),
              row.names = c("Catégorie A", "Catégorie B", "Catégorie C")
            )  %>%
              bind_rows(
                data.frame(
                  row.names = "Ensemble",
                  H_promouvable = colSums(.)[1],
                  F_promouvable = colSums(.)[2]
                )
              ) %>% mutate(Total =  rowSums(.)),
            
            "Réussites aux concours et examen pro: par catégorie/statuts & sexe" = data.frame(
              "H_Réussite" = c(
                avance_hommes_cat_A[1, 3],
                avance_hommes_cat_B[1, 3],
                avance_hommes_cat_C[1, 3]
              ),
              "F_Réussite" =
                c(
                  avance_femmes_cat_A[1, 3],
                  avance_femmes_cat_B[1, 3],
                  avance_femmes_cat_C[1, 3]
                ),
              row.names = c("Catégorie A", "Catégorie B", "Catégorie C")
            ) %>%
              bind_rows(
                data.frame(
                  row.names = "Ensemble",
                  H_Réussite = colSums(.)[1],
                  F_Réussite = colSums(.)[2]
                )
              ) %>% mutate(Total =  rowSums(.))
            
          )
        }, error = function(e) {
          showNotification(
            "Pas de données disponible pour cette tranche!!!",
            type = "warning",
            duration = 5
          )
        })
        
      })
      
      
      
      
      
      tables_parc_pro_info_suppl = reactive({
        tryCatch({
          switch(
            input$tables_select_parc_pro,
            "Réponses qualitatives" = data_rsu$tranche_parc_pro %>% select_if( ~ is.character(.) &
                                                                                 !(is.na(.))) %>% t() %>%
              data.frame() %>% rename(Reponse = names(.))
            
            
          )
        }, error = function(e) {
          showNotification(
            "Pas de données disponible pour cette tranche!!!",
            type = "warning",
            duration = 5
          )
        })
        
      })
      
      ##------------------------------------------------- fin tables avancement de grade-----------------------------------------
      
      
      
      ##----> transformation en objet datatable des tables
      
      output$vent_crois_parc_pro = DT::renderDT({
        #Affichage de la tranche de table si l'utilisateur coche la case "Tableau ... défaut"
        tryCatch({
          if (is.null(input$vent_crois_btn_parc_pro) != TRUE &
              input$sous_theme_parc_pro == "A- Postes pourvus") {
            table_classic(ma_table = tables_parc_pro_postes())
          } else{
            if (is.null(input$vent_crois_btn_parc_pro) != TRUE &
                input$sous_theme_parc_pro == "B- Avancement de grade et promotion interne") {
              table_classic(ma_table = tables_parc_pro_avance())
            } else{
              if (is.null(input$vent_crois_btn_parc_pro) != TRUE  &
                  input$sous_theme_parc_pro == "Informations supplémentaires") {
                table_manuelle(ma_table = tables_parc_pro_info_suppl())
              }
            }
          }
        }, error = function(e) {
          
        })
      })
      
      ##---------------------> Fin de conception des tableaux par défauts du parcours professionnel
      
      
      #-----------------> Représentation graphiques des tableaux croisés
      
      plot_parc_pro_dataset = reactive({
        ifelse(input$sous_theme_parc_pro == "A- Postes pourvus",
               ##--------------------------------------------------Graphiques postes pourvus-----------------------------------
               
               if (input$tables_select_parc_pro != "Statut d’emploi du candidat retenu" &
                   input$tables_select_parc_pro != "Pourvu par des candidats externes ou internes") {
                 if (input$plot_parc_pro == "Camembert") {
                   showNotification(
                     "NB: Le camembert n'offre pas une visualisation adapté aux tableaux croisés !!!",
                     type = "error",
                     duration = 5
                   )
                 }
                 
                 data_rsu$crois_parc_pro_postes_hist = tables_parc_pro_postes() %>% slice(-nrow(.)) %>% mutate(names = rownames(.))  %>% filter(Total !=
                                                                                                                                                  0)
                 
                 data_rsu$val_type_plot_parc_pro_1 = input$val_type_plot_parc_pro_1
                 
                 
                 return(switch(
                   input$plot_parc_pro,
                   "Histogramme" = histo_crois_2(
                     titre = input$plot_title_parc_pro_1,
                     ma_data = data_rsu$crois_parc_pro_postes_hist,
                     type_aff =  data_rsu$val_type_plot_parc_pro_1,
                     couleur = input$colors_plot_parc_pro_1,
                     axe_1 = input$plot_axe_parc_pro_1_1,
                     axe_2 = input$plot_axe_parc_pro_1_2
                   ),
                   
                   "Histogramme superposé (tableau croisé uniquement)" = histo_crois_2(
                     titre = input$plot_title_parc_pro_1,
                     ma_data = data_rsu$crois_parc_pro_postes_hist,
                     type_aff =  data_rsu$val_type_plot_parc_pro_1,
                     couleur = input$colors_plot_parc_pro_1,
                     axe_1 = input$plot_axe_parc_pro_1_1,
                     axe_2 = input$plot_axe_parc_pro_1_2,
                     sup = 'stack'
                   )
                 ))
               }
               
               
               ##--------------------------------------------------Graphiques avancement de grade-----------------------------------
               else{
                 if (input$plot_parc_pro == "Histogramme superposé (tableau croisé uniquement)") {
                   showNotification(
                     "NB: L'histogramme superposé n'offre pas une visualisation adapté aux tableaux ventilés !!!",
                     type = "error",
                     duration = 5
                   )
                 }
                 
                 input$tables_select_parc_pro != "Statut d’emploi du candidat retenu" &
                   input$tables_select_parc_pro != "Pourvu par des candidats externes ou internes"
                 
                 # Création de la table à utilisé pour représenter le camembert conformément aux attendus de plotly
                 data_rsu$tables_parc_pro_postes_cam = tables_parc_pro_postes() %>%   slice(-nrow(.)) %>%
                   mutate(names = gsub("\\.", " ", sub("[0-9]*- ", "", rownames(.))), value = Effectif)  %>% filter(Effectif !=
                                                                                                                      0)
                 
                 data_rsu$tables_parc_pro_postes_hist = tables_parc_pro_postes() %>%  # Création de la table à utilisé pour représenter l'histogramme conformément aux attendus de plotly
                   slice(-nrow(.)) %>% mutate(names = gsub("\\.", " ", sub("[0-9]*- ", "", rownames(.)))) %>% filter(Effectif !=
                                                                                                                       0)   ## Création de la table à utilisé pour représenter l'Histogramme conformément aux attendus de plotly
                 
                 
                 
                 data_rsu$val_type_plot_parc_pro_1 = input$val_type_plot_parc_pro_1
                 
                 return(switch(
                   input$plot_parc_pro,
                   "Camembert" = cam(
                     titre = input$plot_title_parc_pro_1,
                     ma_data = data_rsu$tables_parc_pro_postes_cam,
                     type_aff = data_rsu$val_type_plot_parc_pro_1,
                     couleur = input$colors_plot_parc_pro_1
                   ),
                   
                   "Histogramme" = histo(
                     titre = input$plot_title_parc_pro_1,
                     ma_data = data_rsu$tables_parc_pro_postes_hist,
                     type_aff =  data_rsu$val_type_plot_parc_pro_1,
                     couleur = input$colors_plot_parc_pro_1,
                     axe_1 = input$plot_axe_parc_pro_1_1,
                     axe_2 = input$plot_axe_parc_pro_1_2
                   )
                 ))
               },
               if (input$sous_theme_parc_pro == "B- Avancement de grade et promotion interne") {
                 if (input$plot_parc_pro == "Camembert") {
                   showNotification(
                     "NB: Le camembert n'offre pas une visualisation adapté aux tableaux croisés !!!",
                     type = "error",
                     duration = 5
                   )
                 }
                 
                 
                 data_rsu$tables_parc_pro_avance_hist = tryCatch({
                   tables_parc_pro_avance() %>% slice(-nrow(.)) %>% mutate(names = rownames(.))  %>% filter(Total !=
                                                                                                              0)
                 }, error = function(e) {
                   
                 })
                 
                 data_rsu$val_type_plot_parc_pro_1 = input$val_type_plot_parc_pro_1
                 
                 
                 return(switch(
                   input$plot_parc_pro,
                   "Histogramme" = histo_crois_2(
                     titre = input$plot_title_parc_pro_1,
                     ma_data = data_rsu$tables_parc_pro_avance_hist,
                     type_aff =  data_rsu$val_type_plot_parc_pro_1,
                     couleur = input$colors_plot_parc_pro_1,
                     axe_1 = input$plot_axe_parc_pro_1_1,
                     axe_2 = input$plot_axe_parc_pro_1_2
                   ),
                   
                   "Histogramme superposé (tableau croisé uniquement)" = histo_crois_2(
                     titre = input$plot_title_parc_pro_1,
                     ma_data = data_rsu$tables_parc_pro_avance_hist,
                     type_aff =  data_rsu$val_type_plot_parc_pro_1,
                     couleur = input$colors_plot_parc_pro_1,
                     axe_1 = input$plot_axe_parc_pro_1_1,
                     axe_2 = input$plot_axe_parc_pro_1_2,
                     sup = 'stack'
                   )
                 ))
               })
      })
      
      
      #------> Sorties affichage des tableaux 1_1, 1_2, 1_3, et 1_4
      output$plot_vent_crois_parc_pro = renderPlotly({
        if (is.null(input$vent_crois_btn_parc_pro) != T) {
          tryCatch({
            plot_parc_pro_dataset()
          }, error = function(e) {
            
          })
        }
      })
    }, error = function(e) {
      showNotification("Valider d'abord votre partition",
                       type = "error",
                       duration = 5)
    })
    
    
  })
  
  ## -------------------------------------------Fin Code de conception des tables & graphes  parcours_proffessionnel/mobilité---------------------------------------------------
  
  
  
  
  ## ------------------------------------------Code de conception des tables & graphes manuelles  du parcours professionnel---------------------------------------------------
  
  output$manuelle_vent_parc_pro = DT::renderDT({
    if (is.null(input$manuelle_btn_parc_pro) != T) {
      # Affichage de la table si la case  " Tableau composé manuellement" est coché
      
      tryCatch({
        tranche_manuelle_parc_pro = data_rsu$tranche_parc_pro %>%    # Constitution de la table issue de la séléction manuelle
          select(input$select_manuelle_vent_parc_pro) %>% t() %>%
          data.frame() %>%
          rename(Effectif = names(.))
        
        table_manuelle(ma_table = tranche_manuelle_parc_pro)
        
      }, error = function(e) {
        
      })
    }
  })
  
  ##--------> représentation des tableaux ventilés: graphes 2 sous table  mouvement du perso crée manuellement
  
  plot_parc_pro_manuelle_dataset = reactive({
    tranche_manuelle_parc_pro = data_rsu$tranche_parc_pro %>%
      select(input$select_manuelle_vent_parc_pro) %>% t() %>% data.frame() %>%
      rename(Effectif = names(.)) %>%
      mutate(names = gsub("\\.", " ", sub("[0-9]*- ", "", rownames(.))))
    
    tranche_manuelle_parc_pro_plot = tranche_manuelle_parc_pro %>% filter(Effectif !=
                                                                            0)
    
    #--------> fonction switch permettant d'avoir des rendu au relatif aux choix graphiques faites par l'utilisateur
    observeEvent(input$select_manuelle_vent_parc_pro, {
      updateTextInput(session, "legend_plot_parc_pro", value = paste(gsub(
        "\\.", " ", sub(
          "[0-9]*- ",
          "",
          rownames(tranche_manuelle_parc_pro_plot)
        )
      ), sep = ",")) # mise à jour des colonnes à séléctionner pour constitué les légendes
    })
    
    rownames(tranche_manuelle_parc_pro_plot) = input$legend_plot_parc_pro %>% strsplit(., split = ",") %>% unlist() %>% as.vector() # Mise à jour de la légende à travers le nom des lignes
    data_rsu$val_type_plot_parc_pro_2 = input$val_type_plot_parc_pro_2
    
    
    if (is.null(c(
      input$select_manuelle_vent_parc_pro,
      input$manuelle_btn_parc_pro
    )) != T   &
    input$plot_parc_pro == "Histogramme superposé (tableau croisé uniquement)") {
      showNotification(
        "NB: L'histogramme superposé n'offre pas une visualisation adapté aux tableaux ventilés !!!",
        type = "error",
        duration = 5
      )
    }
    
    switch(
      input$plot_parc_pro,
      "Camembert" = cam(
        titre = input$plot_title_parc_pro_2,
        ma_data = tranche_manuelle_parc_pro_plot,
        type_aff = data_rsu$val_type_plot_parc_pro_2,
        couleur = input$colors_plot_parc_pro_2,
        noms_legende = input$legend_plot_parc_pro %>% strsplit(., split = ",") %>% unlist() %>% as.vector()
      ),
      "Histogramme" = histo(
        titre = input$plot_title_parc_pro_2,
        ma_data = tranche_manuelle_parc_pro_plot,
        type_aff = data_rsu$val_type_plot_parc_pro_2,
        couleur = input$colors_plot_parc_pro_2,
        noms_legende = input$legend_plot_parc_pro %>% strsplit(., split = ",") %>% unlist() %>% as.vector()
        ,
        axe_1 = input$plot_axe_parc_pro_2_1,
        axe_2 = input$plot_axe_parc_pro_2_2
      )
      
    )
  })
  
  #--------->  Graphiques associés aux tables manuelles
  
  output$plot_parc_pro_manuelle =  renderPlotly({
    if (is.null(input$manuelle_btn_parc_pro) != T) {
      tryCatch({
        plot_parc_pro_manuelle_dataset()
      }, error = function(e) {
        
      })
    }
  })
  
  ## -------------------------------------------Fin Code de conception des tables & graphes manuelles  du parcours professionnel---------------------------------------------------
  
  
  
  ## -------------------------------------------Code de conception des tables & graphes par défaut de la sanction disiplinaire---------------------------------------------------
  
  
  sous_theme_sanc_discip_dataset = reactive({
    #Initialisation des sous-thèmes abordés
    switch(
      input$sous_theme_sanc_discip,
      "A- Sanctions disciplinaires notifiés au cours de l'année en cours" = c(
        "Sanctions disciplinaires du 1er groupe: par type de sanction",
        "Sanctions disciplinaires : par type de groupe",
        "Sanctions disciplinaires du 1er groupe: type de sanction & statut professionnel",
        "Sanctions disciplinaires du 1er groupe: type de sanction & genre",
        "Sanctions disciplinaires : type de groupe & genre",
        "Sanctions disciplinaires : type de groupe & statut professionnel",
        "Sanctions ayant un lien direct avec sanction d'un agissement sexiste ou violence à caractère sexuelle : par type de groupe"
      ),
      "Informations supplémentaires" = c("Réponses qualitatives")
    )
  })
  
  observe({
    updateSelectInput(session, "vent_crois_sanc_discip", choices = sous_theme_sanc_discip_dataset()) # mise à jour des colonnes à séléctionner pour sélectionner le sous thème étudier dans la sanction disciplinaire
  })
  
  
  #Recherche des colonnes par recherche sur les mots clées
  
  observeEvent(input$vent_crois_btn_sanc_discip, {
    tryCatch({
      #Premier groupe
      
      prm_grpe_avert_femme_tit_sanc_discip = colonne_match(
        data = data_rsu$tranche_sanc_discip,
        vecteurs_mots =  c("avertissement", "femme", "titulaire")
      )
      prm_grpe_avert_homme_tit_sanc_discip = colonne_match(
        data = data_rsu$tranche_sanc_discip,
        vecteurs_mots =  c("avertissement", "homme", "titulaire")
      )
      prm_grpe_avert_femme_contr_sanc_discip = colonne_match(
        data = data_rsu$tranche_sanc_discip,
        vecteurs_mots =  c("avertissement", "femme", "contractuel")
      )
      prm_grpe_avert_homme_contr_sanc_discip = colonne_match(
        data = data_rsu$tranche_sanc_discip,
        vecteurs_mots =  c("avertissement", "homme", "contractuel")
      )
      
      
      prm_grpe_blm_femme_tit_sanc_discip = colonne_match(
        data = data_rsu$tranche_sanc_discip,
        vecteurs_mots =  c("blâme", "femme", "titulaire")
      )
      prm_grpe_blm_homme_tit_sanc_discip = colonne_match(
        data = data_rsu$tranche_sanc_discip,
        vecteurs_mots =  c("blâme", "homme", "titulaire")
      )
      prm_grpe_blm_femme_contr_sanc_discip = colonne_match(
        data = data_rsu$tranche_sanc_discip,
        vecteurs_mots =  c("blâme", "femme", "contractuel")
      )
      prm_grpe_blm_homme_contr_sanc_discip = colonne_match(
        data = data_rsu$tranche_sanc_discip,
        vecteurs_mots =  c("blâme", "homme", "contractuel")
      )
      #Tout groupe confondu
      
      grpe_femme_tit_sanc_discip = colonne_match(
        data = data_rsu$tranche_sanc_discip,
        vecteurs_mots =  c("Femme", "titulaire"),
        somme = F
      )
      grpe_femme_contr_sanc_discip = colonne_match(
        data = data_rsu$tranche_sanc_discip,
        vecteurs_mots =  c("Femme", "contractuel"),
        somme = F
      )
      grpe_homme_tit_sanc_discip = colonne_match(
        data = data_rsu$tranche_sanc_discip,
        vecteurs_mots =  c("Homme", "titulaire"),
        somme = F
      )
      grpe_homme_contr_sanc_discip = colonne_match(
        data = data_rsu$tranche_sanc_discip,
        vecteurs_mots =  c("Homme", "contractuel"),
        somme = F
      )
      
      sct_discip_agsmt_sexiste_sanc_discip = colonne_match(
        data = data_rsu$tranche_sanc_discip,
        vecteurs_mots =  c(
          "sanction",
          "agissement",
          "sexiste",
          "violence",
          "sexuel"
        ),
        somme = F
      )
      
      
      
      # Conception des objets tableau par défauts
      
      tables_sanc_discip_defaut = reactive({
        tryCatch({
          switch(
            input$vent_crois_sanc_discip,
            
            
            "Sanctions disciplinaires du 1er groupe: par type de sanction" = data.frame(
              row.names = c("Avertissements notifiés", "Blames notifiés"),
              "Effectif" =
                c(
                  sum(
                    prm_grpe_avert_femme_tit_sanc_discip,
                    prm_grpe_avert_homme_tit_sanc_discip,
                    prm_grpe_avert_femme_contr_sanc_discip,
                    prm_grpe_avert_homme_contr_sanc_discip,
                    sct_discip_agsmt_sexiste_sanc_discip[1, 1]
                  ),
                  
                  sum(
                    prm_grpe_blm_femme_tit_sanc_discip,
                    prm_grpe_blm_homme_tit_sanc_discip,
                    prm_grpe_blm_femme_contr_sanc_discip,
                    prm_grpe_blm_homme_contr_sanc_discip,
                    sct_discip_agsmt_sexiste_sanc_discip[1, 2]
                  )
                )
            ) %>%
              bind_rows(
                data.frame(row.names = "Ensemble", "Effectif" = colSums(.)[1])
              ) ,
            
            "Sanctions disciplinaires du 1er groupe: type de sanction & statut professionnel" = data.frame(
              row.names = c("Avertissements notifiés", "Blames notifiés"),
              "Titulaires" = c(
                prm_grpe_avert_femme_tit_sanc_discip + prm_grpe_avert_homme_tit_sanc_discip,
                prm_grpe_blm_femme_tit_sanc_discip + prm_grpe_blm_homme_tit_sanc_discip
              ),
              "Contractuels" = c(
                prm_grpe_avert_femme_contr_sanc_discip + prm_grpe_avert_homme_contr_sanc_discip,
                prm_grpe_blm_femme_contr_sanc_discip + prm_grpe_blm_homme_contr_sanc_discip
              )
            ) %>%
              bind_rows(
                data.frame(
                  row.names = "Ensemble",
                  "Titulaires" = colSums(.)[1],
                  "Contractuels" = colSums(.)[2]
                )
              ) %>% mutate(Total =  rowSums(.)),
            
            "Sanctions disciplinaires du 1er groupe: type de sanction & genre" = data.frame(
              row.names = c("Avertissements notifiés", "Blames notifiés"),
              "Femmes" =
                c(
                  prm_grpe_avert_femme_tit_sanc_discip + prm_grpe_avert_femme_contr_sanc_discip,
                  prm_grpe_blm_femme_tit_sanc_discip + prm_grpe_blm_femme_contr_sanc_discip
                ),
              "Hommes" = c(
                prm_grpe_avert_homme_tit_sanc_discip + prm_grpe_avert_homme_contr_sanc_discip,
                prm_grpe_blm_homme_tit_sanc_discip + prm_grpe_blm_homme_contr_sanc_discip
              )
            ) %>%
              bind_rows(
                data.frame(
                  row.names = "Ensemble",
                  Femmes = colSums(.)[1],
                  Hommes = colSums(.)[2]
                )
              ) %>% mutate(Total =  rowSums(.)),
            
            "Sanctions disciplinaires : par type de groupe" = data.frame(
              row.names = c(
                "1er groupe",
                "2ème groupe",
                "3ème groupe",
                "4ème groupe"
              ),
              "Effectif" =
                c(
                  sum(
                    prm_grpe_avert_femme_tit_sanc_discip,
                    prm_grpe_avert_homme_tit_sanc_discip,
                    prm_grpe_avert_femme_contr_sanc_discip,
                    prm_grpe_avert_homme_contr_sanc_discip,
                    prm_grpe_blm_femme_tit_sanc_discip,
                    prm_grpe_blm_homme_tit_sanc_discip,
                    prm_grpe_blm_femme_contr_sanc_discip,
                    prm_grpe_blm_homme_contr_sanc_discip,
                    sct_discip_agsmt_sexiste_sanc_discip[1, 1],
                    sct_discip_agsmt_sexiste_sanc_discip[1, 2]
                  ),
                  
                  sum(
                    grpe_femme_tit_sanc_discip[1, 1],
                    grpe_femme_contr_sanc_discip[1, 1],
                    grpe_homme_tit_sanc_discip[1, 1],
                    grpe_homme_contr_sanc_discip[1, 1],
                    sct_discip_agsmt_sexiste_sanc_discip[1, 3]
                  ),
                  
                  sum(
                    grpe_femme_tit_sanc_discip[1, 2],
                    grpe_femme_contr_sanc_discip[1, 2],
                    grpe_homme_tit_sanc_discip[1, 2],
                    grpe_homme_contr_sanc_discip[1, 2],
                    sct_discip_agsmt_sexiste_sanc_discip[1, 4]
                  ),
                  
                  sum(
                    grpe_femme_tit_sanc_discip[1, 3],
                    grpe_femme_contr_sanc_discip[1, 3],
                    grpe_homme_tit_sanc_discip[1, 3],
                    grpe_homme_contr_sanc_discip[1, 3],
                    sct_discip_agsmt_sexiste_sanc_discip[1, 5]
                  )
                )
            ) %>%
              bind_rows(
                data.frame(row.names = "Ensemble", "Effectif" = colSums(.)[1])
              ) ,
            
            "Sanctions disciplinaires : type de groupe & genre" = data.frame(
              row.names = c(
                "1er groupe",
                "2ème groupe",
                "3ème groupe",
                "4ème groupe"
              ),
              "Femmes" =
                c(
                  sum(
                    prm_grpe_avert_femme_tit_sanc_discip,
                    prm_grpe_avert_femme_contr_sanc_discip,
                    prm_grpe_blm_femme_tit_sanc_discip,
                    prm_grpe_blm_femme_contr_sanc_discip
                  ),
                  sum(
                    grpe_femme_tit_sanc_discip[1, 1],
                    grpe_femme_contr_sanc_discip[1, 1]
                  ),
                  sum(
                    grpe_femme_tit_sanc_discip[1, 2],
                    grpe_femme_contr_sanc_discip[1, 2]
                  ),
                  sum(
                    grpe_femme_tit_sanc_discip[1, 3],
                    grpe_femme_contr_sanc_discip[1, 3]
                  )
                ),
              
              "Hommes" = c(
                sum(
                  prm_grpe_avert_homme_tit_sanc_discip,
                  prm_grpe_avert_homme_contr_sanc_discip,
                  prm_grpe_blm_homme_tit_sanc_discip,
                  prm_grpe_blm_homme_contr_sanc_discip
                ),
                sum(
                  grpe_homme_tit_sanc_discip[1, 1],
                  grpe_homme_contr_sanc_discip[1, 1]
                ),
                sum(
                  grpe_homme_tit_sanc_discip[1, 2],
                  grpe_homme_contr_sanc_discip[1, 2]
                ),
                sum(
                  grpe_homme_tit_sanc_discip[1, 3],
                  grpe_homme_contr_sanc_discip[1, 3]
                )
              )
            ) %>%
              bind_rows(
                data.frame(
                  row.names = "Ensemble",
                  Femmes = colSums(.)[1],
                  Hommes = colSums(.)[2]
                )
              ) %>% mutate(Total =  rowSums(.)),
            
            "Sanctions disciplinaires : type de groupe & statut professionnel" = data.frame(
              row.names = c(
                "1er groupe",
                "2ème groupe",
                "3ème groupe",
                "4ème groupe"
              ),
              "Titulaires" =
                c(
                  sum(
                    prm_grpe_avert_femme_tit_sanc_discip,
                    prm_grpe_avert_homme_tit_sanc_discip,
                    prm_grpe_blm_femme_tit_sanc_discip,
                    prm_grpe_blm_homme_tit_sanc_discip
                  ),
                  sum(
                    grpe_femme_tit_sanc_discip[1, 1],
                    grpe_homme_tit_sanc_discip[1, 1]
                  ),
                  sum(
                    grpe_femme_tit_sanc_discip[1, 2],
                    grpe_homme_tit_sanc_discip[1, 2]
                  ),
                  sum(
                    grpe_femme_tit_sanc_discip[1, 3],
                    grpe_homme_tit_sanc_discip[1, 3]
                  )
                ),
              
              "Contractuels" = c(
                sum(
                  prm_grpe_avert_femme_contr_sanc_discip,
                  prm_grpe_avert_homme_contr_sanc_discip,
                  prm_grpe_blm_femme_contr_sanc_discip,
                  prm_grpe_blm_homme_contr_sanc_discip
                ),
                sum(
                  grpe_femme_contr_sanc_discip[1, 1],
                  grpe_homme_contr_sanc_discip[1, 1]
                ),
                sum(
                  grpe_femme_contr_sanc_discip[1, 2],
                  grpe_homme_contr_sanc_discip[1, 2]
                ),
                sum(
                  grpe_femme_contr_sanc_discip[1, 3],
                  grpe_homme_contr_sanc_discip[1, 3]
                )
              )
            ) %>%
              bind_rows(
                data.frame(
                  row.names = "Ensemble",
                  "Titulaires" = colSums(.)[1],
                  "Contractuels" = colSums(.)[2]
                )
              ) %>% mutate(Total =  rowSums(.)),
            
            "Sanctions ayant un lien direct avec sanction d'un agissement sexiste ou violence à caractère sexuelle : par type de groupe" = data.frame(
              row.names = c(
                "1er groupe",
                "2ème groupe",
                "3ème groupe",
                "4ème groupe"
              ),
              "Effectif" = c(
                sum(
                  sct_discip_agsmt_sexiste_sanc_discip[1, 1],
                  sct_discip_agsmt_sexiste_sanc_discip[1, 2]
                ),
                sct_discip_agsmt_sexiste_sanc_discip[1, 3],
                sct_discip_agsmt_sexiste_sanc_discip[1, 4],
                sct_discip_agsmt_sexiste_sanc_discip[1, 5]
              )
            ) %>%
              bind_rows(
                data.frame(row.names = "Ensemble", "Effectif" = colSums(.)[1])
              ) ,
            
            
            "Réponses qualitatives" = data_rsu$tranche_sanc_discip %>% select_if( ~ is.character(.) &
                                                                                    !(is.na(.))) %>% t() %>%
              data.frame() %>% rename(Reponse = names(.))
            
          )
        }, error = function(e) {
          showNotification(
            "Pas de données disponible pour cette tranche!!!",
            type = "warning",
            duration = 5
          )
        })
        
      })
      
      
      
      output$vent_crois_sanc_discip = DT::renderDT({
        #Affichage de la tranche de table si l'utilisateur coche la case "Tableau ... défaut"
        
        tryCatch({
          if (is.null(input$vent_crois_btn_sanc_discip) != T &
              input$sous_theme_sanc_discip != "Informations supplémentaires") {
            table_classic(ma_table = tables_sanc_discip_defaut())
          } else{
            if (input$vent_crois_btn_sanc_discip != T) {
              table_manuelle(ma_table = tables_sanc_discip_defaut())
            }
          }
          
        }, error = function(e) {
          
        })
        
      })
      
      ##---------------------> Fin de conception des tableaux par défauts de la sanction disciplinaire
      
      
      
      
      ##-----------------------------------> Affichage graphique de action sociale
      
      plot_sanc_discip_dataset = reactive({
        if (input$vent_crois_sanc_discip %in% c(
          "Sanctions disciplinaires du 1er groupe: par type de sanction",
          "Sanctions disciplinaires : par type de groupe"
        )) {
          if (input$plot_sanc_discip == "Histogramme superposé (tableau croisé uniquement)") {
            showNotification(
              "NB: L'histogramme superposé n'offre pas une visualisation adapté aux tableaux ventilés !!!",
              type = "error",
              duration = 5
            )
          }
          
          data_rsu$tables_sanc_discip_hist = tables_sanc_discip_defaut() %>% slice(-nrow(.)) %>% mutate(value = Effectif, names = rownames(.)) %>% filter(Effectif !=
                                                                                                                                                            0)    # Création de la table à utilisé pour représenter l'histogramme conformément aux attendus de plotly
          data_rsu$tables_sanc_discip_cam = tables_sanc_discip_defaut() %>%  slice(-nrow(.)) %>% mutate(value = Effectif, names = rownames(.)) %>% filter(Effectif !=
                                                                                                                                                            0)   # Création de la table à utilisé pour représenter le camembert conformément aux attendus de plotly
          
          data_rsu$val_type_plot_sanc_discip_1 = input$val_type_plot_sanc_discip_1
          
          return(switch(
            input$plot_sanc_discip,
            "Camembert" = cam(
              titre = input$plot_title_sanc_discip_1,
              ma_data = data_rsu$tables_sanc_discip_cam,
              type_aff = data_rsu$val_type_plot_sanc_discip_1,
              couleur = input$colors_plot_sanc_discip_1
            ),
            
            "Histogramme" = histo(
              titre = input$plot_title_sanc_discip_1,
              ma_data = data_rsu$tables_sanc_discip_hist,
              type_aff =  data_rsu$val_type_plot_sanc_discip_1,
              couleur = input$colors_plot_sanc_discip_1,
              axe_1 = input$plot_axe_sanc_discip_1_1,
              axe_2 = input$plot_axe_sanc_discip_1_2
            )
          ))
        } else{
          if (input$plot_sanc_discip == "Camembert") {
            showNotification(
              "NB: Le camembert n'offre pas une visualisation adapté aux tableaux croisés !!!",
              type = "error",
              duration = 5
            )
          }
          
          if (input$vent_crois_sanc_discip == "Sanctions ayant un lien direct avec sanction d'un agissement sexiste ou violence à caractère sexuelle : par type de groupe") {
            data_rsu$tables_sanc_discip_hist = tables_sanc_discip_defaut() %>% slice(-nrow(.)) %>% mutate(names = rownames(.)) %>% filter(Total !=
                                                                                                                                            0)
            names(data_rsu$tables_sanc_discip_hist) = gsub("\\.",
                                                           " ",
                                                           names(data_rsu$tables_sanc_discip_hist))
            
            data_rsu$val_type_plot_sanc_discip_1 = input$val_type_plot_sanc_discip_1
            
            return(switch(
              input$plot_sanc_discip,
              "Histogramme" = histo_crois_3(
                titre = input$plot_title_sanc_discip_1,
                ma_data = data_rsu$tables_sanc_discip_hist,
                type_aff =  data_rsu$val_type_plot_sanc_discip_1,
                couleur = input$colors_plot_sanc_discip_1,
                axe_1 = input$plot_axe_sanc_discip_1_1,
                axe_2 = input$plot_axe_sanc_discip_1_2
              ),
              
              "Histogramme superposé (tableau croisé uniquement)" = histo_crois_3(
                titre = input$plot_title_sanc_discip_1,
                ma_data = data_rsu$tables_sanc_discip_hist,
                type_aff =  data_rsu$val_type_plot_sanc_discip_1,
                couleur = input$colors_plot_sanc_discip_1,
                axe_1 = input$plot_axe_sanc_discip_1_1,
                axe_2 = input$plot_axe_sanc_discip_1_2,
                sup = 'stack'
              )
            ))
          } else{
            if (input$plot_sanc_discip == "Camembert") {
              showNotification(
                "NB: Le camembert n'offre pas une visualisation adapté aux tableaux croisés !!!",
                type = "error",
                duration = 5
              )
            }
            
            data_rsu$tables_sanc_discip_hist = tables_sanc_discip_defaut() %>% slice(-nrow(.)) %>% mutate(names = rownames(.)) %>% filter(Total !=
                                                                                                                                            0)
            names(data_rsu$tables_sanc_discip_hist) = gsub("\\.",
                                                           " ",
                                                           names(data_rsu$tables_sanc_discip_hist))
            
            data_rsu$val_type_plot_sanc_discip_1 = input$val_type_plot_sanc_discip_1
            
            return(switch(
              input$plot_sanc_discip,
              "Histogramme" = histo_crois_2(
                titre = input$plot_title_sanc_discip_1,
                ma_data = data_rsu$tables_sanc_discip_hist,
                type_aff =  data_rsu$val_type_plot_sanc_discip_1,
                couleur = input$colors_plot_sanc_discip_1,
                axe_1 = input$plot_axe_sanc_discip_1_1,
                axe_2 = input$plot_axe_sanc_discip_1_2
              ),
              
              "Histogramme superposé (tableau croisé uniquement)" = histo_crois_2(
                titre = input$plot_title_sanc_discip_1,
                ma_data = data_rsu$tables_sanc_discip_hist,
                type_aff =  data_rsu$val_type_plot_sanc_discip_1,
                couleur = input$colors_plot_sanc_discip_1,
                axe_1 = input$plot_axe_sanc_discip_1_1,
                axe_2 = input$plot_axe_sanc_discip_1_2,
                sup = 'stack'
              )
            ))
          }
        }
        
      })
      
      
      
      
      #------> Sorties affichage des graphiques
      output$plot_vent_crois_sanc_discip = renderPlotly({
        if (is.null(input$vent_crois_btn_sanc_discip) != T) {
          tryCatch({
            plot_sanc_discip_dataset()
          }, error = function(e) {
            
          })
        }
      })
      
      
      
    }, error = function(e) {
      showNotification("Valider d'abord votre partition",
                       type = "error",
                       duration = 5)
    })
    
    
  })
  
  ## -------------------------------------------Fin code de conception des tables & graphes par défaut de sanction disciplinaire---------------------------------------------------
  
  
  
  ## ------------------------------------------Code de conception des tables & graphes manuelles de sanction disciplinaire---------------------------------------------------
  
  output$manuelle_vent_sanc_discip = DT::renderDT({
    if (is.null(input$manuelle_btn_sanc_discip) != T) {
      # Affichage de la table si la case  " Tableau composé manuellement" est coché
      
      tryCatch({
        tranche_manuelle_sanc_discip = data_rsu$tranche_sanc_discip %>%    # Constitution de la table issue de la séléction manuelle
          select(input$select_manuelle_vent_sanc_discip) %>% t() %>%
          data.frame() %>%
          rename(Effectif = names(.))
        
        table_manuelle(ma_table = tranche_manuelle_sanc_discip)
        
      }, error = function(e) {
        
      })
    }
  })
  
  ##--------> représentation des tableaux ventilés: graphes 2 sous table  sanction disciplinaire crée manuellement
  
  plot_sanc_discip_manuelle_dataset = reactive({
    tranche_manuelle_sanc_discip = data_rsu$tranche_sanc_discip %>%
      select(input$select_manuelle_vent_sanc_discip) %>% t() %>% data.frame() %>%
      rename(Effectif = names(.)) %>%
      mutate(names = gsub("\\.", " ", sub("[0-9]*- ", "", rownames(.))))
    
    tranche_manuelle_sanc_discip_plot = tranche_manuelle_sanc_discip %>% filter(Effectif !=
                                                                                  0)
    
    #--------> fonction switch permettant d'avoir des rendu au relatif aux choix graphiques faites par l'utilisateur
    observeEvent(input$select_manuelle_vent_sanc_discip, {
      updateTextInput(session,
                      "legend_plot_sanc_discip",
                      value = paste(gsub(
                        "\\.", " ", sub(
                          "[0-9]*- ",
                          "",
                          rownames(tranche_manuelle_sanc_discip_plot)
                        )
                      ), sep = ",")) # mise à jour des colonnes à séléctionner pour constitué les légendes
    })
    
    rownames(tranche_manuelle_sanc_discip_plot) = input$legend_plot_sanc_discip %>% strsplit(., split = ",") %>% unlist() %>% as.vector() # Mise à jour de la légende à travers le nom des lignes
    data_rsu$val_type_plot_sanc_discip_2 = input$val_type_plot_sanc_discip_2
    
    
    if (is.null(c(
      input$select_manuelle_vent_sanc_discip,
      input$manuelle_btn_sanc_discip
    )) != T   &
    input$plot_sanc_discip == "Histogramme superposé (tableau croisé uniquement)") {
      showNotification(
        "NB: L'histogramme superposé n'offre pas une visualisation adapté aux tableaux ventilés !!!",
        type = "error",
        duration = 5
      )
    }
    
    switch(
      input$plot_sanc_discip,
      "Camembert" = cam(
        titre = input$plot_title_sanc_discip_2,
        ma_data = tranche_manuelle_sanc_discip_plot,
        type_aff = data_rsu$val_type_plot_sanc_discip_2,
        couleur = input$colors_plot_sanc_discip_2,
        noms_legende = input$legend_plot_sanc_discip %>% strsplit(., split = ",") %>% unlist() %>% as.vector()
      ),
      "Histogramme" = histo(
        titre = input$plot_title_sanc_discip_2,
        ma_data = tranche_manuelle_sanc_discip_plot,
        type_aff = data_rsu$val_type_plot_sanc_discip_2,
        couleur = input$colors_plot_sanc_discip_2,
        noms_legende = input$legend_plot_sanc_discip %>% strsplit(., split = ",") %>% unlist() %>% as.vector()
        ,
        axe_1 = input$plot_axe_sanc_discip_2_1,
        axe_2 = input$plot_axe_sanc_discip_2_2
      )
      
    )
  })
  
  #--------->  Graphiques associés aux tables manuelles
  
  output$plot_sanc_discip_manuelle =  renderPlotly({
    if (is.null(input$manuelle_btn_sanc_discip) != T) {
      tryCatch({
        plot_sanc_discip_manuelle_dataset()
      }, error = function(e) {
        
      })
    }
  })
  
  ## -------------------------------------------Fin Code de conception des tables & graphes manuelles  de sanction disciplinaire---------------------------------------------------
  
  
  
  ###-------------------------------------------Code de conception des tables & graphes  santé et sécurité au travail
  
  
  tranche_sante_secu_dataset = reactive({
    switch(
      input$tranche_select_sante_secu,
      "Tranche 1" = c("A- Mésures générales", "Informations supplémentaires"),
      "Tranche 2" = c(
        "B- Dispositifs de signalement",
        "C- Suicides",
        "D- Acteurs de la prévention",
        "E- Instance de prévention",
        "F- Commissions médicales",
        "G- Actions de prévention",
        "H- Médecine de prévention",
        "Informations supplémentaires"
      ),
      
      "Tranche 3" = c(
        "I- Risques professionnels",
        "J- Protection fonctionnelle",
        "Informations supplémentaires"
      )
      
      
    )
  })
  
  
  observeEvent(input$tranche_select_sante_secu, {
    updateSelectInput(session, "sous_theme_sante_secu", choices = tranche_sante_secu_dataset())
  })
  
  
  sous_theme_sante_secu_dataset = reactive({
    #Initialisation des sous-thèmes abordés
    switch(
      input$sous_theme_sante_secu,
      
      "B- Dispositifs de signalement" = c(
        "Signalements inscrits dans le registre DGI au cours de l'AC*: par types d'actes",
        "Actions menées suite au signalements inscrit au registre DGI"
      ),
      
      "C- Suicides" = "Nombre de suicides ou de tentatives de suicides: état & lieu d'intervention",
      
      "D- Acteurs de la prévention" = c(
        "Dénombrement des acteurs de la prévention en fonction au cours de l'AC*",
        "Dénombrement de la quotité de temps de travail dédié aux acteurs de prévention en fonction*",
        "Acteurs de la prévention en fonction au cours de l'AC*: situation & type d'acteur*",
        "Acteurs de la prévention en fonction*: quotité de temps & type d'acteur",
        "Suivi de formation des acteurs de prévention en fonction*: type de formation & type d'acteur",
        "Agent en poste  au 31 décembre de l'AC* ayant été formé à la santé et à la sécurité au travail: par genre"
      ),
      
      "E- Instance de prévention" = c(
        "Observations inscrites sur le registre SST au cours de l'AC*: par problématique"
      ),
      
      "F- Commissions médicales" = c(
        "Agents et positions par rapport au PPR*: décision/position & genre",
        "Reclassement des agents suite à une inaptitude: requête & genre",
        "Reclassement des agents suite à une inaptitude: type d'inaptitude & requête",
        "Agents ayant fait l'objet d'une action de la conseil médical par type d'action & genre",
        "Autres actions menées par la commission médicale pour les agents l'AC*: type d'action & genre"
      ),
      
      "G- Actions de prévention" = c(
        "Recours / rapport de conflit avec le chef de service / inspection ministérielles de la DDI",
        "Conflits entre l'administration et la FS: par motifs des recours à l'inspection du travail au titre de l'article 5-5 l'AC*",
        "Agents formés à la STT durant l'AC*: domaine de formation & genre",
        "Préconisations en matière de Risques Psychosociaux au cours de l'AC*: acteurs & décision de l'administration",
        "Préconisations émises et mises en oeuvres en matière de Risques Psychosociaux au cours de l'AC*"
      ),
      
      "H- Médecine de prévention" = c(
        "Caractéristiques/Actions de la médecine de prévention",
        "Organisation du suivi médical des agents par la  MP*: par type de visite",
        "Effectif des médecins de prévention",
        "Aménagements de postes de travail au 31 décembre de l'AC* : par action menée",
        "Aménagements de postes de travail au 31 décembre de l'AC* : par décision de l'administration",
        "Suivi médical des agents par la  MP*: type & état"
      ),
      
      "I- Risques professionnels" = c(
        "Actes de violence physique émanant du personnel et envers le personnel: types & résolutions",
        "Actes de violence physique des usagers envers le personnel: types & résolutions",
        "Agents ayant déclaré un accident reconnus par l'administration l'AC*: par type d'accident",
        "Nombre cumulé de jours d'arrêt pour accident reconnus par l'administration l'AC*: par type d'accident",
        "Nombre d'accidents reconnus par l'administration, impliquant un agent contractuel l'AC*: par type d'accident",
        "Nombre d'accident de service(hors accident de trajet) reconnus par l'administration l'AC*: type de décision & genre",
        "Nombre d'accident de service(hors accident de trajet) reconnus par l'administration l'AC*: jours d'arrêt déclarés & genre",
        "Nombre d'accident de trajet reconnus par l'administration l'AC*: type de décision & genre",
        "Nombre d'accident de trajet reconnus par l'administration l'AC*: jours d'arrêt déclarés & genre"
      ),
      
      "J- Protection fonctionnelle" = c(
        "Demandes de protections fonctionnelles formulés: genre & catégorie",
        "Demandes de protections fonctionnelles acceptées: genre & catégorie",
        "MPFF* pour mise en cause d'agents devant la juridiction pénale: genre & catégorie",
        "MPFF* pour mise en cause d'agents devant la juridiction pénale acceptée: genre & catégorie",
        "MPFF* pour poursuite d'agents pour faute de service: genre & catégorie",
        "MPFF* pour poursuite d'agents pour faute de service acceptée: genre & catégorie"
      ),
      "A- Mésures générales" = c(
        "Nombre de mesures générales prises en vue de faciliter la remise/maintien en activité des BOE: par action menée",
        "Nbre de BOE et mesures facilitant la réinsertion ou le maintien en activité des BOE"
      ),
      
      "Informations supplémentaires" = c("Réponses qualitatives")
      
      
    )
  })
  
  
  observe({
    updateSelectInput(session, "vent_crois_sante_secu", choices = sous_theme_sante_secu_dataset()) # mise à jour des colonnes à séléctionner pour sélectionner le sous thème étudier dans la santé et sécurité au travail
  })
  
  #Recherche des colonnes par recherche sur les mots clées
  
  
  observeEvent(input$vent_crois_btn_sante_secu, {
    tryCatch({
      #----------------------acteurs de la prévention
      
      AP_in_fct_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "Nombre",
          "en fonction",
          "d'AP",
          "31 décembre",
          "année",
          "concernée"
        ),
        somme = F
      )
      
      CP_in_fct_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "Nombre",
          "en fonction",
          "CP",
          "31 décembre",
          "année",
          "concernée"
        ),
        somme = F
      )
      
      AP_ayant_p_fct_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "Nombre",
          "ayant",
          "pris",
          "fonction",
          "d'AP",
          "année",
          "concernée"
        )
      )
      
      CP_ayant_p_fct_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "Nombre",
          "ayant",
          "pris",
          "fonction",
          "CP",
          "année",
          "concernée"
        )
      )
      
      AP_ayant_c_fct_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "Nombre",
          "ayant",
          "cessé",
          "fonction",
          "d'AP",
          "année",
          "concernée"
        )
      )
      
      CP_ayant_c_fct_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "Nombre",
          "ayant",
          "cessé",
          "fonction",
          "CP",
          "année",
          "concernée"
        )
      )
      
      AP_rec_ltr_fct_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c("Nombre", "fonction", "d'AP", "reçu", "lettre", "cadrage")
      )
      
      CP_rec_ltr_fct_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c("Nombre", "fonction", "CP", "reçu", "lettre", "cadrage")
      )
      
      
      forma_init_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "Nombre",
          "en fonction",
          "31 décembre",
          "année" ,
          "suivi",
          "formation",
          "initiale"
        ),
        somme = F
      )
      forma_prog_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "Nombre",
          "en fonction",
          "31 décembre",
          "année" ,
          "pas suivi",
          "formation",
          "initiale",
          "programmé"
        ),
        somme = F
      )
      forma_cont_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "Nombre",
          "en fonction",
          "31 décembre",
          "année" ,
          "suivi",
          "formation",
          "continue"
        ),
        somme = F
      )
      
      
      quotite_20_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "Nombre",
          "en fonction",
          "31 décembre",
          "année",
          "moins",
          "20%",
          "temps",
          "travail"
        ),
        somme = F
      )
      quotite_20_49_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "Nombre",
          "en fonction",
          "31 décembre",
          "année" ,
          "entre",
          "20%",
          "49%",
          "temps",
          "travail"
        ),
        somme = F
      )
      quotite_50_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "Nombre",
          "en fonction",
          "31 décembre",
          "année" ,
          "à",
          "50%",
          "temps",
          "travail"
        ),
        somme = F
      )
      quotite_51_99_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "Nombre",
          "en fonction",
          "31 décembre",
          "année" ,
          "entre",
          "51%",
          "99%",
          "temps",
          "travail"
        ),
        somme = F
      )
      quotite_100_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "Nombre",
          "en fonction",
          "31 décembre",
          "année" ,
          "à",
          "100%",
          "temps",
          "travail"
        ),
        somme = F
      )
      
      agent_Femmes_form_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "agent",
          "féminin",
          "en poste",
          "31 décembre",
          "formé",
          "santé",
          "sécurité",
          "travail"
        )
      )
      
      agent_Hommes_form_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "agent",
          "masculin",
          "en poste",
          "31 décembre",
          "formé",
          "santé",
          "sécurité",
          "travail"
        )
      )
      
      
      # ------------------------Médecine de prev/caractéristiques/rapport...
      
      struct_med_prev_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "structure",
          "médecin",
          "prévention",
          "DDI",
          "31 décembre",
          "année",
          "concernée"
        )
      )
      agent_cvr_med_prev_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "total",
          "d'agent",
          "couvert",
          "système",
          "médecin",
          "31 décembre",
          "prévention",
          "concernée"
        )
      )
      mdc_etp_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c("médecin", "ETP"),
        somme = F
      )
      mdc_tiers_t_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c("médecin", "tier", "temps"),
        somme = F
      )
      agent_couvert_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c("nombre", "agent", "couvert"),
        somme = F
      )
      
      visite_site_mp_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "visite",
          "site",
          "réalisé",
          "médecin",
          "prévention",
          "cours"
        )
      )
      rapp_an_mdc_transm_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c("rapport", "annuel", "médecin", "transmi", "DDI")
      )
      
      
      
      # -------------------------------Visite médicale & suivi
      
      agent_theo_vis_med_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "d'agent",
          "théoriquement",
          "visite",
          "obligation",
          "tous",
          "5 ans",
          "année",
          "concernée"
        )
      )
      agent_effect_vis_med_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "d'agent",
          "ayant",
          "effectivement",
          "bénéficié",
          "visite",
          "médicale",
          "obligatoire",
          "tous",
          "5 ans",
          "année",
          "concernée"
        )
      )
      agent_theo_surv_med_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "d'agent",
          "relevant",
          "surveillance",
          "médicale",
          "particulière",
          "par an",
          "correspondante",
          "réalisé",
          "ou non"
        )
      )
      agent_effect_surv_med_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "d'agent",
          "ayant",
          "effectivement",
          "bénéficié",
          "surveillance",
          "médicale",
          "particulière",
          "par an",
          "année",
          "concernée"
        )
      )
      agent_effect_dmd_vis_med_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "d'agent",
          "ayant",
          "bénéficié",
          "visite",
          "demande",
          "médecin",
          "prévention"
        )
      )
      
      
      
      #-------------------------------------- Aménagements de postes de travail
      
      amen_pst_prop_mp_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "total",
          "d'aménagement",
          "proposé",
          "médecin",
          "prévention",
          "année",
          "concernée"
        ),
        somme = F
      )
      amen_pst_rls_mp_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots = c(
          "total",
          "d'aménagement",
          "réalisé",
          "préconisation",
          "formulé",
          "précédente",
          "médecin",
          "prévention"
        )
      )
      
      
      #----------------------------------Suicides
      
      suicides_or_tent_dec_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "Nombre",
          "suicide",
          "déclarés",
          "reconnus",
          "service",
          "année",
          "concernée"
        ),
        somme = F
      )
      suicides_or_tent_lieu_trav_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c("Nombre", "suicide", "lieu", "de travail"),
        somme = F
      )
      
      # Ensemble  Agent categorie & genre dans la tranche 1
      
      
      Femmes_cat_A_tranche_2_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c("Femmes", "Catégorie A"),
        somme = F
      )
      Femmes_cat_B_tranche_2_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c("Femmes", "Catégorie B"),
        somme = F
      )
      Femmes_cat_C_tranche_2_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c("Femmes", "Catégorie C"),
        somme = F
      )
      
      
      Hommes_cat_A_tranche_2_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c("Hommes", "Catégorie A"),
        somme = F
      )
      Hommes_cat_B_tranche_2_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c("Hommes", "Catégorie B"),
        somme = F
      )
      Hommes_cat_C_tranche_2_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c("Hommes", "Catégorie C"),
        somme = F
      )
      
      
      #-----------------------------------Commision médicales
      
      reclas_prop_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "s'étant",
          "proposer",
          "période",
          "préparation",
          "reclassement",
          "concernée"
        ),
        somme = F
      )
      
      reclas_acp_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "ayant",
          "accepté",
          "période",
          "préparation",
          "reclassement",
          "concernée"
        ),
        somme = F
      )
      
      reclas_ref_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "ayant",
          "refusé",
          "période",
          "préparation",
          "reclassement",
          "concernée"
        ),
        somme = F
      )
      
      reclas_effect_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "effectivement",
          "reclassé",
          "période",
          "préparation",
          "reclassement",
          "concernée"
        ),
        somme = F
      )
      
      #---------------------------------------------------
      
      inapt_dem_mp_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "ayant",
          "demandé",
          "reclassé",
          "inaptitude",
          "accident",
          "travail",
          "maladie",
          "professionnelle"
        ),
        somme = F
      )
      inapt_effect_mp_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "effectivement",
          "reclassé",
          "inaptitude",
          "accident",
          "travail",
          "maladie",
          "professionnelle"
        ),
        somme = F
      )
      inapt_dem_af_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "ayant",
          "demandé",
          "reclassé",
          "inaptitude",
          "autre",
          "facteur"
        ),
        somme = F
      )
      inapt_effect_af_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "effectivement",
          "reclassé",
          "inaptitude",
          "autre",
          "facteur"
        ),
        somme = F
      )
      
      #-------------------------------
      
      inapt_defn_comite_ref_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "définitivement",
          "inaptes",
          "emploi",
          "comité",
          "médical",
          "commission",
          "réforme"
        ),
        somme = F
      )
      benef_amenag_comite_ref_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "bénéficiant",
          "d'aménagement",
          "poste",
          "travail",
          "d'horaire"
        ),
        somme = F
      )
      soumis_avis_comite_ref_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "cas",
          "soumis",
          "avis",
          "instance",
          "médicale",
          "comité",
          "médical",
          "réforme"
        ),
        somme = F
      )
      
      #--------------------------------------------
      Femmes_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c("Femmes"),
        somme = F
      )
      Hommes_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c("Hommes"),
        somme = F
      )
      
      #-----------------------Actions de prévention
      
      recours_ISST_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "recours",
          "ISST",
          "suite à",
          "désaccord",
          "persistant",
          "l'administration"
        )
      )
      rppt_trsm_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c("rapports", "transmis")
      )
      insp_DDI_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "inspection",
          "ministérielle",
          'DDI',
          "bénéficié",
          "année",
          "concernée"
        )
      )
      
      
      rit_des_ser_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "recours",
          "l'inspection",
          "travail",
          "désaccord" ,
          "sérieux",
          "persistant",
          "sans lien"
        )
      )
      
      rit_dg_gr_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "recours",
          "l'inspection",
          "travail",
          "hors désaccord" ,
          "danger",
          "grave"
        )
      )
      
      rit_des_dg_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "recours",
          "l'inspection",
          "travail",
          "en lien" ,
          "danger",
          "grave",
          "désaccord"
        )
      )
      
      #-------------------------------------
      
      
      obs_SST_TMS_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "total",
          "observation",
          "inscrite",
          "registre",
          "SST" ,
          "TMS",
          "année",
          "concernée"
        )
      )
      obs_SST_CMR_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "total",
          "observation",
          "inscrite",
          "registre",
          "SST" ,
          "CMR",
          "année",
          "concernée"
        )
      )
      obs_SST_RPS_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "total",
          "observation",
          "inscrite",
          "registre",
          "SST" ,
          "RPS",
          "année",
          "concernée"
        )
      )
      obs_SST_VSS_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "total",
          "observation",
          "inscrite",
          "registre",
          "SST" ,
          "VSS",
          "année",
          "concernée"
        )
      )
      
      
      #------------------------------------------------
      
      signal_reg_DGI_RPS_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "signalement",
          "inscrit",
          "registre",
          "DGI" ,
          "RPS",
          "année",
          "concernée"
        )
      )
      
      signal_reg_DGI_TMS_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "signalement",
          "inscrit",
          "registre",
          "DGI" ,
          "TMS",
          "année",
          "concernée"
        )
      )
      
      signal_reg_DGI_CMR_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "signalement",
          "inscrit",
          "registre",
          "DGI" ,
          "CMR",
          "année",
          "concernée"
        )
      )
      
      signal_reg_DGI_VSS_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "signalement",
          "inscrit",
          "registre",
          "DGI" ,
          "VSS",
          "année",
          "concernée"
        )
      )
      
      signal_reg_DGI_reun_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "signalement",
          "inscrit",
          "registre",
          "DGI" ,
          "réunion",
          "ont conduit",
          "année",
          "concernée"
        )
      )
      
      signal_reg_DGI_sais_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "signalement",
          "inscrit",
          "registre",
          "DGI" ,
          "saisie",
          "objet",
          "inspection",
          "travail",
          "année",
          "concernée"
        )
      )
      #---------------------------------------------------
      
      preco_RPS_acpt_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "préconisation",
          "émise",
          "accepté",
          "l'administration",
          "année",
          "concernée"
        ),
        somme = F
      )
      preco_RPS_meo_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "préconisation",
          "émise",
          "mise",
          "oeuvre",
          "l'administration",
          "année",
          "concernée"
        ),
        somme = F
      )
      preco_RPS_ref_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "préconisation",
          "émise",
          "refusé",
          "l'administration",
          "année",
          "concernée"
        ),
        somme = F
      )
      
      
      
      preco_RPS_ems_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "préconisation",
          "total",
          "émise",
          "cours",
          "année",
          "concernée"
        ),
        somme = F
      )
      
      preco_RPS_DDI_ref_tranche_1_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "préconisation",
          "émise",
          "direction",
          "DDI",
          "RPS",
          "année",
          "concernée"
        ),
        somme = F
      )
      
      #------------------------------------Risques pro
      
      vlc_ph_aar_tranche_2_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "d'acte",
          "violence",
          "physique",
          "avec",
          "arrêt",
          "travail"
        ),
        somme = F
      )
      
      vlc_ph_sar_tranche_2_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "d'acte",
          "violence",
          "physique",
          "sans",
          "arrêt",
          "travail"
        ),
        somme = F
      )
      
      vlc_verb_aar_tranche_2_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "d'acte",
          "violence",
          "verbale",
          "avec",
          "arrêt",
          "travail"
        ),
        somme = F
      )
      
      
      
      vlc_verb_perso_aar_tranche_2_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "du personnel",
          "d'acte",
          "violence",
          "verbale",
          "arrêt",
          "travail"
        ),
        somme = F
      )
      
      vlc_verb_usag_sar_tranche_2_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "usager",
          "d'acte",
          "violence",
          "verbale",
          "sans",
          "arrêt",
          "travail"
        )
      )
      
      #-------------------------------------Accident de service - hors accident de trajet
      
      acd_service_tot_tranche_2_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "d'agent",
          "déclaré",
          "accident",
          "l'administration",
          "année",
          "concernée"
        ),
        somme = F
      )
      
      acd_jours_cml_tranche_2_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "cumulé",
          "pour accident",
          "l'administration",
          "année",
          "concernée"
        ),
        somme = F
      )
      
      
      
      acd_service_sart_tranche_2_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "accident",
          "service",
          "l'administration",
          "sans",
          "arrêt",
          "travail"
        ),
        somme = F
      )
      acd_service_dcs_aart_tranche_2_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "accident",
          "service",
          "l'administration",
          "avec",
          "arrêt",
          "travail",
          "décès"
        ),
        somme = F
      )
      
      acd_service_1_3j_aart_tranche_2_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "accident",
          "service",
          "l'administration",
          "avec",
          "arrêt",
          "travail",
          "entre",
          "1 et 3"
        ),
        somme = F
      )
      acd_service_3j_aart_tranche_2_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "accident",
          "service",
          "l'administration",
          "avec",
          "arrêt",
          "travail",
          "supérieur",
          "3 jours"
        ),
        somme = F
      )
      
      #-------------------------------------------Accident de trajet
      
      acd_trajet_sart_tranche_2_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "accident",
          "trajet",
          "l'administration",
          "sans",
          "arrêt",
          "travail"
        ),
        somme = F
      )
      acd_trajet_dcs_aart_tranche_2_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "accident",
          "trajet",
          "l'administration",
          "avec",
          "arrêt",
          "travail",
          "décès"
        ),
        somme = F
      )
      
      acd_trajet_1_3j_aart_tranche_2_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "accident",
          "trajet",
          "l'administration",
          "avec",
          "arrêt",
          "travail",
          "entre",
          "1 et 3"
        ),
        somme = F
      )
      acd_trajet_3j_aart_tranche_2_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "accident",
          "trajet",
          "l'administration",
          "avec",
          "arrêt",
          "travail",
          "supérieur",
          "3 jours"
        ),
        somme = F
      )
      
      
      #----------------------------
      
      acd_ctrct_tranche_2_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "total",
          "accident",
          "agent",
          "contractuel",
          "l'administration",
          "année",
          "concernée"
        ),
        somme = F
      )
      
      #-----------------------------BOE
      
      mesure_gen_tranche_3_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c(
          "mesure",
          "générale",
          "remise",
          "maintien",
          "activité",
          "BOE"
        ),
        somme = F
      )
      
      
      Nbre_BOE_tranche_3_sante_secu = colonne_match(
        data = data_rsu$tranche_sante_secu,
        vecteurs_mots =  c("BOE", "31 décembre", "année", "concernée")
      )
      
      # Conception des objets tableau par défauts
      
      tables_sante_secu_defaut = reactive({
        switch(
          input$vent_crois_sante_secu,
          "Dénombrement des acteurs de la prévention en fonction au cours de l'AC*" = data.frame(
            row.names = c(
              "En fonction*",
              "Prise de fonction",
              "En cessation",
              "En fonction & lettre de cadrage reçue"
            ),
            "Effectif" =
              c(
                AP_in_fct_tranche_1_sante_secu[1, 1] + CP_in_fct_tranche_1_sante_secu[1, 1],
                AP_ayant_p_fct_tranche_1_sante_secu + CP_ayant_p_fct_tranche_1_sante_secu,
                AP_ayant_c_fct_tranche_1_sante_secu + CP_ayant_c_fct_tranche_1_sante_secu,
                AP_rec_ltr_fct_tranche_1_sante_secu + CP_rec_ltr_fct_tranche_1_sante_secu
              )
          ),
          
          "Dénombrement de la quotité de temps de travail dédié aux acteurs de prévention en fonction*" = data.frame(
            row.names = c("moins de 20%", "20% - 49%", "50%", "51% - 99%", "100%"),
            "Effectif" =
              c(
                sum(quotite_20_tranche_1_sante_secu),
                sum(quotite_20_49_tranche_1_sante_secu),
                sum(quotite_50_tranche_1_sante_secu),
                sum(quotite_51_99_tranche_1_sante_secu),
                sum(quotite_100_tranche_1_sante_secu)
              )
          ) %>%
            bind_rows(
              data.frame(row.names = "Ensemble", "Effectif" = colSums(.)[1])
            ),
          
          
          "Acteurs de la prévention en fonction au cours de l'AC*: situation & type d'acteur*" = data.frame(
            row.names = c(
              "En fonction*",
              "Prise de fonction",
              "En cessation",
              "En fonction + lettre de cadrage reçu"
            ),
            "AP" =
              c(
                AP_in_fct_tranche_1_sante_secu[1, 1],
                AP_ayant_p_fct_tranche_1_sante_secu,
                AP_ayant_c_fct_tranche_1_sante_secu,
                AP_rec_ltr_fct_tranche_1_sante_secu
              ),
            "CP" = c(
              CP_in_fct_tranche_1_sante_secu[1, 1],
              CP_ayant_p_fct_tranche_1_sante_secu,
              CP_ayant_c_fct_tranche_1_sante_secu,
              CP_rec_ltr_fct_tranche_1_sante_secu
            )
          ) %>%
            mutate(Total =  rowSums(.)),
          
          "Acteurs de la prévention en fonction*: quotité de temps & type d'acteur" = data.frame(
            row.names = c("moins de 20%", "20% - 49%", "50%", "51% - 99%", "100%"),
            "AP" =
              c(
                quotite_20_tranche_1_sante_secu[1, 1],
                quotite_20_49_tranche_1_sante_secu[1, 1],
                quotite_50_tranche_1_sante_secu[1, 1],
                quotite_51_99_tranche_1_sante_secu[1, 1],
                quotite_100_tranche_1_sante_secu[1, 1]
              ),
            "CP" = c(
              quotite_20_tranche_1_sante_secu[1, 2],
              quotite_20_49_tranche_1_sante_secu[1, 2],
              quotite_50_tranche_1_sante_secu[1, 2],
              quotite_51_99_tranche_1_sante_secu[1, 2],
              quotite_100_tranche_1_sante_secu[1, 2]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                "AP" = colSums(.)[1],
                "CP" = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "Suivi de formation des acteurs de prévention en fonction*: type de formation & type d'acteur" = data.frame(
            row.names = c(
              "Formations initiales",
              "Formations initiales programmées",
              "Formations continues"
            ),
            "AP" =
              c(
                forma_init_tranche_1_sante_secu[1, 1],
                forma_prog_tranche_1_sante_secu[1, 1],
                forma_cont_tranche_1_sante_secu[1, 1]
              ),
            "CP" = c(
              forma_init_tranche_1_sante_secu[1, 2],
              forma_prog_tranche_1_sante_secu[1, 2],
              forma_cont_tranche_1_sante_secu[1, 2]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                "AP" = colSums(.)[1],
                "CP" = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "Nombre de suicides ou de tentatives de suicides: état & lieu d'intervention" = data.frame(
            row.names = c(
              "Déclarés,imputables au service",
              "Intervenus au travail"
            ),
            "Suicides" =
              c(
                suicides_or_tent_dec_tranche_1_sante_secu[1, 1],
                suicides_or_tent_lieu_trav_tranche_1_sante_secu[1, 1]
              ),
            "Tentatives_de_suicides" = c(
              suicides_or_tent_dec_tranche_1_sante_secu[1, 2],
              suicides_or_tent_lieu_trav_tranche_1_sante_secu[1, 2]
            )
          ) %>% mutate(Total =  rowSums(.)),
          
          "Demandes de protections fonctionnelles formulés: genre & catégorie" = data.frame(
            row.names = c("Catégorie A", "Catégorie B", "Catégorie C"),
            "Femmes" =
              c(
                Femmes_cat_A_tranche_2_sante_secu[1, 1],
                Femmes_cat_B_tranche_2_sante_secu[1, 1],
                Femmes_cat_C_tranche_2_sante_secu[1, 1]
              ),
            "Hommes" = c(
              Hommes_cat_A_tranche_2_sante_secu[1, 1],
              Hommes_cat_B_tranche_2_sante_secu[1, 1],
              Hommes_cat_C_tranche_2_sante_secu[1, 1]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "Demandes de protections fonctionnelles acceptées: genre & catégorie" = data.frame(
            row.names = c("Catégorie A", "Catégorie B", "Catégorie C"),
            "Femmes" =
              c(
                Femmes_cat_A_tranche_2_sante_secu[1, 2],
                Femmes_cat_B_tranche_2_sante_secu[1, 2],
                Femmes_cat_C_tranche_2_sante_secu[1, 2]
              ),
            "Hommes" = c(
              Hommes_cat_A_tranche_2_sante_secu[1, 2],
              Hommes_cat_B_tranche_2_sante_secu[1, 2],
              Hommes_cat_C_tranche_2_sante_secu[1, 2]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "MPFF* pour mise en cause d'agents devant la juridiction pénale: genre & catégorie" = data.frame(
            row.names = c("Catégorie A", "Catégorie B", "Catégorie C"),
            "Femmes" =
              c(
                Femmes_cat_A_tranche_2_sante_secu[1, 3],
                Femmes_cat_B_tranche_2_sante_secu[1, 3],
                Femmes_cat_C_tranche_2_sante_secu[1, 3]
              ),
            "Hommes" = c(
              Hommes_cat_A_tranche_2_sante_secu[1, 3],
              Hommes_cat_B_tranche_2_sante_secu[1, 3],
              Hommes_cat_C_tranche_2_sante_secu[1, 3]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "MPFF* pour mise en cause d'agents devant la juridiction pénale acceptée: genre & catégorie" = data.frame(
            row.names = c("Catégorie A", "Catégorie B", "Catégorie C"),
            "Femmes" =
              c(
                Femmes_cat_A_tranche_2_sante_secu[1, 4],
                Femmes_cat_B_tranche_2_sante_secu[1, 4],
                Femmes_cat_C_tranche_2_sante_secu[1, 4]
              ),
            "Hommes" = c(
              Hommes_cat_A_tranche_2_sante_secu[1, 4],
              Hommes_cat_B_tranche_2_sante_secu[1, 4],
              Hommes_cat_C_tranche_2_sante_secu[1, 4]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "MPFF* pour poursuite d'agents pour faute de service: genre & catégorie" = data.frame(
            row.names = c("Catégorie A", "Catégorie B", "Catégorie C"),
            "Femmes" =
              c(
                Femmes_cat_A_tranche_2_sante_secu[1, 5],
                Femmes_cat_B_tranche_2_sante_secu[1, 5],
                Femmes_cat_C_tranche_2_sante_secu[1, 5]
              ),
            "Hommes" = c(
              Hommes_cat_A_tranche_2_sante_secu[1, 5],
              Hommes_cat_B_tranche_2_sante_secu[1, 5],
              Hommes_cat_C_tranche_2_sante_secu[1, 5]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "MPFF* pour poursuite d'agents pour faute de service acceptée: genre & catégorie" = data.frame(
            row.names = c("Catégorie A", "Catégorie B", "Catégorie C"),
            "Femmes" =
              c(
                Femmes_cat_A_tranche_2_sante_secu[1, 6],
                Femmes_cat_B_tranche_2_sante_secu[1, 6],
                Femmes_cat_C_tranche_2_sante_secu[1, 6]
              ),
            "Hommes" = c(
              Hommes_cat_A_tranche_2_sante_secu[1, 6],
              Hommes_cat_B_tranche_2_sante_secu[1, 6],
              Hommes_cat_C_tranche_2_sante_secu[1, 6]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "Agent en poste  au 31 décembre de l'AC* ayant été formé à la santé et à la sécurité au travail: par genre" = data.frame(
            row.names = c("Agents formés à la santé et à la sécurité au travail"),
            "Femmes" = agent_Femmes_form_tranche_1_sante_secu,
            "Hommes" = agent_Hommes_form_tranche_1_sante_secu
          ) %>% mutate(Total =  rowSums(.)),
          
          
          "Caractéristiques/Actions de la médecine de prévention" = data.frame(
            row.names = c(
              "Nbre de structures de MP* couvrant la DDI en fonction*",
              "Agents couverts par un système MP* en fonction*",
              "Agents couverts",
              "Visite de site réalisée par le MP*",
              "Rapports annuel(précédente à l'AC*) transmis à la DDI"
            ),
            "Effectif" = c(
              struct_med_prev_tranche_1_sante_secu,
              agent_cvr_med_prev_tranche_1_sante_secu,
              agent_couvert_tranche_1_sante_secu[1, 1],
              visite_site_mp_tranche_1_sante_secu,
              rapp_an_mdc_transm_tranche_1_sante_secu
            )
          ),
          
          
          "Effectif des médecins de prévention" = data.frame(
            row.names = c("Médecins en ETP", "Médecins exerçant un tiers temps"),
            "Effectif" = c(
              mdc_etp_tranche_1_sante_secu[1, 1],
              mdc_tiers_t_tranche_1_sante_secu[1, 1]
            )
          ) %>%
            bind_rows(data.frame(
              row.names = "Total", "Effectif" = colSums(.)[1]
            )),
          
          
          "Suivi médical des agents par la  MP*: type & état" = data.frame(
            row.names = c(
              "Visites médicales obligatoires",
              "Surveillances médicales particulières"
            ),
            "Théoriquement_concernés" = c(
              agent_theo_vis_med_tranche_1_sante_secu,
              agent_theo_surv_med_tranche_1_sante_secu
            ),
            "Ayant_bénéficiés" = c(
              agent_effect_vis_med_tranche_1_sante_secu,
              agent_effect_surv_med_tranche_1_sante_secu
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                "Théoriquement_concernés" = colSums(.)[1],
                "Ayant_bénéficiés" = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          
          "Organisation du suivi médical des agents par la  MP*: par type de visite" = data.frame(
            row.names = c(
              "Visites médicales obligatoires",
              "Surveillances médicales particulière",
              "Visites à la demande du MP*"
            ),
            "Effectif" = c(
              agent_theo_vis_med_tranche_1_sante_secu + agent_effect_vis_med_tranche_1_sante_secu,
              agent_theo_surv_med_tranche_1_sante_secu + agent_effect_surv_med_tranche_1_sante_secu,
              agent_effect_dmd_vis_med_tranche_1_sante_secu
            )
          ) %>%
            bind_rows(data.frame(
              row.names = "Total", "Effectif" = colSums(.)[1]
            )),
          
          
          "Aménagements de postes de travail au 31 décembre de l'AC* : par action menée" = data.frame(
            row.names = c("Proposés par un médecin", "Réalisés"),
            "Effectif" = c(
              amen_pst_rls_mp_tranche_1_sante_secu,
              amen_pst_prop_mp_tranche_1_sante_secu[1, 1]
            )
          ) %>%
            bind_rows(data.frame(
              row.names = "Total", "Effectif" = colSums(.)[1]
            )),
          
          "Aménagements de postes de travail au 31 décembre de l'AC* : par décision de l'administration" = data.frame(
            row.names = c(
              "Acceptés et mise en oeuvres",
              "Acceptés et non mise en oeuvres",
              "Pas encore acceptés par l'administration",
              "Réfusés par l'administration"
            ),
            "Effectif" = c(
              amen_pst_prop_mp_tranche_1_sante_secu[1, 2],
              amen_pst_prop_mp_tranche_1_sante_secu[1, 3],
              amen_pst_prop_mp_tranche_1_sante_secu[1, 4],
              amen_pst_prop_mp_tranche_1_sante_secu[1, 5]
            )
          ) %>%
            bind_rows(data.frame(
              row.names = "Total", "Effectif" = colSums(.)[1]
            )),
          
          "Agents et positions par rapport au PPR*: décision/position & genre" = data.frame(
            row.names = c(
              "Propositions de PPR*",
              "Acceptations de PPR*",
              "Refus de PPR*",
              "Reclassés suite au PPR*"
            ),
            "Femmes" =
              c(
                reclas_prop_tranche_1_sante_secu[1, 1],
                reclas_acp_tranche_1_sante_secu[1, 1],
                reclas_ref_tranche_1_sante_secu[1, 1],
                reclas_effect_tranche_1_sante_secu[1, 1]
              ),
            "Hommes" = c(
              reclas_prop_tranche_1_sante_secu[1, 2],
              reclas_acp_tranche_1_sante_secu[1, 2],
              reclas_ref_tranche_1_sante_secu[1, 2],
              reclas_effect_tranche_1_sante_secu[1, 2]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "Reclassement des agents suite à une inaptitude: requête & genre" = data.frame(
            row.names = c("Demandes de reclassement", "Reclassés"),
            "Femmes" =
              c(
                sum(
                  inapt_dem_mp_tranche_1_sante_secu[1, 1] + inapt_dem_af_tranche_1_sante_secu[1, 1]
                ),
                sum(
                  inapt_effect_mp_tranche_1_sante_secu[1, 1] + inapt_effect_af_tranche_1_sante_secu[1, 1]
                )
              ),
            "Hommes" = c(
              sum(
                inapt_dem_mp_tranche_1_sante_secu[1, 2] + inapt_dem_af_tranche_1_sante_secu[1, 2]
              ),
              sum(
                inapt_effect_mp_tranche_1_sante_secu[1, 2] + inapt_effect_af_tranche_1_sante_secu[1, 2]
              )
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "Reclassement des agents suite à une inaptitude: type d'inaptitude & requête" = data.frame(
            row.names = c("Accident du travail ou maladie pro", "Autres facteurs"),
            "Demandes_de_reclassement" =
              c(
                sum(inapt_dem_mp_tranche_1_sante_secu) ,
                sum(inapt_dem_af_tranche_1_sante_secu)
              ),
            "Reclassés" = c(
              sum(inapt_dem_mp_tranche_1_sante_secu) ,
              sum(inapt_dem_af_tranche_1_sante_secu)
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                "Demandes_de_reclassement" = colSums(.)[1],
                "Reclassés" = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          "Agents ayant fait l'objet d'une action de la conseil médical par type d'action & genre" = data.frame(
            row.names = c(
              "Considérés définitivement inaptes par CM*",
              "Bénéficiants d'aménagement horaire/poste de travail",
              "Dont le cas est soumis pour avis aux CM*"
            ),
            "Femmes" = c(
              inapt_defn_comite_ref_tranche_1_sante_secu[1, 1],
              benef_amenag_comite_ref_tranche_1_sante_secu[1, 1],
              soumis_avis_comite_ref_tranche_1_sante_secu[1, 1]
            ),
            "Hommes" = c(
              inapt_defn_comite_ref_tranche_1_sante_secu[1, 2],
              benef_amenag_comite_ref_tranche_1_sante_secu[1, 2],
              soumis_avis_comite_ref_tranche_1_sante_secu[1, 2]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          
          
          "Autres actions menées par la commission médicale pour les agents l'AC*: type d'action & genre" = data.frame(
            row.names = c(
              "Bénéficiaires d'un temps partiel thérapeutique recencés",
              "Mises en disponibilité d'office pour raison de santé",
              "Placés en retraite pour invalidité",
              "Licenciements pour inaptitude physique"
            ),
            "Femmes" = c(
              Femmes_tranche_1_sante_secu[1, 1],
              Femmes_tranche_1_sante_secu[1, 2],
              Femmes_tranche_1_sante_secu[1, 3],
              Femmes_tranche_1_sante_secu[1, 4]
            ),
            "Hommes" = c(
              Hommes_tranche_1_sante_secu[1, 1],
              Hommes_tranche_1_sante_secu[1, 2],
              Hommes_tranche_1_sante_secu[1, 3],
              Hommes_tranche_1_sante_secu[1, 4]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          
          
          "Recours / rapport de conflit avec le chef de service / inspection ministérielles de la DDI" = data.frame(
            row.names = c(
              "Recours à l'ISST",
              "Rapports de conflit transmis par l'inspection du travail au SGG*",
              "Inspections ministérielles de la DDI"
            ),
            "Total" = c(
              recours_ISST_tranche_1_sante_secu,
              rppt_trsm_tranche_1_sante_secu,
              insp_DDI_tranche_1_sante_secu
            )
          ),
          
          
          "Conflits entre l'administration et la FS: par motifs des recours à l'inspection du travail au titre de l'article 5-5 l'AC*" = data.frame(
            row.names = c(
              "Par désaccord sérieux et persisitant",
              "Par danger grave",
              "Par désaccord sérieux et persistant liée à un danger grave"
            ),
            "Effectif" = c(
              rit_des_ser_tranche_1_sante_secu,
              rit_dg_gr_tranche_1_sante_secu,
              rit_des_dg_tranche_1_sante_secu
            )
          ) %>%
            bind_rows(data.frame(
              row.names = "Total", "Effectif" = colSums(.)[1]
            )),
          
          
          
          "Observations inscrites sur le registre SST au cours de l'AC*: par problématique" = data.frame(
            row.names = c(
              "Problématique de TMS",
              "Problématique de CMR",
              "Problématique de RPS",
              "Problématique de VSS"
            ),
            "Effectif" = c(
              obs_SST_TMS_tranche_1_sante_secu,
              obs_SST_CMR_tranche_1_sante_secu,
              obs_SST_RPS_tranche_1_sante_secu,
              obs_SST_VSS_tranche_1_sante_secu
            )
          ) %>%
            bind_rows(data.frame(
              row.names = "Total", "Effectif" = colSums(.)[1]
            )),
          
          
          "Signalements inscrits dans le registre DGI au cours de l'AC*: par types d'actes" = data.frame(
            row.names = c("Danger à prédominance RPS", "TMS", "CMR", "VSS"),
            "Effectif" = c(
              signal_reg_DGI_RPS_tranche_1_sante_secu,
              signal_reg_DGI_TMS_tranche_1_sante_secu,
              signal_reg_DGI_CMR_tranche_1_sante_secu,
              signal_reg_DGI_VSS_tranche_1_sante_secu
            )
          ) %>%
            bind_rows(data.frame(
              row.names = "Total", "Effectif" = colSums(.)[1]
            )),
          
          
          
          
          "Actions menées suite au signalements inscrit au registre DGI" = data.frame(
            row.names = c(
              "Réunion d'un FS spécial",
              "Saisie de l'inspection du travail"
            ),
            "Effectif" = c(
              signal_reg_DGI_reun_tranche_1_sante_secu,
              signal_reg_DGI_sais_tranche_1_sante_secu
            )
          ) %>%
            bind_rows(data.frame(
              row.names = "Total", "Effectif" = colSums(.)[1]
            )),
          
          "Agents formés à la STT durant l'AC*: domaine de formation & genre" = data.frame(
            row.names = c("Prévention des RPS", "Lutte contre les VSS"),
            "Femmes" = c(
              Femmes_tranche_1_sante_secu[1, 5],
              Femmes_tranche_1_sante_secu[1, 6]
            ),
            "Hommes" = c(
              Hommes_tranche_1_sante_secu[1, 5],
              Hommes_tranche_1_sante_secu[1, 6]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          
          "Préconisations en matière de Risques Psychosociaux au cours de l'AC*: acteurs & décision de l'administration" = data.frame(
            row.names = c("Accéptées", "Mises en oeuvre au 31 déc AC*", "Refusées"),
            "Médecine_de_prévention" = c(
              preco_RPS_acpt_tranche_1_sante_secu[1, 1],
              preco_RPS_meo_tranche_1_sante_secu[1, 1],
              preco_RPS_ref_tranche_1_sante_secu[1, 1]
            ),
            "FS" = c(
              preco_RPS_acpt_tranche_1_sante_secu[1, 2],
              preco_RPS_meo_tranche_1_sante_secu[1, 2],
              preco_RPS_ref_tranche_1_sante_secu[1, 2]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                "Médecine_de_prévention" = colSums(.)[1],
                "FS" = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          
          
          "Préconisations émises et mises en oeuvres en matière de Risques Psychosociaux au cours de l'AC*" = data.frame(
            row.names = c("Médecine de prévention", "FS", "DDI Direction"),
            "Emises" = c(
              preco_RPS_ems_tranche_1_sante_secu[1, 1],
              preco_RPS_ems_tranche_1_sante_secu[1, 5],
              preco_RPS_DDI_ref_tranche_1_sante_secu[1, 1]
            ),
            "Mises_en_oeuvre_au_31_déc" = c(
              preco_RPS_meo_tranche_1_sante_secu[1, 1],
              preco_RPS_meo_tranche_1_sante_secu[1, 2],
              preco_RPS_DDI_ref_tranche_1_sante_secu[1, 2]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                "Emises" = colSums(.)[1],
                "Mises_en_oeuvre_au_31_déc" = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          
          "Actes de violence physique émanant du personnel et envers le personnel: types & résolutions" = data.frame(
            row.names = c("Arrêt de travail", "Sans arrêt de travail"),
            "Physiques" = c(
              vlc_ph_aar_tranche_2_sante_secu[1, 1],
              vlc_ph_sar_tranche_2_sante_secu[1, 1]
            ),
            "Verbales" = c(
              vlc_verb_aar_tranche_2_sante_secu[1, 1],
              vlc_verb_perso_aar_tranche_2_sante_secu[1, 2]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                "Physiques" = colSums(.)[1],
                "Verbales" = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          
          
          
          
          "Actes de violence physique des usagers envers le personnel: types & résolutions" = data.frame(
            row.names = c("Arrêt de travail", "Sans arrêt de travail"),
            "Physiques" = c(
              vlc_ph_aar_tranche_2_sante_secu[1, 2],
              vlc_ph_sar_tranche_2_sante_secu[1, 2]
            ),
            "Verbales" = c(
              vlc_verb_aar_tranche_2_sante_secu[1, 2],
              vlc_verb_usag_sar_tranche_2_sante_secu
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                "Physiques" = colSums(.)[1],
                "Verbales" = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          
          
          
          "Agents ayant déclaré un accident reconnus par l'administration l'AC*: par type d'accident" = data.frame(
            row.names = c("Accident de service(HAT*)", "Accident de trajet"),
            "Effectif" = c(
              sum(acd_service_tot_tranche_2_sante_secu[1, c(1, 2)]),
              sum(acd_service_tot_tranche_2_sante_secu[1, c(3, 4)])
            )
          ) %>%
            bind_rows(data.frame(
              row.names = "Total", "Effectif" = colSums(.)[1]
            )),
          
          
          "Nombre cumulé de jours d'arrêt pour accident reconnus par l'administration l'AC*: par type d'accident" = data.frame(
            row.names = c("Accident de service(HAT*)", "Accident de trajet"),
            "Effectif" = c(
              sum(acd_jours_cml_tranche_2_sante_secu[1, c(1, 2)]),
              sum(acd_jours_cml_tranche_2_sante_secu[1, c(3, 4)])
            )
          ) %>%
            bind_rows(data.frame(
              row.names = "Total", "Effectif" = colSums(.)[1]
            )),
          
          
          
          
          "Nombre d'accident de service(hors accident de trajet) reconnus par l'administration l'AC*: type de décision & genre" = data.frame(
            row.names = c(
              "Sans arrêt de travail",
              "Arrêt de travail",
              "Arrêt de travail + décès"
            ),
            "Femmes" =  c(
              acd_service_sart_tranche_2_sante_secu[1, 1],
              acd_service_1_3j_aart_tranche_2_sante_secu[1, 1] + acd_service_3j_aart_tranche_2_sante_secu[1, 1],
              acd_service_dcs_aart_tranche_2_sante_secu[1, 1]
            ),
            "Hommes" = c(
              acd_service_sart_tranche_2_sante_secu[1, 2],
              acd_service_1_3j_aart_tranche_2_sante_secu[1, 2] + acd_service_3j_aart_tranche_2_sante_secu[1, 2],
              acd_service_dcs_aart_tranche_2_sante_secu[1, 2]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          
          
          
          "Nombre d'accident de service(hors accident de trajet) reconnus par l'administration l'AC*: jours d'arrêt déclarés & genre" = data.frame(
            row.names = c("1-3 jours d'arrêt", ">3 jours d'arrêt"),
            "Femmes" =  c(
              acd_service_1_3j_aart_tranche_2_sante_secu[1, 1],
              acd_service_3j_aart_tranche_2_sante_secu[1, 1]
            ),
            "Hommes" = c(
              acd_service_1_3j_aart_tranche_2_sante_secu[1, 2],
              acd_service_3j_aart_tranche_2_sante_secu[1, 2]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)) ,
          
          
          
          "Nombre d'accident de trajet reconnus par l'administration l'AC*: type de décision & genre" = data.frame(
            row.names = c(
              "Sans arrêt de travail",
              "Arrêt de travail",
              "Arrêt de travail + décès"
            ),
            "Femmes" =  c(
              acd_trajet_sart_tranche_2_sante_secu[1, 1],
              acd_trajet_1_3j_aart_tranche_2_sante_secu[1, 1] + acd_trajet_3j_aart_tranche_2_sante_secu[1, 1],
              acd_trajet_dcs_aart_tranche_2_sante_secu[1, 1]
            ),
            "Hommes" = c(
              acd_trajet_sart_tranche_2_sante_secu[1, 2],
              acd_trajet_1_3j_aart_tranche_2_sante_secu[1, 2] + acd_trajet_3j_aart_tranche_2_sante_secu[1, 2],
              acd_trajet_dcs_aart_tranche_2_sante_secu[1, 2]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          
          
          "Nombre d'accident de trajet reconnus par l'administration l'AC*: jours d'arrêt déclarés & genre" = data.frame(
            row.names = c("1-3 jours d'arrêt", ">3 jours d'arrêt"),
            "Femmes" = c(
              acd_trajet_1_3j_aart_tranche_2_sante_secu[1, 1],
              acd_trajet_3j_aart_tranche_2_sante_secu[1, 1]
            ),
            "Hommes" = c(
              acd_trajet_1_3j_aart_tranche_2_sante_secu[1, 2],
              acd_trajet_3j_aart_tranche_2_sante_secu[1, 2]
            )
          ) %>%
            bind_rows(
              data.frame(
                row.names = "Ensemble",
                Femmes = colSums(.)[1],
                Hommes = colSums(.)[2]
              )
            ) %>% mutate(Total =  rowSums(.)),
          
          
          
          
          
          "Nombre d'accidents reconnus par l'administration, impliquant un agent contractuel l'AC*: par type d'accident" =
            data.frame(
              row.names = c("Accident de service(HAT*)", "Accident de trajet"),
              "Effectif" = c(
                sum(acd_ctrct_tranche_2_sante_secu[1, c(1, 2)]),
                sum(acd_ctrct_tranche_2_sante_secu) - sum(acd_ctrct_tranche_2_sante_secu[1, c(1, 2)])
              )
            ) %>%
            bind_rows(data.frame(
              row.names = "Total", "Effectif" = colSums(.)[1]
            )),
          
          
          
          "Nombre de mesures générales prises en vue de faciliter la remise/maintien en activité des BOE: par action menée" = data.frame(
            row.names = c("Etudiées par la FS", "Suite à un avis de FS"),
            "Effectif" = c(
              mesure_gen_tranche_3_sante_secu[1, 1],
              mesure_gen_tranche_3_sante_secu[1, 2]
            )
          ) %>%
            bind_rows(data.frame(
              row.names = "Total", "Effectif" = colSums(.)[1]
            )),
          
          "Nbre de BOE et mesures facilitant la réinsertion ou le maintien en activité des BOE" =
            data.frame(
              row.names = c("Nbre de BOE", "Mesures générale prise"),
              "Effectif" = c(
                Nbre_BOE_tranche_3_sante_secu,
                sum(mesure_gen_tranche_3_sante_secu)
              )
            ) %>%
            bind_rows(data.frame(
              row.names = "Total", "Effectif" = colSums(.)[1]
            )) ,
          
          "Réponses qualitatives" =  data_rsu$tranche_sante_secu %>% select_if( ~ is.character(.) &
                                                                                  !(is.na(.))) %>% t() %>%
            data.frame() %>% rename(Reponse = names(.))
          
          
        )
        
      })
      
      
      #--------------------------------------->Création des objets représentant la table datatable
      
      output$vent_crois_sante_secu = DT::renderDT({
        #Affichage de la tranche de table si l'utilisateur coche la case "Tableau ... défaut"
        tryCatch({
          if (is.null(input$vent_crois_btn_sante_secu) != TRUE &
              input$sous_theme_sante_secu != "Informations supplémentaires") {
            table_classic(ma_table = tables_sante_secu_defaut())
          } else{
            if (is.null(input$vent_crois_btn_sante_secu) != TRUE) {
              table_manuelle(ma_table = tables_sante_secu_defaut())
            }
          }
        }, error = function(e) {
          
        })
      })
      
      ##---------------------> Fin de conception des tableaux par défauts de la santé et sécurité au travail
      
      
      ##-----------------------------------> Affichage graphique santé et sécurité au travail
      
      plot_sante_secu_dataset = reactive({
        if (input$vent_crois_sante_secu %in% c(
          "Nombre de mesures générales prises en vue de faciliter la remise/maintien en activité des BOE: par action menée",
          "Signalements inscrits dans le registre DGI au cours de l'AC*: par types d'actes",
          "Actions menées suite au signalements inscrit au registre DGI",
          "Dénombrement des acteurs de la prévention en fonction au cours de l'AC*",
          "Dénombrement de la quotité de temps de travail dédié aux acteurs de prévention en fonction*",
          "Observations inscrites sur le registre SST au cours de l'AC*: par problématique",
          "Recours / rapport de conflit avec le chef de service / inspection ministérielles de la DDI",
          "Conflits entre l'administration et la FS: par motifs des recours à l'inspection du travail au titre de l'article 5-5 l'AC*",
          "Caractéristiques/Actions de la médecine de prévention",
          "Organisation du suivi médical des agents par la MP*: par type de visite",
          "Effectif des médecins de prévention",
          "Aménagements de postes de travail au 31 décembre de l'AC* : par action menée",
          "Aménagements de postes de travail au 31 décembre de l'AC* : par décision de l'administration",
          "Agents ayant déclaré un accident reconnus par l'administration l'AC*: par type d'accident",
          "Nombre cumulé de jours d'arrêt pour accident reconnus par l'administration l'AC*: par type d'accident",
          "Nombre d'accidents reconnus par l'administration, impliquant un agent contractuel l'AC*: par type d'accident",
          "Organisation du suivi médical des agents par la MP*: par type de visite",
          "Nbre de BOE et mesures facilitant la réinsertion ou le maintien en activité des BOE"
        )) {
          if (input$plot_sante_secu == "Histogramme superposé (tableau croisé uniquement)") {
            showNotification(
              "NB: L'histogramme superposé n'offre pas une visualisation adapté aux tableaux ventilés !!!",
              type = "error",
              duration = 5
            )
          }
          
          data_rsu$tables_sante_secu_hist = tables_sante_secu_defaut() %>% slice(-nrow(.)) %>% mutate(value = Effectif, names = rownames(.)) %>% filter(Effectif !=
                                                                                                                                                          0)    # Création de la table à utilisé pour représenter l'histogramme conformément aux attendus de plotly
          data_rsu$tables_sante_secu_cam = tables_sante_secu_defaut() %>%  slice(-nrow(.)) %>% mutate(value = Effectif, names = rownames(.)) %>% filter(Effectif !=
                                                                                                                                                          0)   # Création de la table à utilisé pour représenter le camembert conformément aux attendus de plotly
          
          data_rsu$val_type_plot_sante_secu_1 = input$val_type_plot_sante_secu_1
          
          
          return(switch(
            input$plot_sante_secu,
            "Camembert" = cam(
              titre = input$plot_title_sante_secu_1,
              ma_data = data_rsu$tables_sante_secu_cam,
              type_aff = data_rsu$val_type_plot_sante_secu_1,
              couleur = input$colors_plot_sante_secu_1
            ),
            
            "Histogramme" = histo(
              titre = input$plot_title_sante_secu_1,
              ma_data = data_rsu$tables_sante_secu_hist,
              type_aff =  data_rsu$val_type_plot_sante_secu_1,
              couleur = input$colors_plot_sante_secu_1,
              axe_1 = input$plot_axe_sante_secu_1_1,
              axe_2 = input$plot_axe_sante_secu_1_2
            )
          ))
        } else{
          if (input$plot_sante_secu == "Camembert") {
            showNotification(
              "NB: Le camembert n'offre pas une visualisation adapté aux tableaux croisés !!!",
              type = "error",
              duration = 5
            )
          }
          
          
          data_rsu$tables_sante_secu_hist = tables_sante_secu_defaut() %>% slice(-nrow(.)) %>% mutate(names = rownames(.)) %>% filter(Total !=
                                                                                                                                        0)
          names(data_rsu$tables_sante_secu_hist) = gsub("\\.", " ", names(data_rsu$tables_sante_secu_hist))
          
          data_rsu$val_type_plot_sante_secu_1 = input$val_type_plot_sante_secu_1
          
          return(switch(
            input$plot_sante_secu,
            "Histogramme" = histo_crois_2(
              titre = input$plot_title_sante_secu_1,
              ma_data = data_rsu$tables_sante_secu_hist,
              type_aff =  data_rsu$val_type_plot_sante_secu_1,
              couleur = input$colors_plot_sante_secu_1,
              axe_1 = input$plot_axe_sante_secu_1_1,
              axe_2 = input$plot_axe_sante_secu_1_2
            ),
            
            "Histogramme superposé (tableau croisé uniquement)" = histo_crois_2(
              titre = input$plot_title_sante_secu_1,
              ma_data = data_rsu$tables_sante_secu_hist,
              type_aff =  data_rsu$val_type_plot_sante_secu_1,
              couleur = input$colors_plot_sante_secu_1,
              axe_1 = input$plot_axe_sante_secu_1_1,
              axe_2 = input$plot_axe_sante_secu_1_2,
              sup = 'stack'
            )
          ))
        }
        
      })
      
      
      
      
      #------> Sorties des affichages des graphiques
      
      output$plot_vent_crois_sante_secu = renderPlotly({
        if (is.null(input$vent_crois_btn_sante_secu) != T) {
          tryCatch({
            plot_sante_secu_dataset()
          }, error = function(e) {
            
          })
        }
      })
      
      
      
      
    }, error = function(e) {
      showNotification("Valider d'abord votre partition",
                       type = "error",
                       duration = 5)
    })
  })
  
  ###-------------------------------------------Fin code de conception des tables & graphes par défaut  santé et sécurité au travail
  
  
  
  
  
  ## ------------------------------------------Code de conception des tables & graphes manuelles  santé et sécurité au travail---------------------------------------------------
  
  output$manuelle_vent_sante_secu = DT::renderDT({
    if (is.null(input$manuelle_btn_sante_secu) != T) {
      # Affichage de la table si la case  " Tableau composé manuellement" est coché
      
      tryCatch({
        tranche_manuelle_sante_secu = data_rsu$tranche_sante_secu %>%    # Constitution de la table issue de la séléction manuelle
          select(input$select_manuelle_vent_sante_secu) %>% t() %>%
          data.frame() %>%
          rename(Effectif = names(.))
        
        table_manuelle(ma_table = tranche_manuelle_sante_secu)
        
      }, error = function(e) {
        
      })
    }
  })
  
  ##--------> représentation des tableaux ventilés: graphes 2 sous table  mouvement du perso crée manuellement
  
  plot_sante_secu_manuelle_dataset = reactive({
    tranche_manuelle_sante_secu = data_rsu$tranche_sante_secu %>%
      select(input$select_manuelle_vent_sante_secu) %>% t() %>% data.frame() %>%
      rename(Effectif = names(.)) %>%
      mutate(names = gsub("\\.", " ", sub("[0-9]*- ", "", rownames(.))))
    
    tranche_manuelle_sante_secu_plot = tranche_manuelle_sante_secu %>% filter(Effectif !=
                                                                                0)
    
    #--------> fonction switch permettant d'avoir des rendu au relatif aux choix graphiques faites par l'utilisateur
    observeEvent(input$select_manuelle_vent_sante_secu, {
      updateTextInput(session,
                      "legend_plot_sante_secu",
                      value = paste(gsub(
                        "\\.", " ", sub(
                          "[0-9]*- ",
                          "",
                          rownames(tranche_manuelle_sante_secu_plot)
                        )
                      ), sep = ",")) # mise à jour des colonnes à séléctionner pour constitué les légendes
    })
    
    rownames(tranche_manuelle_sante_secu_plot) = input$legend_plot_sante_secu %>% strsplit(., split = ",") %>% unlist() %>% as.vector() # Mise à jour de la légende à travers le nom des lignes
    data_rsu$val_type_plot_sante_secu_2 = input$val_type_plot_sante_secu_2
    
    
    if (is.null(c(
      input$select_manuelle_vent_sante_secu,
      input$manuelle_btn_sante_secu
    )) != T   &
    input$plot_sante_secu == "Histogramme superposé (tableau croisé uniquement)") {
      showNotification(
        "NB: L'histogramme superposé n'offre pas une visualisation adapté aux tableaux ventilés !!!",
        type = "error",
        duration = 5
      )
    }
    
    switch(
      input$plot_sante_secu,
      "Camembert" = cam(
        titre = input$plot_title_sante_secu_2,
        ma_data = tranche_manuelle_sante_secu_plot,
        type_aff = data_rsu$val_type_plot_sante_secu_2,
        couleur = input$colors_plot_sante_secu_2,
        noms_legende = input$legend_plot_sante_secu %>% strsplit(., split = ",") %>% unlist() %>% as.vector()
      ),
      "Histogramme" = histo(
        titre = input$plot_title_sante_secu_2,
        ma_data = tranche_manuelle_sante_secu_plot,
        type_aff = data_rsu$val_type_plot_sante_secu_2,
        couleur = input$colors_plot_sante_secu_2,
        noms_legende = input$legend_plot_sante_secu %>% strsplit(., split = ",") %>% unlist() %>% as.vector()
        ,
        axe_1 = input$plot_axe_sante_secu_2_1,
        axe_2 = input$plot_axe_sante_secu_2_2
      )
      
    )
  })
  
  #--------->  Graphiques associés aux tables manuelles
  
  output$plot_sante_secu_manuelle =  renderPlotly({
    if (is.null(input$manuelle_btn_sante_secu) != T) {
      tryCatch({
        plot_sante_secu_manuelle_dataset()
      }, error = function(e) {
        
      })
    }
  })
  
  ## -------------------------------------------Fin Code de conception des tables & graphes manuelles  santé et sécurité au travail---------------------------------------------------
  
  
  ##-----------------------> Partie annexe: Création de l'organigramme récapitulatif des composant de chaques rubriques principales traitant des différentes thématiques RH
  
  
  
  output$dendo_act_soc <- renderGrViz({
    # Création d'un organigramme
    grViz(
      "
    digraph org_chart {
      node [shape=square, style=filled, color= '#7A90A4', fontname=Roman, fontsize=12]


     subgraph cluster_legend {
            label = 'Légende:';
            style = filled;
            color = lightgrey;fontsize=15

            // Noeuds du cluster de la direction
              # Noeuds pour la légende
            legend3 [label='Tableaux par défaut', shape=rectangle, color=gray, fontcolor=white, fontsize=7]
            legend2 [label='Sous-thèmes', shape=rectangle, color='#7A90A4', fontcolor=white, fontsize=7]
            legend1 [label='Thématiques', shape=rectangle, color= '#191970', fontcolor=white, fontsize=7]
            }

      # Définir les noeuds
      A [label='Action sociale', shape= ellipse, fontsize=20, color= '#191970', fontcolor=white]
      A1 [label='A- Préstations sociales']
      A2 [label='Informations supplémentaires']


      C1 [label='Agents bénéficiaires
      de prestations sociales:
      par type de prestation', color= 'gray']
      C2 [label='Agents bénéficiaires
      de prestations sociales:
      type de prestation & genre', color= 'gray']
      C3 [label='Agents bénéficiaires
      de prestations sociales:
      type de prestation
      & catégorie', color= 'gray']
      C4 [label='Agents bénéficiaires
      de prestations sociales:
      catégorie & genre', color= 'gray']

      C5 [label='Réponses qualitatives', color= 'gray']

      # Définir les relations

      A -> {A1 A2}
      A1 -> {C1 C2 C3 C4}

      A2 -> {C5}

    }
    "
    )
    
    
  })
  
  output$dendo_dial_soc = renderGrViz({
    grViz(
      "
    digraph org_chart {
      node [shape=square, style=filled, color= '#7A90A4', fontname=Roman, fontsize=15]


      subgraph cluster_legend {
              label = 'Légende:';
              style = filled;
              color = lightgrey;fontsize=15

              // Noeuds du cluster de la direction
                # Noeuds pour la légende
              legend4 [label='Tableaux par défaut', shape=rectangle, color=gray, fontcolor=white, fontsize=7]
              legend3 [label='Sous-thèmes', shape=rectangle, color='#7A90A4', fontcolor=white, fontsize=7]
              legend2 [label='Tranches', shape=rectangle, color='#7C4C53', fontcolor=white, fontsize=7]
              legend1 [label='Thématiques', shape=rectangle, color= '#191970', fontcolor=white, fontsize=7]
              }

      # Définir les noeuds
      A [label='Dialogue social', shape=ellipse, fontsize=30, color= '#191970', fontcolor=white]

      B1 [label='Tranche 1', color= '#7C4C53', fontsize=20]
      B2 [label='Tranche 2', color= '#7C4C53', fontsize=20]
      B3 [label='Tranche 3', color= '#7C4C53', fontsize=20]

      C1 [label='A- Grève']
      C2 [label='Informations
      supplémentaires']

      C3 [label='B- Organismes consultatifs']
      C4 [label='C- Formation
      des membres de la FS']
      C5 [label='D- Activité de la FS
      hors réunions plénière
      de l instance les enquêtes
      menées']

      C6 [label='E- Consultation de la FS
      pour des projets
      au cours de l`AC']

      C8 [label='F- Formation Spécialisé (FS):
      recours à un expert certifié,
      avis rendus et mesures proposées']
      C9 [label='Informations
      supplémentaires']

      C11 [label='Informations
      supplémentaires']

      D1 [label='Mot d`ordre national
      & local: par nombre
      de jours non travaillés', color= 'gray']

      D2 [label='Représentants du personnel
      de la FS au 31 décembre
      de l`AC:par fonction', color= 'gray']
      D3 [label='Actions engagées par les
      instances au cours de l`AC*:
      action & instance', color= 'gray']
      D4 [label='Actions de la Formation
      Spécialisée (FS)
      au cours de l`AC*', color= 'gray']
      D5 [label='Saisines de la FS par
      le Comité Social:
      par type initiateur', color= 'gray']
      D6 [label='Représentants du personnel
      des instances au 31 décembre
      de l`AC: fonction & instance', color= 'gray']
      D7 [label='Représentants du personnel
      de la CS au 31 décembre
      de l`AC: fonction & genre', color= 'gray']
      D8 [label='Représentants du personnel
      de la FS au 31 décembre
      de l`AC: fonction & genre', color= 'gray']
      D9 [label='Réunions de la FS
      (hors période de circonstance
      exceptionnelle) tenues AC*:
      par motifs', color= 'gray']
      D10 [label='Réunions de la FS sans
      représentants du personnel
      (y compris les annulations
      faute de quorum): par
      type de participant', color= 'gray']

      D11 [label='Membres de la FS en fonction
      au 31 décembre de l`AC*:
      état & type de formation', color= 'gray']
      D12 [label='Membres de la FS en fonction
      au 31 décembre de l`AC*:
      état de la formation & genre', color= 'gray']
      D13 [label='Membres de la FS en fonction
      au 31 décembre de l`AC* ayant
      bénéficié d`une formation
      dans son intégralité:
      type de formation & genre', color= 'gray']
      D14 [label='Membres de la FS en fonction
      au 31 décembre de l`AC*
      ayant bénéficié d`une formation
      dans son intégralité: type
      de formation & Nbr de jours', color= 'gray']

      D15 [label='Enquêtes menées suite
      à un AT* ou MP*: AT*,
      MP* & état de l enquête', color= 'gray']
      D16 [label='Groupes de travail en
      lien avec les travaux
      de la FS & Nbre de visites
      de site réaliséés
      au cours de l`AC*', color= 'gray']

      D17 [label='Projets aménagement et
      nouvelle technologie,
      règlement/consignes:
      type de projet
      & action de la FS', color= 'gray']

      D18 [label='Actions engagées
      par la FS au
      cours de l`AC*', color= 'gray']
      D19 [label='Actions engagées par la
      FS au cours de l`AC*:
      type d`action & décision
      de l administration', color= 'gray']
      D20 [label='Mesures proposées par
      la FS au cours de l`AC*:
      par type de mesure', color= 'gray']
      D21 [label='Demandes de recours à
      une expertise & coût
      globale au cours
      de l`AC*', color= 'gray']
      D22 [label='Réponses qualitatives', color= 'gray']
      D23 [label='Réponses qualitatives', color= 'gray']
      D24 [label='Réponses qualitatives', color= 'gray']


      Z1 [label= '', shape=circle]
      Z2 [label= '', shape=circle, color= 'gray']

      # Définir les relations
      A -> {B1 B2 B3}
      B1 -> {C1 C2}

      B2 -> Z1
      Z1 -> {C3 C4 C5 C6 C9}

      B3 -> {C8 C11}

      C1 -> D1

      C2 -> D22

      C3 -> {Z2 D2 D3 D4 }

      Z2 ->{D5 D6 D7 D8 D9 D10}

     C4 -> {D11 D12 D13 D14}

     C5 -> {D15 D16}

     C6 -> D17

     C8 -> {D18 D19 D20 D21}

     C9 -> D23

    C11-> D24
    }
    "
    )
  })
  
  
  
  
  output$dendo_emploi <- renderGrViz({
    # Création d'un organigramme
    grViz(
      "
    digraph org_chart {
      node [shape=square, style=filled, color= '#7A90A4', fontname=Roman, fontsize=12]



     subgraph cluster_legend {
            label = 'Légende:';
            style = filled;
            color = lightgrey;fontsize=15

            // Noeuds du cluster de la direction
              # Noeuds pour la légende
            legend3 [label='Tableaux par défaut', shape=rectangle, color=gray, fontcolor=white, fontsize=7]
            legend2 [label='Sous-thèmes', shape=rectangle, color='#7A90A4', fontcolor=white, fontsize=7]
            legend1 [label='Thématiques', shape=rectangle, color= '#191970', fontcolor=white, fontsize=7]
            }

      # Définir les noeuds
      A [label='Emploi', shape=ellipse, fontsize=20, color= '#191970', fontcolor=white]
      A1 [label='A- Photographie de la DDI']
      A2 [label='Répartitions des effectifs
      physiques au 31 Décembre:
      par origine ministérielle', color= 'gray']
      A3 [label='Répartitions des effectifs
      physiques au 31 Décembre:
      par catégorie', color= 'gray']
      A4 [label='Répartitions des effectifs
      physiques au 31 Décembre:
      par genre', color= 'gray']
      A5 [label='Répartitions des effectifs
      physiques au 31 Décembre:
      par filière', color= 'gray']
      A6 [label='Moyenne d`âge des
      agents aux 31 Décembre:
      par genre', color= 'gray']

      A7 [label='Moyenne d`âge des
      agents aux 31 Décembre:
      par ministère', color= 'gray']
      A8 [label='Moyenne d`âge des
      agents aux 31 Décembre:
      par catégorie', color= 'gray']
      A9 [label='Répartitions des âges
      des agents: tranche
      d`âge & genre', color= 'gray']
      A10 [label='Répartitions des âges
      des agents de catégorie A:
      tranche d`âge & genre', color= 'gray']
      A11 [label='Répartitions des âges
      des agents de catégorie B:
      tranche d`âge & genre', color= 'gray']
      A12 [label='Répartitions des âges
      des agents de catégorie C:
      tranche d`âge & genre', color= 'gray']
      A13 [label='Répartitions des âges
      des agents aux 31 Décembre:
      par catégorie', color= 'gray']

      Z [label='',shape= circle, color= 'gray']



      B [label='B- Organisation du temps de travail']

      B1 [label='Répartitions des effectifs
      physiques au 31 Décembre: par ETP', color= 'gray']

      B2 [label='Moyenne des ETP des
      agents aux 31 Décembre:
      par ministère', color= 'gray']

      # Définir les relations
      A -> {A1 B}
      A1 -> {Z}

      Z -> {A2 A3 A4 A5 A6 A7 A8 A9 A10 A11 A12 A13}
      B -> {B1 B2}
    }
    "
    )
    
    
  })
  
  output$dendo_formation = renderGrViz({
    grViz(
      "
      digraph org_chart {
        node [shape=square, style=filled, color= '#7A90A4', fontname=Roman, fontsize=12]



     subgraph cluster_legend {
            label = 'Légende:';
            style = filled;
            color = lightgrey;fontsize=15

            // Noeuds du cluster de la direction
              # Noeuds pour la légende
            legend3 [label='Tableaux par défaut', shape=rectangle, color=gray, fontcolor=white, fontsize=7]
            legend2 [label='Sous-thèmes', shape=rectangle, color='#7A90A4', fontcolor=white, fontsize=7]
            legend1 [label='Thématiques', shape=rectangle, color= '#191970', fontcolor=white, fontsize=7]
            }

        # Définir les noeuds
        A [label='Formation', shape=ellipse, fontsize=20, color= '#191970', fontcolor=white]

        B1 [label='A- Agents formés et Nbre
        de jours de formation']
        B2 [label='B- Congés de formation et
        encadrement intermédiaire']
        B3 [label='Informations supplémentaires']

        C1 [label='Agent formés par
        type de formation', color= 'gray']
        C2 [label='Agent formés:
        genre & catégorie', color= 'gray']
        C3 [label='Agent formés:
        type de formation
        & genre', color= 'gray']
        C4 [label='Agent formés:
        type de formation
        & catégorie', color= 'gray']
        C5 [label='Formations reçu
        par les agents: type
        de formation &
        Nbre agent, de jours
        de formation', color= 'gray']

        C6 [label='Demandes de congés
        formation: catégorie
        & décision prise', color= 'gray']
        C7 [label='Nbre de cadres
        intermédiaire issus
        de la DDI: genre
        & catégories', color= 'gray']

        C8 [label='Réponses qualitatives', color= 'gray']

        # Définir les relations
        A -> {B1 B2 B3}
        B1 -> {C1 C2 C3 C4 C5}

        B2 -> {C6 C7}

        B3 -> C8

        }
      "
    )
    
    
    
  })
  
  
  output$dendo_mouv_perso = renderGrViz({
    grViz(
      "
      digraph org_chart {
        node [shape=square, style=filled, color= '#7A90A4', fontname=Roman, fontsize=12]



     subgraph cluster_legend {
            label = 'Légende:';
            style = filled;
            color = lightgrey;fontsize=15

            // Noeuds du cluster de la direction
              # Noeuds pour la légende
            legend3 [label='Tableaux par défaut', shape=rectangle, color=gray, fontcolor=white, fontsize=7]
            legend2 [label='Sous-thèmes', shape=rectangle, color='#7A90A4', fontcolor=white, fontsize=7]
            legend1 [label='Thématiques', shape=rectangle, color= '#191970', fontcolor=white, fontsize=7]
            }

        # Définir les noeuds
        A [label='Mouvement du personnel', shape=ellipse, fontsize=20, color= '#191970', fontcolor=white]

        B1 [label='A- Arrivées']
        B2 [label='B- Départs']
        B3 [label='Informations supplémentaires']

        C1 [label='Motifs d`embauche
        des titulaires', color=gray]
        C2 [label='Motifs d`embauche
        des contractuels', color=gray]
        C3 [label='Catégories & motifs
        d`embauche
        des titulaires', color=gray]
        C4 [label='Catégories & motifs
        d`embauche des
        contractuels', color=gray]
        C5 [label='Statuts d`emploi
        & catégories
        des embauches', color=gray]
        C6 [label='Genres
        & catégories
        des embauches', color=gray]

        C7 [label='Motifs de départ
        des titulaires', color=gray]
        C8 [label='Motifs de départ
        des contractuels', color=gray]
        C9 [label='Catégories & motifs
        de départ des
        titulaires', color=gray]
        C10 [label='Catégories & motifs
        de départ
        des contractuels', color=gray]
        C11 [label='Statuts d`emploi
        & catégories des départs', color=gray]
        C12 [label='Genres & catégories
        des départs', color=gray]
        C13 [label='Réponses qualitatives', color= 'gray']


        # Définir les relations
        A -> {B1 B2 B3}
        B1 -> {C1 C2 C3 C4 C5 C6}

        B2 -> {C7 C8 C9 C10 C11 C12}

        B3 -> C13
        }
      "
    )
  })
  
  output$dendo_orga_trav = renderGrViz({
    grViz(
      "
    digraph org_chart {
      node [shape=square, style=filled, color= '#7A90A4', fontname=Roman, fontsize=20]


          subgraph cluster_legend {
              label = 'Légende:';
              style = filled;
              color = lightgrey;fontsize=15

              // Noeuds du cluster de la direction
                # Noeuds pour la légende
              legend4 [label='Tableaux par défaut', shape=rectangle, color=gray, fontcolor=white, fontsize=7]
              legend3 [label='Sous-thèmes', shape=rectangle, color='#7A90A4', fontcolor=white, fontsize=7]
              legend2 [label='Tranches', shape=rectangle, color='#7C4C53', fontcolor=white, fontsize=7]
              legend1 [label='Thématiques', shape=rectangle, color= '#191970', fontcolor=white, fontsize=7]
          }

      # Définir les noeuds
      A [label='Organisation du travail', shape=ellipse, fontsize=40, color= '#191970', fontcolor=white]

      B1 [label='Tranche 1', color= '#7C4C53', fontsize=30]
      B2 [label='Tranche 2', color= '#7C4C53', fontsize=30]
      B3 [label='Tranche 3', color= '#7C4C53', fontsize=30]

      C1 [label='A- Astreintes et
      interventions']
      C2 [label='B- Télétravail et
      travail à distance']
      C3 [label='C- Heures sup_
      plémentaire']
      C4 [label='D- Temps complet/incomplet
      ou non complet-
      Temps plein & partiel']
      C5 [label='E- Congés']
      C6 [label='F- Abscences au
      travail hors
      raison de santé']
      C7 [label='Informations
      supplémentaires']

      C8 [label='G- CET']
      C9 [label='Informations
      supplémentaires']

      C10 [label='H- Absences au
      travail pour
      raisons de santé']
      C11 [label='I- Jours de carence']
      C12 [label='Informations
      supplémentaires']

      D1 [label='Agent* soumis
      à des astreintes:
      catégorie & sexe', color= 'gray']
      D2 [label='Agent* ayant
      bénéficié d`un repos
      compensateur: par
      catégorie & sexe', color= 'gray']

      D3 [label='Agent* exerçant
      en télétravail: par
      nombre de jour
      de TT*', color= 'gray']
      D4 [label='Agent* exerçant
      en télétravail:
      nombre de jour
      TT* & catégorie', color= 'gray']
      D5 [label='Agent* exerçant
      en télétravail:
      nombre de jour
      TT* & genre', color= 'gray']
      D6 [label='Demandes de TT*
      exprimé au
      fil de l eau :
      catégorie & genre', color= 'gray']
      D7 [label='Refus de TT*
      exprimé au
      fil de l eau :
      catégorie & genre', color= 'gray']
      D8 [label='Demandes de TT*
      pour campagne
      de recensement :
      catégorie & genre', color= 'gray']
      D9 [label='Refus de TT*
      pour campagne
      de recensement :
      catégorie & genre', color= 'gray']

      D10 [label='Agent* par: ayant
      effectué des heures
      supplémentaires l`année
      concernée: catégorie
      & genre', color= 'gray']
      D11 [label='Autres types
      d`heures; écrêtés,
      celui du dispositif
      de l horaire va-
      riable', color= 'gray']

      D12 [label='Demandes de temps
      partiels au cours
      de l`année concernée:
      catégorie & genre', color= 'gray']

      D13 [label='Nombre de dons
      de jours enregistrés
      pour aide
      à la santé*:
      catégorie & genre', color= 'gray']
      D14 [label='Nombre de dons
      de jours reçus
      pour aide
      à la santé*:
      catégorie & genre', color= 'gray']

      D15 [label='Nbre de jours
      de congés bénéficiés
      sur une durée
      égale ou superieur
      à 6 mois: motif
      & genre', color= 'gray']

      D16 [label='Nbre d`agents
      en congés, abscent
      au cours de
      l`année concernée :
      motif & genre', color= 'gray']

      D17 [label='Nbre de jours
      d`abscence pour
      congés hors raison
      de santé l`année
      concernée:
      motif & genre', color= 'gray']

      D18 [label='Réponses
      qualitatives', color= 'gray']

      D19 [label='Agent possédant
      ou ayant ouvert
      un CET: motif
      & genre', color= 'gray']
      D20 [label='Agent ayant
      déposé ou
      ayant utilisé
      des jours
      de CET:
      motif & genre', color= 'gray']
      D21 [label='Jours de CET
      déposés ou
      utilisés:
      motif & genre', color= 'gray']
      D22 [label='Réponses qualitatives', color= 'gray']

      D23 [label='Agent ayant
      pris un/des
      congés maladie
      au cours de
      l`année concernée:
      catégorie & genre', color= 'gray']
      D24 [label='Jours cumulés
      d`arrêt de congé
      maladie au cours
      de l`année
      concernée:
      catégorie & sexe', color= 'gray']
      D26 [label='Nombre cumulé
      de jours d`arrêt
      CMO au cours
      de l`AC*: par
      genre', color= 'gray']
      D27 [label='Nombre cumulé
      d`arrêt CMO:
      période & genre', color= 'gray']
      D29 [label='Nombre cumulé
      de jours de
      congés de CMO:
      période & genre', color= 'gray']
      D30 [label='Nombre cumulé
      de jours d`arrêt
      déclaré/reconnu
      au cours de l`AC*
      pour maladies
      professionnelles:
      état & genre', color= 'gray']
      D31 [label='Nombre cumulé
      de jours d`arrêt
      l`AC* reconnu par
      l`administration:
      type d`accident
      & genre', color= 'gray']
      D32 [label='Maladies professionnelles
      reconnues ayant
      débuté au cours
      de l`année étudiée:
      Imputabilité &
      genre', color= 'gray']
      D33 [label='Maladies professionnelles
      reconnues ayant
      débuté au cours
      de l`année étudiée:
      aptitude & genre', color= 'gray']
      D34 [label='Maladies professionnelles
      entrainant une
      incapacité: type
      d`incapacité
      & genre', color= 'gray']
      D35 [label='Nbre de jours
      de carence imputés
      au cours de
      l`année concernée:
      catégorie & genre', color= 'gray']

      D36 [label='Réponses qualitatives', color= 'gray']

      Z1 [label= '', shape=circle, color= 'gray']
      Z2 [label= '', shape=circle, color= 'gray']

      # Définir les relations
      A -> {B1 B2 B3}
      B1 -> {C1 C2 C3 C4 C5 C6 C7}

      B2 -> {C8 C9}

      B3 -> {C10 C11 C12}


      C1 -> {D1 D2}

      C2 -> {Z1}

      Z1 -> {D3 D4 D5 D6 D7 D8 D9}

      C3 -> {D10 D11 }

      C4 -> {D12}

      C5 -> {D13 D14}

      C6 -> {D15 D16 D17}

      C7 -> {D18}

      C8 -> {D19 D20 D21}

      C9 -> D22

      C10 -> {Z2}
      Z2 -> {D23 D24 D26 D27 D29 D30 D31 D32 D33 D34}
      C11 -> D35

      C12 -> D36


    }
   "
    )
    
    
  })
  
  output$dendo_parc_pro = renderGrViz({
    grViz(
      "
      digraph org_chart {
        node [shape=square, style=filled, color= '#7A90A4', fontname=Roman, fontsize=12]



     subgraph cluster_legend {
            label = 'Légende:';
            style = filled;
            color = lightgrey;fontsize=15

            // Noeuds du cluster de la direction
              # Noeuds pour la légende
            legend3 [label='Tableaux par défaut', shape=rectangle, color=gray, fontcolor=white, fontsize=7]
            legend2 [label='Sous-thèmes', shape=rectangle, color='#7A90A4', fontcolor=white, fontsize=7]
            legend1 [label='Thématiques', shape=rectangle, color= '#191970', fontcolor=white, fontsize=7]
            }

        # Définir les noeuds
        A [label='Parcours professionnels', shape=ellipse, fontsize=20, color= '#191970', fontcolor=white]

        B1 [label='A- Postes pourvus']
        B2 [label='B- Avancement de
        grade et promotion
        interne']
        B3 [label='Informations supplémentaires']

        C1 [label='Statut d’emploi
        du candidat
        retenu', color=gray]
        C2 [label='Pourvu par
        des candidats
        externes ou
        internes', color=gray]
        C3 [label='Postes pourvus:
        Sexe & périmètre
        de provenance', color=gray]
        C4 [label='Postes pourvus:
        Sexe & Statut
        d`emploi', color=gray]

        C5 [label='Postes promus
        & promouvable:
        en fonction
        du sexe', color=gray]
        C6 [label='Postes promus:
        croisé par sexe
        & catégorie', color=gray]
        C7 [label='Postes promouvable:
        croisé par sexe
        & catégorie', color=gray]
        C8 [label='Réussites aux concours
        et examen pro: par
        catégorie/statuts
        & sexe', color=gray]

        C9 [label='Réponses qualitatives', color= 'gray']


        # Définir les relations
        A -> {B1 B2 B3}
        B1 -> {C1 C2 C3 C4}

        B2 -> {C5 C6 C7 C8 }

        B3 -> C9
        }
      "
    )
    
  })
  
  output$dendo_sanc_discip = renderGrViz({
    grViz(
      "
      digraph org_chart {
        node [shape=square, style=filled, color= '#7A90A4', fontname=Roman, fontsize=12]



     subgraph cluster_legend {
            label = 'Légende:';
            style = filled;
            color = lightgrey;fontsize=15

            // Noeuds du cluster de la direction
              # Noeuds pour la légende
            legend3 [label='Tableaux par défaut', shape=rectangle, color=gray, fontcolor=white, fontsize=7]
            legend2 [label='Sous-thèmes', shape=rectangle, color='#7A90A4', fontcolor=white, fontsize=7]
            legend1 [label='Thématiques', shape=rectangle, color= '#191970', fontcolor=white, fontsize=7]
            }

        # Définir les noeuds
        A [label='Sanction disciplinaire', shape=ellipse, fontsize=20, color= '#191970', fontcolor=white]

        B1 [label='A- Sanctions disciplinaires
        notifiés au cours de
        l`année en cours']
        B2 [label='Informations supplémentaires']

        C1 [label='Sanctions disciplinaires
        du 1er groupe:
        par type de
        sanction', color=gray]
        C2 [label='Sanctions disciplinaires :
        par type de
        groupes', color=gray]
        C3 [label='Sanctions disciplinaires
        du 1er groupe:
        type de sanction &
        statut professionnel', color=gray]
        C4 [label='Sanctions disciplinaires
        du 1er groupe:
        type de sanction
        & genre', color=gray]
        C5 [label='Sanctions disciplinaires:
        type de groupe
        & genre', color=gray]
        C6 [label='Sanctions disciplinaires:
        type de groupe &
        statut professionnel', color=gray]
        C7 [label='Sanctions ayant un
        lien direct avec
        sanction d`un
        agissement sexiste
        ou violence à
        caractère sexuelle:
        par type de
        groupe', color=gray]


        C8 [label='Réponses qualitatives', color= 'gray']


        # Définir les relations
        A -> {B1 B2}
        B1 -> {C1 C2 C3 C4 C5 C6 C7 }
        B2 -> C8
        }
      "
    )
    
    
  })
  
  output$dendo_sante_secu = renderGrViz({
    grViz(
      "
    digraph org_chart {
      node [shape=square, style=filled, color= '#7A90A4', fontname=Roman, fontsize=30]


          subgraph cluster_legend {
              label = 'Légende:';
              style = filled;
              color = lightgrey;fontsize=15

              // Noeuds du cluster de la direction
                # Noeuds pour la légende
              legend4 [label='Tableaux par défaut', shape=rectangle, color=gray, fontcolor=white, fontsize=7]
              legend3 [label='Sous-thèmes', shape=rectangle, color='#7A90A4', fontcolor=white, fontsize=7]
              legend2 [label='Tranches', shape=rectangle, color='#7C4C53', fontcolor=white, fontsize=7]
              legend1 [label='Thématiques', shape=rectangle, color= '#191970', fontcolor=white, fontsize=7]
          }

      # Définir les noeuds
      A [label='Santé et sécurité au travail', shape=ellipse, fontsize=70, color= '#191970', fontcolor=white]

      B1 [label='Tranche 1', color= '#7C4C53', fontsize=40]
      B2 [label='Tranche 2', color= '#7C4C53', fontsize=40]
      B3 [label='Tranche 3', color= '#7C4C53', fontsize=40]

      C1 [label='A- Mésures générales']
      C2 [label='Informations
      supplémentaires']

      C3 [label='B- Dispositifs de
      signalement']
      C4 [label='C- Suicides']
      C5 [label='D- Acteurs de la
      prévention']
      C6 [label='E- Instance de
      prévention']
      C7 [label='F- Commissions
      médicales']
      C8 [label='G- Actions de
      prévention']
      C9 [label='H- Médecine de
      prévention']
      C10 [label='Informations
      supplémentaires']


      C11 [label='I- Risques
      professionnels']
      C12 [label='J- Protection
      fonctionnelle']
      C13 [label='Informations
      supplémentaires']

      ########
      D1 [label='Nombre de mesures
      générales prises
      en vue de faciliter
      la remise/maintien
      en activité des BOE:
      par action menée', color= 'gray']
      D2 [label='Nbre de BOE et
      mesures facilitant
      la réinsertion ou
      le maintien en
      activité des BOE', color= 'gray']

      D3 [label='Réponses qualitatives', color= 'gray']

      D4 [label='Signalements inscrits
      dans le registre
      DGI au cours de
      l`AC*: par types
      d`actes', color= 'gray']
      D5 [label='Actions menées
      suite au signa-
      lements inscrit
      au registre DGI', color= 'gray']

      D6 [label='Nombre de suicides
      ou de tentatives
      de suicides: état &
      lieu d`intervention', color= 'gray']

      D7 [label='Dénombrement des ac-
      teurs de la prévention
      en fonction au
      cours de l`AC*', color= 'gray']
      D8 [label='Dénombrement de la
      quotité de temps
      de travail dédié
      aux acteurs de
      prévention en
      fonction*', color= 'gray']
      D9 [label='Acteurs de la préven-
      tion en  fonction au
      cours de l`AC*:
      situation & type
      d`acteur*', color= 'gray']
      D10 [label='Acteurs de la prévent-
      ion en fonction*:
      quotité de temps
      & type d`acteur', color= 'gray']
      D11 [label='Suivi de formation
      des acteurs de
      prévention en
      fonction*: type
      de formation
      & type d`acteur', color= 'gray']
      D12 [label='Agent en poste au
      31 décembre de
      l`AC* ayant été
      formé à la santé
      et à la sécurité
      au travail:par
      genre', color= 'gray']


      D13 [label='Observations inscrites
      sur le registre
      SST au cours
      de l`AC*:
      par problématique', color= 'gray']

      D14 [label='Agents et positions
      par rapport au PPR*:
      décision/position
      & genre', color= 'gray']
      D15 [label='Reclassement des
      agents suite à
      une inaptitude:
      requête & genre', color= 'gray']
      D16 [label='Reclassement des
      agents suite à
      une inaptitude:
      type d`inaptitude
      & requête', color= 'gray']
      D17 [label='Agents ayant fait
      l`objet d`une action
      de la conseil médical
      par type d`action
      & genre', color= 'gray']
      D18 [label='Autres actions me-
      nées par la com-
      mission médicale
      pour les agents l`AC*:
      type d`action
      & genre', color= 'gray']

      D19 [label='Recours/rapport de
      conflit avec le chef
      de service/inspection
      ministérielles de
      la DDI', color= 'gray']
      D20 [label='Conflits entre l`admi-
      nistration et la FS:
      par motifs des
      recours à l`inspection
      du travail au titre
      de l`article 5-5 l`AC*', color= 'gray']
      D21 [label='Agents formés à
      la STT durant l`AC*:
      domaine de forma-
      tion & genre', color= 'gray']
      D22 [label='Préconisations en matière
      de Risques Psycho-
      sociaux au cours
      de l`AC*: acteurs
      & décision de
      l`administration', color= 'gray']
      D23 [label='Préconisations émises
      et mises en oeuvres
      en matière de
      Risques Psycho-
      sociaux au cours
      de l`AC*', color= 'gray']

      D24 [label='Caractéristiques/Actions
      de la médecine
      de prévention', color= 'gray']
      D25 [label='Organisation du
      suivi médical des
      agents par la MP*:
      par type de visite', color= 'gray']
      D26 [label='Effectif des
      médecins de
      prévention', color= 'gray']
      D27 [label='Aménagements de
      postes de travail
      au 31 décembre
      de l`AC*: par
      action menée', color= 'gray']
      D28 [label='Aménagements de postes
      de travail au
      31 décembre
      de l`AC* : par
      décision de
      l`administration', color= 'gray']
      D29 [label='Suivi médical
      des agents
      par la MP*:
      type & état', color= 'gray']

      D30 [label='Réponses qualitatives', color= 'gray']

      D31 [label='Actes de violence
      physique émanant
      du personnel et
      envers le personnel:
      types & résolutions', color= 'gray']
      D32 [label='Actes de violence
      physique des
      usagers envers
      le personnel:
      types & résolutions', color= 'gray']
      D33 [label='Agents ayant déclaré
      un accident reconnus
      par l`admi_
      nistration l`AC*:
      par type d`accident', color= 'gray']
      D34 [label='Nombre cumulé de
      jours d`arrêt pour
      accident reconnus
      par l`administra-
      tion l`AC*: par
      type d`accident', color= 'gray']
      D35 [label='Nombre d`accidents
      reconnus par l`administra-
      tion, impliquant
      un agent con-
      tractuel l`AC*:
      par type d`accident', color= 'gray']
      D36 [label='Nombre d`accident
      de service(hors
      accident de trajet)
      reconnus par l`admi-
      nistration l`AC*:
      type de décision
      & genre', color= 'gray']
      D37 [label='Nombre d`accident
      de service(hors
      accident de trajet)
      reconnus par
      l`administration
      l`AC*: jours
      d`arrêt déclarés
      & genre', color= 'gray']
      D38 [label='Nombre d`accident
      de trajet reconnus
      par l`administration
      l`AC*: type de
      décision & genre', color= 'gray']
      D39 [label='Nombre d`accident
      de trajet reconnus
      par l`administration
      l`AC*: jours
      d`arrêt déclarés
      & genre', color= 'gray']

      D40 [label='Demandes de pro-
      tections fonction-
      nelles formulés:
      genre & catégorie', color= 'gray']
      D41 [label='Demandes de protec-
      tions fonctionnelles
      acceptées: genre
      & catégorie', color= 'gray']
      D42 [label='MPFF* pour mise
      en cause d`agents
      devant la juridic-
      tion pénale: genre
      & catégorie', color= 'gray']
      D43 [label='MPFF* pour mise
      en cause d`agents
      devant la juridic-
      tion pénale acceptée:
      genre & catégorie', color= 'gray']
      D44 [label='MPFF* pour poursuite
      d`agents pour faute
      de service: genre
      & catégorie', color= 'gray']
      D45 [label='MPFF* pour poursuite
      d`agents pour faute
      de service acceptée:
      genre & catégorie', color= 'gray']
      D46 [label='Réponses qualitatives', color= 'gray']

      Z1 [label= '', shape=circle, color= 'gray']
      Z2 [label= '', shape=circle, color= 'gray']
      Z3 [label= '', shape=circle, color= 'gray']

      # Définir les relations

      A -> {B1 B2 B3}
      B1 -> {C1 C2}

      B2 -> {C3 C4 C5 C6 C7 C8 C9 C10}


      B3 -> {C11 C12 C13}

      C1 -> {D1 D2}

      C2 -> {D3}

      C3 -> {D4 D5}

      C4 -> {D6}

      C5 -> {Z1}

      Z1 -> {D7 D8 D9 D10 D11 D12}

      C6 -> D13

      C7 -> {D14 D15 D16 D17 D18}

      C8 -> {Z2}

      Z2 -> {D19 D20 D21 D22 D23}

      C9 -> { D24 D25 D26 D27 D28 D29 }

      C10 -> D30

      C11 -> {Z3}

      Z3 -> {D31 D32 D33 D34 D35 D36 D37 D38 D39}

      C12 -> {D40 D41 D42 D43 D44 D45}

      C13 -> D46


    }
   "
    )
  })
  
}
