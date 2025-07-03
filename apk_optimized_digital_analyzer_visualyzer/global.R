#Author: Emiliano QUENUM
#Linkedin: https://www.linkedin.com/in/emiliano-quenum-50973a253


pacman::p_load(plotly, dplyr, stringr, tidyverse, readODS)


#Fonction d'importation de fichier xlsx, ods, csv

import_file = function(my_path) {
  if (grepl("\\.xlsx$", my_path)) {
    return(openxlsx::read.xlsx(my_path))
    
  } else{
    if (grepl("\\.ods$", my_path)) {
      return(read_ods(my_path))
      
    } else{
      return(read_csv(file = my_path))
    }
  }
}

# Fonction permetant de vérifié et de retourné pour un liste de chaine de caractère ceux qui contiennent des mots particulier
mots_cle_inside =  function(chaine, vecteur) {
  all(stri_detect_fixed(chaine, vecteur))
}


#Fonction de nétoyage de base de données par suppression de données manquantes (OMIT NA DATA)

clean_dt = function(ma_data) {
  return(ma_data %>%
           select(where(~ all(!is.na(
             .
           )))))
}

# Fonction permettant de retrouver des colonnes à partir de mots clés
colonne_match = function(data, vecteurs_mots, somme = TRUE) {
  inside_cols_crois = sapply(names(data), mots_cle_inside, vecteur = vecteurs_mots)
  
  if (somme == TRUE) {
    return(data %>%
             select(names(data)[inside_cols_crois]) %>%
             sum())
  } else{
    return(data %>%
             select(names(data)[inside_cols_crois]))
  }
}



# Fonction permetant de vérifié et de retourné pour un liste de chaine de caractère ceux qui contiennent des mots particulier

mots_cle_inside_no_case =  function(chaine, mot_cle) {
  all(grepl(mot_cle, chaine, ignore.case = TRUE))
}


#Fonction permettant de généralisé la fonction précédente à plusieur listes à la fois avec sapply

name_feuille_match = function(my_vector, mot_cle, name_or_no = TRUE) {
  inside_cols_crois = sapply(my_vector, mots_cle_inside_no_case, mot_cle = mot_cle)
  if (name_or_no) {
    return(names(which(inside_cols_crois)))
  } else{
    return(which(inside_cols_crois))
  }
}


# Fonction permettant de lire lire les  fichiers ".ods" et en particulier les feuilles de décembre relative au bases des données des effectif


BDD_effectif_creator = function(file_path_effectif,
                                colnames_extract,
                                mot_cle = "cembre",
                                DDI,
                                current_year) {
  feuilles = name_feuille_match(my_vector = ods_sheets(file_path_effectif),
                                mot_cle =  mot_cle)
  
  initiale_traitement = function(nbr_feuille) {
    return(
      read_ods(
        file_path_effectif,
        sheet = feuilles[nbr_feuille],
        col_names = TRUE,
        na =  c("/", "", NA)
      ) %>%
        select(
          colnames_extract,
          matches("(?i)Date.*naissance"),
          matches("(?i)^Direction.*principale$")
        ) %>%
        filter(Physique != 0 &
                 if_any(
                   matches("(?i)^Direction.*principale$"),
                   ~ str_starts(., DDI)
                 )) %>%
        rename(birthday = matches("(?i)Date.*naissance"))  %>%
        mutate(
          birthday = as.Date(birthday, format = "%d/%m/%Y"),
          Age = current_year - as.integer(format(birthday, "%Y"))
        ) %>%
        mutate(across(c("Age", "ETP"), as.numeric), across(
          -c("Age", "ETP", "birthday"), as.factor
        )) %>%
        select(-c(
          birthday,
          matches("(?i)^Direction.*principale$"),
          matches("(?i)^Physique")
        ))
    )
  }
  
  if (length(feuilles) == 0) {
    effectif_DDI = read_ods(
      file_path_effectif,
      sheet = ods_sheets(file_path_effectif)[1],
      col_names = TRUE,
      na =  c("/", "", NA)
    )
    
  } else{
    if (length(feuilles) == 1) {
      effectif_DDI =  initiale_traitement(nbr_feuille = 1)
      
      
    } else{
      effectif_DDI_1 = initiale_traitement(nbr_feuille = 1)
      effectif_DDI_2 = initiale_traitement(nbr_feuille = 2)
      effectif_DDI =   bind_rows(effectif_DDI_1, effectif_DDI_2)
      
    }
    
  }
  return(effectif_DDI)
  
}


# Fonction de groupage et agrégation dynamique
groupage_vars <- function(data,
                          key,
                          vecteur_vars = NULL,
                          agg_func = "count",
                          func_name = "count") {
  # Vérifier si vecteur_vars est NULL
  if (is.null(vecteur_vars)) {
    if (agg_func == "count") {
      # Groupage et comptage par key seulement
      key_group <- data %>%
        group_by(across(all_of(key))) %>%
        count(name = func_name)
    }
  } else {
    # Fonction à appliquer dynamiquement
    agg_fun <- switch(
      agg_func,
      "sum" = ~ sum(.x, na.rm = TRUE),
      "mean" = ~ mean(.x, na.rm = TRUE),
      "count" = ~ n(),
    )
    
    key_group <- data %>%
      group_by(across(all_of(key))) %>%
      # summarise(across(all_of(vecteur_vars), agg_fun, .names = "{agg_func}_{col}"))
      
      summarise(across(all_of(vecteur_vars), agg_fun, .names = func_name))
    
    
  }
  
  return(key_group)
}



#Fonction de visualisation des camemberts des tableaux ventilés de dimension: dim= n*1

cam = function(titre = "Graphique en Camembert..",
               ma_data,
               type_aff = "Valeur",
               couleur,
               noms_legende = gsub("\\.", " ", sub("[0-9]*- ", "", ma_data$names))) {
  # Créer le graphique en camembert avec les valeurs affichées
  graph_plot_cam = ma_data %>%
    plot_ly(
      labels = ~ noms_legende,
      values = ~ Effectif,
      type = 'pie',
      textinfo = 'text',
      text = ~ ifelse(
        type_aff == "Valeur",
        return(Effectif),
        ifelse(type_aff == "Pourcentage", return(paste(
          round(100 * Effectif / sum(Effectif)), "%"
        )), return(paste(
          Effectif, "\n", round(100 * Effectif / sum(Effectif)), "%"
        )))
      ),
      marker = list(colors = couleur, line = list(
        color = 'white', width = 2
      ))
    )
  
  graph_plot_cam = graph_plot_cam %>%
    layout(
      title = titre,
      legend = list(
        orientation = 'h',
        x = 0.5,
        y = -0.3,
        xanchor = 'center',
        yanchor = 'top'
      ),
      margin = list(
        l = 50,
        r = 50,
        t = 50,
        b = 100
      )
    )  %>%
    config(displayModeBar = T) %>%
    config(displaylogo = FALSE) %>%
    config(modeBarButtonsToRemove = list('hoverClosestPie')) %>%
    
    config(toImageButtonOptions = list(format = "png"))
  
  return(graph_plot_cam)
}


#Fonction de visualisation des histogrammes des tableaux ventilés de dimension : dim=  n*1

histo = function(titre,
                 ma_data,
                 type_aff = "Valeur",
                 couleur,
                 noms_legende =  ma_data$names,
                 axe_1,
                 axe_2) {
  # Créer le graphique en camembert avec les valeurs affichées
  graph_plot_hist = ma_data %>%
    plot_ly(
      x = ~ noms_legende,
      y = ~ if (type_aff == "Valeur" |
                type_aff == "Valeur & Pourcentage") {
        Effectif
      } else{
        round(100 * Effectif / sum(Effectif), 1)
      },
      
      type = 'bar',
      marker = list(color = couleur),
      textposition = 'auto',
      text = ~ ifelse(
        type_aff == "Valeur",
        return(Effectif),
        ifelse(type_aff == "Pourcentage", return(paste(
          round(100 * Effectif / sum(Effectif), 1), "%"
        )), return(paste(
          Effectif, "\n", round(100 * Effectif / sum(Effectif), 1), "%"
        )))
      )
    ) %>%
    layout(
      title = titre,
      xaxis = list(title = axe_1) ,
      yaxis = list(title = axe_2)
    )
  
  graph_plot_hist = graph_plot_hist %>%
    config(displaylogo = FALSE)  %>%
    config(displayModeBar = TRUE)  %>%
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
  
  return(graph_plot_hist)
}


#Fonction de visualisation des histogrammes des tableaux croisés de dimension: dim=  n*2
histo_crois_2 = function(titre,
                         ma_data,
                         type_aff = "Valeur",
                         couleur,
                         noms_legende =  ma_data$names,
                         val_1 = ma_data[, 1],
                         val_2 = ma_data[, 2],
                         axe_1,
                         axe_2,
                         sup = 'group') {
  # Créer le graphique en camembert avec les valeurs affichées
  graph_plot_hist = ma_data %>%
    plot_ly(
      x = ~ noms_legende,
      y = ~ if (type_aff == "Valeur" |
                type_aff == "Valeur & Pourcentage") {
        val_1
      } else{
        round(100 * val_1 / ma_data$Total)
      },
      type = 'bar',
      marker = list(color = couleur[1]),
      name = names(ma_data)[1],
      textposition = 'auto',
      
      text = ~ ifelse(
        type_aff == "Valeur" ,
        return(val_1),
        ifelse(type_aff == "Pourcentage", return(paste(
          round(100 * val_1 / ma_data$Total), "%"
        )), return(paste(
          val_1, "\n", round(100 * val_1 / ma_data$Total), "%"
        )))
      )
      
      
    ) %>% add_trace(
      y =  ~ if (type_aff == "Valeur" |
                 type_aff == "Valeur & Pourcentage") {
        val_2
      } else{
        round(100 * val_2 / ma_data$Total)
      },
      name = names(ma_data)[2] ,
      marker = list(color = couleur[2]),
      textposition = 'auto',
      
      text = ~ ifelse(
        type_aff == "Valeur",
        return(val_2),
        ifelse(type_aff == "Pourcentage", return(paste(
          round(100 * val_2 / ma_data$Total), "%"
        )), return(paste(
          val_2, "\n", round(100 * val_2 /  ma_data$Total), "%"
        )))
      )
    ) %>%
    layout(
      title = titre,
      xaxis = list(title = axe_1) ,
      yaxis = list(title = axe_2),
      barmode = sup
    )
  
  graph_plot_hist = graph_plot_hist %>%
    config(displaylogo = FALSE)  %>%
    config(displayModeBar = TRUE)  %>%
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
  
  return(graph_plot_hist)
}

# Fonction d'affichage des représentation en histogramme des tableaux de dimension dim= n*3

histo_crois_3 = function(titre,
                         ma_data,
                         type_aff = "Valeur",
                         couleur,
                         noms_legende =  ma_data$names,
                         val_1 = ma_data[, 1],
                         val_2 = ma_data[, 2],
                         val_3 = ma_data[, 3],
                         axe_1,
                         axe_2,
                         sup = 'group') {
  # Créer le graphique en camembert avec les valeurs affichées
  graph_plot_hist = ma_data %>%
    plot_ly(
      x = ~ noms_legende,
      y = ~ if (type_aff == "Valeur" |
                type_aff == "Valeur & Pourcentage") {
        val_1
      } else{
        round(100 * val_1 / ma_data$Total)
      },
      type = 'bar',
      marker = list(color = couleur[1]),
      name = names(ma_data)[1],
      textposition = 'auto',
      
      text = ~ ifelse(
        type_aff == "Valeur",
        return(val_1),
        ifelse(type_aff == "Pourcentage", return(paste(
          round(100 * val_1 / ma_data$Total), "%"
        )), return(paste(
          val_1, "\n", round(100 * val_1 / ma_data$Total), "%"
        )))
      )
      
    ) %>%
    add_trace(
      y =  ~ if (type_aff == "Valeur" |
                 type_aff == "Valeur & Pourcentage") {
        val_2
      } else{
        round(100 * val_2 / ma_data$Total)
      },
      name = names(ma_data)[2] ,
      marker = list(color = couleur[2]),
      textposition = 'auto',
      
      text = ~ ifelse(
        type_aff == "Valeur",
        return(val_2),
        ifelse(type_aff == "Pourcentage", return(paste(
          round(100 * val_2 / ma_data$Total), "%"
        )), return(paste(
          val_2, "\n", round(100 * val_2 / ma_data$Total), "%"
        )))
      )
      
    ) %>%
    add_trace(
      y =  ~ if (type_aff == "Valeur" |
                 type_aff == "Valeur & Pourcentage") {
        val_3
      } else{
        round(100 * val_3 / ma_data$Total)
      },
      name = names(ma_data)[3] ,
      marker = list(color = couleur[3]),
      textposition = 'auto',
      
      text = ~ ifelse(
        type_aff == "Valeur",
        return(val_3),
        ifelse(type_aff == "Pourcentage", return(paste(
          round(100 * val_3 / ma_data$Total), "%"
        )), return(paste(
          val_3, "\n", round(100 * val_3 / ma_data$Total), "%"
        )))
      )
    ) %>%
    layout(
      title = titre,
      xaxis = list(title = axe_1) ,
      yaxis = list(title = axe_2),
      barmode = sup
    )
  
  graph_plot_hist = graph_plot_hist %>%
    config(displaylogo = FALSE)  %>%
    config(displayModeBar = TRUE)  %>%
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
  
  return(graph_plot_hist)
}

# Fonction d'affichage des  tableaux de dimension dim= n*n

table_classic = function(ma_table, off_name = F) {
  return(
    DT::datatable(
      ma_table %>% rename_with(~ str_replace_all(., "\\_", " "), everything()),
      class = "cell-border hover order-column",
      extensions = 'Buttons',
      rownames = if (off_name ==
                     F) {
        gsub("\\.", " ", sub("\\.", " ", rownames(ma_table)))
      } else{
        NULL
      },
      # Remplacement des points par des tiret "-" dans le nom des lignes
      options = list(
        dom = 'Blfrtp',
        buttons = c('copy', 'excel', 'csv'),
        #Ajout option d'exportation des données dans différents formats
        paging = F,
        searching = F,
        ordering = F,
        
        initComplete = JS(
          # Code javascript pour l'ajustement des couleurs des colonnes de la table
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': 'gray', 'color': '#fff', 'font-size': '10px'});",
          "$('table.dataTable').css({'font-size': '10px','text-align':'center', 'font-family': 'roman'});",
          "$('table.dataTable th, table.dataTable td').css({'text-align': 'center', 'vertical-align': 'middle'});",
          "$('table.dataTable th').css({'white-space': 'nowrap'});",
          "}"
        )
      )
    )
  )
}


# Fonction d'affichage des  tableaux de dimension dim= n*1

table_manuelle = function(ma_table) {
  return(
    DT::datatable(
      ma_table %>% rename_with(~ str_replace_all(., "\\_", " "), everything()) ,
      class = "cell-border hover order-column",
      extensions = 'Buttons',
      rownames = gsub("\\.", " ", sub("[0-9]*- ", "", rownames(ma_table))),
      options = list(
        dom = 'Blfrp',
        buttons = c('copy', 'excel', 'csv'),
        #Ajout option d'exportation des données dans différents formats
        paging = F,
        searching = F,
        ordering = F,
        
        initComplete = JS(
          # Code javascript pour l'ajustement des couleurs des colonnes de la table
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': 'gray', 'color': '#fff', 'font-size': '10px'});",
          "$('table.dataTable').css({'font-size': '10px','text-align':'center', 'font-family': 'roman'});",
          "$('table.dataTable th, table.dataTable td').css({'text-align': 'center', 'vertical-align': 'middle'});",
          "$('table.dataTable th').css({'white-space': 'nowrap'});",
          "}"
        )
      )
    )
  )
}

ma_palette_de_couleur <- c(
  "aigue-marine" = "aquamarine",
  "aigue-marine moyenne" = "mediumaquamarine",
  "amande blanchi" = "blanchedalmond",
  "blanc antique" = "antiquewhite",
  "azur" = "azure",
  
  "beige" = "beige",
  "blé" = "wheat",
  "blé pâle" = "palegoldenrod",
  "bleu" = "blue",
  "bleu moyen" = "mediumblue",
  "bleu acier" = "steelblue",
  "bleu acier clair" = "lightsteelblue",
  "bleu acier foncé" = "darkslategray",
  "bleu ardoise" = "slateblue",
  "bleu ardoise foncé" = "darkslateblue",
  "bleu ardoise moyen" = "mediumslateblue",
  "bleu quenum" = "cadetblue",
  "bleu ciel profond" = "deepskyblue",
  "bleu clair" = "lightblue",
  "bleu dodger" = "dodgerblue",
  "bleu ciel" = "skyblue",
  "bleu glacial" = "aliceblue",
  "bleu marine" = "navy",
  "bleu pâle" = "paleturquoise",
  "bleu foncé" = "darkblue",
  "bleu royal" = "royalblue",
  "bordeaux" = "maroon",
  "bleu violet" = "blueviolet",
  "bleuet" = "cornflowerblue",
  "bois de santal" = "burlywood",
  "brun" = "sienna",
  "brun clair" = "tan",
  "brun sable" = "sandybrown",
  "brique de feu" = "firebrick",
  
  "chartreuse" = "chartreuse",
  "cyan clair" = "lightcyan",
  "cyan foncé" = "darkcyan",
  "chocolat" = "chocolate",
  "coquillage" = "seashell",
  "corail" = "coral",
  "corail clair" = "lightcoral",
  
  "gris" = "gray",
  "gris ardoise" = "slategray",
  "gris clair" = "lightgrey",
  "gris james" = "dimgray",
  "gris ardoise clair" = "lightslategray",
  
  "jaune clair" = "lightyellow",
  "jaune vert" = "yellowgreen",
  
  "kaki" = "khaki",
  "kaki foncé" = "darkkhaki",
  
  "jaune" = "yellow",
  
  "lavende" = "lavender",
  "lavande foncée" = "darkviolet",
  "magenta" = "magenta",
  "marron" = "brown",
  "marron foncé" = "saddlebrown",
  "marron rosé" = "rosybrown",
  
  "noir" = "black",
  
  "orchidée" = "orchid",
  "olive terne" = "olivedrab",
  "or" = "gold",
  "or foncé" = "darkgoldenrod",
  "orangé" = "darkorange",
  "orange" = "orange",
  
  "pêche" = "peachpuff",
  "pérou" = "peru",
  "rose" = "pink",
  "prune" = "plum",
  
  "rose profond" = "deeppink",
  "rose vif" = "hotpink",
  "rose saumon" = "salmon",
  "rose clair" = "lightpink",
  "rose brumeux" = "mistyrose",
  "rouge" = "red",
  "rouge tomate" = "tomato",
  "rouge violet moyen" = "mediumvioletred",
  "rouge orangé" = "orangered",
  "rougeur lavande" = "lavenderblush",
  
  "saumon clair" = "lightsalmon",
  "sarcelle foncé" = "darkcyan",
  
  "Turquoise" = "cyan",
  "turquoise moyen" = "mediumturquoise",
  
  "vert gazon" = "lawngreen",
  "violet" = "purple",
  "violet moyen" = "mediumorchid",
  "violet pâle" = "palevioletred",
  "vert clair" = "lightgreen",
  "vert mer clair" = "lightseagreen",
  "vert printemps" = "springgreen",
  "vert mer" = "seagreen",
  "vert mer moyen" = "mediumseagreen",
  "vert citron" = "limegreen",
  "vert olive foncé" = "darkolivegreen",
  "vert forêt" = "forestgreen"
)