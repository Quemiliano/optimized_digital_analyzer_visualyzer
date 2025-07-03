pacman::p_load(shiny, utils)
app_dir = "C:/Users/PC-Internet/Documents/RSU/apk_rsu"
# Lancez l'application Shiny et récupérez l'URL
url <- shiny::runApp(app_dir, launch.browser = TRUE)
message("Application launched at: ", url)

# Ouvrez l'URL dans le navigateur par défaut (nécessite le package 'utils')
utils::browseURL(url)
