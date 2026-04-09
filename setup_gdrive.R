# ==============================================================
# SCRIPT DE AUTENTICACION UNICA PARA TELEMETRIA EN GOOGLE DRIVE
# ==============================================================

# Instalar googledrive si no está instalado
if (!require(googledrive)) install.packages("googledrive")

# 1. Reemplaza el correo de abajo con tu correo de Google real asociado a la nube
# donde quieres guardar la telemetría.
tu_correo <- "nahuel.roel@cienciassociales.edu.uy"

# 2. Configura que las credenciales se guarden en la carpeta oculta (.secrets)
# IMPORTANTE: al desplegar a shinyapps.io, esta carpeta DEBE ir incluida
options(gargle_oauth_cache = ".secrets")

# 3. Dispara la autenticación
# Se te abrirá una pestaña del navegador pidiendo permiso
# ¡Importante! 'scopes' fuerza a que pida permisos completos de escritura
googledrive::drive_auth(
    cache = ".secrets",
    email = tu_correo,
    scopes = "https://www.googleapis.com/auth/drive"
)

cat("\n¡Autenticación completada!\nVerifica que se haya creado una carpeta '.secrets' en tu proyecto.\nSi es así, ya puedes correr y publicar app.R\n")
