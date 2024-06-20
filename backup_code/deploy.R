library(rsconnect)

# official version ----

deployApp(account = 'psrcwa',
         appName = 'dashboard-name', # name of your app in the url https://psrcwa.shinyapps.io/dashboard-name
         appTitle = 'Dashboard Window Title') # name that will be displayed in the window title