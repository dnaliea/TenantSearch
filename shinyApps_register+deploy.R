#info from here: https://www.shinyapps.io/admin/#/dashboard

devtools::install_github('rstudio/shinyapps')
library(shinyapps)

shinyapps::setAccountInfo(name='flffy',
                          token='8639FE81E79E304552E0922D57323465',
                          secret='hx7ymYJ3mWyiBQCE2pJYlql6fPm5YPV6rQcZdQNd')


shinyapps::deployApp('path/to/your/app')

