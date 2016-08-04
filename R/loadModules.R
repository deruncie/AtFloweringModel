# a <- Rcpp::setRcppClass("Environ")
# a <- Rcpp::setRcppClass("Plant")
# a <- Rcpp::setRcppClass("Wilczek_Plant")

Rcpp::loadModule('class_Environ',TRUE)
# Rcpp::loadModule('class_Plant',TRUE)
# Rcpp::loadModule('class_New_Plant',TRUE)
Rcpp::loadModule('class_Wilczek_Plant',TRUE)
Rcpp::loadModule('class_New_Plant',TRUE)
# Rcpp::loadModule('class_Population',TRUE)

