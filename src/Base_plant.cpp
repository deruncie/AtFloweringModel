#include <Rcpp.h>
using namespace Rcpp;

// #include "Environ_class.h"
#include "Base_Plant_class.h"

void Base_Plant::set_genotype_info(NumericMatrix param_ranges_,NumericMatrix design_matrix_genotype_,List param_transformations_){
  param_ranges = clone(param_ranges_);
  design_matrix_genotype = clone(design_matrix_genotype_);
  param_transformations = clone(param_transformations_);
}

NumericVector Base_Plant::update_coefs(NumericVector new_coefs){
  Function Update_coefs_R("Update_coefs_genotype");
  List result = Update_coefs_R(new_coefs,params,design_matrix_genotype,param_ranges,param_transformations);
  NumericVector new_params = as<NumericVector>(result["new_params"]);
  penalty = as<double>(result["penalty"]);

  if(new_params.length() > 0) update_params(new_params);

  // Rcout << penalty << std::endl;
  return new_params;
}

void Base_Plant::update_Signal_threshold(double thresh) {
  params["Signal_threshold"] = thresh;
  developmental_state = 0;
  bolting_day = 0;
  transition_day = 0;
}
double Base_Plant::TT_fun(NumericVector temps, NumericVector dts){
  Rcout << "base_TT\n";
  return sum(pmax(temps-params["T_base"],0) * dts);
}
double Base_Plant::PTT_fun(double sumTT_night, double sumTT_day){
  return params["Pnight"]*sumTT_night + params["Pday"]*sumTT_day;
}
double Base_Plant::Vern_fun(double old_state, NumericVector temps, NumericVector dts){
  if(old_state >= 1) return 1;
  double Vsat = params["Vsat"];
  double T_vmin = params["T_vmin"];
  double T_vmax = params["T_vmax"];
  double k = params["k"];
  double w = params["w"];
  double xi = params["xi"];
  double F_b = params["F_b"];

  double Ve = 0;

  for(int i = 0; i < temps.length();i++){
    if(temps[i] > T_vmin && temps[i] < T_vmax) {
      Ve += exp(k) * pow(temps[i] - T_vmin,w) * pow(T_vmax - temps[i],xi) * dts[i];
    }
  }
  return std::min(old_state + (Ve/Vsat*(1-F_b)),1.0);
}
double Base_Plant::Signal_fun(double repression, double dayl){
  double D_LD = params["D_LD"];
  double CLDL = params["CLDL"];
  double CSDL = params["CSDL"];
  double D = 0;

  if(dayl > CLDL){
    D = D_LD;
  }
  else if(dayl > CSDL){
    D = (dayl - CSDL) * (D_LD - D)/(CLDL - CSDL);
  }
  return repression * D;
}
void Base_Plant::develop_n(int n_days){
  for(int i = 0; i < n_days; i++){
    develop_day();
    if(age >= env.numDays()) continue;
  }
}

int Base_Plant::get_predicted_bolting_day(){
  if(bolting_day > 0) return bolting_day;
  return predict_bolting();
}

void Base_Plant::add_bolting_days(IntegerVector days){
  observed_bolting_days = days;
}

int Base_Plant::predict_bolting(){
  // if development already simulated sufficiently, search for date when threshold crossed
  // else, simulate until threshold is crossed
  // if threshold never crossed, return last_day + 1

  // check_plant();

  while(age < env.numDays() && developmental_state < 2) develop_day();
  if(developmental_state == 2) return bolting_day;
  // if no bolting by end of env:
  return env.numDays()+1;
}

double Base_Plant::get_predicted_bolting_PTT(){
  if(developmental_state < 2) predict_bolting();
  if(developmental_state == 2) return cumPTT[bolting_day-1];
  return cumPTT.back() + PTT.back();
}

NumericVector Base_Plant::get_observed_bolting_PTTs(){
  if(max(observed_bolting_days) > age){
    develop_n(max(observed_bolting_days - age));
    // Rcout << age << ", " << observed_bolting_days << std::endl;
  }
  NumericVector PTT_out(observed_bolting_days.length());
  for(int i = 0; i < observed_bolting_days.length(); i++){
    if(observed_bolting_days[i] <= cumPTT.size()){
      PTT_out[i] = cumPTT[observed_bolting_days[i]-1];
    }
    else {
      PTT_out[i] = NA_REAL;
    }
  }
  return PTT_out;
}
