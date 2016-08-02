// #include <Rcpp.h>
#include <math.h>
#include <RcppArmadillo.h>
using namespace Rcpp;

#include "Environ_class.h"

class Plant {
public:
  Plant(String id_, String gen_, String environ_,NumericVector params_, NumericMatrix env_):
          id(id_),gen(gen_),environ(environ_),
          age(0),transition_day(0),bolting_day(0),penalty(0) {params = clone(params_);env = clone(env_); }
  String id, gen, environ;
  void update_params(NumericVector);  // directly change parameters
  void set_genotype_info(NumericMatrix,NumericMatrix,List);
  NumericVector update_coefs(NumericVector);
  void update_Signal_threshold(double thresh) {params["Signal_threshold"] = thresh;bolting_day = 0;transition_day=0;}
  double get_penalty() {return penalty;}
  void update_days_to_calculate(IntegerVector);
  void add_bolting_days(IntegerVector);
  void develop_day();
  void develop_n(int);
  int predict_flowering();
  int predict_bolting();
  NumericVector get_params() {return params;}
  int get_predicted_bolting_day();
  double get_predicted_bolting_PTT();
  NumericVector get_observed_bolting_PTTs();
  NumericVector get_numLeaves() {return wrap(TT);}
  NumericVector get_size() {return wrap(PTT);}
  NumericVector get_Vern() {return wrap(Vern);}
  NumericVector get_FT_signal() {return wrap(FT_signal);}
  NumericVector get_Total_signal() {return wrap(Total_signal);}
  double TT_fun(NumericVector,NumericVector);
  double PTT_fun(double,double);
  double Vern_fun(double,NumericVector,NumericVector);
  double Signal_fun(double,double);
protected:
  NumericVector params;
  NumericMatrix param_ranges,design_matrix_genotype;
  List param_transformations;
  std::set<std::string> TT_params = {"T_base","Pnight","Pday"};
  std::set<std::string> PTT_params = {};
  std::set<std::string> Vern_params = {"F_b","Vsat","k","w","xi","T_vmin","T_vmax"};
  std::set<std::string> FT_params = {"D_LD","CSDL","CLDL"};
  std::set<std::string> Total_signal_params = {"D_SD"};
  std::set<std::string> Transition_params = {"Signal_threshold"};
  int age;
  int transition_day;
  int bolting_day;
  double penalty;
  IntegerVector observed_bolting_days;
  Environ env;
  void check_plant();
  std::vector<double> TT; // thermal time history by day
  std::vector<double> PTT; // PTU time history by day
  std::vector<double> Vern; // Vern time history by day
  std::vector<double> FT_signal; // FT history by day
  std::vector<double> Total_signal; // cumPTU history by day
  NumericVector cumTT;
  NumericVector cumPTT;
};

void Plant::set_genotype_info(NumericMatrix param_ranges_,NumericMatrix design_matrix_genotype_,List param_transformations_){
  param_ranges = clone(param_ranges_);
  design_matrix_genotype = clone(design_matrix_genotype_);
  param_transformations = clone(param_transformations_);
}

NumericVector Plant::update_coefs(NumericVector new_coefs){
  Function Update_coefs_R("Update_coefs_genotype");
  List result = Update_coefs_R(new_coefs,params,design_matrix_genotype,param_ranges,param_transformations);
  NumericVector new_params = as<NumericVector>(result["new_params"]);
  penalty = as<double>(result["penalty"]);

  if(new_params.length() > 0) update_params(new_params);

  // Rcout << penalty << std::endl;
  return new_params;
}
double Plant::TT_fun(NumericVector temps, NumericVector dts){
  return sum(pmax(temps-params["T_base"],0) * dts);
}
double Plant::PTT_fun(double sumTT, double dayl){
  // dayl already in sumTT(day)
  return sumTT;
}
double Plant::Vern_fun(double old_state, NumericVector temps, NumericVector dts){
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
double Plant::Signal_fun(double repression, double dayl){
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

void Plant::check_plant() {
  // after updating parameters, goes through to check which summaries need to be re-set

  // PTT depends on TT. So if TT is reset, so is PTT
  if(TT.size() == 0) {
    PTT.clear();
    cumPTT = NumericVector(0);
  }

  // FT depends on PTT and Vern
  if(FT_signal.size() > std::min(PTT.size(),Vern.size())) {
    FT_signal.clear();
  }

  // Total signal depends on PTT and FT_signal
  if(Total_signal.size() > PTT.size() ||
     Total_signal.size() > Vern.size() ||
     Total_signal.size() > FT_signal.size()) {
    Total_signal.clear();
  }

}

void Plant::develop_day(){
  check_plant();
  if(age >= env.numDays()) return;
  age++;
  // always 1st night, then day.
  // calc TT for day
  if(TT.size() < age){  // only do calculation if TT hasn't already been calculated.
    double TT_night = params["Pnight"]*TT_fun(env.get_GrndTmp_Night(age),env.get_TimeSteps_Night(age));
    double TT_day   = params["Pday"]  *TT_fun(env.get_GrndTmp_Day(age),env.get_TimeSteps_Day(age));
    TT.push_back(TT_night + TT_day);
    if(TT.size() != age) Rcout << "TT wrong length\n";

    // add leaves
    PTT.push_back(PTT_fun(TT_day,env.get_Daylength(age)));
    if(PTT.size() != age) Rcout << "PTT wrong length\n";
  }

      // update meristem age
  // if vegetative:
  // calc vern for day
  if(Vern.size() < age){
    double current_vern = params["F_b"];
    if(Vern.size() > 0) current_vern = Vern[age-2];
    // Vernalize during night
    current_vern = Vern_fun(current_vern,env.get_GrndTmp_Night(age),env.get_TimeSteps_Night(age));
    // Vernalize during next day
    current_vern = Vern_fun(current_vern,env.get_GrndTmp_Day(age),env.get_TimeSteps_Day(age));
    Vern.push_back(current_vern);
    if(Vern.size() != age) Rcout << "Vern wrong length\n";
  }

  // calc FT for day
  if(FT_signal.size() < age){
    FT_signal.push_back(Signal_fun(Vern[age-1]*PTT[age-1], env.get_Daylength(age)));
    if(FT_signal.size() != age) Rcout << "FT_signal wrong length\n";
  }

  // calc total signal
  double new_Total_signal = 0;
  if(Total_signal.size() > 0) new_Total_signal = Total_signal.back();
  if(Total_signal.size() < age) {
    new_Total_signal += Vern[age-1]*PTT[age-1]*params["D_SD"];
    new_Total_signal += FT_signal[age-1]*(1-params["D_SD"]);
    Total_signal.push_back(new_Total_signal);
    if(Total_signal.size() != age) Rcout << "Total_signal wrong length " << age << " " << Total_signal.size() << std::endl;
  }
}
void Plant::develop_n(int n_days){
  for(int i = 0; i < n_days; i++){
    develop_day();
    if(age >= env.numDays()) continue;
  }
}

void Plant::update_params(NumericVector new_p){
  age = 0;
  transition_day = 0;
  bolting_day = 0;

  CharacterVector new_p_names = new_p.names();
  for(int i = 0; i < new_p.length();i++){
    std::string name = as<std::string>(new_p_names[i]);
    if(TT_params.find(name) != TT_params.end()) {
      // Rcout << "TT\n";
      TT.clear();
    }
    if(PTT_params.find(name) != PTT_params.end()) {
      // Rcout << "PTT\n";
      PTT.clear();
    }
    if(Vern_params.find(name) != Vern_params.end()) {
      // Rcout << "Vern\n";
      Vern.clear();
    }
    if(FT_params.find(name) != FT_params.end()) {
      // Rcout << "FT\n";
      FT_signal.clear();
    }
    if(Total_signal_params.find(name) != Total_signal_params.end()) {
      // Rcout << "Total_signal\n";
      Total_signal.clear();
    }
    params[name] = new_p[i];
  }
  check_plant();
}

int Plant::get_predicted_bolting_day(){
  if(bolting_day > 0) return bolting_day;
  return predict_bolting();
}

int Plant::predict_bolting(){
  // if development already simulated sufficiently, search for date when threshold crossed
  // else, simulate until threshold is crossed
  // if threshold never crossed, return last_day + 1

  // check_plant();

  double Signal_threshold = params["Signal_threshold"];
  if(age == 0) develop_day();
  // Rcout << Total_signal.back() << " " << Signal_threshold << " " << age << std::endl;
  if(age > 0 & Total_signal.back() > Signal_threshold) {
    int i = 0;
    while(Total_signal[i] < Signal_threshold){
      i++;
    }
    transition_day = i+1;
    bolting_day = transition_day;
    return bolting_day;
  }
  while(Total_signal.back() < Signal_threshold && age < env.numDays()) {
    develop_day();
  }
  if(Total_signal.back() >= Signal_threshold) {
    transition_day = age;
    bolting_day = transition_day;
    return bolting_day;
  }
  transition_day = age + 1;
  bolting_day = transition_day;
  return bolting_day;
}
double Plant::get_predicted_bolting_PTT(){
  if(bolting_day == 0) predict_bolting();
  if(cumPTT.length() < PTT.size()){
    cumPTT = NumericVector(PTT.size());
    std::partial_sum(PTT.begin(),PTT.end(),cumPTT.begin());
  }
  if(bolting_day <= env.numDays()) return cumPTT[bolting_day-1];
  return cumPTT[bolting_day-2] + PTT.back();
}
void Plant::add_bolting_days(IntegerVector days){
  observed_bolting_days = days;
}
NumericVector Plant::get_observed_bolting_PTTs(){
  if(max(observed_bolting_days) > age){
    develop_n(max(observed_bolting_days - age));
    // Rcout << age << ", " << observed_bolting_days << std::endl;
  }
  if(cumPTT.length() < PTT.size()){
    cumPTT = NumericVector(PTT.size());
    std::partial_sum(PTT.begin(),PTT.end(),cumPTT.begin());
  }
  NumericVector PTT_out(observed_bolting_days.length());
  for(int i = 0; i < observed_bolting_days.length(); i++){
    if(observed_bolting_days[i] <= cumPTT.length()){
      PTT_out[i] = cumPTT[observed_bolting_days[i]-1];
    }
    else {
      PTT_out[i] = NA_REAL;
    }
  }
  return PTT_out;
}

RCPP_MODULE(class_Plant) {
  class_<Plant>("Plant")
    .constructor<String,String,String,NumericVector, NumericMatrix>()
    // .constructor<String,String,String,NumericVector, NumericMatrix,NumericMatrix,List,NumericMatrix>()
    // .constructor<String,String,String,NumericVector, NumericMatrix,NumericMatrix,List,NumericMatrix>()
     .field("id",&Plant::id)
     .field("gen",&Plant::gen)
     .field("environ",&Plant::environ)
    .method("set_genotype_info",&Plant::set_genotype_info)
    .method("get_penalty",&Plant::get_penalty)
    .method("get_params",&Plant::get_params)
    .method("develop_n",&Plant::develop_n)
    .method("get_numLeaves",&Plant::get_numLeaves)
    .method("get_size",&Plant::get_size)
    .method("get_Vern",&Plant::get_Vern)
    .method("get_FT_signal",&Plant::get_FT_signal)
    .method("get_Total_signal",&Plant::get_Total_signal)
    .method("update_params",&Plant::update_params)
    .method("update_coefs",&Plant::update_coefs)
    .method("TT_fun",&Plant::TT_fun)
    .method("PTT_fun",&Plant::PTT_fun)
    .method("Vern_fun",&Plant::Vern_fun)
    .method("Signal_fun",&Plant::Signal_fun)
    .method("get_predicted_bolting_day",&Plant::get_predicted_bolting_day)
    .method("update_Signal_threshold",&Plant::update_Signal_threshold)
    .method("get_predicted_bolting_PTT",&Plant::get_predicted_bolting_PTT)
    .method("add_bolting_days",&Plant::add_bolting_days)
    .method("get_observed_bolting_PTTs",&Plant::get_observed_bolting_PTTs)
  ;
}


/*** R
# load('/Users/der7/Documents/Arabidopsis/Compendium_data/full_environ_data_list.Robj')
# env = environ_data[[1]]
# save(env,file='test_env.RData')
load('test_env.RData')
a = new(Environ,as.matrix(env))

params = list(
  CSDL = 8,
  CLDL = 14,

  D_LD = 1,
  D_SD = 0.626,

  T_base = 3,

  k=-5.17485563597551,	#same vernalization model
  w=2.22568888596679,
  xi=0.995904471970916,
  T_vmin = -3.5,
  T_vmax = 6,
  Vsat = 960,

  Pday = 1,
  Pnight = 0,

  F_b = 0.4,
  Signal_threshold = 20000
)

design_matrix_genotype = matrix(c(1,0,0,0,1,0,0,0,1,0,0,1),nr=3)
rownames(design_matrix_genotype) = c('CSDL','CLDL','T_base')
colnames(design_matrix_genotype) = c('A','B','C','D')
new_coefs = c(A=6.5,T_vmin = -3.6, B = 34,C = 1,D=3)
param_ranges = t(data.frame(CSDL = c(0,24),CLDL = c(0,24),D_LD = c(0,1),D_SD = c(0,1),T_base = c(-Inf,Inf),
                 k = c(-Inf,Inf), w = c(-Inf,Inf), xi = c(-Inf,Inf), T_vmin = c(-Inf,Inf), T_vmax = c(-Inf,Inf), Vsat = c(0,Inf),
                 Pday = c(0,1),Pnight = c(0,1),F_b = c(0,1),Signal_threshold = c(0,Inf)))

param_transformations = lapply(names(params),function(x) identity)
names(param_transformations) = names(params)

plant = new(Plant,'Col:RP1','Col','RP1',unlist(params),as.matrix(env))
plant$set_genotype_info(param_ranges,design_matrix_genotype,param_transformations)
plant$develop_n(100)
plot(plant$get_Total_signal())
plant$id

plant$update_coefs(new_coefs)
plant$develop_n(100)
  plot(plant$get_Total_signal())


plant$get_predicted_bolting_PTT()
plant$add_bolting_days(c(20,40,60,80,100,120,250,255))
plant$get_observed_bolting_PTTs()

plant$get_predicted_bolting_day()
abline(v=57)
abline(h=20000)
plant$update_Signal_threshold(50000)
plant$get_predicted_bolting_day()
plot(plant$get_Total_signal())
plant$get_predicted_bolting_day()
abline(v=173);abline(h=50000)


plant$develop_day()
for(i in 1:1000) plant$develop_day()
plot(plant$get_Total_signal())
library(microbenchmark)
microbenchmark(
{
  plant$update_params(c(T_base=0))
  for(i in 1:1000) plant$develop_day()
},
{
  plant$update_params(c(T_base=0))
  plant$develop_n(1000)
}
)
plot(plant$get_Total_signal())
length(plant$get_numLeaves())
length(plant$get_size())
length(plant$get_Vern())
length(plant$get_FT_signal())


plant$update_coefs(c(T_base=0))
plant$develop_n(1000)
plot(plant$get_Total_signal())

library(microbenchmark)
microbenchmark(
{
  plant$update_params(c(T_base=runif(1,0,3)))
  plant$develop_n(10)
  # plot(plant$get_Total_signal())
},
{
  new_coefs = c(A=runif(1,0,12),T_vmin = runif(1,0,12), B = runif(1,0,12),C = 1,D=3)
  a=plant$update_coefs(new_coefs)
  plant$develop_n(10)
  # plot(plant$get_Total_signal())
}
)

# Population works, but provides basically no speed advantage.
# pop = new(Population)
# for(i in 1:100) pop$add_Plant('Col:RP1','Col','RP1',unlist(params),as.matrix(env))
# pop$update_params(c(T_base=0))
# pop$develop_n(100)
# pop$get_Total_signal(99)
#
# plants = lapply(1:100,function(x) new(Plant,'Col:RP1','Col','RP1',unlist(params),as.matrix(env)))
# plants = lapply(plants,function(plant) {
#   plant$update_params(c(T_base=0))
#   plant$develop_n(100)
#   plant
# })
# plants[[100]]$get_Total_signal()
#
# library(microbenchmark)
# microbenchmark(
# {
# pop$update_params(c(T_base=0))
#   pop$develop_n(1000)
#   # pop$get_Total_signal(99)
# },
# {
#   plants = lapply(plants,function(plant) {
#     plant$update_params(c(T_base=0))
#     plant$develop_n(1000)
#     plant
#   })
#   # plants[[100]]$get_Total_signal()
# }
# )





*/
