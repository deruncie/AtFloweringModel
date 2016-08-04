#include <Rcpp.h>
using namespace Rcpp;

#include "Base_Plant_class.h"

class Wilczek_Plant : public Base_Plant {
public:
  Wilczek_Plant(String id_, String gen_, String environ_,NumericVector params_, List env_): Base_Plant(id_,gen_,environ_,params_,env_) {
    TT_params = {"T_base"};
    PTT_params = {"Pnight","Pday"};
    Vern_params = {"F_b","Vsat","k","w","xi","T_vmin","T_vmax"};
    FT_params = {"D_LD","CSDL","CLDL"};
    Total_signal_params = {"D_SD"};
    Transition_params = {"Signal_threshold"};
  }
  virtual ~Wilczek_Plant() {}
  virtual void update_params(NumericVector);
  virtual void develop_day();
  virtual double TT_fun(NumericVector,NumericVector);
  virtual double PTT_fun(double,double);
  virtual double Signal_fun(double,double);
  NumericVector get_cumPTT() {return(wrap(cumPTT));}
protected:
  void check_plant();
};

double Wilczek_Plant::TT_fun(NumericVector temps, NumericVector dts){
  return sum(pmax(temps-params["T_base"],0) * dts);
}
double Wilczek_Plant::PTT_fun(double sumTT_night, double sumTT_day){
  return params["Pnight"]*sumTT_night + params["Pday"]*sumTT_day;
}
double Wilczek_Plant::Signal_fun(double repression, double dayl){
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

void Wilczek_Plant::check_plant() {
  // after updating parameters, goes through to check which summaries need to be re-set

  if(bolting_day > 0 && developmental_state == 0) {
    Rcout << "bolting_day: " << bolting_day;
    Rcout << ", transition_day: " << transition_day;
    Rcout << ", developmental_state: " << developmental_state;
    Rcout << ", cumPTT.size(): " << cumPTT.size() << std::endl;
  }

  // PTT depends on TT. So if TT is reset, so is PTT
  if(PTT.size() > TT.size()) {
    PTT.clear();
    cumPTT.clear();
    Rcout << "PTT size invalid\n";
  }

  // FT depends on PTT and Vern
  if(FT_signal.size() > std::min(PTT.size(),Vern.size())) {
    FT_signal.clear();
    Rcout << "FT_signal size invalid\n";
  }

  // Total signal depends on PTT and FT_signal
  if(Total_signal.size() > PTT.size() ||
     Total_signal.size() > Vern.size() ||
     Total_signal.size() > FT_signal.size()) {
    Total_signal.clear();
    Rcout << "Total_signal size invalid\n";
  }

}

void Wilczek_Plant::develop_day(){
  check_plant();
  if(age >= env.numDays()) return;
  age++;
  // always 1st night, then day.
  // calc TT for day
  if(TT.size() < age){  // only do calculation if TT hasn't already been calculated.
    double TT_night = TT_fun(env.get_GrndTmp_Night(age),env.get_TimeSteps_Night(age));
    double TT_day   = TT_fun(env.get_GrndTmp_Day(age),env.get_TimeSteps_Day(age));
    TT.push_back(TT_night + TT_day);
    if(TT.size() != age) Rcout << "TT wrong length\n";

    // add leaves
    PTT.push_back(PTT_fun(TT_night,TT_day));
    if(PTT.size() != age) Rcout << "PTT wrong length\n";

    if(age == 1) {
      cumPTT.push_back(PTT.back());
    }
    else {
      cumPTT.push_back(cumPTT.back() + PTT.back());
    }
    if(cumPTT.size() != age) Rcout << "cumPTT wrong length\n";
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

  // check bolting
  if(developmental_state == 0 && Total_signal[age-1] > params["Signal_threshold"]) {
    developmental_state = 2; // bolting
    transition_day = age;
    bolting_day = age;
  }

}

void Wilczek_Plant::update_params(NumericVector new_p){
  age = 0;
  developmental_state = 0;
  transition_day = 0;
  bolting_day = 0;

  CharacterVector new_p_names = new_p.names();
  for(int i = 0; i < new_p.length();i++){
    std::string name = as<std::string>(new_p_names[i]);
    if(TT_params.find(name) != TT_params.end()) {
      // Rcout << "TT\n";
      TT.clear();
      PTT.clear();
      cumPTT.clear();
      FT_signal.clear();
      Total_signal.clear();
    }
    if(PTT_params.find(name) != PTT_params.end()) {
      // Rcout << "PTT\n";
      PTT.clear();
      cumPTT.clear();
      FT_signal.clear();
      Total_signal.clear();
    }
    if(Vern_params.find(name) != Vern_params.end()) {
      // Rcout << "Vern\n";
      Vern.clear();
      FT_signal.clear();
      Total_signal.clear();
    }
    if(FT_params.find(name) != FT_params.end()) {
      // Rcout << "FT\n";
      FT_signal.clear();
      Total_signal.clear();
    }
    if(Total_signal_params.find(name) != Total_signal_params.end()) {
      // Rcout << "Total_signal\n";
      Total_signal.clear();
    }
    params[name] = new_p[i];
  }
  check_plant();
}

RCPP_MODULE(class_Wilczek_Plant) {
  class_<Base_Plant>("Base_Plant")
  .constructor<String,String,String,NumericVector, List>()
     .field("id",&Base_Plant::id)
     .field("gen",&Base_Plant::gen)
     .field("environ",&Base_Plant::environ)
     .method("set_genotype_info",&Base_Plant::set_genotype_info)
     .method("get_penalty",&Base_Plant::get_penalty)
     .method("get_params",&Base_Plant::get_params)
     .method("develop_n",&Base_Plant::develop_n)
     .method("get_numLeaves",&Base_Plant::get_numLeaves)
     .method("get_size",&Base_Plant::get_size)
     .method("get_Vern",&Base_Plant::get_Vern)
     .method("get_FT_signal",&Base_Plant::get_FT_signal)
     .method("get_Total_signal",&Base_Plant::get_Total_signal)
     .method("update_coefs",&Base_Plant::update_coefs)
     .method("add_bolting_days",&Base_Plant::add_bolting_days)
     .method("get_predicted_bolting_day",&Base_Plant::get_predicted_bolting_day)
     .method("update_Signal_threshold",&Base_Plant::update_Signal_threshold)
     .method("get_predicted_bolting_PTT",&Base_Plant::get_predicted_bolting_PTT)
     .method("get_observed_bolting_PTTs",&Base_Plant::get_observed_bolting_PTTs)
     .method("get_developmental_state",&Base_Plant::get_developmental_state)
  ;
  class_<Wilczek_Plant>("Wilczek_Plant")
    .constructor<String,String,String,NumericVector, List>()
    .method("develop_day",& Wilczek_Plant::develop_day)
    .method("update_params",& Wilczek_Plant::update_params)
    .method("TT_fun",& Wilczek_Plant::TT_fun)
    .method("Signal_fun",& Wilczek_Plant::Signal_fun)
    .method("get_cumPTT",& Wilczek_Plant::get_cumPTT)
    .derives<Base_Plant>("Base_Plant")
  ;
}


/*** R
# load('/Users/der7/Documents/Arabidopsis/Compendium_data/full_environ_data_list.Robj')
# env = environ_data[[1]]
# save(env,file='test_env.RData')
load('test_env.RData')
  a = new('Environ',as.list(env))

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

  plant = new(Wilczek_Plant,'Col:RP1','Col','RP1',unlist(params),as.list(env))
  plant$set_genotype_info(param_ranges,design_matrix_genotype,param_transformations)
  plant$develop_n(10)
  plot(plant$get_Total_signal())
  plant$id
  plant$get_size()
  plant$get_FT_signal()
  plant$get_numLeaves()

  temps = seq(0,40,length=40)
  plot(temps,sapply(temps,function(x) plant$TT_fun(x,1)))
  plant$get_params()

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
