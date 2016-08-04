#include <Rcpp.h>
using namespace Rcpp;

#include "Base_Plant_class.h"

class Leaf {
public:
  Leaf(int day_emerged_, double vern_,double age_): day_emerged(day_emerged_),vern_repression(vern_),age_activation(age_) {}
  double make_FT(double FT_max, double general_repression) {return FT_max*general_repression*vern_repression*age_activation;}
  int day_emerged;
private:
  double vern_repression;
  double age_activation;
};


class New_Plant : public Base_Plant {
public:
  New_Plant(String id_, String gen_, String environ_,NumericVector params_, List env_): Base_Plant(id_,gen_,environ_,params_,env_) {
    TT_params = {"Tmin","T10","Topt","Tmax","Pnight","Pday"};
    PTT_params = {};
    Vern_params = {"F_b","Vsat","k","w","xi","T_vmin","T_vmax"};
    FT_params = {"FT_LD_vs_SD","FT_base"};
    Total_signal_params = {"FT_vs_GA"};
    Transition_params = {"Signal_threshold"};
    Bolting_params = {"Bolting_threshold"};
    Leaf_params = {"TT_germ","TT_phyllochron"};
  }
  virtual ~New_Plant() {}
  virtual void update_params(NumericVector);
  virtual void develop_day();
  virtual double TT_fun(NumericVector,NumericVector);
  virtual double PTT_fun(double,double);
  virtual double Signal_fun(double,double);
  virtual double HighTemp_fun(NumericVector,NumericVector) {return 1;}
  virtual NumericVector get_numLeaves() ;
  virtual NumericVector get_cumTT() {return wrap(cumTT);}
  virtual NumericVector get_GA_signal() {return wrap(GA_signal);}
protected:
  virtual void check_plant();
  std::set<std::string> Bolting_params = {"Bolting_threshold"};
  std::set<std::string> Leaf_params = {"TT_germ","TT_phyllochron"};
  std::vector<Leaf> Leaves; // Leaves making FT, photosynthate
  std::vector<double> GA_signal; // FT + GA
  std::vector<double> cumTT; // FT + GA
  std::vector<double> bolt_length; // FT + GA
};

double New_Plant::TT_fun(NumericVector temps, NumericVector dts){
  double Tmin = params["Tmin"];
  double T10 =  params["T10"];
  double Topt = params["Topt"];
  double Tmax = params["Tmax"];
  double T_20 = T10 + (20-10)/(Topt-10) * (1-T10);

  double TT = 0;
  for(int i = 0; i < temps.length(); i++) {
    double t = temps[i];
    if(t < Tmin) continue;
    if(t < 10) {
      TT += (t-Tmin)/(10-Tmin) * T10 * dts[i];
      continue;
    }
    if(t < Topt){
      TT += (T10 + (t-10)/(Topt-10) * (1-T10)) * dts[i];
      continue;
    }
    if( t < Tmax) {
      TT += (1 - (t - Topt) / (Tmax - Topt)) * dts[i];
      continue;
    }
  }
  return TT/T_20 * (20-3);  // re-scale TTs so that they are similar to Wilczek_TT (T_base = 3)
}

double New_Plant::PTT_fun(double sumTT_night, double sumTT_day){
  return params["Pnight"]*sumTT_night + params["Pday"]*sumTT_day;
}
double New_Plant::Signal_fun(double dayl, double HT_repression){
  Function FT_per_leaf("FT_per_leaf");
  double FT_max = as<double>(FT_per_leaf(dayl,params["FT_LD_vs_SD"]));

  double total_FT = 0;
  std::vector<Leaf>::iterator leaf_it;
  for(leaf_it = Leaves.begin(); leaf_it < Leaves.end(); leaf_it++){
    total_FT += leaf_it->make_FT(FT_max, HT_repression);
  }
  return total_FT;
}

void New_Plant::check_plant() {}

void New_Plant::develop_day(){
  check_plant();
  if(age >= env.numDays()) return;
  age++;

  // Plant calculations
  // TT
  // sugar (PTT)
  // calc TT increment
  double TT_night;
  double TT_day;
  // always 1st night, then day.
  // calc TT for day
  if(TT.size() < age){  // only do calculation if TT hasn't already been calculated.
    TT_night = TT_fun(env.get_GrndTmp_Night(age),env.get_TimeSteps_Night(age));
    TT_day   = TT_fun(env.get_GrndTmp_Day(age),env.get_TimeSteps_Day(age));
    TT.push_back(TT_night + TT_day);
    if(TT.size() != age) Rcout << "TT wrong length\n";

    if(age == 1) {
      cumTT.push_back(TT.back());
    }
    else {
      cumTT.push_back(cumTT.back() + TT.back());
    }
    if(cumTT.size() != age) Rcout << "cumTT wrong length\n";
  }

  // calc sugar increment. Could do this by running photosynthesis model across leaves
  if(PTT.size() < age){
    // should PTTs not accumulate until after TT_germ?
    if(cumTT[age-1] < params["TT_germ"]) {
      PTT.push_back(0.0);
    }
    else {
      PTT.push_back(PTT_fun(TT_night,TT_day));
    }
    if(PTT.size() != age) Rcout << "PTT wrong length\n";

    if(age == 1) {
      cumPTT.push_back(PTT.back());
    }
    else {
      cumPTT.push_back(cumPTT.back() + PTT.back());
    }
    if(cumPTT.size() != age) Rcout << "cumPTT wrong length\n";
  }

  if(developmental_state == 0) { // vegetative: add leaves, calculate signal
    // Meristem calculations:
    // Vern
    // Leaf initiation

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

    // add leaves
    int num_leaves = floor(std::max(0.0,(cumTT[age-1] - params["TT_germ"])/params["TT_phyllochron"]));
    while(Leaves.size() < num_leaves) {
      Leaf new_leaf(age,Vern[age-1],params["FT_base"] + cumPTT[age-1]);
      Leaves.push_back(new_leaf);
    }

    // calculate signals
    double HT_repression = HighTemp_fun(env.get_GrndTmp_Day(age),env.get_TimeSteps_Day(age));

    if(FT_signal.size() < age) {
      FT_signal.push_back(Signal_fun(env.get_Daylength(age),HT_repression));
      if(FT_signal.size() != age) Rcout << "FT_signal wrong length\n";
    }
    if(GA_signal.size() < age) {
      GA_signal.push_back(cumPTT[age-1] * Vern[age-1]);
      if(GA_signal.size() != age) Rcout << "GA_signal wrong length\n";
    }
    if(Total_signal.size() < age) {
      double new_Total_signal = (params["FT_vs_GA"] * FT_signal[age-1] + params["GA_vs_FT"] * GA_signal[age-1]) * HT_repression;
      Total_signal.push_back(new_Total_signal);
      if(Total_signal.size() != age) Rcout << "Total_signal wrong length " << age << " " << Total_signal.size() << std::endl;
    }

    // check if transition
    if(Total_signal[age-1] > params["Signal_threshold"]){
      developmental_state = 1; // state = transition
      transition_day = age;
    }
  }

  if(developmental_state == 1) {
    if(bolt_length.size() < age) bolt_length.resize(age);
    // bolt growth is proportional to PTT(today) and size at transition
    bolt_length[age-1] = bolt_length[age-2] + cumPTT[transition_day-1]*PTT[age-1];
    if(bolt_length[age-1] > params["Bolting_threshold"]){
      developmental_state = 2; // state = bolting
      bolting_day = age;
    }
  }
}

void New_Plant::update_params(NumericVector new_p){
  age = 0;
  developmental_state = 0;
  transition_day = 0;
  bolting_day = 0;
  bolt_length.clear();

  CharacterVector new_p_names = new_p.names();
  CharacterVector::iterator new_p_names_it;
  for(new_p_names_it = new_p_names.begin(); new_p_names_it < new_p_names.end(); new_p_names_it++){
    std::string name = as<std::string>(*new_p_names_it);
    if(TT_params.find(name) != TT_params.end()) {
      // Rcout << "TT\n";
      TT.clear();
      PTT.clear();
      Leaves.clear();
      FT_signal.clear();
      GA_signal.clear();
      Total_signal.clear();
      bolt_length.clear();
    }
    if(PTT_params.find(name) != PTT_params.end()) {
      // Rcout << "PTT\n";
      PTT.clear();
      Leaves.clear();
      FT_signal.clear();
      GA_signal.clear();
      Total_signal.clear();
      bolt_length.clear();
    }
    if(Vern_params.find(name) != Vern_params.end()) {
      // Rcout << "Vern\n";
      Vern.clear();
      Leaves.clear();
      FT_signal.clear();
      GA_signal.clear();
      Total_signal.clear();
      bolt_length.clear();
    }
    if(FT_params.find(name) != FT_params.end()) {
      // Rcout << "FT\n";
      FT_signal.clear();
      Total_signal.clear();
      bolt_length.clear();
    }
    if(Total_signal_params.find(name) != Total_signal_params.end()) {
      // Rcout << "Total_signal\n";
      Total_signal.clear();
      bolt_length.clear();
    }
    params[name] = new_p[name];
  }
  check_plant();
}
NumericVector New_Plant::get_numLeaves() {
  NumericVector numLeaves(age,0.0);
  for(int i = 1; i <= age; i++) {
    for(int j = 0; j < Leaves.size(); j++){
      if(Leaves[j].day_emerged <= i) {
        numLeaves[i-1]++;
      }
      else continue;
    }
  }
  return numLeaves;
  // NumericVector cumTT_R = wrap(cumTT);
  // return cumTT_R/params["TT_phyllochron"];
}


RCPP_MODULE(class_New_Plant) {
  class_<Base_Plant>("Base_Plant")
  .constructor<String,String,String,NumericVector, List>()
  .field("id",&Base_Plant::id)
  .field("gen",&Base_Plant::gen)
  .field("environ",&Base_Plant::environ)
  .method("set_genotype_info",&Base_Plant::set_genotype_info)
  .method("get_penalty",&Base_Plant::get_penalty)
  .method("get_params",&Base_Plant::get_params)
  .method("develop_n",&Base_Plant::develop_n)
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
  class_<New_Plant>("New_Plant")
    .constructor<String,String,String,NumericVector, List>()
    .method("develop_day",& New_Plant::develop_day)
    .method("update_params",& New_Plant::update_params)
    .method("TT_fun",& New_Plant::TT_fun)
    .method("Signal_fun",& New_Plant::Signal_fun)
    .method("get_numLeaves",& New_Plant::get_numLeaves)
    .method("get_cumTT",& New_Plant::get_cumTT)
    .method("get_GA_signal",& New_Plant::get_GA_signal)
    .derives<Base_Plant>("Base_Plant")
  ;
}


/*** R
# load('/Users/der7/Documents/Arabidopsis/Compendium_data/full_environ_data_list.Robj')
# env = environ_data[[1]]
# save(env,file='test_env.RData')
load(sprintf('%s/data/Seaton_FT_max_fun.Robj',path.package("AtFloweringModel")))
load('test_env.RData')
  a = new(Environ,as.matrix(env))

  params = list(
    CSDL = 8,
    CLDL = 14,

    D_LD = 1,
    D_SD = 0.626,

    T_base = 3,
    Tmin = -0.2,
    T10 = 0.318,
    Topt = 25,
    Tmax = 50,

    FT_base = 0,
    FT_LD_vs_SD = 100,
    TT_germ = 100,
    TT_phyllochron = 100,
    GA_vs_FT = 0.5,

    k=-5.17485563597551,	#same vernalization model
      w=2.22568888596679,
        xi=0.995904471970916,
        T_vmin = -3.5,
        T_vmax = 6,
        Vsat = 960,

        Pday = 1,
        Pnight = 0,

        F_b = 0.4,
        Signal_threshold = 20000,
    Bolting_threshold = 1000
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

  plant = new(New_Plant,'Col:RP1','Col','RP1',unlist(params),as.matrix(env))
  plant$new_env(a)
  plant$set_genotype_info(param_ranges,design_matrix_genotype,param_transformations)
  plant$develop_n(100)
  plant$get_params()
  plant$get_numLeaves()

  temps = seq(0,40,length=40)
  plot(temps,sapply(temps,function(x) plant$TT_fun(x,1)))

  plot(plant$get_Total_signal())
  plant$id

  plant$update_coefs(new_coefs)
  plant$get_params()
  plant$get_size()

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

