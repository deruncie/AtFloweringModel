#include <Rcpp.h>
using namespace Rcpp;

#include "Environ_class.h"

class Base_Plant {
public:
  Base_Plant(String id_, String gen_, String environ_,NumericVector params_, List env_);
  // generic values
  String id, gen, environ;

  // generic functions
  void set_genotype_info(NumericMatrix,NumericMatrix,Function);
  NumericVector update_coefs(NumericVector);
  void update_Signal_threshold(double thresh);
  double get_penalty() {return penalty;}
  void add_bolting_days(IntegerVector);
  void develop_n(int);
  NumericVector get_params() {return params;}
  int get_developmental_state() {return developmental_state;}
  int predict_bolting();
  int get_predicted_bolting_day();
  int get_predicted_transition_day();
  double get_predicted_bolting_PTT();
  IntegerVector get_observed_bolting_days() {return observed_bolting_days;}
  NumericVector get_observed_bolting_PTTs();
  NumericVector get_size() {return wrap(cumPTT);}
  NumericVector get_Vern() {return wrap(Vern);}
  NumericVector get_FT_signal() {return wrap(FT_signal);}
  NumericVector get_Total_signal() {return wrap(Total_signal);}

  // virtual functions
  virtual ~Base_Plant() {}
  virtual void update_params(NumericVector) {}  // directly change parameters
  virtual void develop_day() {}
  virtual double TT_fun(NumericVector,NumericVector);
  virtual double PTT_fun(double,double);
  virtual double Vern_fun(NumericVector,NumericVector);
  virtual double Signal_fun(double,double);
  virtual NumericVector get_numLeaves() {return wrap(TT);}
  // int predict_flowering();
protected:
  int age, transition_day, bolting_day, developmental_state;
  double penalty;
  NumericVector params;
  NumericMatrix param_ranges,design_matrix_genotype;
  Function param_transformations;
  std::set<std::string> TT_params = {"T_base"};
  std::set<std::string> PTT_params = {"Pnight","Pday"};
  std::set<std::string> Vern_params = {"F_b","Vsat","k","w","xi","T_vmin","T_vmax"};
  std::set<std::string> FT_params = {"D_LD","CSDL","CLDL"};
  std::set<std::string> Total_signal_params = {"D_SD"};
  std::set<std::string> Transition_params = {"Signal_threshold"};
  IntegerVector observed_bolting_days;
  Environ env;
  std::vector<double> TT; // thermal time history by day
  std::vector<double> PTT; // PTU time history by day
  std::vector<double> Vern; // Vern time history by day
  std::vector<double> FT_signal; // FT history by day
  std::vector<double> Total_signal; // cumPTU history by day
  std::vector<double> cumPTT; // cumPTU history by day
  // virtual functions
  virtual void check_plant() {}
};
