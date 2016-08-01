#include <Rcpp.h>
using namespace Rcpp;

typedef std::vector<int> int_vector;

class Day {
public:
  Day(double Daylength);
  void add_hour(double GrndTmp, double PAR, double timestep);
  double Daylength;
  std::vector<double> GrndTmps, PARs, timesteps;
};
class Night {
public:
  Night();
  void add_hour(double GrndTmp, double timestep);
  std::vector<double> GrndTmps, timesteps;
};

class Environ {
public:
  Environ() {}
  Environ(NumericMatrix);
  int numDays() {return Days.size();}
  double get_Daylength(int);
  NumericVector get_GrndTmp_Day(int);
  NumericVector get_GrndTmp_Night(int);
  NumericVector get_PAR_Day(int);
  NumericVector get_TimeSteps_Day(int);
  NumericVector get_TimeSteps_Night(int);
  // double sumTemps(IntegerVector);
protected:
  std::vector<int> Year;
  std::vector<int> Date;
  std::vector<Day> Days;
  std::vector<Night> Nights;
};
