#include <Rcpp.h>
using namespace Rcpp;

typedef std::vector<int> int_vector;

class Day {
public:
  Day(int year, int doy, double dayl): Year(year), DOY(doy), Daylength(dayl) {}
  void add_hour(double GrndTmp, double PAR, double timestep,int hr);
  int Year, DOY;
  double Daylength;
  std::vector<double> GrndTmps, PARs, timesteps;
  std::vector<int> HourInDay;
};
class Night {
public:
  Night(int year, int doy): Year(year), DOY(doy) {}
  void add_hour(double GrndTmp, double timestep,int hr);
  std::vector<double> GrndTmps, timesteps;
  int Year, DOY;
  std::vector<int> HourInDay;
};

class Environ {
public:
  Environ() {}
  Environ(List);
  int numDays() {return Days.size();}
  double get_Daylength(int);
  NumericVector get_GrndTmp_Day(int);
  NumericVector get_GrndTmp_Night(int);
  NumericVector get_PAR_Day(int);
  NumericVector get_TimeSteps_Day(int);
  NumericVector get_TimeSteps_Night(int);
  void repeat_last_day();
  List make_env_list();
protected:
  std::vector<Day> Days;
  std::vector<Night> Nights;
};
