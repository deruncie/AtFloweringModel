#include <Rcpp.h>
using namespace Rcpp;

#include "Environ_class.h"

Day::Day(double dayl) {
  Daylength = dayl;
}
Night::Night(){}
void Day::add_hour(double temp, double par, double dt) {
  GrndTmps.push_back(temp);
  PARs.push_back(par);
  timesteps.push_back(dt);
}
void Night::add_hour(double temp, double dt) {
  GrndTmps.push_back(temp);
  timesteps.push_back(dt);
}

Environ::Environ(NumericMatrix env){
  Day current_day(env(0,3));
  Night current_night;
  NumericVector HourInDay = env(_,2);
  bool is_night = true;
  if(env(0,4) == 1 || (env(0,4) > 0 && env(1,4) == 0)) is_night = false;
  for(int i = 0; i < env.nrow(); i++){
    double hr_light = env(i,4);
    if(is_night && hr_light > 0){
      // end the night
      current_night.add_hour(env(i,5),1 - hr_light);
      Nights.push_back(current_night);
      // start the next day
      current_day = Day(env(i,3));
      is_night = false;
    }
    else if(!is_night && hr_light < 1.0){
      // end the day
      current_day.add_hour(env(i,5),env(i,7),hr_light);
      Days.push_back(current_day);
      // start the next night
      current_night = Night();
      is_night = true;
    }

    if(is_night){
      current_night.add_hour(env(i,5),1 - hr_light);
    }
    else {
      current_day.add_hour(env(i,5),env(i,7),hr_light);
    }
  }
}
// NumericVector Environ::get_Daylength(IntegerVector days) {
//   NumericVector Daylengths(days.length());
//
//   IntegerVector::iterator it;
//   NumericVector::iterator it_dayl;
//   for(it = days.begin(), it_dayl = Daylengths.begin(); it < days.end(); it++, it_dayl++){
//     *it_dayl = Days[*it].Daylength;
//   }
//   return Daylengths;
// }
double Environ::get_Daylength(int day){
  return Days[day-1].Daylength;
}

NumericVector Environ::get_GrndTmp_Day(int day) {
  return wrap(Days[day-1].GrndTmps);
}
NumericVector Environ::get_GrndTmp_Night(int day) {
  return wrap(Nights[day-1].GrndTmps);
}
NumericVector Environ::get_PAR_Day(int day) {
  return wrap(Days[day-1].PARs);
}
NumericVector Environ::get_TimeSteps_Day(int day) {
  return wrap(Days[day-1].timesteps);
}
NumericVector Environ::get_TimeSteps_Night(int day) {
  return wrap(Nights[day-1].timesteps);
}
// double Environ::sumTemps(IntegerVector days){
//   double s = 0;
//   // Rcout << s << std::endl;
//   for(int i = 0; i < days.length(); i++){
//     NumericVector temps = wrap(Days[i].GrndTmps);
//     // Rcout << temps << std::endl;
//     NumericVector dts = wrap(Days[i].timesteps);
//     s += sum(temps*dts);
//   }
//   return(s);
// }

RCPP_MODULE(class_Environ) {
  class_<Environ>("Environ")
  .constructor<NumericMatrix>()
  .method("get_Daylength",&Environ::get_Daylength)
  .method("get_GrndTmp_Day",&Environ::get_GrndTmp_Day)
  .method("get_GrndTmp_Night",&Environ::get_GrndTmp_Night)
  .method("get_PAR_Day",&Environ::get_PAR_Day)
  .method("get_TimeSteps_Day",&Environ::get_TimeSteps_Day)
  .method("get_TimeSteps_Night",&Environ::get_TimeSteps_Night)
  // .method("sumTemps",&Environ::sumTemps)
  ;
}
/*** R
load('/Users/der7/Documents/Arabidopsis/Compendium_data/full_environ_data_list.Robj')
env = environ_data[[1]]
a = new(Environ,as.matrix(env))

a$get_GrndTmp_Day(1)
a$get_GrndTmp_Night(1)
a$get_PAR_Day(1)
env$Grnd.Tmp[1:24]
# a$sumTemps(1:100)

# microbenchmark(
#   {
# temp_mat = matrix(env$Grnd.Tmp,nr=24)
# l_mat = matrix(env$Hrs.light,nr=24)
# t1 = colSums(temp_mat*l_mat)[1:100]
# },
#
# t2 = sapply(1:100,function(x) sum(a$get_GrndTmp_Day(x)*a$get_TimeSteps_Day(x))),
# t3 = a$sumTemps(1:100)
# )
#
# plot(t1,t2);abline(0,1)
# plot(t1,t3);abline(0,1)

library(microbenchmark)
# microbenchmark(
#   sapply(1:100,function(x) a$get_Daylength(x)),
# sapply(1:100,function(x) env$Daylength[24*(x-1)+12])
# )

# microbenchmark(a$get_Daylength(1),a$get_Daylength2(1))
# microbenchmark(a$get_GrndTmp_Day(1),a$get_GrndTmp_Day2(1))
*/
