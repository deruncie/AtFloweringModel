#include <Rcpp.h>
using namespace Rcpp;

#include "Environ_class.h"

void Day::add_hour(double temp, double par, double dt, int hr) {
  GrndTmps.push_back(temp);
  PARs.push_back(par);
  timesteps.push_back(dt);
  HourInDay.push_back(hr);
}
void Night::add_hour(double temp, double dt, int hr) {
  GrndTmps.push_back(temp);
  timesteps.push_back(dt);
  HourInDay.push_back(hr);
}

Environ::Environ(List env){
  NumericVector Year = clone(as<NumericVector>(env["Year"]));
  NumericVector DOY = clone(as<NumericVector>(env["Date"]));
  NumericVector HourInDay = clone(as<NumericVector>(env["Hour.in.Day"]));
  NumericVector Daylength = clone(as<NumericVector>(env["Daylength"]));
  NumericVector HrsLight = clone(as<NumericVector>(env["Hrs.light"]));
  NumericVector GrndTmp = clone(as<NumericVector>(env["Grnd.Tmp"]));
  NumericVector PAR = clone(as<NumericVector>(env["PAR"]));
  // NumericVector Year = env(_,0);
  // NumericVector DOY = env(_,1);
  // NumericVector HourInDay = env(_,2);
  // NumericVector HrsLight = env(_,3);
  // NumericVector Daylength = env(_,4);
  // NumericVector GrndTmp = env(_,5);
  // NumericVector PAR = env(_,6);

  // Days.reserve(GrndTmp.length()/24);
  // Nights.reserve(GrndTmp.length()/24);

  Day current_day(Year[0],DOY[0],Daylength[0]);
  Night current_night(Year[0],DOY[0]);
  bool is_night = true;
  if(HrsLight[0] == 1.0 || (HrsLight[0] > 0.0 && HrsLight[1] == 0.0)) {
    Nights.push_back(current_night); // add empty night
    is_night = false;
  }
  for(int i = 0; i < GrndTmp.length(); i++){
    // double hr_light = env(i,4);
    if(is_night && HrsLight[i] > 0){
      // end the night
      if(HrsLight[i] < 1) current_night.add_hour(GrndTmp[i],1 - HrsLight[i],HourInDay[i]);
      Nights.push_back(current_night);
      // start the next day
      current_day = Day(Year[i],DOY[i],Daylength[i]);
      is_night = false;
    }
    else if(!is_night && HrsLight[i] < 1.0){
      // end the day
      if(HrsLight[i] > 0) current_day.add_hour(GrndTmp[i],PAR[i],HrsLight[i],HourInDay[i]);
      Days.push_back(current_day);
      // start the next night
      current_night = Night(Year[i],DOY[i]);
      is_night = true;
    }

    if(is_night){
      current_night.add_hour(GrndTmp[i],1 - HrsLight[i],HourInDay[i]);
    }
    else {
      current_day.add_hour(GrndTmp[i],PAR[i],HrsLight[i],HourInDay[i]);
    }
  }
  if(is_night) {
    Nights.push_back(current_night);
  }
  else {
    Days.push_back(current_day);
  }
}

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

void Environ::repeat_last_day() {
  if(Days.size() > 0) {
    Day last_day = Days.back();
    Night last_night = Nights.back();
    last_day.DOY++;
    last_night.DOY++;
    Days.push_back(last_day);
    Nights.push_back(last_night);
  }
  else {
    Rcout << "Environ is empty\n";
  }
}
List Environ::make_env_list() {
  std::vector<double> year;
  std::vector<double> doy;
  std::vector<double> hourInDay;
  std::vector<double> daylength;
  std::vector<double> hrsLight;
  std::vector<double> grndTmp;
  std::vector<double> par;

  for(int i = 0; i < Days.size(); i++){
    for(int j = 0; j < Nights[i].GrndTmps.size(); j++){
      year.push_back(Nights[i].Year);
      doy.push_back(Nights[i].DOY);
      hourInDay.push_back(Nights[i].HourInDay[j]);
      daylength.push_back(Days[i].Daylength);
      hrsLight.push_back(1.0-Nights[i].timesteps[j]);
      grndTmp.push_back(Nights[i].GrndTmps[j]);
      par.push_back(0);
    }
    for(int j = 0; j < Days[i].GrndTmps.size(); j++){
      if(Days[i].timesteps[j] < 1.0) continue;
      year.push_back(Days[i].Year);
      doy.push_back(Days[i].DOY);
      hourInDay.push_back(Days[i].HourInDay[j]);
      daylength.push_back(Days[i].Daylength);
      hrsLight.push_back(Days[i].timesteps[j]);
      grndTmp.push_back(Days[i].GrndTmps[j]);
      par.push_back(Days[i].PARs[j]);
    }
  }

  NumericVector Year = wrap(year);
  NumericVector DOY = wrap(doy);
  NumericVector HourInDay = wrap(hourInDay);
  NumericVector Daylength = wrap(daylength);
  NumericVector HrsLight = wrap(hrsLight);
  NumericVector GrndTmp = wrap(grndTmp);
  NumericVector PAR = wrap(par);

  return List::create(_["Year"] = Year,
                      _["Date"] = DOY,
                      _["Hour.in.Day"] = HourInDay,
                      _["Hrs.light"] = HrsLight,
                      _["Daylength"] = Daylength,
                      _["Grnd.Tmp"] = GrndTmp,
                      _["PAR"] = PAR);
}

RCPP_MODULE(class_Environ) {
  class_<Environ>("Environ")
  .constructor<List>()
  .method("get_Daylength",&Environ::get_Daylength)
  .method("get_GrndTmp_Day",&Environ::get_GrndTmp_Day)
  .method("get_GrndTmp_Night",&Environ::get_GrndTmp_Night)
  .method("get_PAR_Day",&Environ::get_PAR_Day)
  .method("get_TimeSteps_Day",&Environ::get_TimeSteps_Day)
  .method("get_TimeSteps_Night",&Environ::get_TimeSteps_Night)
  .method("repeat_last_day",&Environ::repeat_last_day)
  .method("make_env_list",&Environ::make_env_list)
  // .method("sumTemps",&Environ::sumTemps)
  ;
}
/*** R
# load('/Users/der7/Documents/Arabidopsis/Compendium_data/full_environ_data_list.Robj')
# env = environ_data[[1]]
# a = new(Environ,as.list(env))
# e = data.frame(a$make_env_list())
# a$repeat_last_day()
# e = data.frame(a$make_env_list())
# #
# tail(e,n=50)

#
# a$get_GrndTmp_Day(1)
# a$get_GrndTmp_Night(1)
# a$get_PAR_Day(1)
# env$Grnd.Tmp[1:24]
# # a$sumTemps(1:100)
#
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
