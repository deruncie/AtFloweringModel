
// class Population {
// public:
//   Population() {}
//   std::vector<Plant> Plants;
//   void add_Plant(String id_, NumericVector params_, NumericMatrix env_);
//   void update_params(NumericVector);  // directly change parameters
//   void develop_n(int);
//   NumericVector get_Total_signal(int i) {return Plants[i].get_Total_signal();}
// };
// void Population::add_Plant(String id_, NumericVector params_, NumericMatrix env_){
//   Plant new_plant(id_,params_,env_);
//   Plants.push_back(new_plant);
// }
// void Population::develop_n(int days){
//   // for(int i = 0; i < Plants.size(); i++){
//   //   Plants[i].develop_n(days);
//   // }
//   std::vector<Plant>::iterator it;
//   for(it = Plants.begin(); it < Plants.end(); it++){
//     it->develop_n(days);
//   }
// }
// void Population::update_params(NumericVector new_params){
//   std::vector<Plant>::iterator it;
//   for(it = Plants.begin(); it < Plants.end(); it++){
//     it->update_params(new_params);
//   }
//   // for(int i = 0; i < Plants.size(); i++){
//   //   Plants[i].update_params(new_params);
//   // }
// }
//
// RCPP_MODULE(class_Population) {
//   class_<Population>("Population")
//     .constructor()
//     .method("develop_n",&Population::develop_n)
//     .method("add_Plant",&Population::add_Plant)
//     .method("update_params",&Population::update_params)
//     .method("get_Total_signal",&Population::get_Total_signal)
//   ;
// }
