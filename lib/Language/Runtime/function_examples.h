//Dummy function definitions for compilation testing
//Taken from example output from SNC

#define NULL 0

void act_assign_gather_weather_data_15();
void record_blk_nodeid_2();
void record_blk_temp_8();
void init();
void on_boot();

//global variables
float _fld_var_humidity_12;
int _fld_var_nodeid_0;
float _fld_var_pressure_9;
float _fld_var_temp_6;
time_t _fld_var_time_3;
time_t _rule_28_var_30;

void act_assign_gather_weather_data_15(){
  spawn(&record_blk_nodeid_2,0);
  join(NULL,0,0);
  flush_buffer(0);
  store_value(0,4,&_fld_var_nodeid_0,sizeof(int));      //didn't end lines here
  store_value(0,3,&_fld_var_time_3,sizeof(time_t));     //didn't end lines here
  store_value(0,2,&_fld_var_temp_6,sizeof(float));      //didn't end lines here
  store_value(0,1,&_fld_var_pressure_9,sizeof(float));  //didn't end lines here
  store_value(0,0,&_fld_var_humidity_12,sizeof(float)); //didn't end lines here
  finish_record(0);
}
