//Dummy function definitions for compilation testing
//Taken from example output from SNC

#include "runtime.h"

#define NULL 0

void act_assign_gather_weather_data_15();
void record_blk_nodeid_2();
void record_blk_temp_8();
void init();
void on_boot();

//global variables
float _fld_var_humidity_12 = 0;
int _fld_var_nodeid_0 = 0;
float _fld_var_pressure_9 = 0;
float _fld_var_temp_6 = 0;
time_t _fld_var_time_3 = 0;
time_t _rule_28_var_30 = 0;

void act_assign_gather_weather_data_15(){
  spawn(&record_blk_nodeid_2,0);
  spawn(&record_blk_temp_8,0);
  join(NULL,0,0);
  flush_buffer(0);
  store_value(0,4,&_fld_var_nodeid_0,sizeof(int));
  store_value(0,2,&_fld_var_temp_6,sizeof(float));
  finish_record(0);
}

void record_blk_temp_8(){
  float reg_7 = Get_Temperature();
  float reg_8 = Get_Time();
  _fld_var_temp_6 = reg_7;
  _fld_var_time_3 = reg_8;
}

void record_blk_nodeid_2(){
  int reg_1 = Get_Node_Id();
  _fld_var_nodeid_0 = reg_1;
}

