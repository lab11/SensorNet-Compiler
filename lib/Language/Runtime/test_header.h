//added these manually
#ifndef SNL_H
#define SNL_H

#define table_number 1
#define field_zero_number 8
#define field_one_number 8 //an example of a second one

typedef unsigned char bool;
typedef unsigned long int time_t; // For the moment standard posix time with a 
typedef unsigned long int interval_t; 
typedef unsigned int size_t;

//done adding things manually other then #endif at the end


/*time_t Get_Time(); 
int Get_Node_Id();
float Get_Temperature();
float Get_Brightness();
float Get_Pressure();
float Get_Humidity();
int Set_Led(const bool state); 
int Get_Led();*/
void Print_Message(const char* message);
bool Other_Function(const int foo,
                    const float bar,
                    const bool stuff); 
char * Test_Set(float foo, char * buf, size_t buflen); 

void Take_Picture(float delay);
int  Film_Remaining(); 
bool Is_Snowing(float temp,float pressure,float humidity);

void Set_Motion_Threshold(float trigger_level);

char * string_coerce_str(const char *str , char * buf, size_t buflen);
char * string_coerce_int(const int foo, char * buf, size_t buflen);  
char * string_coerce_bool(const bool foo, char * buf, size_t buflen);  
char * string_coerce_float(const float foo, char * buf, size_t buflen);  
char * string_coerce_time(const time_t foo, char * buf, size_t buflen);  
char * string_coerce_interval(const interval_t foo, char * buf, size_t buflen);  

#endif

