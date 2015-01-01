#ifndef LIB_H
#define LIB_H

#include <SPI.h>
#include <Wire.h>
#include <Scout.h>
#include <GS.h>
#include <bitlash.h>
#include <lwm.h>
#include <js0n.h>


// Gets the current absolute time. 
unsigned long int Get_Time() {
	//this calls SleepHandler.seconds() every iteration
	//int time = Scout.now;	
	return millis(); //Arduino time library accessor method
}

// ------------ Local Data OPs -------------
// 	Returns an integer unique to a specific node 
//	Let's try using the Pinoccio's serial #
int Get_Node_Id() {
	uint32_t serial = Scout.getHwSerial();
	//return the last 8 bits - most likely to be different?
	//simple casting should work, I think
	uint8_t last_serial = serial;
	return last_serial;
}

// ------------ Temperature Sensor OPs -------------
//	Returns the temperature in Celsius
//	Pinoccio has onboard temperature sensor, we'll use that
float Get_Temperature() {
	return Scout.getTemperature();
}

// ------------ Brightness Sensor OPs -------------
// Returns the current relative brightness
// Don't have a sensor for this yet, so returning state of charge instead
float Get_Brightness() {
	return Scout.getBatteryPercentage();
}

// ------------ LED Output OPs -------------
// Sets the state of the external LED - namely, the Pinoccio's torch
int Set_Led(int state) {
	if(!state)
    	Led.disable();
	else
		Led.enable();
} 

// Gets the current state of the external LED 
// Returns true if on
int Get_Led() {
	return Led.isOff();
}

float Get_Pressure() {
	return 0;
}

float Get_Humidity() {
	return 0;
}

//these following functions are all no-ops
void Print_Message(const char* message) {
  Serial.println(message);
}
bool Other_Function(const int foo,
                    const float bar,
                    const bool stuff) {
  return false;
} 

char * Test_Set(float foo, char * buf, size_t buflen) {
  return 0;
}

void Take_Picture(float delay) {//do nothing
}

int  Film_Remaining() {
  return 0;
}

bool Is_Snowing(float temp,float pressure,float humidity) {
  return false;
}

void Set_Motion_Threshold(float trigger_level) {
  //do nothing
}


#endif
