/*	By Colin Szechy
 *	This references the Arduino Time library from 
 *	http://playground.arduino.cc/Code/Time
 *	not a standard Arduino library - needs to be provided 
 *	or - can use uptime.seconds() from Pinoccio library
 */

//default Scout libraries - from the "Bootstrap" example
#include <SPI.h>
#include <Wire.h>
#include <Scout.h>
#include <GS.h>
#include <bitlash.h>
#include <lwm.h>
#include <js0n.h>

#include <SleepHandler.h>

//typedefs have to be declared in a separate header file. 
//When runtime.h is ready...
//typedef unsigned long long int uint64_t - in Arduino 'stdint.h'
//typedef unsigned long long int time_t;


//	all-important setup 
//	should be constant across devices (?) - we'll see
void setup() {
	//default Scout setup
	Scout.setup("CompilerNet", "v0.01", 1);
}

//	infinite loop - maintaining/setting up functions, handling table ops
void loop() {
	//default Scout looping method - a superclass's method, if you will
	Scout.loop();
}

// Gets the current absolute time. 

unsigned long long int get_time() {
	//this calls SleepHandler.seconds() every iteration
	int time = Scout.now;	
	//return time.now(); Arduino time library accessor method
}

// ------------ Local Data OPs -------------
// 	Returns an integer unique to a specific node 
//	Let's try using the Pinoccio's serial #
int get_node_id() {
	//
	uint32_t serial = Scout.getHwSerial();
	//return the last 8 bits - most likely to be different?
	//simple casting should work, I think
	uint8_t last_serial = serial;
	return last_serial;
}

// ------------ Temperature Sensor OPs -------------
//	Returns the temperature in Celsius
//	Pinoccio has onboard temperature sensor, we'll use that
float get_temperature() {
	return Scout.getTemperature();
}

// ------------ Brightness Sensor OPs -------------
// Returns the current relative brightness
// Don't have a sensor for this yet, so returning state of charge instead
float get_brightness() {
	return Scout.getBatteryPercentage();
}

// ------------ LED Output OPs -------------
// Sets the state of the external LED - namely, the Pinoccio's torch
int set_led(int state) {
	if(!state)
    	Led.disable();
	else
		Led.enable();
} 

// Gets the current state of the external LED 
// Returns true if on
int get_led() {
	return Led.isOff();
}
