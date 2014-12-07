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
#include "sensor.h"

#include <SleepHandler.h>
//from http://github.com/ivanseidel/LinkedList
#include <LinkedList.h>	
//from http://code.google.com/p/arduino-buffered-serial
#include <ByteBuffer.h> 

//Data tables - first index is table, second is column. Row is irrelevant


//function table global
LinkedList<function> function_queue;// = LinkedList<function>();

//one-to-one correspondence between buffers and table #'s
ByteBuffer[table_number] buffers;


//	all-important setup 
//	should be constant across devices (?) - we'll see
void setup() {
	//default Scout setup
	Scout.setup("CompilerNet", "v0.01", 1);
	//create linked list table - from http://github.com/ivanseidel/LinkedList
	
	/*** Will need to be built dynamically ***/
	//for each table, initialize buffer with proper size
	//one char for column value, one '\0', 
  //four char for float(max), two '/0' to signify the end
	//e.g. for table 1:
	buffers[0].init(field_zero_number*8);
	//buffers[1].init(field_one_number*8);

	//Create linked list for function queue 
	function_queue = LinkedList<function>();
}

//	infinite loop - maintaining/setting up functions, handling data and function operations
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

/*may be deprecated, not sure yet
data_piece parse_data_piece_buffer(int table) {
	data_piece dp;
	//if there's data in the buffer
	if(data_table[table].buffer.size() != 0)
		
		//pull out the terminating zero on the data
		data_table[table].buffer.getFromBack();
		//read out double
		dp.data = data_table[table].buffer.getFloatFromBack();
		//pull out terminating zero
		data_table[table].buffer.getFromBack();
		
		//pull out length of cstring
		int length = data_table[table].buffer.getFromBack();
		//pull out terminating zero
		data_table[table].buffer.getFromBack();
		//pull out cstring
		char temp_name[length+1];
		for(int i = 0; i < length; ++i)
			temp_name[i] = data_table[table].buffer.getFromBack();
		char temp_name[length] = 
		dp.name = temp_name;
	else
		return 0;

	return dp;
}*/

/*void store_value(int table, int val_num, data_piece dp) {
  //1033 is 1024 chars for string, one '\0', 
  //one char for column value, one '\0', 
  //four char for float(max), two '/0' to signify the end
}*/

void store_value(int table, int val_num, void *buff, size_t buf_lens){
  //one char for column value, one '\0', 
  //four char for float(max), two '/0' to signify the end

 	//place val_num/column - look up string later, when data sent away
  buffer[table].put(val_num);
	//place value that's neccessary
	if(buf_lens == 4) {
		buffer[table].putFloat(buff);
		buffer[table].put(2);	//TRUE for float, four bytes/32 bits
	}
	else {
		buffer[table].put(buff);
		buffer[table].put(1);	//false for just one byte/8 bits
	}
	//place end char
	buffer[table].put('\0');

}

//pops most recent data piece off of the table's buffer into table, so in reverse
void buffer_pop(int table) {
	//pop off null character
	buffer[table].getFromBack();
	//is it a float next?
	int f = buffer[table].getFromBack();
	float d = 0;
	if(f == 2)
		data = buffer[table].getFloatFromBack();
	else
		data = buffer[table].getFromBack();

	//get column number
	int col_num = buffer[table].getFromBack();

	/***WILL NEED TO BE GENERATED DYNAMICALLY***/
	//case statement on table #
	switch(table) {
		case: 0
			table_zero.items[col_num].data = d;
		break;
		/*case: 1
			table_one.items[col_num].data = d;
		break;*/
		default:
			//do nothing
	}

	return dp;
}

void flush_buffer(int table) {
	buffer[table].clear();
}

void finish_record(int table) {
	while(buffer[table].getSize() != buffer[table].getCapacity())
		buffer_pop(table);
	flush_buffer(table);
}
