/*	By Colin Szechy
 */

//default Scout libraries - from the "Bootstrap" example
#include <SPI.h>
#include <Wire.h>
#include <Scout.h>
#include <GS.h>
#include <bitlash.h>
#include <lwm.h>
#include <js0n.h>
#include "runtime.h"
#include "test_header.h"

#include <SleepHandler.h>
//from http://github.com/ivanseidel/LinkedList
#include <LinkedList.h>	
//from http://code.google.com/p/arduino-buffered-serial
#include <ByteBuffer.h> 


//function table global
LinkedList<function> queue;// = LinkedList<function>();

//table global variables (defined in runtime.h)
//tablezero table_zero;

//one-to-one correspondence between buffers and table #'s
ByteBuffer buffers [table_number];

void generate_function_queue(LinkedList<function> &queue);
void check_function_queue(LinkedList<function> &queue);

//	all-important setup 
//	should be constant across devices (?) - we'll see
void setup() {
	//default Scout setup
	Scout.setup("CompilerNet", "v0.01", 1);
	//create linked list table - from http://github.com/ivanseidel/LinkedList
	Serial.println("in setup()");
	/*** Will need to be built dynamically ***/
	//for each table, initialize buffer with proper size
	//one char for column value, one '\0', 
  //four char for float(max), two '/0' to signify the end,
  //254 characters for possible string (instead of four
  //for float(max))
	//e.g. for table 1:
	for(int i = 0; i < table_number; --i)
		buffers[i].init(field_zero_number*258);
	//buffers[1].init(field_one_number*8);

	//Create linked list for function queue 
	queue = LinkedList<function>();
	//generate_function_queue(queue);
  //      Serial.println("Leaving setup()");
  init_snl();
}

//	infinite loop - maintaining/setting up functions, handling data and function operations
void loop() {
        Serial.println("New loop");
	//default Scout looping method - a superclass's method, if you will
	Scout.loop();

	//check function queue and execute functions if neccessary
	check_function_queue(queue);

}

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
  buffers[table].put(val_num);
	//place value that's neccessary by checking first value in buff
	if(*((char*)buff) == 'f') {				//float
                //++buff;
		buffers[table].putFloat(*((float*)(buff+1)));
		buffers[table].put(3);	//four bytes/32 bits - a float
	}
	else if(*((char*)buff) == 's') {		//string
		//put it on in reverse order so easy to read back
		for(char *b = (char*)buff + buf_lens; b >= buff; --b)
			buffers[table].put(*((char*)buff));
		/*while(*buff) {
			buffers[table].put(*buff);
			++buff;
		}*/
		buffers[table].put(2);	//signifying a string in reverse
	}
	else {											//char
		buffers[table].put(*(char*)buff);
		buffers[table].put(1);	//just one byte/8 bits - a char
        }
	//place end char
	buffers[table].put('\0');

}

//pops most recent data piece off of the table's buffer into table, so in reverse
void buffer_pop(int table) {
	//pop off null character
	buffers[table].getFromBack();
	//is it a float next?
	int f = buffers[table].getFromBack();
	float d = 0;
  int c = 0;
  char s[256];
	//string
	if(f == 3) {
		char gotten = buffers[table].getFromBack();
                int j = 0;
		while(gotten != '\0') {
			s[j] = gotten;
			gotten = buffers[table].getFromBack();
                        ++j;
		}
	}
	//float
	else if(f == 2) {
		d = buffers[table].getFloatFromBack();
	}
	//just one char
	else {
		c = buffers[table].getFromBack();
	}

	//get column number
	int col_num = buffers[table].getFromBack();

	/***WILL NEED TO BE GENERATED DYNAMICALLY***/
	//case statement simply on table #
	switch(table) {
		case 0: {
                        table_zero.items[col_num].type = f;
                        
                        if(f == 3)
                          strcpy(table_zero.items[col_num].s, s);
                        else if(f == 2)
                          table_zero.items[col_num].f = d;
                        else
                          table_zero.items[col_num].i = c;
                }
		break;
		/*case 1:	//for table 1
			table_one.items[col_num].data = d;
		break;*/
		default:
    break;
			//do nothing
	}
}

void flush_buffer(int table) {
	buffers[table].clear();
}

void broadcast_table(int table) {
  //generate dynamically based on table #
  switch (table){
    case 0: {
      //empty out to Serial for now
        Serial.println("Add time: ");
        Serial.println(Get_Time());
        Serial.println("Node id: ");
        Serial.println(table_zero.items[0].i);
        Serial.println("Temperature: ");
        Serial.println(table_zero.items[1].i); 
        Serial.println("Time according to funct");
        Serial.println(table_zero.items[2].i);
      }
    break;
    default:
    break;
  }
}

void finish_record(int table) {
	while(buffers[table].getSize() != buffers[table].getCapacity())
		buffer_pop(table);
	flush_buffer(table);  
        broadcast_table(table);
}

//find out where func goes in the queue!
//based on last_init + next
void sort_place_queue(LinkedList<function> &queue, function &func) {
	int i = 0; 
	bool not_found_place = true;
	for(i; not_found_place && (i < queue.size()); ++i) {
		if(queue.get(i).next_time < func.next_time)
			not_found_place = false;
	}
	queue.add(i, func);
}

//completely dynamically generated function queue
//the code provided here is an illustrative example
//and to test compilation
/*void generate_function_queue(LinkedList<function> &queue) {
	//create string of names to be copied into function<queues>
	//braces control scope in C++
        Serial.println("generating function queue");
	//
	{
		function funct;
		//define function name
		//funct.name = "act_assign_gather_weather_data_15";
		//pass function pointer
		funct.func = &act_assign_gather_weather_data_15;
		//place it
                funct.next_time = 5;
                funct.cycle = 5;
		sort_place_queue(queue, funct);
	}
	{
		function funct;
		//funct.name = "record_blk_nodeid_2";
		funct.func = &record_blk_nodeid_2;
                funct.next_time = 5;
                funct.cycle = 5;
		sort_place_queue(queue, funct);
	}
Serial.println("finished generating function queue");

}*/

void check_function_queue(LinkedList<function> &queue) {
    Serial.println("checking function queue");
	//in main, polling to see if any functions should have occured
	int i = 0;
	for(i; i < queue.size(); ++i) {
                Serial.println(i);
								//if function should have already executed
                Serial.println(queue.get(i).next_time);
                Serial.println(Get_Time());
		if(queue.get(i).next_time < Get_Time()) {
			//get and execute function
			function f = queue.get(i);
			queue.remove(i);
			f.func();	//how do we execute this function ptr?
			//replace function
			f.next_time += f.cycle;
			sort_place_queue(queue, f);
		}
	}
  Serial.println("Done checking function queue");
}

void spawn(void (*func)(void), int sem_id) {
        Serial.println("Spawning function");
	++sem_id;
	void();
	//TODO: find in queue so that way can edit last_ran and next
	--sem_id;
}

void join(void (*func)(bool timed_out), int sem_id, int timeout) {
  //currently a no-op
}

//schedules interrupt
//int_num refers to which peripheral to schedule
//valid digital I/O interrupt pins on Pinoccio:
// 0-7: INT4, INT5, INT0, INT1, INT2, INT3, INT6, INT7
// I think:
//INT0 - SCL 			INT1 - SDA 		INT2 - RX1 	INT3 - TX1
//INT4 - D4				INT5 - D5 		INT6 - D7  	INT7 - battery
void schedule_interrupt(void (*x)(void), int num){
	switch (num){
		//Motion Detected
		//we're going to tie this to a button on pin D4
		case 0: {
				attachInterrupt(0, x, RISING);
			break;
		}
		default:
			break;
	}
}

// Schedules a function to be called some number of seconds into the future
void schedule_relative(void (*x)(void), int seconds) {
	function funct;
	funct.func = x;
	funct.cycle = 0;	//disable cycling functionality for now
	//generate the next time for it to execute
	funct.next_time = millis()/1000 + seconds;
	sort_place_queue(queue, funct);
}

void schedule_absolute(void (*x)(void), time_t time) {
	function funct;
	funct.func = x;
	funct.next_time = time;
	funct.cycle = 0; 	//disable cycling functionality for now
	sort_place_queue(queue, funct);
}
