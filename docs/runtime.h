// The SNC --> C Compiler  Interface

// ------------ Asynchronous calls for gather -------------

/* spawn(void (*func)(void), int sem_id);
 *   func - The asynchronously executed callback, it should either return 
 *          immidiately, or start a data gather actions and block until it's 
 *          complete. 
 *   sem_id - The ID of the semaphor that will be taken before func() is 
 *            executed, and will be released when it is done. 
 *
 * join(void (*func)(bool timed_out), int sem_id, int timeout);
 *   func - The callback to be executed when the semaphor is completely 
 *          released, which is also where error handling can go. 
 *   sem_id - the semaphor on which join() will block, waiting until there
 *            are no other threads that own the semaphor, or till the timeout
 *            has passed. 
 *   timeout - the number of seconds to wait before deciding one of the spawned
 *             threads has failed and returning to the user.
 *
 * This interface is meant to be used as follows 
 *
 *   GATHER{ 
 *      SAVE foo() AS foo,
 *      SAVE bar() AS bar
 *   } INTO TABLE_FOO_BAR; 
 *
 * will be turned into something vaguely like: 
 *
 *   void foo_call() { 
 *   	  // Calls foo().
 *        // Waits for it to block before continuing execution on 
 *        //   the main thread.
 *        // When control is returned uses store_value() to place
 *        //   the resulting value in the next record to be created
 *   }
 *  
 *   void bar_call() { 
 *   	// the same, but for bar() 
 *   }
 *  
 *   void complete_gather(bool t){
 *  	if(t) {
 *  	    // Handle failure
 *  	}
 *  	// call finish_record(TABLE_FOO_BAR)
 *   }
 * 
 * and a chunk somewhere in the main control flow path for that event:
 *
 *   flush_buffer(TABLE_FOO_BAR); 
 *   spawn(&foo_call,sem1);
 *   spawn(&bar_call,sem1);
 *   join(&complete_gather,sem1,30);
 *
 * Note that if foo() and bar() don't block, then the main thread will 
 * only regain control when they're done, and all the calls are executed 
 * one after another.
 *
 * Just to be super clear, spawn will *always* execute the function immidiately
 * even if there's only a single thread at a time, so if foo() and bar() block
 * then execution looks like this: 
 *
 *       			      Main	Foo()	Bar()
 *      			       |
 *      			       |
 *       spawn(&foo)  ---------------- '          ,
 *       					  |
 *       Foo() blocks ---------------- ,	  '
 *       			       |
 *       spawn(&bar)  ---------------- '		 ,
 *       						 |
 *       Bar() blocks ---------------- ,		 '			
 *      			       |
 *      			       |
 *       Join()       ---------------- '
 *      
 *      
 *       Bar() finishes waiting ------                   ,
 *                                                       |
 *       Bar() finishes executing ----                   '
 *      
 *       
 *       Foo() finishes waiting ------            ,
 *                                                |
 *                                                |
 *       Foo() finishes executing ---- ,          '
 *       Control is returned to the    |
 *       main thread so                |
 *       complete_gather() can run     |
 *    
 *
 * However if Foo() and Bar() don't block (this should only be the case if
 * they are always going to return very quickly):
 *
 *        			      Main	Foo()	Bar()
 *      			       |
 *      			       |
 *       spawn(&foo)  ---------------- '          ,
 *       					  |
 *       Foo() finishes executing ---- ,          '
 *       			       |
 *       spawn(&bar)  ---------------- '		 ,
 *       						 |
 *       Bar() finishes executing ---- ,                 '
 *       Join()       ---------------- |
 *       Control is returned to the    |
 *       main thread so                |
 *       complete_gather() can run     |
 *
 * */


typedef unsigned char bool;
typedef unsigned int size_t;
typedef unsigned long int interval_t; 
typedef unsigned long int time_t; // For the moment standard posix time with a 
		 	          // granularity of one second. 

void spawn(void (*func)(void), int sem_id);
void join(void (*func)(bool), int sem_id, int timeout);

// ------------ Timer OPs -------------


// Schedules a function to be called some number of seconds into the future
void schedule_relative(void (*x)(void), interval_t seconds); 

// Schedules a function to be called at some absolute time
void schedule_absolute(void (*x)(void), time_t time); 

// Gets the current absolute time. 
time_t Get_Time(); 

// ------------ Timer OPs -------------

// Call a function when an interrupt with a particular number is triggered. 
void schedule_interrupt(void (*x)(void), int int_num);

// ------------ Local Data OPs -------------

// Returns an integer unique to a specific node 
int Get_Node_Id();

// ------------ Temprature Sensor OPs -------------

// Returns the temperature in Celsius
float Get_Temperature();

// ------------ Brightness Sensor OPs -------------

// Returns the current relative brightness
float Get_Brightness();

// ------------ LED Output OPs -------------

// Sets the state of the external LED  
int Set_Led(bool state); 

// Gets the current state of the external LED 
int Get_Led();

// ------------ Output OPs -------------

void Print_Message(const char* message);

// ------------ Email Output OPs -------------

void send_email(const char* address, const char * message, int max_len) ;

// ------------ Table OPs -------------

/* We assume that all tables are statically allocated, and that at runtime 
 * each will only try to write a single record entry at a time. 
 *
 * store_value(int table, int val_num, void * buf, size_t buf_len); 
 *   table - the table number 
 *   val_num - the number for the value you're storing, basically the column 
 *             number. 
 *   buf - pointer to the buffer containing the value
 *   buf_len - the number of bytes to copy into the buffer. 
 *
 *   Will store a value into a particular table's record buffer.
 *
 * flush_buffer(int table)
 *   table - the table number 
 *
 *   Clears the buffer associated with a particular table, marking all the 
 *   fields as unwritten. 
 *
 *  finish_record(int table)
 *   table - the table number 
 *
 *   takes all the values in the buffer and writes them as a single row to
 *   the table. Then it calls flush_buffer()
 */

void store_value(int table, int val_num, void * buf, size_t buf_len); 
void flush_buffer(int table);
void finish_record(int table); 

/* The SNC --> OS/Runtime interface
 
data Config_Data = {
  devices::[used_devices]
  tables::[table_info]
}

data Table_Info = {
  name:: String,
  id::Int,
  records::[Record_info]
]

data record_info = {
  name::String,
  id::Int,
  data_type::String,
  length::Int
}

*/ 
