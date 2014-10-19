// The SNC <-> C Compiler  Interface

// Gather Block Statements 

#define GET_GATHER_ID();

void spawn(void (*x)(void), int sem_id);
void join(void (*x)(void), int sem_id, int timeout);

// Timer OP

void schedule_relative(void (*x)(void),time_t seconds); 
void schedule_absolute(void (*x)(void),time_t time); 

time_t get_time(); 

// 
#define GET_EVENT_IRQ()

// Interrupts should be event 
void schedule_interrupt(void (*x)(void),int int_num,  int type);

// Local ID Ops

int get_node_id();

// temprature

int configure_temprature();
int get_temprature();

// led

int set_led(bool state); 
int get_led();

// table ops 

void store_value(int table, int val_num, void * buf, size_t buf_len); 
void finish_record(int table); 

/* The SNC <-> OS interface
 
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
