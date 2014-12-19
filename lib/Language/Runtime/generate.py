import json

cfile = open("runtime.ino", "r+")
while True:
	chunk = cfile.readline()
	if chunk == '':
		break
	else:
		print chunk

def generate_function_queue(name, cycle_time, first_run):
	#generates generate_function_queue
	pass
def buffer_pop(table_num):
	#generates switch statement in runtime.ino's buffer_pop()
	pass
def broadcast_table(table_num):
	#generates switch statement in runtime.ino's broadcast_table()
	pass
def gen_table_structs(table_num):
	#generates table_zero, etc. structures in runtime.h
	pass
