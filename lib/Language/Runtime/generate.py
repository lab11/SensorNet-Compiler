import json

cfile = open("runtime.ino", "r+")
while True:
	chunk = cfile.readline()
	if chunk == '':
		break
	else:
		print chunk

generate_buffers(cfile)
buffer_pop(cfile)
broadcast_table(cfile)
preprocessor_vars(cfile)
table_struct(cfile)

def generate_buffers(file, name, cycle_time, first_run):
	#	one line per table, placed in setup()
	#	format is "buffers[i].init(field_"i"_number*258);"
	pass

def buffer_pop(file, table_num):
	#	generates switch statement in runtime.ino's buffer_pop()
	#	Based on format of case 0 - swap table_zero for table_i
	pass

def broadcast_table(file, table_num, table_struct):
	#	generates switch statement in runtime.ino's broadcast_table()
	#	Based on structure of table
	#	Since not even close to production, dump out to Serial
	pass

def preprocessor_vars(file, table_num, table_item_nums):
	#	Generates number for runtime.h's "table_number"
	# Generates field_i_numbers based on number of items

def table_struct(file, table_num, table_item_Nums):
	#	Generates one unique struct for each table
	#	Based on number of items in table, and named in format "tablei"
	pass
