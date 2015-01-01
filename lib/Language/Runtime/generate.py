import ijson
from pprint import pprint
import sys

i = 0
with open('example/runtime.json') as json_data:
    parser = ijson.parse(json_data)
    for prefix, event, value in parser:
    	print prefix
    	print event
    	print value
    print d
    print i
    i += 1

sys.exit()

cfile = open('example/runtime_base.h', "r+")
while True:
	chunk = cfile.readline()
	if chunk == '':
		break
	else:
		print chunk

new_cfile = open('example/runtime.ino')

hfile = open('example/runtime_lib.h', "r+")
while True:
	chunk = hfile.readline()
	if chunk == '':
		break
	else:
		print chunk

new_hfile = open('example/runtime.h')

generate_buffers(cfile)
buffer_pop(cfile)
broadcast_table(cfile)
preprocessor_vars(cfile)
table_struct(cfile)
concatenate_files(cfile, sncFile)

def generate_buffers(cfile, n):
	#	one line per table, placed in setup()
	#	format is "buffers[i].init(field_"i"_number*258);"
	cfile.write("buffers[" + n + "].init(field_" \
		+ n + "number*258);")
	pass

def buffer_pop(cfile, table_num):
	#	generates switch statement in runtime.ino's 
	#	buffer_pop(). Based on format of case 0 - swap 
	#	table_0 for table_i
	cfile.write("		case " + table_num + ": { \
				table_" + table_num + ".items[col_num].type = f; \
        if(f == 3) \
        	strcpy(table_" + table_num + ".items[col_num].s, s); \
        else if(f == 2) \
          table_" + table_num + ".items[col_num].f = d; \
        else \
          table_" + table_num + ".items[col_num].i = c; \
    } \
		break; \
		")
	pass

def broadcast_table(cfile, table_num, table_struct):
	#	generates switch statement in runtime.ino's 
	#	broadcast_table(). Based on structure of table
	#	Since not even close to production, dump out to Serial
	for x in range(0, table_num):
		cfile.write("Serial.println(table_" + table_num + 
			".items[" + x + "].")
		if table_struct[x] == "f":
			cfile.write("f);")
		elif table_struct[x] == "s":
			cfile.write("s);")
		elif table_struct[x] == "d":
			cfile.write("d);")
		elif table_struct[x] == "c":
			cfile.write("c);")
		else:
			cfile.write("ERROR")

def preprocessor_vars(hfile, table_num, table_item_nums):
	#	Generates number for runtime.h's "table_number"
	# 	Generates field_i_numbers based on number of items
	for x in range(0, table_num):
		hfile.write("#define field_" + x + "_number" +
			table_item_nums[x])

def table_struct(hfile, table_num, table_item_Nums):
	#	Generates one unique struct for each table
	#	Based on number of items in table, and named in 
	#	format "tablei"
	for x in range(0, table_num):
		hfile.write("struct table" + table_num + "{ \
			struct table_item items [field_"
			+ table_item_nums[x] + "_items]; \n } \
			table_" + table_item_nums[x] + ";")

def concatenate_files(cfile, other_file):
	#	attaches other_file to end of file
	cfile.write("/*	********************************* \n" \
		+ "SensorNet Compiler output: \n*/")
	while True:
		temp = other_file.readline()
		cfile.write(temp)
		break