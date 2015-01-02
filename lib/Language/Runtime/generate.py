import ijson
from pprint import pprint
import sys

#	An enum representing integer values 
#	mapped to table data types
class Type(object):
	integer = 0
	string = 1
	char = 2
	floater = 3

num_tables = 1

table_structs = [];

with open('example/runtime.json') as json_data:
    parser = ijson.parse(json_data)
    #num_tabs = ijson.items(json_data, 'tables.number')
    #num = (o for o in num_tabs)
    #for number in num:
    #	num_tables = number
    #parser = ijson.parse(json_data)

    #	Find number of tables - for some reason, method above
    #	results in an invalid JSON exception when try to use
    #	the parser
    i = 0
    for prefix, event, value in parser:
    	if prefix == ("tables.number"):
    		num_tables = int(value)
    		print "There are " + str(num_tables) + " tables."
    		#	add num_tables amount of lists
    		for x in range (0, num_tables):
    			table_structs.append([])

    	while i < num_tables:
    		if prefix == ("tables." + str(i) + ".item.type"):
    			print "In table " + str(i) + ", " + value
    			val = value
    			if val == "int":
    				x = 0
    			elif val == "string":
    				x = 1
    			elif val == "char":
    				x = 2
    			elif val == "float":
    				x = 3
    			else:
    				x = 4
    			table_structs[i].append(x)

    		i = i + 1
    	i = 0

print table_structs

cfile = open('example/runtime_base.ino', "r+")
#	Creates this file - this is output!
new_cfile = open('example/runtime/runtime.ino', "w")

hfile = open('example/runtime_base.h', "r+")
#	Creates this file - this is output!
new_hfile = open('example/runtime/runtime.h', "w")

#	Skip forward to where buffer setup is
while True:
	line = cfile.readline()
	new_cfile.write(line)
	if "Scout.setup" in line:
		break

def generate_buffers(cfile, table_num):
	#	one line per table, placed in setup()
	#	format is "buffers[i].init(field_"i"_number*258);"
	for n in range(0, table_num):	
		cfile.write("\tbuffers[" + str(n) + "].init(field_"
			+ str(n) + "_number*258);\n")

generate_buffers(new_cfile, num_tables)

while True:
	line = cfile.readline()
	new_cfile.write(line)
	if "(table)" in line:
		break

def buffer_pop(cfile, table_num):
	#	generates switch statement in runtime.ino's 
	#	buffer_pop(). Based on format of case 0 - swap 
	#	table_0 for table_i
	for n in range(0, table_num):
		cfile.write("		case " + str(n) + ": {\n \
				table_" + str(n) + ".items[col_num].type = f;\n \
	        if(f == 3)\n \
	        	strcpy(table_" + str(n) + ".items[col_num].s, s);\n \
	        else if(f == 2)\n \
	          	table_" + str(n) + ".items[col_num].f = d;\n \
	        else\n \
	          	table_" + str(n) + ".items[col_num].i = c;\n \
	    	}\n \
			break;\n \
			")

buffer_pop(new_cfile, num_tables)

def broadcast_line(cfile, table_num, table_struct):
	for x in range(0, len(table_struct)):
		cfile.write("\t\t\tSerial.println(\"Table " 
			+ str(table_num) + ", Item " + str(x) + ":\");\n")
		cfile.write("\t\t\tSerial.println(table_" + str(table_num) + 
			".items[" + str(x) + "].")
		if table_struct[x] == Type.floater:
			cfile.write("f);")
		elif table_struct[x] == Type.string:
			cfile.write("s);")
		elif table_struct[x] == Type.integer:
			cfile.write("i);")
		elif table_struct[x] == Type.char:
			cfile.write("s);")
		else:
			cfile.write("ERROR")
		cfile.write("\n")

def broadcast_table(cfile, table_num, table_struct):
	#	generates switch statement in runtime.ino's 
	#	broadcast_table(). Based on structure of table
	#	Since not even close to production, dump out to Serial
	for x in range(0, table_num):
		cfile.write("\t\tcase " + str(x) + ": {\n")
		broadcast_line(new_cfile, x, table_structs[x])
		cfile.write("\t\t}\n\t\tbreak;\n")

while True:
	line = cfile.readline()
	new_cfile.write(line)
	if "(table)" in line:
		break

broadcast_table(new_cfile, num_tables, table_structs)

while True:
    line = cfile.readline()
    if line == '':
        break
    else:
    	new_cfile.write(line)

cfile.close()

print "C file generation finished."

def preprocessor_vars(hfile, table_num, table_structs):
	#	Generates number for runtime.h's "table_number"
	# 	Generates field_i_numbers based on number of items
	hfile.write("#define table_number " + str(table_num)
		+ "\n")
	for x in range(0, table_num):
		hfile.write("#define field_" + str(x) + "_number " +
			str(len(table_structs[x])) + "\n")

while True:
	line = hfile.readline()
	new_hfile.write(line)
	if "define peripherals" in line:
		break

preprocessor_vars(new_hfile, num_tables, table_structs)

def table_struct(hfile, table_num, table_structs):
	#	Generates one unique struct for each table
	#	Based on number of items in table, and named in 
	#	format "tablei"
	for x in range(0, table_num):
		hfile.write("struct table" + str(x) + " { \n \
	struct table_item items [field_"
			+ str(x) + "_number];\n} \
table_" + str(x) + ";\n\n")

while True:
	line = hfile.readline()
	new_hfile.write(line)
	if "/* Table structs begin here*/" in line:
		break

table_struct(new_hfile, num_tables, table_structs)

while True:
	line = hfile.readline()
	if line == '':
		break
	else:
		new_hfile.write(line)

hfile.close()
new_hfile.close()

#sys.exit()

def concatenate_files(cfile, other_file):
	#	attaches other_file to end of cfile
	cfile.write("/*	********************************* \n" \
		+ "SensorNet Compiler output: \n*/")
	while True:
		temp = other_file.readline()
		if temp == '':
			break
		else:
			cfile.write(temp)

sncFile = open('example/snc.c')

while True:
	line = sncFile.readline()
	if "Global Variables" in line:
		break

#concatenate the SNC and the runtime.ino/new_cfile
concatenate_files(new_cfile, sncFile)

sncFile.close()
new_cfile.close()