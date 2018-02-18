#!/usr/bin/env python2

import sys
from elftools.elf.elffile import ELFFile
from elftools.dwarf.enums import ENUM_DW_TAG
from os.path import splitext

class Function:
	def __init__(self, typ, name, ret, params):
		self.type = typ
		self.name = name
		self.ret = ret
		self.parameters = params
	def __str__(self):
		params = ", ".join(map(str, self.parameters))
		return "%s %s(%s)" % (self.type, self.name, params)

class Parameter:
	def __init__(self, typ, name):
		self.type = typ
		self.name = name
	def __str__(self):
		return "%s %s" % (self.type, self.name)

def get_die_at_offset(cu, offset):
	top = cu.get_top_DIE()
	for d in cu.iter_DIEs():
		if d.offset == offset:
			return d

def get_parameters_from_die(cu, die):
	parameters = []
	for child in die.iter_children():
		if child.tag == "DW_TAG_formal_parameter":
			name = child.attributes["DW_AT_name"].value
			# print "n: " + name
			tdie = get_die_at_offset(cu, child.attributes["DW_AT_type"].value)
			if not tdie:
				print("Failed to get type die at offset 0x%x" % child.attributes["DW_AT_type"].value)
				return parameters
			if tdie.tag == "DW_TAG_typedef":
				tdie = get_die_at_offset(cu, tdie.attributes["DW_AT_type"].value)
			if "DW_AT_name" in tdie.attributes:
				typ = tdie.attributes["DW_AT_name"].value
			else:
				typ = "void*"
			# print "t: " + typ
			p = Parameter(typ, name)
			parameters.append(p)
	return parameters

def generate_c(modulename, outputname, functions):
	outc = open(outputname, "w")

	ctemplate = """\
#include <flipper.h>

struct _$MODULE$ {
$STRUCTDEF$
};

enum { $TAGS$ };

$FUNCTIONPROTOS$

$VARIABLES$

#ifdef ATSAM4S

const char _fmr_app_name[] __attribute__((section(".name"))) = "$MODULE$";

#define JT_SECTION __attribute__((section(".module")))

#else /* ATSAM4S */

#define JT_SECTION

extern unsigned char $MODULE$_bin[];
extern unsigned $MODULE$_bin_len;

LF_MODULE(_module, "$MODULE$", "$DESCRIPTION$", &$MODULE$_bin, &$MODULE$_bin_len);

$FUNCTIONS$

#endif /* ATSAM4S */

const struct _$MODULE$ _jumptable JT_SECTION = {
$STRUCTBODY$
};
"""
	ctemplate = ctemplate.replace("$MODULE$", modulename)

	functs = []
	struct = []
	tags = []
	for f in functions:
		functs.append(str(f) + ";")
		tags.append("_%s_%s" % (modulename, f.name))
		struct.append("%s (*%s)(%s);" % (f.type, f.name, ", ".join(map(str, f.parameters))))
	ctemplate = ctemplate.replace("$FUNCTIONPROTOS$", "\n\t".join(functs))
	ctemplate = ctemplate.replace("$STRUCTDEF$", "\t" + "\n\t".join(struct))
	ctemplate = ctemplate.replace("$TAGS$", ", ".join(tags))

	functs = []
	struct = []
	for f in functions:
		struct.append("&%s" % f.name)

		args = []
		for p in f.parameters:
			args.append("lf_infer(%s)" % p.name)
		retl = ["lf_void_t", "", "lf_int8_t", "lf_int16_t", "", "lf_int32_t"]
		statement = "lf_invoke(lf_get_current_device(), &_module, %s, %s, lf_args(%s));" % ("_" + modulename + "_" + f.name, retl[f.ret + 1], ", ".join(args))
		if f.type == "void":
			body = statement
			ret = ";"
		else:
			body = ""
			ret = " " + statement
		functs.append("LF_WEAK " + str(f) + " {\n%s\treturn%s\n}\n" % (body, ret))
	ctemplate = ctemplate.replace("$VARIABLES$\n\n", "")
	ctemplate = ctemplate.replace("$STRUCTBODY$", "\t" + ",\n\t".join(struct))
	ctemplate = ctemplate.replace("$FUNCTIONS$", "\n".join(functs))

	outc.write(ctemplate)
	outc.close()

def generate_py(modulename, outputname, functions):
	print("Not yet implemented!")
	sys.exit(1)

def process_file(filename, modulename, language, outputname):
	functions = []

	with open(filename, "rb") as f:
		elffile = ELFFile(f)

		if not elffile.has_dwarf_info():
			print("File has no DWARF info. Compile with -g.")
			sys.exit(1)

		dwarfinfo = elffile.get_dwarf_info()

		funcs_addr = 0
		funcs_size = 0
		vars_addr = 0
		vars_size = 0
		for section in elffile.iter_sections():
			if section.name == ".lf.funcs":
				funcs_addr = section["sh_addr"]
				funcs_size = section["sh_size"]
			if section.name == ".lf.vars":
				vars_addr = section["sh_addr"]
				vars_size = section["sh_size"]

		if funcs_addr == 0:
			outc = open(outputname, "w")
			sys.exit(0)

		# This iterates through all CUs, even the ones without .lf.funcs section
		i = 0
		for cu in dwarfinfo.iter_CUs():
			top = cu.get_top_DIE()
			for child in top.iter_children():
				if child.tag == "DW_TAG_subprogram" and "DW_AT_low_pc" in child.attributes:
					address = child.attributes["DW_AT_low_pc"].value
					if address in range(funcs_addr, funcs_addr + funcs_size):
						name = child.attributes["DW_AT_name"].value
						# print "n: " + name
						if "DW_AT_type" in child.attributes.keys():
							tdie = get_die_at_offset(cu, child.attributes["DW_AT_type"].value)
							if tdie == None:
								print("Failed to get die at offset: " + hex(child.attributes["DW_AT_type"].value))
								return 1
							while tdie.tag == "DW_TAG_typedef":
								tdie = get_die_at_offset(cu, tdie.attributes["DW_AT_type"].value)
							if "DW_AT_name" in tdie.attributes:
								type = tdie.attributes["DW_AT_name"].value
								ret = tdie.attributes["DW_AT_byte_size"].value
							elif "DW_AT_byte_size" in tdie.attributes:
								type = "void *"
								ret = tdie.attributes["DW_AT_byte_size"].value
							else:
								type = "void"
								ret = 0x2
						params = get_parameters_from_die(cu, child)
						functions.append(Function(type, name, ret, params))

		if language.lower() == "c":
			generate_c(modulename, outputname, functions)
		elif language.lower() == "python":
			generate_py(modulename, outputname, functions)
		else:
			print("Invalid language: " + language)
			sys.exit(1)

if __name__ == "__main__":
	if len(sys.argv) >= 5:
		elf = sys.argv[1]
		name = sys.argv[2]
		language = sys.argv[3]
		output = sys.argv[4]
		process_file(elf, name, language, output)
	else:
		print("Usage: fdwarf <input file.elf> <module name> <language [c|py])> <output file [.c|.py]>")
		sys.exit(1)
