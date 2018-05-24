#!/usr/bin/env python3

import sys
from elftools.elf.elffile import ELFFile
from elftools.dwarf.enums import ENUM_DW_TAG
from os.path import splitext
import os

class Parameter:
	def __init__(self, typ, name):
		self.type = typ
		self.name = name
	def __str__(self):
		return "%s %s" % (self.type, self.name)

class Function:
	def __init__(self, typ, name, ret, params):
		self.type = typ
		self.name = name
		self.ret = ret
		self.parameters = params
	def __str__(self):
		if self.parameters:
			params = ", ".join(map(str, self.parameters))
		else:
			params = "void"
		return "%s %s(%s)" % (self.type, self.name, params)

class Module:
	def __init__(self, name, start, end):
		self.name = name
		self.start = start
		self.end = end
		self.funcs = []

def get_die_at_offset(cu, offset):
	for d in cu.iter_DIEs():
		if d.offset == cu.cu_offset + offset:
			return d
	print("Fatal: Failed to find die at offset %s in CU '%s'" % (str(hex(offset)), cu.get_top_DIE().attributes["DW_AT_name"].value))

def get_name(cu, die):
	while "DW_AT_abstract_origin" in die.attributes:
		die = get_die_at_offset(cu, die.attributes["DW_AT_abstract_origin"].value)
	if "DW_AT_name" in die.attributes:
		return die.attributes["DW_AT_name"].value.decode("utf-8")
	print("Fatal: Encountered nameless subprogram at offset %s in CU %s" % (hex(die.offset), cu.get_top_DIE().attributes["DW_AT_name"].value))
	print(die)

def get_bytesize(cu, die):
	while "DW_AT_abstract_origin" in die.attributes:
		die = get_die_at_offset(cu, die.attributes["DW_AT_abstract_origin"].value)
	if "DW_AT_type" in die.attributes:
		tdie = get_die_at_offset(cu, die.attributes["DW_AT_type"].value)
		while tdie.tag == "DW_TAG_typedef":
			tdie = get_die_at_offset(cu, tdie.attributes["DW_AT_type"].value)
		if "DW_AT_byte_size" in tdie.attributes:
			return tdie.attributes["DW_AT_byte_size"].value
	return -1

def get_type(cu, die):
	while "DW_AT_abstract_origin" in die.attributes:
		die = get_die_at_offset(cu, die.attributes["DW_AT_abstract_origin"].value)
	if "DW_AT_type" in die.attributes:
		tdie = get_die_at_offset(cu, die.attributes["DW_AT_type"].value)
		if "DW_AT_name" in tdie.attributes:
			return tdie.attributes["DW_AT_name"].value.decode("utf-8")
		else:
			return "void*"
	return "void"

def get_parameters_from_die(cu, die):
	parameters = []
	for child in die.iter_children():
		if child.tag == "DW_TAG_formal_parameter":
			p = Parameter(get_type(cu, child), get_name(cu, child))
			parameters.append(p)
	return parameters

def generate_c(modules, outdir):

	for m in modules:
		outc = open(os.path.join(outdir, m.name + ".c"), "w")
		ctemplate = """\
#include <flipper.h>

enum { $TAGS$ };

$FUNCTIONPROTOS$

void *$MODULE$_interface[] = {
$STRUCTBODY$
};

LF_MODULE($MODULE$, "$MODULE$", $MODULE$_interface);

$FUNCTIONS$
"""
		functs = []
		struct = []
		tags = []
		for f in m.funcs:
			functs.append(str(f) + ";")
			tags.append("_" + f.name)
			struct.append("%s (*%s)(%s);" % (f.type, f.name, ", ".join(map(str, f.parameters))))
		ctemplate = ctemplate.replace("$FUNCTIONPROTOS$", "\n".join(functs))
		ctemplate = ctemplate.replace("$STRUCTDEF$", "\t" + "\n\t".join(struct))
		ctemplate = ctemplate.replace("$TAGS$", ", ".join(tags))

		functs = []
		struct = []
		for f in m.funcs:
			struct.append("&%s" % f.name)

			args = []
			for p in f.parameters:
				args.append("lf_infer(%s)" % p.name)
			retl = ["lf_void_t", "", "lf_int8_t", "lf_int16_t", "", "lf_int32_t"]
			ftype = retl[f.ret + 1]
			if f.type == "int":
				ftype = "lf_int_t"
			elif f.type == "void *":
				ftype = "lf_ptr_t"
			lf_args = "lf_args(%s)" % ", ".join(args)
			if len(args) == 0:
				lf_args = "NULL"
			if f.type != "void":
				retstatement = "return (%s)retval;" % (f.type)
			else:
				retstatement = ""
			body = "lf_return_t retval;\n\t" + "lf_invoke(lf_get_selected(), \"$MODULE$\", %s, %s, &retval, %s);\n\t%s" % ("_" + f.name, ftype, lf_args, retstatement)
			functs.append("LF_WEAK " + str(f) + " {\n\t%s\n}\n" % body)
		ctemplate = ctemplate.replace("$VARIABLES$\n\n", "")
		ctemplate = ctemplate.replace("$STRUCTBODY$", "\t" + ",\n\t".join(struct))
		ctemplate = ctemplate.replace("$FUNCTIONS$", "\n".join(functs))
		ctemplate = ctemplate.replace("$MODULE$", m.name)
		outc.write(ctemplate)
		outc.close()

def generate_py(modules, outfile):
	print("Not yet implemented!")
	sys.exit(1)

def get_modules(elffile, dwarfinfo):
	modules = []

	for section in elffile.iter_sections():
		if section.name.startswith(".lm."):
			funcs_addr = section["sh_addr"]
			funcs_size = section["sh_size"]
			module = Module(section.name.split(".lm.",1)[1], funcs_addr, funcs_addr + funcs_size)
			modules.append(module)

	# This iterates through all CUs, even the ones without .lf.funcs section
	for cu in dwarfinfo.iter_CUs():
		for die in cu.iter_DIEs():
			for child in die.iter_children():
				if child.tag == "DW_TAG_subprogram" and "DW_AT_low_pc" in child.attributes:
					address = child.attributes["DW_AT_low_pc"].value
					for module in modules:
						if address in range(module.start, module.end):
							params = get_parameters_from_die(cu, child)
							type = get_type(cu, child)
							name = get_name(cu, child)
							bs = get_bytesize(cu, child)
							module.funcs.append(Function(type, name, bs, params))
							break
	return modules

def test():
	file = "/Users/George/Development/flipper/build/atmegau2/atmegau2.elf"
	elf = ELFFile(open(file, "rb"))
	dwarf = elf.get_dwarf_info()
	modules = get_modules(elf, dwarf)
	for m in modules:
		print("Got module %s" % m.name)
		for f in m.funcs:
			print("\t%s" % str(f))

def process_file(filename, language, outdir):
	functions = []

	with open(filename, "rb") as f:
		elffile = ELFFile(f)

		if not elffile.has_dwarf_info():
			print("File has no DWARF info. Compile with -g.")
			sys.exit(1)

		dwarfinfo = elffile.get_dwarf_info()
		modules = get_modules(elffile, dwarfinfo)

		if not os.path.exists(outdir):
			os.makedirs(outdir)

		if language.lower() == "c":
			generate_c(modules, outdir)
		elif language.lower() == "python":
			generate_py(modules, outdir)
		else:
			print("Invalid language: " + language)
			sys.exit(1)

if __name__ == "__main__":
	if len(sys.argv) >= 4:
		elf = sys.argv[1]
		language = sys.argv[2]
		outdir = sys.argv[3]
		process_file(elf, language, outdir)
	else:
		print("Usage: fdwarf <input file.elf> <language [c|py])> <output directory>")
		sys.exit(1)
