#ifndef __error_strings_h__
#define __error_strings_h__

#ifndef __osmium__

#define ERROR_STRING(...) __VA_ARGS__

#else

#define ERROR_STRING(...) ""

#endif

#define E_OK_S                   "No error."
#define E_FMR_PACKET_CRC_S       "FMR packet failed checksum."
#define E_NO_MEM_S               "Out of memory."
#define E_TOO_BIG_S              "Too many arguments."
#define E_FVM_LOAD_S             "FVM already loaded."
#define E_FVM_SYM_S              "FVM symbol not found."
#define E_OPEN_SOCK_S            "Unable to open socket."
#define E_CONN_SOCK_S            "Unable to connect socket."
#define E_FLIPPER_UNBOUND_S      "No device present."
#define E_FLIPPER_NOT_FOUND_S    "Device not found for name given."
#define E_HID_MANAGER_S          "HID manager could not be loaded."
#define E_HID_NO_DEV_S           "HID manager could not find the device."
#define E_HID_TOO_MANY_S         "There are too many HID devices attached."
#define E_HID_OPEN_DEV_S         "HID manager unable to open device."
#define E_HID_DISCONN_DEV_S      "Device was disconnected."
#define E_HID_WRITE_S            "HID manager unable to write to device."
#define E_HID_TIMEOUT_S          "HID manager time out."
#define E_IOKIT_DICT_S           "IOKit dictionary error."
#define E_DL_NOT_FOUND_S         "DL not found."
#define E_DL_LOAD_S              "DL load error."
#define E_DL_LOADED_S            "DL already loaded."
#define E_FS_OPEN_S              "No such file."
#define E_FS_ADD_LEAF_S          "Failed to create file."
#define E_FS_NO_LEAF_S           "File doesn't exist."
#define E_UNIMPLEMENTED_S        "Feature not yet implemented."

#define ERROR_STRING_ARRAY E_OK_S, E_FMR_PACKET_CRC_S, E_NO_MEM_S, E_TOO_BIG_S, E_FVM_LOAD_S, E_FVM_SYM_S, E_OPEN_SOCK_S, E_CONN_SOCK_S, E_FLIPPER_UNBOUND_S, E_FLIPPER_NOT_FOUND_S, \
						   E_HID_MANAGER_S, E_HID_MANAGER_S, E_HID_NO_DEV_S, E_HID_TOO_MANY_S, E_HID_OPEN_DEV_S, E_HID_DISCONN_DEV_S, E_HID_WRITE_S, E_HID_TIMEOUT_S, E_IOKIT_DICT_S, \
						   E_DL_NOT_FOUND_S, E_DL_LOAD_S, E_FS_OPEN_S, E_FS_ADD_LEAF_S, E_FS_NO_LEAF_S, E_UNIMPLEMENTED_S

extern char *error_messages[];

#endif
