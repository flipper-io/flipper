#ifndef __error_strings_h__
#define __error_strings_h__

#ifndef __osmium__

#define ERROR_STRING(msg) msg

#else

#define ERROR_STRING(msg) ""

#endif

#define E_OK_S                   "All clear."
#define E_FMR_PACKET_CRC_S       "FMR packet failed checksum."
#define E_NO_MEM_S               "Out of memory."
#define E_TOO_BIG_S              "Too many arguments."
#define E_FVM_LOAD_S             "FVM already loaded."
#define E_FVM_SYM_S              "FVM symbol not found."
#define E_OPEN_SOCK_S            "Unable to open socket."
#define E_CONN_SOCK_S            "Unable to connect socket."
#define E_FLIPPER_UNBOUND_S      "Flipper is not attached."
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
#define E_FS_OPEN_S              "Failed to open file."
#define E_FS_ADD_LEAF_S          "Failed to create file."
#define E_FS_NO_LEAF_S           "File doesn't exist."

#endif
