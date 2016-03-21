#include <flipper/flipper.h>
#include <error/error.h>

#include <IOKit/IOKitLib.h>
#include <IOKit/hid/IOHIDLib.h>
#include <IOKit/hid/IOHIDDevice.h>
#include <unistd.h>

// http://developer.apple.com/technotes/tn2007/tn2187.html


struct rawhid_struct {
	IOHIDDeviceRef ref;
	int disconnected;
	uint8_t *buffer;
	int buffer_used;
	int buffer_report_id;
};


static void unplug_callback(void *hid, IOReturn ret, void *ref)
{
	// This callback can only be called when the "run loop" (managed by macos)
	// is run.  If the GUI is running it when idle, this will get called
	// automatically.  If not, the run loop needs to be run explicitly
	// before checking the result of this function.
	//printf("HID/macos: unplugged callback!\n");
	((struct rawhid_struct *)hid)->disconnected = 1;
}


static void input_callback(void *context, IOReturn result, void *sender,
                           IOHIDReportType type, uint32_t reportID, uint8_t *report,
                           CFIndex reportLength)
{
	struct rawhid_struct *hid;

	//printf("input callback\n");
	if (!context) return;
	hid = (struct rawhid_struct *)context;
	hid->buffer_used = reportLength;
	hid->buffer_report_id = reportID;
	//printf("id = %d, reportLength = %d\n", reportID, (int)reportLength);
	//if (hid->ref == sender) printf("ref matches :-)\n");
	//if (report == hid->buffer) printf("buffer matches :)\n");
}


void * rawhid_open_only1(int vid, int pid, int usage_page, int usage)
{
	IOHIDManagerRef hid_manager;
	CFMutableDictionaryRef dict;
	IOReturn ret;
	CFSetRef device_set;
	IOHIDDeviceRef device_list[256];
	uint8_t *buf;
	struct rawhid_struct *hid;
	int num_devices;

	// get access to the HID Manager
	hid_manager = IOHIDManagerCreate(kCFAllocatorDefault, kIOHIDOptionsTypeNone);
	if (hid_manager == NULL || CFGetTypeID(hid_manager) != IOHIDManagerGetTypeID()) {
		error.raise(E_HID_MANAGER, "HID/macos: unable to access HID manager\n");
		return NULL;
	}
	// configure it to look for our type of device
	dict = IOServiceMatching(kIOHIDDeviceKey);
	if (dict == NULL) {
		error.raise(E_IOKIT_DICT, "HID/macos: unable to create iokit dictionary\n");
		return NULL;
	}
	if (vid > 0) {
		CFDictionarySetValue(dict, CFSTR(kIOHIDVendorIDKey),
		                     CFNumberCreate(kCFAllocatorDefault, kCFNumberIntType, &vid));
	}
	if (pid > 0) {
		CFDictionarySetValue(dict, CFSTR(kIOHIDProductIDKey),
		                     CFNumberCreate(kCFAllocatorDefault, kCFNumberIntType, &pid));
	}
	if (usage_page > 0) {
		CFDictionarySetValue(dict, CFSTR(kIOHIDPrimaryUsagePageKey),
		                     CFNumberCreate(kCFAllocatorDefault, kCFNumberIntType, &usage_page));
	}
	if (usage > 0) {
		CFDictionarySetValue(dict, CFSTR(kIOHIDPrimaryUsageKey),
		                     CFNumberCreate(kCFAllocatorDefault, kCFNumberIntType, &usage));
	}
	IOHIDManagerSetDeviceMatching(hid_manager, dict);

	// now open the HID manager
	ret = IOHIDManagerOpen(hid_manager, kIOHIDOptionsTypeNone);
	if (ret != kIOReturnSuccess) {
		error.raise(E_HID_MANAGER, "HID/macos: Unable to open HID manager (IOHIDManagerOpen failed)\n");
		return NULL;
	}
	// get a list of devices that match our requirements
	device_set = IOHIDManagerCopyDevices(hid_manager);
	if (device_set == NULL) {
		error.raise(E_HID_NO_DEV, "HID/macos: no devices found\n");
		return NULL;
	}
	num_devices = (int)CFSetGetCount(device_set);
	//printf("number of devices found = %d\n", num_devices);
	if (num_devices < 1) {
		CFRelease(device_set);
		error.raise(E_HID_NO_DEV, "HID/macos: no devices found, even though HID manager returned a set\n");
		return NULL;
	}
	if (num_devices > 256) {
		CFRelease(device_set);
		error.raise(E_HID_TOO_MANY, "HID/macos: too many devices, we get confused if more than 256!\n");
		return NULL;
	}
	CFSetGetValues(device_set, (const void **)&device_list);
	CFRelease(device_set);
	// open the first device in the list
	ret = IOHIDDeviceOpen(device_list[0], kIOHIDOptionsTypeNone);
	if (ret != kIOReturnSuccess) {
		error.raise(E_HID_OPEN_DEV, "HID/macos: error opening device\n");
		return NULL;
	}
	// return this device
	hid = (struct rawhid_struct *)malloc(sizeof(struct rawhid_struct));
	buf = (uint8_t *)malloc(0x1000);
	if (hid == NULL || buf == NULL) {
		IOHIDDeviceRegisterRemovalCallback(device_list[0], NULL, NULL);
		IOHIDDeviceClose(device_list[0], kIOHIDOptionsTypeNone);
		error.raise(E_NO_MEM, "HID/macos: Unable to allocate memory\n");
		return NULL;
	}
	hid->ref = device_list[0];
	hid->disconnected = 0;
	hid->buffer = buf;
	hid->buffer_used = 0;

	// register a callback to receive input
	IOHIDDeviceRegisterInputReportCallback(hid->ref, hid->buffer, 0x1000,
	                                       input_callback, hid);

	// register a callback to find out when it's unplugged
	IOHIDDeviceScheduleWithRunLoop(hid->ref, CFRunLoopGetCurrent(), kCFRunLoopDefaultMode);
	IOHIDDeviceRegisterRemovalCallback(hid->ref, unplug_callback, hid);
	return hid;
}

int rawhid_status(void *hid)
{
	if (!hid) return -1;
	// if a GUI is causing the run loop run, this will likely mess it up.  Just
	// comment it out and if the callback still gets called without this, then
	// there's no need to run the run loop here!
	while (CFRunLoopRunInMode(kCFRunLoopDefaultMode, 0, true) == kCFRunLoopRunHandledSource) ;
	if (((struct rawhid_struct *)hid)->disconnected) {
		error.raise(E_HID_DISCONN_DEV, "HID/macos: status: disconnected\n");
		return -1;
	}
	//printf("HID/macos: status: ok\n");
	return 0;
}

void rawhid_close(void *hid)
{
	IOHIDDeviceRef ref;
	
	if (!hid) return;
	ref = ((struct rawhid_struct *)hid)->ref;
	IOHIDDeviceRegisterRemovalCallback(ref, NULL, NULL);
	IOHIDDeviceClose(ref, kIOHIDOptionsTypeNone);
	free(hid);
}

int rawhid_read(void *h, void *buf, int bufsize, int timeout_ms)
{
	struct rawhid_struct *hid;
	int r, len;

	//printf("begin read\n");
	hid = (struct rawhid_struct *)h;
	if (!hid || hid->disconnected) {
		error.raise(E_HID_DISCONN_DEV, "HID/macos: status: disconnected\n");
		return -1;
	}
	while (CFRunLoopRunInMode(kCFRunLoopDefaultMode, 0, true) == kCFRunLoopRunHandledSource) {
		if (hid->buffer_used) {
			len = hid->buffer_used;
			if (len > bufsize) len = bufsize;
			memcpy(buf, hid->buffer, len);
			hid->buffer_used = 0;
			return len;
		}
		if (hid->disconnected) {
			error.raise(E_HID_DISCONN_DEV, "HID/macos: status: disconnected\n");
			return -1;
		}
	}
	r = CFRunLoopRunInMode(kCFRunLoopDefaultMode, (double)timeout_ms / 1000.0, true);
	if (r == kCFRunLoopRunTimedOut) {
		error.raise(E_HID_TIMEOUT, "HID/macos: read timeout\n");
		return 0;
	}
	if (hid->buffer_used) {
		len = hid->buffer_used;
		if (len > bufsize) len = bufsize;
		memcpy(buf, hid->buffer, len);
		hid->buffer_used = 0;
		return len;
	}
	if (hid->disconnected) {
		error.raise(E_HID_DISCONN_DEV, "HID/macos: status: disconnected\n");
		return -1;
	}
	return 0;
	//num = bufsize;
	//ret = IOHIDDeviceGetReport(ref, kIOHIDReportTypeInput, 0, buf, &num);
	//if (!ret) return -1;
	//return num;
}

int rawhid_write(void *hid, const void *buf, int len, int timeout_ms)
{
	IOReturn ret;
	
	if (((struct rawhid_struct *)hid)->disconnected) {
		error.raise(E_HID_DISCONN_DEV, "HID/macos: status: disconnected\n");
		return -1;
	}
	ret = IOHIDDeviceSetReport(((struct rawhid_struct *)hid)->ref,
	                          kIOHIDReportTypeOutput, 0, buf, len);
	if (ret != kIOReturnSuccess) {
		error.raise(E_HID_WRITE, "HID/macos: write error\n");
		return -1;
	}
	return 0;
}
