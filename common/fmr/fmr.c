#define __private_include__

#include <fmr/fmr.h>

#include <platform/fmr.h>

#include <platform/fmr.h>

#include <platform/fmr.h>

#include <fs/crc.h>

#include <led/led.h>

#include <usart/usart.h>

/* ~ Global scratch space for FMR processing. ~ */

fmr_packet fmrpacket;

/* ~ The target with whom we are currently communicating. ~ */

struct _target *sender;

/* ~ The implementation of the virtual interface for the FMR driver. ~ */

const struct _fmr fmr = {
	
	fmr_configure,
	
	fmr_bind,
	
	fmr_invoke,
	
	fmr_resolve
	
};

/* ~ Master invocation function. ~ */

uint32_t fmr_parse(const struct _target *sender) {
	    
	/* ~ Compare the checksums of the packets to ensure the data was sent successfully. ~ */
	
	uint16_t cs = checksum((void *)(&fmrpacket.recipient), fmrpacket.header.length - sizeof(struct _fmr_header));
	
	uint32_t retval = 0;
	
	/* ~ If the checksums are different, then we have a problem. ~ */
	
	if (cs != fmrpacket.header.checksum) {
		
		/* ~ Set the status led to its error color to alert the user of a problem. ~ */
		
		led_set_rgb(LED_COLOR_ERROR);
		
		/* ~ Skip the call. ~ */
		
		goto end;
		
	}
	
	struct _target *target = (void *)(fmr_access_array(fmrpacket.recipient.target));
    
	/* ~ If all is well, perform the function call. ~ */
    
	retval = target -> call();
    
end:
	
	/* ~ Return whatever we received back to the device that sent us a message. ~ */
	
	sender -> bus -> push(&retval, sizeof(uint32_t));
	
	return 0;
	
}

/* ~ Send a constructed packet to its target. ~ */

void fmr_broadcast(void) {
	
	sender -> bus -> push(&fmrpacket, (fmrpacket.header.length));
	
}

/* ~ Retrieve a packet from its sender. ~ */

void fmr_retrieve(void) {
	
	/* ~ If the bus is synchronous, we can load in an entire packet at once. ~*/
	
	if (sender -> bus -> synchronous)
		
		sender -> bus -> pull(&fmrpacket, sizeof(fmr_packet));
	
	/* ~ If the bus is asynchronous, we have to load it in chunks. ~ */
	
	else {
        		
		/* ~ Wait for synchronization. ~ */
		
		while (sender -> bus -> get() != 0xFE);
		
		/* ~ Load the header of the packet. ~ */
		
		for (unsigned i = 1; i < sizeof(struct _fmr_header); i ++) ((uint8_t *)(&fmrpacket.header))[i] = sender -> bus -> get();
        
		/* ~ Load the body of the packet. ~ */
		
		for (unsigned i = 0; i < (fmrpacket.header.length - sizeof(struct _fmr_header)); i ++) ((uint8_t *)(&fmrpacket.recipient))[i] = sender -> bus -> get();
		
		/* ~ Flush any remaining data. ~ */
		
		while (sender -> bus -> ready()) { (void) sender -> bus -> get(); }
		
	}
	
}