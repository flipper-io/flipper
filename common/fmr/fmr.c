#define __private_include__

#include <fmr/fmr.h>

/* ~ Global scratch space for FMR processing. ~ */

fmr_packet fmrpacket;

/* ~ The target with whom we are currently communicating. ~ */

struct _target *sender;

/* ~ Send a constructed packet to its target. ~ */

void fmr_broadcast(void) {
	
	sender -> bus -> push(&fmrpacket, (fmrpacket.header.length));
	
}

/* ~ Retrieve a packet from its sender. ~ */

void fmr_retrieve(uint32_t length) {
	
	/* ~ If the bus is synchronous, we can load in an entire packet at once. ~*/
	
	if (sender -> bus -> synchronous)
		
		sender -> bus -> pull(&fmrpacket, length);
	
	/* ~ If the bus is asynchronous, we have to load it in chunks. ~ */
	
	else {
		
		/* ~ Wait for synchronization. ~ */
		
		while (sender -> bus -> get() != 0xFE);
		
		/* ~ Load the header of the packet. ~ */
		
		for (unsigned i = 1; i < sizeof(struct _fmr_header); i ++) ((char *)(&fmrpacket.header))[i] = sender -> bus -> get();
		
		/* ~ Load the body of the packet. ~ */
		
		for (unsigned i = 0; i < (fmrpacket.header.length) - sizeof(struct _fmr_header); i ++) ((char *)(&fmrpacket.recipient.object))[i] = sender -> bus -> get();
		
		/* ~ Flush any remaining data. ~ */
		
		while (sender -> bus -> ready()) { (void) sender -> bus -> get(); }
		
	}
	
}