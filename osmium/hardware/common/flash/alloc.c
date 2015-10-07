#define __private_include__

#include <flash/flash.h>

#include <fs/fs.h>

typedef struct _block {
	
	uint32_t size;
	
	fsp next;
	
} block;

/* There is a memory leak in this somewhere. 20 bytes of data is being allocated but not free'd each time it is executed. */

fsp flash_alloc(uint32_t length) {
	
	/* Each of these variables are pointers that store the address of blocks that have been copied in from external memory. */
	
	block *current, *last, *current_smallest, *last_smallest;
	
	fsp _current, _last, _current_smallest, _last_smallest;
	
	uint32_t smallest_size;
	
	uint32_t available;
	
	if (length < sizeof(block) - sizeof(uint32_t)) length = sizeof(block) - sizeof(uint32_t);
	
	for (smallest_size = 0, _current = _free_list, current = flash_dereference(_free_list, sizeof(block)), _last = 0, last = 0; _current; _last = _current, last = current, _current = current -> next, free(current), current = flash_dereference(current -> next, sizeof(block))) {
		
		if ((current -> size) < length) continue;
		
		if ((current -> size) == length) {
			
			if (last) {
				
				last -> next = current -> next;
				
				flash_push(last, sizeof(block), _last);
				
			}
			
			else {
				
				_free_list = current -> next;
				
				flash_push(&_free_list, sizeof(fsp), _FREE_LIST);
				
			}
			
			return (_current + offsetof(block, next));
			
		}
		
		else {
			
			if (smallest_size == 0 || current -> size < smallest_size) {
				
				smallest_size = current -> size;
				
				current_smallest = current;
				
				_current_smallest = _current;
				
				last_smallest = last;
				
				_last_smallest = _last;
				
			}
			
		}
		
	}
	
	if (smallest_size) {
		
		if (smallest_size - length < sizeof(block)) {
			
			if (last_smallest) {
				
				last_smallest -> next = current_smallest -> next;
				
				flash_push(last_smallest, sizeof(block), _last_smallest);
				
				free(last_smallest);
				
			}
			
			else {
				
				_free_list = current_smallest -> next;
				
				flash_push(&_free_list, sizeof(fsp), _FREE_LIST);
				
			}
			
			return (_current_smallest + offsetof(block, next));
			
		}
		
		_current = _current_smallest;
		
		smallest_size -= length;
		
		_current += smallest_size;
		
		last_smallest = (block *)(flash_dereference(_current, sizeof(block)));
		
		_last_smallest = _current;
		
		last_smallest -> size = length;
		
		flash_push(last_smallest, sizeof(block), _last_smallest);
		
		current_smallest -> size = smallest_size - sizeof(uint32_t);
		
		flash_push(current_smallest, sizeof(block), _current_smallest);
		
		free(last_smallest);
		
		return (_last_smallest + offsetof(block, next));
		
	}
	
	available = 2162688 - _break_value;
	
	if (available >= length && available >= length + sizeof(uint32_t)) {
		
		current = (block *)(flash_dereference(_break_value, sizeof(block)));
		
		_current = _break_value;
		
		current -> size = length;
		
		flash_push(current, sizeof(block), _current);
		
		fsp block = (_break_value + offsetof(struct _block, next));
		
		_break_value += length + sizeof(uint32_t);
		
		flash_push(&_break_value, sizeof(fsp), _BREAK_VALUE);
		
		return block;
		
	}
	
	return 0;
	
}

void flash_free(fsp pointer) {
	
	block *current, *last, *new;
	
	fsp _current, _last, _new;
	
	if (pointer == 0) return;
	
	_new = pointer;
	
	_new -= sizeof(uint32_t);
	
	new = (block *)(flash_dereference(_new, sizeof(block)));
	
	new -> next = 0;
	
	flash_push(new, sizeof(block), _new);
	
	if (_free_list == 0) {
		
		if (pointer + new -> size == _break_value) {
			
			_break_value = _new;
			
			flash_push(&_break_value, sizeof(fsp), _BREAK_VALUE);
			
		}
		
		else {
			
			_free_list = _new;
			
			flash_push(&_free_list, sizeof(fsp), _FREE_LIST);
			
		}
		
		return;
		
	}
	
	for (_current = _free_list, current = flash_dereference(_free_list, sizeof(block)), _last = 0, last = 0; _current; _last = _current, last = current, _current = current -> next, current = flash_dereference(current -> next, sizeof(block))) {
		
		if (current < new) continue;
		
		new -> next = _current;
		
		if ((_new + offsetof(block, next)) + new -> size == _current) {
			
			new -> size += current -> size + sizeof(uint32_t);
			
			new -> next = current -> next;
			
		}
		
		flash_push(new, sizeof(block), _new);
		
		if (last == 0) {
			
			_free_list = _new;
			
			flash_push(&_free_list, sizeof(fsp), _FREE_LIST);
			
			return;
			
		}
		
		break;
		
	}
	
	last -> next = _new;
	
	_last = (_last + offsetof(block, next));
	
	if (_last + last -> size == _new) {
		
		last -> size += new -> size + sizeof(uint32_t);
		
		last -> next = new -> next;
		
	}
	
	flash_push(last, sizeof(block), _last);
	
	for (_current = _free_list, current = flash_dereference(_free_list, sizeof(block)), _last = 0, last = 0; current -> next != 0; _last = _current, last = current, _current = current -> next, current = flash_dereference(current -> next, sizeof(block))) free(last);
	
	if ((_last + current -> size) == _break_value) {
		
		if (last == 0) {
			
			_free_list = 0;
			
			flash_push(&_free_list, sizeof(fsp), _FREE_LIST);
			
		}
		
		else {
			
			last -> next = 0;
			
			flash_push(last, sizeof(block), _last);
			
		}
		
		_break_value = _last - sizeof(uint32_t);
		
		flash_push(&_break_value, sizeof(fsp), _BREAK_VALUE);
		
	}
	
}