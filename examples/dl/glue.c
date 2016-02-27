extern void main();

/* ~ Program initialization function. ~ */

__attribute__((section(".init"))) void __init() {
	
	/* ~ Call the main function. ~ */
	
	main();
	
}