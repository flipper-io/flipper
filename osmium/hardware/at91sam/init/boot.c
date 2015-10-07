#include <flipper/flipper.h>

#include <platform/at91sam.h>

__attribute__ ((constructor)) void at91sam_init(void) {
	
	AT91C_BASE_MC -> MC_FMR = AT91C_MC_FWS_1FWS;
	
	AT91C_BASE_WDTC -> WDTC_WDMR = AT91C_WDTC_WDDIS;
	
	AT91C_BASE_PMC -> PMC_MOR = ((AT91C_CKGR_OSCOUNT & (0x06 << 8)) | AT91C_CKGR_MOSCEN);
	
	while(!(AT91C_BASE_PMC -> PMC_SR & AT91C_PMC_MOSCS));
	
	AT91C_BASE_PMC -> PMC_PLLR = (AT91C_CKGR_DIV & 14 ) | (AT91C_CKGR_PLLCOUNT & (28 << 8)) | (AT91C_CKGR_MUL & (72 << 16));
	
	while(!(AT91C_BASE_PMC -> PMC_SR & AT91C_PMC_LOCK));
	
	while(!(AT91C_BASE_PMC -> PMC_SR & AT91C_PMC_MCKRDY));
	
	AT91C_BASE_PMC -> PMC_MCKR |= AT91C_PMC_PRES_CLK_2;
	
	while(!(AT91C_BASE_PMC -> PMC_SR & AT91C_PMC_MCKRDY));
	
	AT91C_BASE_PMC -> PMC_MCKR |= AT91C_PMC_CSS_PLL_CLK;
	
	while(!(AT91C_BASE_PMC -> PMC_SR & AT91C_PMC_MCKRDY));
	
}

void _delay_ms(unsigned long time) {
	
	for (volatile unsigned int i = 0; (i < (F_CPU / 10250) * (time)); i ++);
	
}