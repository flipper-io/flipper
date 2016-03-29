#include <flipper/platform/platform.h>

void fiq_handler(void) {



}

void default_handler(void) {



}

void spurious_handler(void) {



}

void boot(void) {

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

	AT91C_BASE_AIC -> AIC_SVR[0] = (unsigned)(&fiq_handler);

	for (int i = 1; i < 31; i ++) AT91C_BASE_AIC -> AIC_SVR[i] = (unsigned)(&default_handler);

	AT91C_BASE_AIC -> AIC_SPU  = (unsigned)(&spurious_handler);

}
