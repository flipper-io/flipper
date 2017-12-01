#define __private_include__
#include <flipper/is25lp.h>
#include <flipper/atsam4s/atsam4s.h>
#include <flipper/spi.h>

int is25lp_configure(void) {
	/* Create a pinmask for the NVM pins. */
	const unsigned int NVM_PIN_MASK = FLASH_PCS_PIN;
	/* Configure the NVM SPI peripheral. 8 bits per transfer. SPI mode 3. SCK = MCK. */
	SPI->SPI_CSR[FLASH_PCS] = SPI_CSR_SCBR(8) | SPI_CSR_DLYBCT(1) | SPI_CSR_BITS_8_BIT | SPI_CSR_NCPHA | SPI_CSR_CSAAT;
	/* Disable PIOA interrupts on the peripheral pins. */
	PIOA->PIO_IDR = NVM_PIN_MASK;
	/* Disable the peripheral pins from use by the PIOA. */
	PIOA->PIO_PDR = NVM_PIN_MASK;
	/* Hand control of the peripheral pins to peripheral A. */
	PIOA->PIO_ABCDSR[0] &= ~NVM_PIN_MASK;
	PIOA->PIO_ABCDSR[1] &= ~NVM_PIN_MASK;
	/* DISABLE MODE FAULT DETECTION ON NPCS0 FOR THE LOVE OF GOD. Datasheet page 699. */
	SPI->SPI_MR = SPI_MR_PCS(FLASH_PCS) | SPI_MR_MSTR | SPI_MR_MODFDIS;
	return lf_success;
}

int is25lp_wait_ready(void) {
	uint8_t wait[] = { IS25LP_RDSR };
	while (1) {
		spi_push(wait, sizeof(wait));
		if (spi_get() & (1 << IS25LP_SR_WIP)) continue;
		break;
	}

}

int is25lp_write_sector(uint32_t sector, void *source) {
	SPI->SPI_MR = SPI_MR_PCS(FLASH_PCS);
	/* Erase the sector. */
	uint8_t erase[] = { IS25LP_SER, (sector >> 16) & 0xFF, (sector >> 8) & 0xFF, sector & 0xFF };
	spi_push(erase, sizeof(erase));
	/* Wait until ready. */
	SPI->SPI_MR = SPI_MR_PCS(USER_PCS);
	return lf_success;
}
