#include <flipper/is25lp.h>
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

void is25lp_wait_ready(void) {
	uint8_t wait[] = { IS25LP_RDSR };
	while (1) {
		spi_write(wait, sizeof(wait));
		if (spi_get() & (1 << IS25LP_SR_WIP)) {
			spi_end();
			break;
		}
		spi_end();
	}
}

#define is25lp_sector_to_page(sector) (sector*IS25LP_SECTOR_SIZE)

int is25lp_write_sector(uint32_t sector, void *src, uint32_t length) {
	uint32_t _mr = SPI->SPI_MR;
	SPI->SPI_MR = SPI_MR_PCS(FLASH_PCS) | SPI_MR_MSTR | SPI_MR_MODFDIS;
	/* Erase the sector. */
	uint8_t erase[] = { IS25LP_SER, (sector >> 16) & 0xFF, (sector >> 8) & 0xFF, sector & 0xFF };
	spi_write(erase, sizeof(erase));
	/* Wait until ready. */
	is25lp_wait_ready();
	uint32_t page = is25lp_sector_to_page(sector);
	for (int i = 0; (i < IS25LP_SECTOR_SIZE) && length; i++, page++) {
		uint8_t write[] = { IS25LP_PP, (page >> 16) & 0xFF, (page >> 8) & 0xFF, page & 0xFF };
		spi_write(write, sizeof(write));
		if (length >= IS25LP_PAGE_SIZE) {
			spi_write(src, IS25LP_PAGE_SIZE);
			length -= IS25LP_PAGE_SIZE;
			src += IS25LP_PAGE_SIZE;
		} else {
			spi_write(src, length);
			length -= length;
		}
		spi_end();
	}
	SPI->SPI_MR = _mr;
	return lf_success;
}
