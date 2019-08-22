#include <stdio.h>
#include <string.h>

#include <flipper.h>

/* Defines the XMODEM flow control bytes. */
#define SOH 0x01
#define EOT 0x04
#define ACK 0x06
#define NAK 0x15
#define ETB 0x17
#define CAN 0x18
#define XLEN 128

/* Defines the layout of an XMODEM packet. */
struct LF_PACKED _xpacket {
    uint8_t header;
    uint8_t number;
    uint8_t _number;
    uint8_t data[XLEN];
    uint16_t crc;
};

/* See utils/copy_x.s for the source of this applet. These are the raw thumb instructions that result from the
 * compilation of the applet. */
uint8_t applet[] = { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x09, 0x48, 0x0A, 0x49, 0x0B, 0x4A, 0x02, 0xE0,
                     0x08, 0xC9, 0x08, 0xC0, 0x01, 0x3A, 0x00, 0x2A, 0xFA, 0xD1, 0x09, 0x48, 0x0A, 0x49, 0x06, 0x4A,
                     0x11, 0x43, 0x01, 0x60, 0x70, 0x47, 0x00, 0xBF, 0xAF, 0xF3, 0x00, 0x80, 0xAF, 0xF3, 0x00, 0x80,
                     0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x00, 0x00, 0x00,
                     0x04, 0x0A, 0x0E, 0x40, 0x08, 0x0A, 0x0E, 0x40, 0x01, 0x00, 0x00, 0x5A };

/* Place the applet in RAM somewhere far away from the region used by the SAM-BA. */
#define _APPLET IRAM_ADDR + 0x800
#define _APPLET_STACK _APPLET
#define _APPLET_ENTRY _APPLET + 0x04
#define _APPLET_DESTINATION _APPLET + 0x30
#define _APPLET_SOURCE _APPLET_DESTINATION + 0x04
#define _APPLET_PAGE _APPLET_SOURCE + 0x04
#define _APPLET_WORDS _APPLET_PAGE + 0x04
#define _PAGEBUFFER _APPLET + sizeof(applet)

#define EFC_CLB 0x09
#define EFC_SGPB 0x0B
#define EFC_GGPB 0x0D

#define REGADDR(reg) ((uint32_t)(uintptr_t)&reg)

/* Defines the number of times communication will be retried. */
#define RETRIES 4

int sam_read(void *dst, uint32_t length) {
    struct _lf_device *dev = lf_get_selected();

    void *ptr;
    size_t size = 128;
    lf_assert(lf_malloc(dev, size, &ptr), E_MALLOC, "failed to allocate");
    while (length) {
        size_t len = (length >= size) ? size : length;
        lf_assert(uart0_read(ptr, len), E_UNIMPLEMENTED, "failed to read");
        lf_assert(lf_pull(dev, dst, ptr, len), E_UNIMPLEMENTED, "failed to pull");
        dst += len;
        length -= len;
    }

    lf_assert(lf_free(dev, ptr), E_MALLOC, "failed to free");
    return lf_success;
fail:
    lf_assert(lf_free(dev, ptr), E_MALLOC, "failed to free");
    return lf_error;
}

int sam_write(void *src, uint32_t length) {
    struct _lf_device *dev = lf_get_selected();

    void *ptr;
    size_t size = 128;
    lf_assert(lf_malloc(dev, size, &ptr), E_MALLOC, "failed to allocate");
    while (length) {
        size_t len = (length >= size) ? size : length;
        lf_assert(lf_push(dev, ptr, src, len), E_UNIMPLEMENTED, "failed to push");
        lf_assert(uart0_write(ptr, len), E_UNIMPLEMENTED, "failed to write");
        src += len;
        length -= len;
    }

    lf_assert(lf_free(dev, ptr), E_MALLOC, "failed to free");
    return lf_success;
fail:
    lf_assert(lf_free(dev, ptr), E_MALLOC, "failed to free");
    return lf_error;
}

/* Instructs the SAM-BA to jump to the given address. */
void sam_ba_jump(uint32_t address) {
    char buffer[11];
    sprintf(buffer, "G%08X#", address);
    sam_write(buffer, sizeof(buffer) - 1);
}

/* Instructs the SAM-BA to write a word to the address provided. */
void sam_ba_write_word(uint32_t destination, uint32_t word) {
    char buffer[20];
    sprintf(buffer, "W%08X,%08X#", destination, word);
    sam_write(buffer, sizeof(buffer) - 1);
}

uint8_t getbyte(void)
{
    uint32_t retries = 0;
    while (!uart0_ready() && retries++ < 8)
        ;
    return uart0_get();
}

/* Instructs the SAM-BA to read a byte from the address provided. */
uint8_t sam_ba_read_byte(uint32_t source) {
    char buffer[12];
    sprintf(buffer, "o%08X,#", source);
    sam_write(buffer, sizeof(buffer) - 1);
    return getbyte();
}

/* Instructs the SAM-BA to write a byte from the address provided. */
void sam_ba_write_byte(uint32_t destination, uint8_t byte) {
    char buffer[20];
    sprintf(buffer, "O%08X,%02X#", destination, byte);
    sam_write(buffer, sizeof(buffer) - 1);
}

/* Instructs the SAM-BA to read a word from the address provided. */
uint32_t sam_ba_read_word(uint32_t source) {
    char buffer[12];
    sprintf(buffer, "w%08X,#", source);
    sam_write(buffer, sizeof(buffer) - 1);
    uint8_t retries = 0;
    while (!uart0_ready() && retries++ < 8)
        ;
    uint32_t result = 0;
    sam_read(&result, sizeof(uint32_t));
    return result;
}

/* Writes the given command and argument into the EFC0->EEFC_FCR register. */
void sam_ba_write_efc_fcr(uint8_t command, uint32_t arg) {
    sam_ba_write_word(REGADDR(EFC0->EEFC_FCR), (EEFC_FCR_FKEY_PASSWD | EEFC_FCR_FARG(arg) | command));
}

/* Moves data from the host to the device's RAM using the SAM-BA and XMODEM protocol. */
int sam_ba_copy(uint32_t destination, void *src, uint32_t length) {

    uint16_t crc;
    char ack[1];
    char buffer[20];
    int tries = 0;

    do {
        lf_assert(uart0_reset(), E_UNIMPLEMENTED, "failed to reset");
        sprintf(buffer, "S%08X,%08X#", destination, length);
        sam_write(buffer, sizeof(buffer) - 1);
        usleep(10000);
        sam_read(ack, sizeof(ack));
        if (!memcmp(ack, (const uint8_t[]){ 'C' }, 1)) break;

    } while (tries++ < 4);

    lf_assert(tries < 4, E_UNIMPLEMENTED, "Failed to get CTS ACK. (0x%02x)", ack[0]);

    /* Calculate the number of packets needed to perform the transfer. */
    int i = 0;

    char c;

    while (length) {

        uint32_t len = (length > XLEN) ? XLEN : length;

        /* Construct the XMODEM packet. */
        struct _xpacket packet = { SOH, (i + 1), ~(i + 1), { 0 }, 0x0000 };
        /* Copy the chunk of data into the packet. */
        memcpy(packet.data, (void *)(src + (i * XLEN)), len);
        /* Calculate the checksum of the data and write it to the packet in little endian format. */
        lf_crc(packet.data, sizeof(packet.data), &crc);
        packet.crc = little(crc);

        /* Transfer the packet to the SAM-BA. */
        sam_write(&packet, sizeof(packet));

        c = getbyte();

        if (c == CAN) {
            lf_assert(false, E_TIMEOUT, "XMODEM cancelled transfer.");
            return lf_error;
        }

        lf_assert(c == ACK, E_UNIMPLEMENTED, "Failed to get ACK. (0x%02X)", c);

        /* Decrement the length appropriately. */
        length -= len;
        i++;

        usleep(1000);
    }

    /* Send end of transmission. */
    uart0_put(EOT);

    c = getbyte();

    lf_assert(c == ACK, E_UNIMPLEMENTED, "Failed to get EOT ACK. (0x%02X)", c);

    return lf_success;

fail:
    return lf_error;
}

/* Computes the greatest integer from the result of the division of x by y. */
#define lf_ceiling(x, y) ((x + y - 1) / y)

void *load_page_data(FILE *firmware, size_t size) {
    size_t pages = lf_ceiling(size, IFLASH0_PAGE_SIZE);
    uint8_t *raw = (uint8_t *)malloc(pages * IFLASH0_PAGE_SIZE);
    for (size_t i = 0; i < pages; i++) {
        for (size_t j = 0; j < IFLASH0_PAGE_SIZE; j++) {
            uint8_t c = fgetc(firmware);
            if (!feof(firmware)) {
                raw[((i * IFLASH0_PAGE_SIZE) + j)] = c;
            } else {
                raw[((i * IFLASH0_PAGE_SIZE) + j)] = 0;
            }
        }
    }
    /* Close the file. */
    fclose(firmware);
    return raw;
}

int enter_normal_mode(void) {
    int tries = 0;
    char ack[3];

    do {
        lf_assert(uart0_reset(), E_UNIMPLEMENTED, "failed to reset");
        sam_write("N#", 2);
        usleep(1000);
        sam_read(ack, sizeof(ack));
        if (!memcmp(ack, (const uint8_t[]){ '\n', '\r' }, 3)) return lf_success;

        /* If we failed the first time around, enter DFU. */
        if (!tries) { sam_enter_dfu(); }

    } while (tries++ < 4);

    sam_write("N#", 2);

fail:
    return lf_error;
}

extern void lf_connect_debug_server(void);

int main(int argc, char *argv[]) {

    void *pagedata = NULL;
    FILE *firmware = NULL;

    /* Ensure the correct argument count. */
    if (argc < 2) {
        lf_debug("Please provide a path to the firmware.\n");
        return EXIT_FAILURE;
    }

    // lf_set_debug_level(LF_DEBUG_LEVEL_ALL);

    // Go to fdebug
    lf_connect_debug_server();

    lf_debug("\r\n\r\nFDEBUG - 1.0\r\n");

    /* Attach to a Flipper device. */
    lf_assert(carbon_attach(), E_NO_DEVICE, "failed to attach device");
    carbon_select_u2(lf_get_selected());

    /* Open the firmware image. */
    firmware = fopen(argv[1], "rb");
    lf_assert(firmware, E_UNIMPLEMENTED, "The file being opened, '%s', does not exist.\n", argv[1]);

    /* Determine the size of the file. */
    fseek(firmware, 0L, SEEK_END);
    size_t firmware_size = ftell(firmware);
    fseek(firmware, 0L, SEEK_SET);

    /* Reset the 4S. */
    sam_reset();

    /* Enter normal mode. (Values are sent as binary.)*/
    lf_debug(KYEL "Entering device firmware update mode." KNRM);
    lf_assert(enter_normal_mode(), E_UNIMPLEMENTED, "Failed to enter normal mode.");
    lf_debug(KGRN " Successfully entered DFU mode." KNRM);

    /* Ensure the security bit is clear. */
    lf_debug(KYEL "Checking security bit." KNRM);
    sam_ba_write_efc_fcr(EEFC_FCR_FCMD_GGPB, 0);
    lf_assert((sam_ba_read_word(REGADDR(EFC0->EEFC_FRR)) & 0x01) == 0, E_UNIMPLEMENTED,
              KRED "The device's security bit is set." KNRM);
    lf_debug(KGRN " Security bit is clear." KNRM);

    /* Move the copy applet into RAM. */
    lf_debug("Uploading copy applet.");
    lf_assert(sam_ba_copy(_APPLET, applet, sizeof(applet)), E_UNIMPLEMENTED, KRED "Failed to upload copy applet." KNRM);
    lf_debug(KGRN " Successfully uploaded copy applet." KNRM);

    /* Write the stack address into the applet. */
    sam_ba_write_word(_APPLET_STACK, IRAM_ADDR + IRAM_SIZE);
    /* Write the entry address into the applet. */
    sam_ba_write_word(_APPLET_ENTRY, _APPLET + 0x09);
    /* Write the destination of the page data into the applet. */
    sam_ba_write_word(_APPLET_DESTINATION, IFLASH0_ADDR);
    /* Write the source of the page data into the applet. */
    sam_ba_write_word(_APPLET_SOURCE, _PAGEBUFFER);

    /* Obtain a linear buffer of the firmware precalculated by page. */
    pagedata = load_page_data(firmware, firmware_size);

    /* Calculate the number of pages to send. */
    size_t pages = lf_ceiling(firmware_size, IFLASH0_PAGE_SIZE);
    /* Send the firmware, page by page. */
    for (size_t page = 0; page < pages; page++) {
        /* Print the page count. */
        lf_debug("Uploading page %zu / %zu. (%.2f%%)", page + 1, pages, ((float)(page + 1)) / pages * 100);
        fflush(stdout);
        /* Copy the page. */
        int e = sam_ba_copy(_PAGEBUFFER, (void *)(pagedata + (page * IFLASH0_PAGE_SIZE)), IFLASH0_PAGE_SIZE);
        lf_assert(e, E_UNIMPLEMENTED, KRED "Failed to upload page %zu of %zu." KNRM, page + 1, pages);

        /* Write the page number into the applet. */
        sam_ba_write_word(_APPLET_PAGE, EEFC_FCR_FARG(page));
        /* Execute the applet to load the page into flash. */
        sam_ba_jump(_APPLET);

        /* Wait until the EFC has finished writing the page. */
        uint8_t retries = 0, fsr = 0;
        while (!((fsr = sam_ba_read_byte(REGADDR(EFC0->EEFC_FSR))) & 1) && retries++ < 4) {
            if (fsr & 0xE) { lf_debug(KRED "Flash write error on page %zu.\n" KNRM, page); }
        }

        retries = 0;
        /* Clear the progress message. */
        if (page < pages - 1) lf_debug("\33[2K\r");
    }

    /* Print statistics about the memory usage. */
    lf_debug(KGRN "\n Successfully uploaded all pages. %zu bytes used. (%.2f%% of flash)\n" KNRM, firmware_size,
           (float)firmware_size / IFLASH0_SIZE * 100);

    /* Set GPNVM1 to boot from flash memory. */
    sam_ba_write_efc_fcr(EEFC_FCR_FCMD_SGPB, 0x01);

    lf_debug("Checking GPNVM1 bit.\n");
    sam_ba_write_efc_fcr(EEFC_FCR_FCMD_GGPB, 0);
    uint8_t retries = 0;
    if (!(sam_ba_read_byte(REGADDR(EFC0->EEFC_FRR)) & (1 << 1)) && retries++ < RETRIES) {
        if (retries > RETRIES) {
            lf_debug(KRED " GPNVM1 bit is not set.\n" KNRM);
            return EXIT_FAILURE;
        }
        /* Set GPNVM1 to boot from flash memory. */
        sam_ba_write_efc_fcr(EEFC_FCR_FCMD_SGPB, 0x01);
        /* Read the state of the GPNVM bits. */
        sam_ba_write_efc_fcr(EEFC_FCR_FCMD_GGPB, 0);
    }
    lf_debug(KGRN "The device's GPNVM1 bit is set.\n" KNRM);

    if (argc > 2) {
        if (!strcmp(argv[2], "verify")) {
            lf_debug("\nVerifying flash contents.\n");
            uint32_t errors = 0, perrors = 0, total = lf_ceiling(firmware_size, sizeof(uint32_t));
            uint8_t retries = 0;
            for (uint32_t i = 0; i < total; i++) {
                uint32_t addr = IFLASH0_ADDR + (i * sizeof(uint32_t));
                if ((i % (IFLASH0_PAGE_SIZE / sizeof(uint32_t)) == 0)) {
                    lf_debug(" Checking address 0x%08x (page %lu)->%s\n", addr,
                           i / (IFLASH0_PAGE_SIZE / sizeof(uint32_t)), (!perrors) ? KGRN "GOOD" KNRM : KRED "BAD" KNRM);
                }
                uint32_t word = sam_ba_read_word(addr);
                uint32_t _word = *(uint32_t *)(pagedata + (i * sizeof(uint32_t)));
                uint8_t match = ((uint16_t)word == (uint16_t)_word);
                // lf_debug("0x%08x: 0x%08x (0x%08x)->%s\n", addr, word, _word, (match) ? KGRN "GOOD" KNRM : KRED "BAD"
                // KNRM);
                if (!match && retries < RETRIES) {
                    if (retries == 0) {
                        perrors++;
                        errors++;
                    }
                    retries++;
                    continue;
                }
                retries = 0;
                perrors = 0;
            }
            lf_debug("Verification complete. %s%i word errors detected.\n\n" KNRM, (errors) ? KRED : KGRN, errors);
        }
    }

    free(pagedata);

    /* Go back to FMR mode. */
    sam_exit_dfu();

    lf_debug("Resetting the CPU.");
    sam_reset();
    lf_debug(KGRN " Successfully reset the CPU.\n" KNRM "----------------------");

#warning Go back to FMR baud here.

    lf_debug(KGRN "\nSuccessfully uploaded new firmware.\n" KNRM);

    return EXIT_SUCCESS;

fail:
    if (pagedata) free(pagedata);
    return EXIT_FAILURE;
}
