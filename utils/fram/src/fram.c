#define __private_include__
#include <flipper.h>

int main(int argc, char *argv[]) {

    /* Check args. */
    if (argc < 2) {
        fprintf(stderr, "Please provide a path to the file to load.\n");
        exit(EXIT_FAILURE);
    }

    /* Open file. */
    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        fprintf(stderr, "Failed to open file for reading.\n");
        exit(EXIT_FAILURE);
    }

    /* Attach flipper. */
    struct _lf_device *_device = flipper.attach();

    /* Obtain file size. */
    size_t fsize = 0;
    fseek(fp, 0L, SEEK_END);
    fsize = ftell(fp);
    fseek(fp, 0L, SEEK_SET);

    /* Create file buffer. */
    uint8_t *fbuf = malloc(fsize);
    if (!fbuf) {
        fprintf(stderr, "Failed to allocate buffer memory.\n");
        fclose(fp);
        exit(EXIT_FAILURE);
    }

    /* Read file. */
    fread(fbuf, sizeof(uint8_t), fsize, fp);

    /* Close file. */
    fclose(fp);

    lf_error_pause();
    /* Load the application into RAM. */
    fmr_return value = lf_ram_load(_device, fbuf, fsize);
    if ((int32_t)value == -1) {
        fprintf(stderr, "Failed to load application into RAM.\n");
        free(fbuf);
        exit(EXIT_FAILURE);
    } else {
        printf("Got return value 0x%08x.\n", value);
    }
    lf_error_resume();

    /* Free buffer. */
    free(fbuf);

    return EXIT_SUCCESS;
}