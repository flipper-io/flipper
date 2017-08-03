#import <CoreFoundation/CoreFoundation.h>
#import <flipper/flipper.h>

int main(int argc, const char * argv[]) {
    
    flipper.attach();
    
    struct _lf_device *device = lf_device_create(NULL, NULL);
    lf_attach(device);
    lf_attach(device);
    printf("There are %zu devices attached.\n", lf_ll_count(lf_attached_devices));
    lf_detach(device);
    printf("There are %zu devices attached.\n", lf_ll_count(lf_attached_devices));
    
    return 0;
}
