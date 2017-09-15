#import <CoreFoundation/CoreFoundation.h>
#import <flipper/flipper.h>

int main(int argc, const char * argv[]) {
    
    carbon_attach_hostname("localhost");
    
    gpio.write(1, 1);
    
    return 0;
}
