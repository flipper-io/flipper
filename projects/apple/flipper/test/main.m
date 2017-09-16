#import <CoreFoundation/CoreFoundation.h>
#import <flipper/flipper.h>

int main(int argc, const char * argv[]) {
    
    carbon_attach_hostname("localhost");
    
    //carbon_attach();
    led.configure();
    
    return 0;
}
