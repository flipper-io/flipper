#import <CoreFoundation/CoreFoundation.h>
#import <flipper/flipper.h>

int main(int argc, const char * argv[]) {
    
    //carbon_attach_hostname("localhost");
    //carbon_attach();
    
    flipper.attach();
    //led.configure();
    led.rgb(0, 0, 10);
    
    return 0;
}
