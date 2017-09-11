#import <CoreFoundation/CoreFoundation.h>
#import <flipper/flipper.h>

int main(int argc, const char * argv[]) {
    
    flipper.attach();
    
    led.rgb(0, 0, 0);
    
    return 0;
}
