//
//  GPIO.m
//  Flipper
//
//  Created by George Morgan on 3/31/17.
//  Copyright © 2017 Flipper. All rights reserved.
//

#import "GPIO.h"
#include <flipper/gpio.h>

@implementation GPIO

+ (void) set:(int) pin {
    gpio_write(1 << pin, 0);
}

+ (void) clear:(int) pin {
    gpio_write(0, 1 << pin);
}

+ (void) enable:(int) pin {
    gpio_enable(1 << pin, 0);
}

+ (void) disable:(int) pin {
    gpio_enable(0, 1 << pin);
}

@end
