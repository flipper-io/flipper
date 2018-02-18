//
//  LED.m
//  Flipper
//
//  Created by George Morgan on 10/27/16.
//  Copyright © 2016 Flipper. All rights reserved.
//

#import "LED.h"
#import <flipper/led.h>

@implementation LFLED

+ (void) configure {
    led_configure();
}

+ (void) setR:(uint8_t)r G:(uint8_t)g B:(uint8_t)b {
    led_rgb(r, g, b);
}

@end
