//
//  LFLED.m
//  Flipper
//
//  Created by George Morgan on 1/11/18.
//  Copyright © 2018 Flipper. All rights reserved.
//

#import "LFLED.h"
#import <flipper/led.h>

@implementation LFLED

- (void) r:(int)r g:(int)g b:(int)b {
    led.rgb(r, g, b);
}

@end
