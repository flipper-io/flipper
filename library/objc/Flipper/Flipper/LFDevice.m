//
//  LFDevice.m
//  Flipper
//
//  Created by George Morgan on 12/27/16.
//  Copyright © 2016 Flipper. All rights reserved.
//

#import "LFDevice.h"
#include <flipper.h>
#include <flipper/carbon.h>

@implementation LFDevice

+ (void) attach {
    lf_error_pause();
    flipper_attach();
}

+ (void) attachDevice:(NSString *)name withHostname:(NSString *)hostname {
    lf_error_pause();
    carbon_attach_hostname([hostname cStringUsingEncoding:NSUTF8StringEncoding]);
}

@end
