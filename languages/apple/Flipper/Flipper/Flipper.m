//
//  Flipper.m
//  Flipper
//
//  Created by George Morgan on 1/11/18.
//  Copyright © 2018 Flipper. All rights reserved.
//

#import "LFDevice.h"

@implementation Flipper

static LFDevice *globalDevice = nil;

+ (LFDevice *) attach {
    if (globalDevice == nil) {
        globalDevice = [[self alloc] initOverUSB];
    }
    return globalDevice;
}

+ (LFDevice *) attachNetwork:(NSString *)host {
    if (globalDevice == nil) {
        globalDevice = [[self alloc] initOverNetwork:host];
    }
    return globalDevice;
}

+ (LFGPIO *) gpio {
    [Flipper attach];
    return [globalDevice gpio];
}

+ (LFLED *) led {
    [Flipper attach];
    return [globalDevice led];
}

@end
