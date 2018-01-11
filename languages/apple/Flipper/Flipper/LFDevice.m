//
//  LFDevice.m
//  Flipper
//
//  Created by George Morgan on 1/11/18.
//  Copyright © 2018 Flipper. All rights reserved.
//

#import "LFDevice.h"
#include <flipper.h>

@implementation LFDevice

- (LFDevice *) initOverUSB {
    self = [super init];
    flipper_attach();
    return self;
}

- (LFDevice *) initOverNetwork:(NSString *)host {
    self = [super init];
    carbon_attach_hostname([host UTF8String]);
    return self;
}

@end
