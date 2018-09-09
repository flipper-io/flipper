//
//  LFDevice.h
//  Flipper
//
//  Created by George Morgan on 12/27/16.
//  Copyright © 2016 Flipper. All rights reserved.
//

#import "LED.h"
#import <Foundation/Foundation.h>

@interface LFDevice : NSObject {
}

@property(readonly) LFLED *led;

+ (void)attach;
+ (void)attachDevice:(NSString *)name withHostname:(NSString *)hostname;

@end
