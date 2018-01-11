//
//  LFDevice.h
//  Flipper
//
//  Created by George Morgan on 1/11/18.
//  Copyright © 2018 Flipper. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "LFGPIO.h"
#import "LFLED.h"

@interface LFDevice : NSObject {
    LFGPIO *_gpio;
    LFLED *_led;
}

@property (strong) LFGPIO *gpio;
@property (strong) LFLED *led;

- (LFDevice *) initOverUSB;
- (LFDevice *) initOverNetwork:(NSString *)host;

@end

@interface Flipper : NSObject

+ (LFDevice *) attach;
+ (LFDevice *) attachNetwork:(NSString *)host;

+ (LFGPIO *) gpio;
+ (LFLED *) led;

@end
