//
//  main.m
//  FlipperTest
//
//  Created by George Morgan on 10/27/16.
//  Copyright © 2016 Flipper. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <flipper/libflipper.h>

int main(int argc, const char * argv[]) {
	@autoreleasepool {
        LFDevice *flipper = [[LFDevice alloc] initWithName:@"flipper"];
        LFLED *led = [flipper led];
        [led configure];
        [led setR:0 G:0 B:0];
	}
    return 0;
}
