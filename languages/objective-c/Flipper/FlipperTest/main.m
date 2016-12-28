//
//  main.m
//  FlipperTest
//
//  Created by George Morgan on 10/27/16.
//  Copyright © 2016 Flipper. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <Flipper/Flipper.h>

int main(int argc, const char * argv[]) {
	@autoreleasepool {
        LFDevice *flipper = [[LFDevice alloc] initWithName:@"flipper"];
        [[flipper led] setR:5 G:0 B:0];
	}
    return 0;
}
