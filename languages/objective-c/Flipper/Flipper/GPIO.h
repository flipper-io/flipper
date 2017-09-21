//
//  GPIO.h
//  Flipper
//
//  Created by George Morgan on 3/31/17.
//  Copyright © 2017 Flipper. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface GPIO : NSObject

+ (void) set:(int) pin;
+ (void) clear:(int) pin;
+ (void) enable:(int) pin;
+ (void) disable:(int) pin;

@end
