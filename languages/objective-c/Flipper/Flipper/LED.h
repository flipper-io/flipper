//
//  LED.h
//  Flipper
//
//  Created by George Morgan on 10/27/16.
//  Copyright © 2016 Flipper. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface LFLED : NSObject

- (void) setR:(uint8_t)r G:(uint8_t)g B:(uint8_t)b;

@end
