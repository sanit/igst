//
//  main.m
//  igst
//
//  Created by Sanit Sirisawatvatana on 1/5/18.
//  Copyright © 2018 Sanit Sirisawatvatana. All rights reserved.
//
#import <UIKit/UIKit.h>
#import "AppDelegate.h"
#include "gstpub.h"

int main(int argc, char * argv[]) {
    argv[1] = "-Q" ;
    smalltalkArgs(argc, argv);
    //initSmalltalk();
    @autoreleasepool {
        return UIApplicationMain(argc, argv, nil, NSStringFromClass([AppDelegate class]));
    }
}
