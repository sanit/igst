//
//  MethodViewController.h
//  igst
//
//  Created by Sanit Sirisawatvatana on 5/5/18.
//  Copyright © 2018 Sanit Sirisawatvatana. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface MethodViewController : UIViewController
@property (weak, nonatomic) IBOutlet UITextView *methodCode;

@property (nonatomic, strong) NSString *selectorName ;
@property (nonatomic, strong) NSString *className ;

@end
