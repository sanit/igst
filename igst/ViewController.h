//
//  ViewController.h
//  igst
//
//  Created by Sanit Sirisawatvatana on 1/5/18.
//  Copyright © 2018 Sanit Sirisawatvatana. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface ViewController : UIViewController
@property (weak, nonatomic) IBOutlet UITextView *inputView;
@property (weak, nonatomic) IBOutlet UITextView *outputView;
- (IBAction)runCode:(id)sender;
- (IBAction)printIt:(id)sender;
- (IBAction)clearOutput:(id)sender;
- (IBAction)clearAll:(id)sender;
- (IBAction)browseCode:(id)sender;
- (IBAction)saveCode:(id)sender;

@property (nonatomic, strong) NSString *fileName ;
@end

