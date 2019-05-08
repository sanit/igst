//
//  ViewController.m
//  igst
//
//  Created by Sanit Sirisawatvatana on 1/5/18.
//  Copyright © 2018 Sanit Sirisawatvatana. All rights reserved.
//

#import "ViewController.h"
#include "gstpub.h"
#include "lex.h"
#include "lib.h"
#include "sysdep.h"
#include "instancevars.h"

extern void printObject(OOP oop) ;

UITextView *gst_outputview ;

@interface ViewController ()

@end

@implementation ViewController
///var/containers/Bundle/Application/29DC46B2-CB99-439F-8EB8-10AA32E5CE31/igst.app/gst.im
- (void)viewDidLoad {
    [super viewDidLoad];
    [self setTitle: _fileName];
    _inputView.text = @"'Hello' printNl!" ;
    [_inputView setFont:[UIFont fontWithName:@"Courier" size:18]] ;
    [_outputView setFont:[UIFont fontWithName:@"Courier" size:18]];
    gst_outputview = _outputView ;
    [self loadCode] ;
    initSmalltalk();
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}


- (IBAction)runCode:(id)sender {
    [_inputView resignFirstResponder] ;
    NSString *sourceCode = [NSString stringWithFormat:@"%@!",[_inputView text]];
//    OOP result=evalExpr([sourceCode UTF8String]) ;
//    printObject(result) ;
    pushCString([sourceCode UTF8String]) ;
    parseStream();
    popStream(false);
}

- (IBAction)printIt:(id)sender {
     [_inputView resignFirstResponder] ;
    NSString *sourceCode = [NSString stringWithFormat:@"(%@) printNl!",[_inputView text]];
    pushCString([sourceCode UTF8String]) ;
    parseStream();
    popStream(false);
}

- (IBAction)clearOutput:(id)sender {
    _outputView.text = @"" ;
}

- (IBAction)clearAll:(id)sender {
    _inputView.text = @"" ;
    _outputView.text = @"" ;
}

- (IBAction)browseCode:(id)sender {
}

- (IBAction)saveCode:(id)sender {
    NSError *error;
    NSString *stringToWrite = _inputView.text ;
    NSString *filePath = [[NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES) firstObject] stringByAppendingPathComponent: _fileName];
    [stringToWrite writeToFile:filePath atomically:YES encoding:NSUTF8StringEncoding error:&error];
}


- (void) loadCode{

    NSError *error;
    NSString *filePath = [[NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES) firstObject] stringByAppendingPathComponent: _fileName];
    NSString *fileContents = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:&error];
    if (fileContents == nil){
        _inputView.text = @"'Hello' printNl!" ;
    } else {
        _inputView.text = fileContents ;
    }
    //NSLog(@"Error : %@",error) ;
}

- (NSString *) loadFile: (NSString *) fileName {
    NSBundle *mainBundle = [NSBundle mainBundle];
    NSString *filePath = [mainBundle pathForResource: fileName ofType:@"st"];
    NSStringEncoding encoding;
    NSError *error;
    NSString *fileContents = [[NSString alloc] initWithContentsOfFile:filePath
                                                         usedEncoding:&encoding
                                                                error:&error];
    return fileContents ;
}

@end
