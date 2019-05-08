//
//  MethodViewController.m
//  igst
//
//  Created by Sanit Sirisawatvatana on 5/5/18.
//  Copyright © 2018 Sanit Sirisawatvatana. All rights reserved.
//

#import "MethodViewController.h"
#include "igstlib.h"
#include "gstpub.h"
#include "lex.h"
#include "lib.h"
#include "sym.h"
#include "sysdep.h"
#include "instancevars.h"


@interface MethodViewController ()

@end

@implementation MethodViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    [self setTitle: [NSString stringWithFormat:@"%@>%@",_className,_selectorName]];
    initSmalltalk();
    _methodCode.text = [self getMethodSource] ;
    // Do any additional setup after loading the view.
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

/*
#pragma mark - Navigation

// In a storyboard-based application, you will often want to do a little preparation before navigation
- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender {
    // Get the new view controller using [segue destinationViewController].
    // Pass the selected object to the new view controller.
}
*/

-(NSString *)getMethodSource {
    char *command = (char*)malloc(100) ;
    sprintf(command,"^((%s methodDictionary at:#%s) methodSourceString)!",[_className UTF8String],[_selectorName UTF8String]) ;
    char *strObj = evalSmalltalk(command) ;
    NSString *nsObject = [NSString stringWithUTF8String:strObj];

    return nsObject ;
}

@end
