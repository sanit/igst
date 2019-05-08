//
//  igstlib.m
//  gst
//
//  Created by Sanit Sirisawatvatana on 24/3/18.
//  Copyright © 2018 Sanit Sirisawatvatana. All rights reserved.
//

#include "instancevars.h"
#include "gstpub.h"
#include "lex.h"
#include "lib.h"
#include "sym.h"
#include "sysdep.h"
#include "instancevars.h"

void gst_output(char *a){
    NSString *result = [NSString stringWithFormat:@"%@",[NSString stringWithUTF8String:a]] ;
    [gst_outputview insertText:result];
}

char* evalSmalltalk(char *command){
    OOP result=evalExpr(command) ;
    int len ;
    len = stringOOPLen(result) ;
    char *strObj = (char*) malloc(len) ;
    strncpy(strObj,oopToObj(result)->data,len) ;
    return strObj ;
}

