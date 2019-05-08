//
//  ObjectViewController.m
//  igst
//
//  Created by Sanit Sirisawatvatana on 3/5/18.
//  Copyright © 2018 Sanit Sirisawatvatana. All rights reserved.
//

#import "ObjectViewController.h"
#import "SelectorViewController.h"
#include "igstlib.h"
#include "gstpub.h"
#include "lex.h"
#include "lib.h"
#include "sym.h"
#include "sysdep.h"
#include "instancevars.h"

extern void printObject(OOP oop) ;

@interface ObjectViewController ()

@end

@implementation ObjectViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    [self setTitle: @"Class Hierarchy"];
    initSmalltalk();
    _objectData = [self getAllClasses] ;
    
    // Uncomment the following line to preserve selection between presentations.
    // self.clearsSelectionOnViewWillAppear = NO;
    
    // Uncomment the following line to display an Edit button in the navigation bar for this view controller.
    // self.navigationItem.rightBarButtonItem = self.editButtonItem;
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

#pragma mark - Table view data source

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView {
    return 1;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
    return [_objectData count];
}


- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:@"cell" forIndexPath:indexPath];
    
    // Configure the cell...
    cell.textLabel.text = _objectData[indexPath.row] ;
    return cell;
}


/*
// Override to support conditional editing of the table view.
- (BOOL)tableView:(UITableView *)tableView canEditRowAtIndexPath:(NSIndexPath *)indexPath {
    // Return NO if you do not want the specified item to be editable.
    return YES;
}
*/

/*
// Override to support editing the table view.
- (void)tableView:(UITableView *)tableView commitEditingStyle:(UITableViewCellEditingStyle)editingStyle forRowAtIndexPath:(NSIndexPath *)indexPath {
    if (editingStyle == UITableViewCellEditingStyleDelete) {
        // Delete the row from the data source
        [tableView deleteRowsAtIndexPaths:@[indexPath] withRowAnimation:UITableViewRowAnimationFade];
    } else if (editingStyle == UITableViewCellEditingStyleInsert) {
        // Create a new instance of the appropriate class, insert it into the array, and add a new row to the table view
    }   
}
*/

/*
// Override to support rearranging the table view.
- (void)tableView:(UITableView *)tableView moveRowAtIndexPath:(NSIndexPath *)fromIndexPath toIndexPath:(NSIndexPath *)toIndexPath {
}
*/

/*
// Override to support conditional rearranging of the table view.
- (BOOL)tableView:(UITableView *)tableView canMoveRowAtIndexPath:(NSIndexPath *)indexPath {
    // Return NO if you do not want the item to be re-orderable.
    return YES;
}
*/

/*
#pragma mark - Navigation

// In a storyboard-based application, you will often want to do a little preparation before navigation
- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender {
    // Get the new view controller using [segue destinationViewController].
    // Pass the selected object to the new view controller.
}
*/

- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender {
    // Get the new view controller using [segue destinationViewController].
    // Pass the selected object to the new view controller.
    SelectorViewController *d = [segue destinationViewController] ;
    NSIndexPath *indexPath = [self.tableView indexPathForSelectedRow] ;
    d.className = _objectData[indexPath.row] ;
}


-(NSArray*)getAllClasses {
    NSArray *objects ;
    NSArray *sortObjs ;
    char *command = "| o | \n o:= 'Object'.\n Object allSubclasses do: [ :x | o := o ,',',(x printString)]. ^o!" ;
//    char *command = " | o | \n o := String new. Magnitude selectors do: [ :x | o := o ,',',(x printString)]. ^o! " ;
    char *strObj = evalSmalltalk(command) ;
    NSString *nsObject = [NSString stringWithUTF8String:strObj];
    objects = [nsObject  componentsSeparatedByString:@","];
    
    NSMutableArray* newArray = [[NSMutableArray alloc] initWithObjects: nil];
    
    for (NSString* s in objects)
    {
        if (![s hasSuffix: @"class"]) {
            [newArray addObject:s];
        }
    }
    sortObjs = [newArray sortedArrayUsingSelector:@selector(localizedCaseInsensitiveCompare:)];
    //printObject(result) ;
    
    return sortObjs ;
}

@end
