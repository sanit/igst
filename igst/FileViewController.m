//
//  FileViewController.m
//  igst
//
//  Created by Sanit Sirisawatvatana on 5/5/18.
//  Copyright © 2018 Sanit Sirisawatvatana. All rights reserved.
//

#import "FileViewController.h"
#import "ViewController.h"

@interface FileViewController ()

@end

@implementation FileViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    [self listFile] ;
    
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
    return [_fileData count];
}


- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:@"cell" forIndexPath:indexPath];
    
    // Configure the cell...
    cell.textLabel.text = _fileData[indexPath.row] ;
    cell.detailTextLabel.text = @"15/2/2018" ;
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

// In a storyboard-based application, you will often want to do a little preparation before navigation
- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender {
    // Get the new view controller using [segue destinationViewController].
    // Pass the selected object to the new view controller.
    ViewController *d = [segue destinationViewController] ;
    if ([segue.identifier isEqualToString:@"newSegue"]){
        d.fileName = [NSString stringWithFormat:@"%@%lu.%@",@"prog",[_fileData count]+1,@"gst"] ;
    } else {
        ViewController *d = [segue destinationViewController] ;
        NSIndexPath *indexPath = [self.tableView indexPathForSelectedRow] ;
        d.fileName = _fileData[indexPath.row] ;
    }
}


-(void)listFile {
    //-----> LIST ALL FILES <-----//
    NSError *error;
    //NSLog(@"LISTING ALL FILES FOUND");
    NSMutableArray *items = [[NSMutableArray alloc] init] ;
    int count;
    NSArray *paths= NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES) ;
    NSString *directoryPath = [paths objectAtIndex:0] ;
    NSArray *directoryContent = [[NSFileManager defaultManager] contentsOfDirectoryAtPath:directoryPath error:&error];
    for (count = 0; count < (int)[directoryContent count]; count++)
    {
        //NSLog(@"File %d: %@", (count + 1), [directoryContent objectAtIndex:count]) ;
        //NSString *imgfile = @"gst.im" ;
        NSString *fileName = [directoryContent objectAtIndex:count] ;
        //if (! [fileName isEqualToString:imgfile]){
        if ([[fileName pathExtension] isEqualToString:@"gst"]){
            [items addObject:fileName] ;
        }
    }
    NSArray *sortedFile = [items sortedArrayUsingSelector:@selector(compare:)];
    _fileData = sortedFile ;

//    NSFileManager *fileMgr;
//    NSString *entry;
//    NSString *documentsDir;
//    NSDirectoryEnumerator *enumerator;
//    BOOL isDirectory;
//
//    // Create file manager
//    fileMgr = [NSFileManager defaultManager];
//
//    // Path to documents directory
//    documentsDir = [NSHomeDirectory() stringByAppendingPathComponent:@"Documents"];
//
//    // Change to Documents directory
//    [fileMgr changeCurrentDirectoryPath:documentsDir];
//
//    // Enumerator for docs directory
//    enumerator = [fileMgr enumeratorAtPath:documentsDir];
//
//    // Get each entry (file or folder)
//    while ((entry = [enumerator nextObject]) != nil)
//    {
//        // File or directory
//        if ([fileMgr fileExistsAtPath:entry isDirectory:&isDirectory] && isDirectory)
//            NSLog (@"Directory - %@", entry);
//        else
//            NSLog (@"  File - %@", entry);
//    }
}

-(void)viewWillAppear:(BOOL)animated{
    [super viewWillAppear:animated];
    [self listFile] ;
    [self.tableView reloadData] ;
}

@end
