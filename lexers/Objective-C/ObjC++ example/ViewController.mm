//
//  ViewController.m
//  ObjC++ example
//
//  Created by Libor on 3/13/12.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#import "ViewController.h"

@implementation ViewController
@synthesize iboLabel = _iboLabel;

- (void)viewDidLoad
{
    [super viewDidLoad];
    cppClass = new ExampleClass(0);
	// Do any additional setup after loading the view, typically from a nib.
}

- (void)viewDidUnload
{
    [self setIboLabel:nil];
    [super viewDidUnload];
    // Release any retained subviews of the main view.
}

- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    return (interfaceOrientation != UIInterfaceOrientationPortraitUpsideDown);
}

- (IBAction)ibaButtonTouched:(UIButton *)sender {
    cppClass->incrementCounter();
    NSString *text = [NSString stringWithFormat:@"Value: %i", cppClass->getCounter()];
    [self.iboLabel setText: text];
    
}
@end
