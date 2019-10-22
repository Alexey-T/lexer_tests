//
//  ViewController.h
//  ObjC++ example
//
//  Created by Libor on 3/13/12.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
#include "ExampleClass.cpp"

@interface ViewController : UIViewController {
    ExampleClass *cppClass;
}

- (IBAction)ibaButtonTouched:(UIButton *)sender;
@property (weak, nonatomic) IBOutlet UILabel *iboLabel;

@end
