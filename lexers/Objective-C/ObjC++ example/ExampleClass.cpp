//
//  ExampleClass.cpp
//  ObjC++ example
//
//  Created by Libor on 3/13/12.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#include <iostream>
class ExampleClass {
private:
    int counter;
public:
    ExampleClass(int counter) {
        this->counter = counter;
    }
    
    int getCounter() {
        return counter;
    }
    void incrementCounter() {
        this->counter++;
    }
};
