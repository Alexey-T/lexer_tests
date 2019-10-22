//    Copyright 2013 Anna Bassi, EnginSoft S.P.A.
// 
//    Licensed under the Apache License, Version 2.0 (the "License");
//    you may not use this file except in compliance with the License.
//    You may obtain a copy of the License at
// 
//        http://www.apache.org/licenses/LICENSE-2.0
// 
//    Unless required by applicable law or agreed to in writing, software
//    distributed under the License is distributed on an "AS IS" BASIS,
//    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//    See the License for the specific language governing permissions and
//    limitations under the License.



mode(7)
// BASIC COMMANDS AND OPERATIONS
0.4 + 4/2 
// This is a comment
// Let's divide the previous value by two
0.4 + 4/2
ans/2
(0.4 + 4)/(3-4^0.5) // A comment after the command
// Two expressions in the same line
1*2 , 1.1 + 1.3
// The expression is toooooooo long
1 + 1/2 + 1/3 + ...
1/4 + 1/5 + ...
1/6
// An expression with suppressed output
1 + 1/2 + 1/3 + 1/4 + 1/5 + 1/6;
// The result is stored in the ans variable
ans

// PREDEFINED VARIABLES
%pi // pi = 3.1415....
sin(%pi)
%i // imaginary unit
sqrt(-1)
exp(%i*%pi)+1 // The famous Euler relation

// EXTENDED ARITHMETIC AND FORMATS
ieee(2) // set floating point exceptions for Inf and Nan
1/0
0/0, %inf*%inf, %inf*%nan
ieee(0) // unset floating point exceptions for Inf and Nan
//1/0 would give an error
//0/0 would give an error
format('v',20); %pi // Change visualization
format('e',20); %pi // Change visualization
format("v",10); %pi // Restore original visualization

// VARIABLES
// Define two double variables a and b
a = 4/3; 
b = 3/4;
// Define variable c as expression of a and b
c = a*b;
// Display the result
disp(c)
// Two strings
a = 'Hello';
b = 'World';
// String concatenation
c = a + " " + b + "!" ;
disp(c);
// Concatenation of a string with a number
d = "Length of " + a + " is " + string(length(a))
// Example of a true expression (boolean)
res = 1>0
// Example of a false expression (boolean)
res = 1<0
// a contains a number
a = 1; 
disp(a)
// a is now a string
a = 'Hello!'; 
disp(a)

// FUNCTIONS
// Examples of input arguments
rand
sin(%pi)
max(1,2)
max(1,2,5,4,2)
// Examples of output arguments
a = rand()
v = max(1,2,5,4,2)
[v,k] = max(1,2,5,4,2)
 
// RESOLUTION OF A QUADRATIC EQUATION
// Define input data
a = 3; b = -2; c = -1/3;
// Compute delta
Delta = b^2-4*a*c; 
// Compute solutions
x1 = (-b+sqrt(Delta))/(2*a);
x2 = (-b-sqrt(Delta))/(2*a);
// Display the solutions
disp(x1); disp(x2);
// Exact solutions
x1e = (1+sqrt(2))/3
x2e = (1-sqrt(2))/3
// Compute differences between solutions
diff_x1 = abs(x1-x1e)
diff_x2 = abs(x2-x2e)

// THE END!
