//    Copyright 2012 Manolo Venturin, EnginSoft S.P.A.
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

// Estimation of coefficient for a linear model of the form
// y = a_0 + a_1*x1 + a_1*x1 + ... + a_n*xn 

// Close all opened figures and clear workspace
xdel(winsid());
// clear;
clc;

// Test function for the generation of data
function y = evaldata(pcoeff, x)
    // pcoeff   : Problem coefficient ([a0, a1, ..., an]) of y = a_0 + a_1*x1 + a_1*x1 + ... + a_n*xn 
    // x        : Evaluation point [x1, x2, ..., xn] (row vector)
    // Problem dimension
    n = length(x);
    
    // Adding noise to the coefficients: Uniform distribution between [-0.01,0.01]
    pcoeff = pcoeff + 2*(rand(1,n+1)-0.5)*0.01;
    
    // Evaluate formula  y = a_0 + a_1*x1 + a_1*x1 + ... + a_n*xn 
    y = sum(pcoeff.*[1,x]);
endfunction


// Generate data
function [xdata,ydata] = generatedata(pcoeff, n, neval)
    // pcoeff   : Problem coefficient ([a0, a1, ..., an]) of y = a_0 + a_1*x1 + a_1*x1 + ... + a_n*xn 
    // n        : Problem dimension
    // neval    : Number of evaluation points (neval>n)
    
    // Set xdata to randon point evaluation
    xdata = rand(neval,n);

    // Evaluate ydata
    ydata = zeros(neval,1);
    for i=1:neval
        x = xdata(i,:);
        y = evaldata(pcoeff, x);
        ydata(i) = y; 
    end
endfunction


// Estimate coefficient ([a0, a1, ..., an])
function pstar = estimatecoeff(xdata, ydata)
    [neval, n] = size(xdata);
    
    // Vandermonde matrix
    V = [ones(neval,1),xdata];
    pstar = V\ydata;
endfunction

// Estimate coefficient with zero coefficient ([a0, a1, ..., an])
function pzero = estimatecoeffzero(xdata, ydata, zeroindex)
    [neval, n] = size(xdata);
    
    // Vandermonde matrix
    V = [ones(neval,1),xdata];
    index = setdiff(1:n+1,zeroindex);
    V = V(:,index);
    pnonzero = V\ydata;
    // adding zero coeff
    pzero = zeros(n+1,1);
    pzero(index) = pnonzero;
endfunction


// Main script
// -----------

// FIRST TEST CASE
// ---------------
n = 10;         // Problem dimensions
neval = 100;    // Number of evaluation points
pcoeff = 1:n+1; // Problem coefficient ([a0, a1, ..., an])

// Evaluation points
[xdata,ydata] = generatedata(pcoeff, n, neval);

// Estimate coeffictients
pstar = estimatecoeff(xdata, ydata);


// SECOND TEST CASE
// ----------------
// with zero coefficient
n = 10;         // Problem dimensions
neval = 100;    // Number of evaluation points
pcoeff = 1:n+1; // Problem coefficient ([a0, a1, ..., an])
pcoeff([2,5,8]) = 0;    // Some zero coefficients

// Evaluation points
[xdata,ydata] = generatedata(pcoeff, n, neval);

// Estimate coeffictients
pstar = estimatecoeff(xdata, ydata);

// Find "zero" coefficient (define tolerance)
tol = 0.1;
zeroindex = find(abs(pstar)<=tol);

// re-estimate coeffiecients
pzero = estimatecoeffzero(xdata, ydata, zeroindex);
