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

// The first image

// Close all opened figures and clear workspace
xdel(winsid());
clear;
clc;

pathdir = get_absolute_file_path('ex0.sce');
exec(pathdir + "polyfit.sci");

// Define Runge function
deff('[y]=f(x)','y = 1 ./(1+x.^2)');

// Interpolation points
xi = linspace(-3,3,7)';
yi = f(xi);

// Evaluation points
xval = linspace(-3,3,101)';
yrunge = f(xval);

// Import function (Atoms: DATA ANALYSIS AND STATISTICS)
exec("polyfit.sci",-1);

yval = [];
for i=0:length(xi)-1
	// degree i
	pfit = polyfit(xi, yi, i);
	pval = horner(pfit,xval);
	yval = [yval,pval];
end

plot(xval,yval);
plot(xval,yrunge,'b-');
plot(xi,yi,'ro');

