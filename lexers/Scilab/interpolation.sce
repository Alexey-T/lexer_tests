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

// A collection of interpolation examples

// Close all opened figures and clear workspace
xdel(winsid());
clear;
clc;

pathdir = get_absolute_file_path('interpolation.sce');
exec(pathdir + "polyfit.sci");

// Figure #1: Plotting of the Runge function
// ----------
// Define Runge function
deff('[y]=f(x)','y = 1 ./(1+x.^2)');

// Interpolation points
xi = linspace(-5,5,7)';
yi = f(xi);

// Visualization data
xrunge = linspace(-6,6,101)';
yrunge = f(xrunge);

// Plot
scf(1);
clf(1);
plot(xrunge,yrunge,'b-');
plot(xi,yi,'or');
xlabel("x");
ylabel("y");
title("Runge function");


// Figure #2: Intepolation points
// ----------

// Plot 
scf(2);
clf(2);
plot(xi,yi*0,'xo');
p = get("hdl");
p.children.mark_mode = "on";
p.children.mark_style = 4;
p.children.thickness = 4;
p.children.mark_foreground = 2;
xlabel("x");
ylabel("y");
title("Interpolation Points");

// Evaluation points
xval = linspace(-6,6,101)';

// Figure #3: Piecewise interpolation
// ----------

// Interpolation
xx_c = xval;
yy_c = interp1(xi,yi,xx_c,'nearest','extrap');

// Plot
scf(3);
clf(3);
plot(xrunge,yrunge,'k-');
plot(xx_c,yy_c,'b-');
plot(xi,yi,'or');
xlabel("x");
ylabel("y");
title("Piecewise interpolation");
legend(["Runge func";"Interp.";"Interp. val"]);


// Figure #4: Linear interpolation
// ----------

// Interpolation
xx_l = xval;
yy_l = interp1(xi,yi,xx_c,'linear','extrap');

// Plot
scf(4);
clf(4);
plot(xrunge,yrunge,'k-');
plot(xx_l,yy_l,'b-');
plot(xi,yi,'or');
xlabel("x");
ylabel("y");
title("Linear interpolation");
legend(["Runge func";"Interp.";"Interp. val"]);


// Figure #5: Polynomial interpolation
// ----------

// Import function
exec("polyfit.sci",-1);

// Interpolation
xx_p = xval;
[Pn] = polyfit(xi, yi, length(xi)-1);
yy_p = horner(Pn,xx_p);

// Plot
scf(5);
clf(5);
plot(xrunge,yrunge,'k-');
plot(xx_p,yy_p,'b-');
plot(xi,yi,'or');
xlabel("x");
ylabel("y");
title("Polynomial interpolation");
legend(["Runge func";"Interp.";"Interp. val"]);

// Figure #6: Spline interpolation
// ----------

// Splines examples
d = splin(xi, yi,"not_a_knot");
// d = splin(xi, yi,"natural");
// d = splin(xi, yi,"periodic");

xx_s = xval;
yy_s = interp(xx_s, xi, yi, d,"linear");

// Plot
scf(6);
clf(6);
plot(xrunge,yrunge,'k-');
plot(xx_s,yy_s,'b-');
plot(xi,yi,'or');
xlabel("x");
ylabel("y");
title("Spline interpolation");
legend(["Runge func";"Interp.";"Interp. val"]);


// Figure #7: Gaussian Radial Basis Interpolation (RBF)
// ----------

// Gaussian RBF
deff('[y]=rbf_gauss(r,sigma)','y = exp(-r.^2 ./(2*sigma))'); 

// Plot
scf(7);
clf(7);
r = linspace(0,3);
y1 = rbf_gauss(r,0.1);
y2 = rbf_gauss(r,1.0);
y3 = rbf_gauss(r,2.0);
plot(r,y1,'k-');
plot(r,y2,'b-');
plot(r,y3,'r-');
xlabel("$r$");
ylabel("$\phi(r)$");
title("Gaussian rbf");
legend(["$\sigma = 0.1$";"$\sigma = 1.0$";"$\sigma = 2.0$"]);


// Figure #8/9: Gaussian Radial Basis Interpolation (RBF)
// -----------

// Evaluation point: the same of the orginal plot
xx_rbf = xval;
yy_runge = f(xval);	// used for error computation
np = length(xval);

sigmaval = linspace(0.1,1.0,101);
rbf_error = zeros(np,1);
for index = 1:length(sigmaval)
	disp(["Performing iteration: " + string(index) + "/" + string(length(sigmaval))]);
	yy_rbf = zeros(np,1);
	sigma = sigmaval(index);
	
	// Compute interpolation coefficient
	n = length(xi);
	Phi_ij = zeros(n,n);
	for i = 1:n,
		for j = 1:n
			r = norm(xi(i)-xi(j));
			Phi_ij(i,j) = rbf_gauss(r,sigma);
		end
	end
	acoeff = Phi_ij\yi;

	// Loop over all point to be evaluated
	for k=1:np
		// Eval all rbf function
		fval = zeros(length(acoeff),1);
		for i=1:length(acoeff)
			// Evaluate distances
			r = norm(xi(i)-xx_rbf(k));
			// Evaluate rbf 
			fval(i) = rbf_gauss(r,sigma);
		end

		// Evaluate rbf interpolation in the given point
		yy_rbf(k) = fval'*acoeff;
	end

	// Evaluate error
	rbf_error(index) = sum(abs(yrunge-yy_rbf));
end

// Plot error
scf(8);
clf(8);
plot(sigmaval,rbf_error);
xlabel("sigma");
ylabel("error");
title("Intepolation error");

// Find minimum error
[errmin,indmin] = min(rbf_error);
sigma = sigmaval(indmin);

// Re-evalute rbf
n = length(xi);
Phi_ij = zeros(n,n);
for i = 1:n,
	for j = 1:n
		r = norm(xi(i)-xi(j));
		Phi_ij(i,j) = rbf_gauss(r,sigma);
	end
end
acoeff = Phi_ij\yi;

yy_rbf = zeros(np,1);
for k=1:np
	fval = zeros(length(acoeff),1);
	for i=1:length(acoeff)
		r = norm(xi(i)-xx_rbf(k));
		fval(i) = rbf_gauss(r,sigma);
	end
	yy_rbf(k) = fval'*acoeff;
end

// Plot optimal RBF
scf(9);
clf(9);
subplot(1,2,1);
plot(xrunge,yrunge,'k-');
plot(xx_rbf,yy_rbf,'b-');
plot(xi,yi,'or');
xlabel("x");
ylabel("y");
title("Rbf interpolation");
legend(["Runge func";"Interp.";"Interp. val"]);


// Plot non optimal RBF
// Re-evalute rbf
sigma = 0.2;
n = length(xi);
Phi_ij = zeros(n,n);
for i = 1:n,
	for j = 1:n
		r = norm(xi(i)-xi(j));
		Phi_ij(i,j) = rbf_gauss(r,sigma);
	end
end
acoeff = Phi_ij\yi;

yy_rbf = zeros(np,1);
for k=1:np
	fval = zeros(length(acoeff),1);
	for i=1:length(acoeff)
		r = norm(xi(i)-xx_rbf(k));
		fval(i) = rbf_gauss(r,sigma);
	end
	yy_rbf(k) = fval'*acoeff;
end

subplot(1,2,2);
plot(xrunge,yrunge,'k-');
plot(xx_rbf,yy_rbf,'b-');
plot(xi,yi,'or');
xlabel("x");
ylabel("y");
title("Rbf interpolation");
legend(["Runge func";"Interp.";"Interp. val"]);


// Figure #10: Example of approximation in 1D (full polinomial)
// ----------
np = 100;
noise = 0.7*(rand(np,1)-0.5);

x = linspace(0,2,np)';
yexact = x.^2 + x;
ynoise = yexact + noise;

// degree 1
p1 = polyfit(x, ynoise, 1);
p1val = horner(p1,x);

// degree 2
p2 = polyfit(x, ynoise, 2);
p2val = horner(p2,x);

// plot
scf(10);
clf(10);
plot(x,yexact,'k-');
plot(x,ynoise,'b-');
plot(x,p1val,'r-');
plot(x,p2val,'g-');
xlabel("x");
title("Best polynomial approximation");
legend(["yexact";"ynoise";"p1val";"p2val"]);


// Figure #11: Example of approximation in 2D (plane)
// ----------
// Generating random points along a plane
np = 30;
noise = 0.5*(rand(np,1)-0.5);

// Extract data
x = rand(np,1);
y = rand(np,1);
znoise = -x+2*y+noise;

// Vandermonde matrix for P(x,y) = a+b*x+c*y
V = [ones(np,1),x,y];
// Find coefficient i.e. minimize error norm
coeff = V\znoise;

// Evaluate polynomial in a grid for plotting
ndiv = 40;
xdiv = linspace(0,1,ndiv);
ydiv = linspace(0,1,ndiv);
[X,Y] = meshgrid(xdiv,ydiv);
Z = zeros(np,np);
for i=1:size(X,1)
	for j=1:size(X,2)
		xval = X(i,j);
		yval = Y(i,j);
		Z(i,j) = coeff(1)+coeff(2)*xval+coeff(3)*yval;
	end
end

// Plot data
fz = scf(11);
clf(11);
fz.color_map=jetcolormap(32);
surf(X,Y,Z)

plot3d(x,y,znoise,theta=40,alpha=60);
fz.children.children(1).surface_mode="off";
fz.children.children(1).mark_mode="on";
fz.children.children(1).mark_size=2;
fz.children.children(1).mark_style=9;
fz.children.children(1).mark_foreground=3;
fz.children.children(1).mark_background=3;
