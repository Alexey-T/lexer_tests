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

// This example plot Pareto fronts and sets.

clear 
example = ["zdt1"; "zdt2"; "zdt3";];
index = 3;	// first example


// A collection of multiobjective functions

// ZDT1 multiobjective function
function f = zdt1(x)
f1 = x(1);
g  = 1 + 9 * sum(x(2:$)) / (length(x)-1);
h  = 1 - sqrt(f1 ./ g);
f = [f1, g.*h];
endfunction

// ZDT2 multiobjective function
function f = zdt2(x)
f1 = x(1);
g  = 1 + 9 * sum(x(2:$)) / (length(x)-1);
h  = 1 -(f1 ./ g).^2;
f = [f1, g.*h];
endfunction

// ZDT3 multiobjective function
function f = zdt3(x)
f1 = x(1);
g  = 1 + 9 * sum(x(2:$)) / (length(x)-1);
h  = 1 -sqrt(f1 ./ g) - f1 ./ g .* sin(10*%pi*f1);
f = [f1, g.*h];
endfunction

select index,
	case 1 then
		// ZDT1
		disp(example(index));
		// Compute Pareto front
		f1_opt = linspace(0,1);
		f2_opt = 1 - sqrt(f1_opt);
		
		// Domain bounds
		xmin = [0;0];
		xmax = [1;1];
		
	case 2 then
		// ZDT2
		disp(example(index));
		// Compute Pareto front
		f1_opt = linspace(0,1);
		f2_opt = 1 - f1_opt.^2;
		
		// Domain bounds
		xmin = [0;0];
		xmax = [1;1];
	
	case 3 then
		// ZDT3
		disp(example(index));
		// Compute Pareto front
		// Optimal front function definition
		function f2 = optimal_front(f1)
			f2 = 1 - sqrt(f1) - f1.*sin(10*%pi*f1);
			indf = find(f1>0.0830015349 & f1<=0.1822287280);
			f2(indf) = %nan;
			
			indf = find(f1>0.2577623634 & f1<=0.4093136748);
			f2(indf) = %nan;
			
			indf = find(f1>0.4538821041 & f1<=0.6183967944);
			f2(indf) = %nan;
			
			indf = find(f1>0.6525117038 & f1<=0.8233317983);
			f2(indf) = %nan;
			
			indf = find(f1>0.8518328654);
			f2(indf) = %nan;	
		endfunction
		// Compute the optimal front
		f1_opt = linspace(0,1,1001);
		f2_opt = optimal_front(f1_opt);
		
		// Domain bounds
		xmin = [0;0];
		xmax = [1;1];
end

// Multiobjective function
deff('y=multiobjfct(x)','y = '+example(index)+'(x);');


// Plot Pareto front
scf(1);
plot(f1_opt, f2_opt, 'b.');
title("Optimal Pareto front","fontsize",3);
xlabel("$f_1$","fontsize",4);
ylabel("$f_2$","fontsize",4);

// Compute Pareto set
x1space = linspace(xmin(1),xmax(1),101);
x2space = linspace(xmin(2),xmax(2),101);
[X,Y] = meshgrid(x1space,x2space);

F1 = zeros(size(X));
F2 = zeros(size(Y));

for i=1:size(X,1)
	for j=1:size(X,2)
		x1 = X(i,j);
		x2 = Y(i,j);
		x = [x1;x2];
		f1f2 = multiobjfct(x);
		F1(i,j) = f1f2(1);
		F2(i,j) = f1f2(2);
	end
end

F1 = F1(:);
F2 = F2(:);

X = X(:);
Y = Y(:);

fobj_pop_opt = [F1,F2];
pop_opt = [X,Y];

halt('Press Enter to continue');

// Compute Pareto front and filter
[f_pareto,pop_pareto] = pareto_filter(fobj_pop_opt,pop_opt);

// Plotting Pareto front
scf(2);
plot(fobj_pop_opt(:,1),fobj_pop_opt(:,2),'g.');
plot(f_pareto(:,1),f_pareto(:,2),'r.');
plot(f1_opt, f2_opt, 'k-');
title("Pareto front","fontsize",3);
xlabel("$f_1$","fontsize",4);
ylabel("$f_2$","fontsize",4);
legend(['Function values','Pareto pop.','Pareto front.']);

halt('Press Enter to continue');

scf(3);
plot(pop_opt(:,1),pop_opt(:,2),'g.');
plot(pop_pareto(:,1),pop_pareto(:,2),'k.');
plot(pop_pareto(:,1),pop_pareto(:,2),'b.');
title("Pareto Set","fontsize",3);
xlabel("$x_1$","fontsize",4);
ylabel("$x_2$","fontsize",4);
legend(['Domain','Pareto pop.']);

halt('Press Enter to continue');

scf(4);
plot(pop_pareto(:,1),pop_pareto(:,2),'b.');
title("Pareto Set","fontsize",3);
xlabel("$x_1$","fontsize",4);
ylabel("$x_2$","fontsize",4);
legend(['Domain']);
