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

// Exercice 1: Iterative plot of zdt1
clear 

// ZDT1 multiobjective function
function f = zdt1(x)
f1 = x(1);
g  = 1 + 9 * sum(x(2:$)) / (length(x)-1);
h  = 1 - sqrt(f1 ./ g);
f = [f1, g.*h];
endfunction

// Min boundary function
function Res = min_bd_zdt1(n)
Res = zeros(n,1);
endfunction

// Max boundary function
function Res = max_bd_zdt1(n)
Res = ones(n,1);
endfunction

// Problem dimension
dim = 2;

// Example of use of the genetic algorithm
funcname    = 'zdt1';
PopSize     = 500;
Proba_cross = 0.7;
Proba_mut   = 0.1;
NbGen       = 20;
NbCouples   = 110;
Log         = %T;
pressure    = 0.1;

// Setting paramters of optim_nsga2 function 
ga_params = init_param();
// Parameters to adapt to the shape of the optimization problem
ga_params = add_param(ga_params,'minbound',min_bd_zdt1(dim));
ga_params = add_param(ga_params,'maxbound',max_bd_zdt1(dim));
ga_params = add_param(ga_params,'dimension',dim);
ga_params = add_param(ga_params,'beta',0);
ga_params = add_param(ga_params,'delta',0.1);
// Parameters to fine tune the Genetic algorithm.
// All these parameters are optional for continuous optimization.
// If you need to adapt the GA to a special problem. 
ga_params = add_param(ga_params,'init_func',init_ga_default);
ga_params = add_param(ga_params,'crossover_func',crossover_ga_default);
ga_params = add_param(ga_params,'mutation_func',mutation_ga_default);
ga_params = add_param(ga_params,'codage_func',coding_ga_identity);
ga_params = add_param(ga_params,'nb_couples',NbCouples);
ga_params = add_param(ga_params,'pressure',pressure);

// Define s function shortcut
deff('y=fobjs(x)','y = zdt1(x);');

// Compute optimzal front
f1_opt = linspace(0,1);
f2_opt = 1 - sqrt(f1_opt);

// Create a global variable
global currPop;

// Create a function for initialize the global variable
function Pop_init = init_ga_previous(popsize,param)
	global currPop;
	Pop_init = currPop;
endfunction

// Performing optimization
for i=1:NbGen
	printf("Performing generation : %d",i);
	NbGenloc = 1;
	[pop_opt, fobj_pop_opt] = optim_nsga2(fobjs, PopSize, NbGenloc, Proba_mut, Proba_cross, Log, ga_params);
	if i>1
		// Change init generation function
		ga_params = add_param(ga_params,'init_func',init_ga_previous);
	end
	// Save population
	currPop = pop_opt;
	
	// Compute Pareto front and filter
	[f_pareto,pop_pareto] = pareto_filter(fobj_pop_opt,pop_opt);
	
	// Plotting final population
	scf(1);
	// clear figure
	clf(1);
	drawlater();
	plot(f1_opt,f2_opt,'b-');
	plot(fobj_pop_opt(:,1),fobj_pop_opt(:,2),'g.');
	plot(f_pareto(:,1),f_pareto(:,2),'r.');
	title("Pareto front (#NbGen="+string(i)+")","fontsize",3);
	xlabel("$f_1$","fontsize",4);
	ylabel("$f_2$","fontsize",4);
	legend(['Opt. Pareto front';'Pop.';'Pareto front']);
	a=get("current_axes"); // get the handle of the newly created axes
	a.data_bounds=[0,0;1,5];
	drawnow();
	// Save figure to png
	xs2png(1,"image"+filesep()+sprintf("ex1_%03d.png",i));
end
