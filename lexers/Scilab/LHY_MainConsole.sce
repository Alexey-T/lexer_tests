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

// Testing the model using US drug data
clc

// Import LHY functions
pathdir = get_absolute_file_path('LHY_MainConsole.sce');
getd(pathdir + 'model');

// Setting LHY model parameter
param = [];
param.tau = 5e4; // Number of innovators per year (initiation)
param.s = 0.61;  // Annual rate at which light users attract 
                 // non-users (initiation)
param.q = 3.443; // Constant which measures the deterrent
                 //  effect of heavy users (initiation)
param.smax = 0.1;// Upper bound for s effective (initiation)
param.a = 0.163; // Annual rate at which light users quit
param.b = 0.024; // Annual rate at which light users escalate 
                 // to heavy use
param.g = 0.062; // Annual rate at which heavy users quit
param.delta = 0.291; // Forgetting rate

// Setting initial conditions
Tbegin = 1970;   // Initial time
Tend = 2020;     // Final time
Tstep = 1/12     // Time step (one month)
L0 = 1.4e6;      // Light users at the initial time
H0 = 0.13e6;     // Heavy users at the initial time
Y0 = 0.11e6;     // Decaying heavy user at the initial time

// Assigning ODE solver data
y0 = [L0;H0;Y0];
t0 = Tbegin;
t = Tbegin:Tstep:(Tend+100*%eps);
f = LHY_System;

// Solving the system
LHY = ode(y0, t0, t, f);

// Plotting of model data
LHY_Plot(t, LHY);
