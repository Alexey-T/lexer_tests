spline1=spmak(t',alpha');
error1=y-fnval(spline1,x)';
a = [1 2]';
b = '[1 2]';
alpha=(d'*p)/((A*p)'*p);
