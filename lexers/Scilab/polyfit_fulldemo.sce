// demo

// Interpolation

xb=0:0.01:20;
yb=sin(xb)+sin(2*xb);
xib=0:1:20; //coef. interp. nodes
y1b=sin(xib)+sin(2*xib);

// interpolations
yi1b=interp1(xib,y1b,xb,'linear');
yi2b=interp1(xib,y1b,xb,'nearest');
yi3b=interp1(xib,y1b,xb,'spline');

// error interp.
bl1=yb-yi1b;
bl2=yb-yi2b;
bl3=yb-yi3b;

// max error interp.
blm1=max(abs(bl1));
blm2=max(abs(bl2));
blm3=max(abs(bl3));

scf(1); clf(1);
subplot(4,1,1)
plot(xb,yb,xb,yi1b,xib,y1b,'*');
legend('Base','Linear')
xgrid(1)
subplot(4,1,2)
plot(xb,yb,xb,yi2b,xib,y1b,'*');
legend('Base','Nearest')
xgrid(1)
subplot(4,1,3)
plot(xb,yb,xb,yi3b,xib,y1b,'*');
legend('Base','Spline')
xgrid(1)
subplot(4,1,4)
plot(xb,yb,xb,bl1,xb,bl2,xb,bl3);
xgrid(1)
legend('Base','Linear error','Nearest error','Spline error')


// Polynomial

function [u]=polyfit(x,y,n)
      // Vandermonde
      for k=n:-1:0
          if k==n then w=0;
          end
          w=w+1;
          Xu(:,w)=[x.^k]; //yes, i remove two lines
      end

      // QR
      [q r k]=qr(Xu,'0');
      s = inv(r) * (q' * y); // s = r \ (q' * y) 
      for o=1:length(s)
          u(find(k(:,o)>0))=s(o);
      end
endfunction

function [y]=polyval(u,x,n)
      for k=1:n+1
          if k==1 then y=[];
          end
          w=(n+1)-k;
          y=y+u(k)*x.^w;
      end
endfunction

// xc=[6.1 8 10.3 11.3 12.3 13.2 14.2 16.1];
// yc=[61.3 50 46.4 46.4 40.8 31.6 29.9 22];
xc=[1 2 3 4 5 6 7 8 9 10 11 12 13 14];
yc=[22 12 20 26 23 9 1 15 14 20 28 24 14 12];
xc=xc';
yc=yc';

p=min(xc);
k=max(xc);
dx=(k-p)/50;
xx=(p:dx:k);

n=3;

f=polyfit(xc,yc,n);
yy=polyval(f,xx,n);

scf(2); clf(2);
subplot(2,1,1)
plot(xc,yc,'o',xx,yy);
title(string(n) + ' degree')
xgrid(1)

n=length(xc)-1;

f=polyfit(xc,yc,n);
yy=polyval(f,xx,n);

subplot(2,1,2)
plot(xc,yc,'o',xx,yy)
title(string(n) + ' degree')
xgrid(1)
