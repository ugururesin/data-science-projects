%%% OCTAVE COMMANDS - PLOTTING %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% PLOTTING
t = [0:0.01:0.98];
y1 = sin(2*pi*4*t);
plot(t,y1);
%
y2 = cos(2*pi*4*t);
plot(t,y1);
hold on;		%to combine 2 plots		
plot(t,y2,'r'); %r:red
legend('sin', 'cos')
title('my plot')
cd 'C:\Users\uuresin\Desktop'; print -dpng 'myPlot.png'
close
%
figure(1); plot(t,y1);
figure(2); plot(t,y2);
%
subplot(1,2,1);			%Divides plot a 1x2 grid, access first element
plot(t,y1);				%Put this plot in first
%
subplot(1,2,2);			%Divides plot a 1x2 grid, access second element
plot(t,y2);				%Plot this plot in second
axis([0.5 1 -1 1])		%Set horizontal axis [0.5 1] and vertical axis [-1 1] for second plot
%
clf;	%clear a figure%
%
A = magic(5)
imagesc(A)	%5x5 grid of colors!
imagesc(A), colorbar, colormap gray;	%5x5 gray colormap!
%


%% HISTOGRAM
w = randn(1,10000);	%10000 random numbers
hist(w)				%histogram
hist(w,50)			%histogram with 50 bins