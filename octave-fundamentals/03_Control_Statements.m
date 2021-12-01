%%% OCTAVE COMMANDS - CONTROL STATEMENTS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% FOR LOOP
v = zeros(10,1);
for i=1:10,
v(i) = 2^i;
end;

indices = 1:10;
for i=indices,
disp(i);
end;


%% WHILE LOOP
i = 1;
while 1<=5,
v(i) = 100;
i = i+1;
end;

i=1;
while true,
	v(i)=999;
	i = i+1;
		if i==6,
			break;
		end;
end;