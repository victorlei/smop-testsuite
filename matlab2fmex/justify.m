function dummy=justify(dummy,linelength)

if nargin==1
 linelength=112;
end
r=char(10);
rets=findstr(dummy,r);rets=[0 rets];
found=1;
while found==1
 found=0;
 rets=findstr(dummy,r);rets=[0 rets];
 for i=1:length(rets)-1
  if found==0
   if (dummy(rets(i)+1)~='!')
    if length(dummy(rets(i)+1:rets(i+1)-1))>linelength
     found=1;
     dummy=[dummy(1:rets(i)+linelength-1),'&',r,...
            '      &',dummy(rets(i)+linelength:length(dummy))];
    end
   end
  end
 end
end



%%%r=char(10);
%%%rets=findstr(dummy,r);rets=[0 rets];
%%%found=1;
%%%while found==1
%%% found=0;
%%% rets=findstr(dummy,r);rets=[0 rets];
%%% for i=1:length(rets)-1
%%%  if found==0
%%%   if ((dummy(rets(i)+1)~='c')&(dummy(rets(i)+1)~='C'))
%%%    if length(dummy(rets(i)+1:rets(i+1)-1))>72
%%%     found=1;
%%%     dummy=[dummy(1:rets(i)+72),r,'     &',dummy(rets(i)+73:length(dummy))];
%%%    end
%%%   end
%%%  end
%%% end
%%%end




