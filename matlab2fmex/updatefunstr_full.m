function [funstr,funstrwords,funstrwords_b,funstrwords_e,funstrnumbers,funstrnumbers_b,funstrnumbers_e,s]=updatefunstr_full(funstr,funstrwords,funstrwords_b,funstrwords_e,funstrnumbers,funstrnumbers_b,funstrnumbers_e,oneline)
s=length(funstr);
to0out={'.*','./','.''','.^'};
if nargin==1
 lo=1;hi=s;
 funstrwords=cell(s,1);funstrwords_b=cell(s,1);funstrwords_e=cell(s,1);
 funstrnumbers=cell(s,1);funstrnumbers_b=cell(s,1);funstrnumbers_e=cell(s,1);
else
 lo=oneline;hi=oneline;
 funstrwords{oneline}=cell(0);
 funstrwords_b{oneline}=[];funstrwords_e{oneline}=[];
 funstrnumbers{oneline}=cell(0);
 funstrnumbers_b{oneline}=[];funstrnumbers_e{oneline}=[];
end
for i=lo:hi
 tempw_b=zeros(1,10);   tempw_e=zeros(1,10);
 tempn_b=zeros(1,10);   tempn_e=zeros(1,10);
 %funstr{i}='def.*4./tug.^23.45e-4.''+a2';
 both=~isspace(funstr{i});
 for j=1:length(to0out)
  temp=findstr(funstr{i},to0out{j});
  both(temp)=0;         both(temp+1)=0;
 end
 both=double(both&iskeep(funstr{i}));
 both(2:end)=both(2:end)-both(1:end-1);
 bothind=find(both==1);         ll=length(bothind);
 bothind2=find(both==-1);       ll2=length(bothind2);
 if ll2<ll, bothind2=[bothind2 length(both)+1]; end
 cw=1;                          cn=1;
 couple=0;
 for j=1:ll
  if isletter(funstr{i}(bothind(j)))
   funstrwords{i}{cw}=funstr{i}(bothind(j):bothind2(j)-1);
   tempw_b(cw)=bothind(j);
   tempw_e(cw)=bothind2(j)-1;
   cw=cw+1;
  else
   if couple==0
    if j~=ll
     if ~isletter(funstr{i}(bothind(j+1)))
      if (strcmpi(funstr{i}(bothind(j+1)-1),'e')|strcmpi(funstr{i}(bothind(j+1)-2),'e')|strcmpi(funstr{i}(bothind(j+1)-1),'d')|strcmpi(funstr{i}(bothind(j+1)-2),'d'))
       couple=1;
      end
     end
    end
    negsign=0;
    if bothind(j)>1
     if funstr{i}(bothind(j)-1)=='-'
      if bothind(j)>2
       temps=find(~isspace(funstr{i}));
       temps=temps(temps<bothind(j)-1);
       if ~isempty(temps)
        if ~(iskeep(funstr{i}(temps(end)))|(funstr{i}(temps(end))==')'))
         negsign=1;
        end
       else
        negsign=1;
       end         
      else
       negsign=1;
      end
     end
    end
    funstrnumbers{i}{cn}=funstr{i}(bothind(j)-negsign:bothind2(j+couple)-1);
    tempn_b(cn)=bothind(j)-negsign;
    tempn_e(cn)=bothind2(j+couple)-1;
    cn=cn+1;
   else
    couple=0;
   end
  end
 end
 funstrwords_b{i}=tempw_b(1:cw-1);
 funstrwords_e{i}=tempw_e(1:cw-1);
 funstrnumbers_b{i}=tempn_b(1:cn-1);
 funstrnumbers_e{i}=tempn_e(1:cn-1);
end
