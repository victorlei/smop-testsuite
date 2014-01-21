function [funstr,funstrwords,funstrwords_b,funstrwords_e,funstrnumbers,funstrnumbers_b,funstrnumbers_e,s,fs_good]=updatefunstr_f(funstr,funstrwords,funstrwords_b,funstrwords_e,funstrnumbers,funstrnumbers_b,funstrnumbers_e,fs_good,oneline)

%%%if any(strfind(version,'R14')) %& 0 %0.d0 returns d0 as a word for this regexp version !
s=length(funstr);
to0out={'.*','./','.''','.^'};
global numstr wordstr
if isempty(numstr)
 numstr='(\<(\d+\.\d+|\d+\.|\.\d+|\d+)([eEdDqQ][+-]?\d+)?)';
 wordstr='(\<[a-z_A-Z]\w*)';
end
%numstr='(\<(\d+\.\d+|\d+\.|\.\d+|\d+)([eEdDqQ][+-]?\d+)?)';
%wordstr='(\<[a-z_A-Z]\w*)';
%ss='a11(de(2))-3.111d+21.*.322E-1*(-21.4e-2./b3.'') .or. 2.3+.12-2. - sqrt(fe_b(a(2)))';
%regexp(ss,'\<(\d+\.\d+|\d+\.|\.\d+|\d+)([eEdD][+-]?\d+)?','match'); % this gets the numbers
%regexp(ss,'\<[a-z_A-Z]\w*','match') % this gets the words
if nargin==1
 lo=1;hi=s;fs_good=[];
 funstrwords=cell(s,1);funstrwords_b=cell(s,1);funstrwords_e=cell(s,1);
 funstrnumbers=cell(s,1);funstrnumbers_b=cell(s,1);funstrnumbers_e=cell(s,1);
 ind=lo:hi;
else
 lo=oneline;hi=oneline;
 fs_good=fs_good(fs_good<lo|fs_good>hi);
%%%  funstrwords{oneline}=cell(0);
%%%  funstrwords_b{oneline}=[];funstrwords_e{oneline}=[];
%%%  funstrnumbers{oneline}=cell(0);
%%%  funstrnumbers_b{oneline}=[];funstrnumbers_e{oneline}=[];
 ind=oneline;
end

empty=cellfun('isempty',{funstr{ind}});
bad1=strncmp({funstr{ind}},'!',1);
bad2=strncmp({funstr{ind}},'%',1);
%if any(bad1|bad2), kb, end
good=~(empty|bad1|bad2);
fs_good=[fs_good,ind(good)];
goodind=ind(good);

if (length(fs_good)/s > 1/2 | length(fs_good)>1000) & nargin==1  %do all the lines
 [funstrnumbers,funstrnumbers_b,funstrnumbers_e]=regexp(funstr,numstr,'match','start','end');
 [funstrwords,funstrwords_b,funstrwords_e]=regexp(funstr,wordstr,'match','start','end');
else %do only those lines defined by goodind 
 if (hi-lo) > 0
  [funnum1,funnum2,funnum3]=regexp({funstr{goodind}},numstr,'match','start','end');
  [funstrnumbers{goodind}]=deal(funnum1{:});
  [funstrnumbers_b{goodind}]=deal(funnum2{:});
  [funstrnumbers_e{goodind}]=deal(funnum3{:});

  [funword1,funword2,funword3]=regexp({funstr{goodind}},wordstr,'match','start','end');
  [funstrwords{goodind}]=deal(funword1{:});
  [funstrwords_b{goodind}]=deal(funword2{:});
  [funstrwords_e{goodind}]=deal(funword3{:});
 elseif ~isempty(goodind)
  [funstrnumbers{goodind},funstrnumbers_b{goodind},funstrnumbers_e{goodind}]=regexp(funstr{goodind},numstr,'match','start','end');
  [funstrwords{goodind},funstrwords_b{goodind},funstrwords_e{goodind}]=regexp(funstr{goodind},wordstr,'match','start','end');
 end
end
fs_good=sort(fs_good);
%'fffsssssssssss',keyboard

%%%else
%%%
%%% s=length(funstr);
%%% to0out={'.*','./','.''','.^'};
%%% if nargin==1
%%%  lo=1;hi=s;fs_good=[];
%%%  funstrwords=cell(s,1);funstrwords_b=cell(s,1);funstrwords_e=cell(s,1);
%%%  funstrnumbers=cell(s,1);funstrnumbers_b=cell(s,1);funstrnumbers_e=cell(s,1);
%%% else
%%%  lo=oneline;hi=oneline;
%%%  fs_good=fs_good(fs_good<lo|fs_good>hi);
%%%  funstrwords{oneline}=cell(0);
%%%  funstrwords_b{oneline}=[];funstrwords_e{oneline}=[];
%%%  funstrnumbers{oneline}=cell(0);
%%%  funstrnumbers_b{oneline}=[];funstrnumbers_e{oneline}=[];
%%% end
%%% for i=lo:hi
%%%  tempw_b=zeros(1,10);   tempw_e=zeros(1,10);
%%%  tempn_b=zeros(1,10);   tempn_e=zeros(1,10);
%%%  %funstr{i}='-.102+def.*4./tug.^-23.45e-4.''+a2';
%%%  both=~isspace(funstr{i});
%%%  if ~isempty(both)
%%%   for j=1:length(to0out)
%%%    temp=findstr(funstr{i},to0out{j});
%%%    both(temp)=0;         both(temp+1)=0;
%%%   end
%%%   both=double(both&iskeep_f(funstr{i}));
%%%   both(2:end)=both(2:end)-both(1:end-1);
%%%   bothind=find(both==1);         ll=length(bothind);
%%%   bothind2=find(both==-1);       ll2=length(bothind2);
%%%   if ll2<ll, bothind2=[bothind2 length(both)+1]; end
%%%   cw=1;                          cn=1;
%%%   couple=0;
%%%   for j=1:ll
%%%    if isletter(funstr{i}(bothind(j)))
%%%     funstrwords{i}{cw}=funstr{i}(bothind(j):bothind2(j)-1);
%%%     tempw_b(cw)=bothind(j);
%%%     tempw_e(cw)=bothind2(j)-1;
%%%     cw=cw+1;
%%%    else
%%%     if couple==0
%%%      if j~=ll
%%%       if ~isletter(funstr{i}(bothind(j+1)))
%%%        if (strcmpi(funstr{i}(bothind(j+1)-1),'e')|strcmpi(funstr{i}(bothind(j+1)-2),'e')|strcmpi(funstr{i}(bothind(j+1)-1),'d')|strcmpi(funstr{i}(bothind(j+1)-2),'d'))
%%%         couple=1;
%%%        end
%%%       end
%%%      end
%%%      negsign=0;
%%%      if bothind(j)>1
%%%       if funstr{i}(bothind(j)-1)=='-'
%%%        if bothind(j)>2
%%%         temps=find(~isspace(funstr{i}));
%%%         temps=temps(temps<bothind(j)-1);
%%%         if ~isempty(temps)
%%%          if ~(iskeep_f(funstr{i}(temps(end)))|(funstr{i}(temps(end))==')'))
%%%           negsign=1;
%%%          end
%%%         else
%%%          negsign=1;
%%%         end         
%%%        else
%%%         negsign=1;
%%%        end
%%%       end
%%%      end
%%%      funstrnumbers{i}{cn}=funstr{i}(bothind(j)-negsign:bothind2(j+couple)-1);
%%%      tempn_b(cn)=bothind(j)-negsign;
%%%      tempn_e(cn)=bothind2(j+couple)-1;
%%%      cn=cn+1;
%%%      %'int1',funstr{i},j,negsign,funstrnumbers{i},kb
%%%     else
%%%      couple=0;
%%%     end
%%%    end
%%%   end
%%%   funstrwords_b{i}=tempw_b(1:cw-1);
%%%   funstrwords_e{i}=tempw_e(1:cw-1);
%%%   funstrnumbers_b{i}=tempn_b(1:cn-1);
%%%   funstrnumbers_e{i}=tempn_e(1:cn-1);
%%%   if any(~isspace(funstr{i}))
%%%    temp10=funstr{i}(~isspace(funstr{i}));
%%%    if ~isempty(temp10)
%%%     if (temp10(1)~='!')&(temp10(1)~='%')
%%%      fs_good=[fs_good,i];
%%%     end
%%%    end
%%%   end
%%%  end
%%% end
%%% fs_good=sort(fs_good);
%%%
%%%
%%%end

%should I get rid of empty lines?