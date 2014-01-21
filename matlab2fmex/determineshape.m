function [tempval]=determineshape(howmany,subscripts,centercomma,parens,i,j,whichsub);
declare_globals
tempstr=''; %default
tempval=cell(1); %default

instr=subscripts{whichsub};
%Find which words and numbers are in this subscript
if howmany==1
 if isinf(j)
  start=parens(1);            finish=parens(2);
  [outflag,howmany3,subscripts3,centercomma3,parens]=inwhichlast(i,parens(1));
 else
  start=parens(1);           finish=parens(2);
 end
 nums=find((funstrnumbers_b{i}>=start)&(funstrnumbers_b{i}<=finish));
 words=find((funstrwords_b{i}>=start)&(funstrwords_b{i}<=finish));
else
 if whichsub==1
  start=parens(1);          finish=centercomma(1);
 elseif whichsub==howmany
  start=centercomma(whichsub-1);   finish=parens(2);
 else
  start=centercomma(whichsub-1);   finish=centercomma(whichsub);
 end
 nums=find((funstrnumbers_b{i}>=start)&(funstrnumbers_b{i}<=finish));
 words=find((funstrwords_b{i}>=start)&(funstrwords_b{i}<=finish));
end
%Run through words. Discard any that are fortran only words.
%'detttttttttttt',funstr{i}
tempval=inf;evalstr='';
if length(words)==0 %Have only numbers. Should be ready for evaluation
 evalstr=instr;
 tempval=eval(evalstr);
else
 %'nowwwwwwwwww',kb
 last=funstrwords_b{i}(words(1))-1;
 evalstr=funstr{i}(start+1:last);
 for m=1:length(words)
  if any(strcmp(funstrwords{i}{words(m)},funwords))
   evalstr=[evalstr,funstr{i}(last+1:funstrwords_e{i}(words(m)))];
   last=funstrwords_e{i}(words(m));
  elseif any(strcmp(funstrwords{i}{words(m)},inoutother3))
   evalstr=[evalstr,funstr{i}(last+1:funstrwords_b{i}(words(m))-1),'cw.',funstrwords{i}{words(m)}];
   last=funstrwords_e{i}(words(m));
  else
   evalstr=[evalstr,funstr{i}(last+1:funstrwords_b{i}(words(m))-1)];
   last=funstrwords_e{i}(words(m));
  end
 end
 evalstr=[evalstr,funstr{i}(last+1:finish-1)];
 try
  tempval=eval(evalstr);
 catch
  tempval=nan;
 end
end
%evalstr,tempval
%'detttttttttttt',funstr{i},tempval,kb




%'got',funstr{i},funstr1{ss},subscripts,howmany,k,evalstr,tempstr,kb



%First map the current word into the old word if need be.
% map={'minval','min';'maxval','max';'product','prod'};
% temp3=[];
% ss=zeros(length(funstr1),1);
% ss=ones(length(funstr1),1);
% %Use of map to find the right line
% %if strcmp(funstrwords{i}{j},'length')
% % 'sssssssssssssssssss',funstr{i},kb
% %end
% thisword=funstrwords{i}{j};
% if any(strcmp(funstrwords{i}{j},{map{:}}))
%  [temp3(1),temp3(2)]=find(strcmp(funstrwords{i}{j},map));
% end
% %Map this i,j to the original set, then determine
% if ~isempty(temp3)
%  temp1=nnz([find(strcmp(map{temp3(1),1},funstrwords{i})|strcmp(map{temp3(1),2},funstrwords{i}))]);
% else
%  temp1=nnz([find(strcmp(thisword,funstrwords{i}))]);
% end
% for k=1:length(funstr1) %Find lines in funstr1 that have the same number of thisword or orig word
%  if isempty(temp3)
%   temp=nnz([find(strcmp(thisword,funstrwords1{k}))]);
%  else
%   temp=nnz([find(strcmp(map{temp3(1),2},funstrwords1{k}))]);
%  end
%  if temp==temp1
%   ss(k)=1;
%  end
% end
% for k=1:length(funstr1) 
%  %find lines in funstr1 which has all the same vars in the right order in funstr{i}
%  if ss(k)
%   last=0;keepit=1;
%   for m=1:length(funstrwords1{k})
%    if keepit
%     if any(strcmp(funstrwords1{k}{m},inoutother3)) %Got a variable
%      ;%Where's the occurrences of this var in funstrwords{i}
%      temp2=find(strcmp(funstrwords1{k}{m},funstrwords{i}));
%      if ~isempty(temp2)
%       temp2=temp2(temp2>last); %only consider vars after the last find
%       if ~isempty(temp2)
%        last=temp2(1);
%       else %Couldn't find funstrwords1{ss}{m} in funstrwords{i} past the last one
%        keepit=0;
%       end
%      else %Couldn't find funstrwords1{ss}{m} in funstrwords{i}
%       keepit=0;
%      end
%     end
%    end
%   end
%   if keepit==0
%    ss(k)=0;
%   end
%  end
% end
% ss=find(ss);
% %Closest line earlier in funstr1 which is still active is the winner
% ss=ss(ss<=i);
% ss=ss(end);
% temp=find(funstrwords_b{i}==funstrwords_b{i}(j));
% if isempty(temp3)
%  temp1=find(strcmp(funstrwords{i},thisword));
% else
%  temp1=find(strcmp(map{temp3(1),1},funstrwords{i})|strcmp(map{temp3(1),2},funstrwords{i}));
% end
% temp2=find(temp==temp1);
% if isempty(temp3)
%  temp=find(strcmp(funstrwords1{ss},thisword));
% else
%  temp=find(strcmp(funstrwords1{ss},map{temp3(1),2}));
% end
% temp=temp(temp2);
% 
% %Now we are ready to get subscripts from funstrwords1{ss}{temp}
% [howmany,subscripts,centercomma,parens]=hassubscript(ss,temp,funstr1,funstrwords_e1);
