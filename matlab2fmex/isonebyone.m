function out=isonebyone(i,start,finish);
declare_globals
%try
temp1=makeMATLABcallstring(1,{funstr{i}(start:finish)},[],[start finish],i,inf);
% catch
%  funstr{i},funstr{i}(start:finish),kb
% end
if any(strcmp(temp1,typs{5}))
 out=1;
else
 out=0;
end
% goon=1;
% out=1;
% temp=(findstr('[',funstr{i}));
% temp1=(findstr(':',funstr{i}));
% temp=unique([temp temp1]);
% temp=temp((temp>start)&(temp<finish));
% if ~isempty(temp)
%  for j=1:length(temp)
%   if ~inmakefunction(i,temp(j),start,finish)
%    goon=0;
%    out=0;
%   end
%  end
% end
% if goon
%  temp_num=find(((funstrnumbers_b{i}>=start)&(funstrnumbers_e{i}<=finish)));
%  temp_word=find(((funstrwords_b{i}>=start)&(funstrwords_e{i}<=finish)));
%  if length(temp_word)==0
%   out=1;%Only numbers without brackets, must be 1 number
%  else
%   out=1;  temp4=1;
%   for j=1:length(temp_word)
%    if length(find(strcmp(funstrwords{i}{temp_word(j)},make_words)))>0
%     temp4=0;
%    end
%   end
%   for j=length(temp_word):-1:1
%    if out==1
%     [outflag,howmany3,subscripts3,centercomma3,parens3]=inwhichlast(i,funstrwords_b{i}(temp_word(j)));
%     goon=1;
%     if (outflag==3)
%      if ~isempty(parens3)
%       if parens3(1)>start
%        goon=0;
%       end
%      end
%     end
%     if goon
%      if length(find(strcmp(funstrwords{i}(temp_word(j)),inoutother3)))>0 %If a variable, then
%       if ((inwhichlast(i,funstrwords_b{i}(temp_word(j)))~=3)|(temp4))
%        if prod(size(getfield(cw,funstrwords{i}{temp_word(j)})))~=1
% 	[howmany,subscripts,centercomma,parens]=hassubscript(i,temp_word(j));
% 	if howmany==0
% 	 out=0;
% 	elseif howmany==2
% 	 temp3(1)=isonebyone(i,parens(1),centercomma);
% 	 temp3(2)=isonebyone(i,centercomma,parens(2));
% 	 if ~((temp3(1)==1)&(temp3(2)==1))
% 	  out=0;
% 	 end
% 	end
%        end
%       end
%      elseif length(find(strcmp(funstrwords{i}(temp_word(j)),convertedwords)))>0
%       out=1;
%       [howmany,subscripts,centercomma,parens]=hassubscript(i,temp_word(j));
%       typestr=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);
%       if length(find(strcmp(funstrwords{i}(temp_word(j)),make_words)))>0
%        eval(['[temp1,temp2,temp5]=',funstrwords{i}{temp_word(j)},'_make(''',typestr,''');']);
%        if (length(findstr(temp5{2},'r'))+length(findstr(temp5{2},'c'))+length(findstr(temp5{2},'i'))+length(findstr(temp5{2},'l')))~=0
% 	out=0;
%        end
%       elseif length(find(strcmp(funstrwords{i}{temp_word(j)}(1:end-1),make_words)))>0
%        eval(['[temp1,temp2,temp5]=',funstrwords{i}{temp_word(j)}(1:end-1),'_make(''',typestr,''');']);
%        if (length(findstr(temp5{2},'r'))+length(findstr(temp5{2},'c'))+length(findstr(temp5{2},'i'))+length(findstr(temp5{2},'l')))~=0
%         out=0;
%        end
%       else
%        if length(typestr)>0
%         if (length(findstr(typestr,'r'))+length(findstr(typestr,'c'))+length(findstr(typestr,'i'))+length(findstr(typestr,'l')))~=0
%          out=0;
%         end
%        else
%         out=0;
%        end
%       end        
%      elseif length(find(strcmpi(funstrwords{i}(temp_word(j)),'mlcall')))>0
%       out=0;
%      end
%     end
%    end
%   end
%  end
% end