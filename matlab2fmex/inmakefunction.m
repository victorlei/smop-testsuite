function outflag=inmakefunction(i,spot,left,right);
declare_globals
needparentest=0;
outflag=0;
needtest=0;
if nargin<3
 needparentest=1;
end
if needparentest
 leftp=length(findstr('(',funstr{i}(1:spot-1)));
 rightp=length(findstr(')',funstr{i}(1:spot-1)));
 if leftp>rightp
  needtest=1;
 end
else
 needtest=1;
end
if needtest
 if nargin<3
  temp=find(funstrwords_b{i}<spot);
 else
  temp=find(((funstrwords_b{i}<right)&(funstrwords_b{i}>left)));
 end
 if ~isempty(temp)
  for j=1:length(temp)
   if (length(find(strcmp(funstrwords{i}(temp(j)),make_words)))>0)|(length(find(strcmp(funstrwords{i}{temp(j)}(1:end-1),make_words)))>0)
    [howmany,subscripts,centercomma,parens]=hassubscript(i,temp(j));
    if howmany>0
     if ((parens(1)<spot)&(parens(2)>spot))
      outflag=1;
     end
    end
   end
  end
 end
end
