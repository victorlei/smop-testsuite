function outflag=insubscript(i,spot,left,right);
declare_globals
needparentest=0;
if nargin<3
 needparentest=1;
end
outflag=0;
needtest=0;
leftpp=funstr{i}(1:spot)=='(';
rightpp=funstr{i}(1:spot)==')';
if needparentest
 leftp=length(find(leftpp));
 rightp=length(find(rightpp));
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
  both_p=leftpp-rightpp;                 c_p=cumsum(both_p);
  last0=max(find(c_p==0));
  leftp_loc=find(leftpp);
  leftp_loc=leftp_loc(leftp_loc>last0);%Now only those which start after the last 0
  leftp_loc=leftp_loc(c_p(leftp_loc)<=c_p(spot));%Now only those which are left of closed groups
  %Now lets run through them. Last check is to see if each closes before spot
  for j=length(leftp_loc):-1:1
   if ~outflag
    if length(find(c_p(leftp_loc(j)+1:spot-1)<c_p(leftp_loc(j))))==0
     %Ready to go with this open paren
     temp=find(funstrwords_b{i}<leftp_loc(j));
     if ~isempty(temp)
      if (length(find(strcmp(funstrwords{i}(temp(end)),inoutother3)))>0)
       [howmany,subscripts,centercomma,parens]=hassubscript(i,temp(end));
       if howmany>0
	if ((parens(1)<spot)&(parens(2)>spot))
	 outflag=1;
	end
       end
      end
     end
    end
   end
  end
 end
end
