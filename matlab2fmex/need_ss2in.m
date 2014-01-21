function fid=need_ss2in(howmany,subscripts,centercomma,parens,i,j,tempstr);
declare_globals
fid=zeros(1,howmany);
tempstr_poss={'r';'c';'i';'s';'t';'u'};
loglist={'~';'==',;'&';'|';'>';'<';'xor';'any';'all';'isnan';'isinf';'isfinite'};
if length(inoutother{3})==0
 inoutother3={inoutother{1}{:},inoutother{2}{:}};
else
 inoutother3={inoutother{1}{:},inoutother{2}{:},inoutother{3}{:}};
end
for k=1:howmany
 if any(strcmp(tempstr(k),{'r';'c';'i';'l'}))
  if ~strcmp(subscripts{k}(~isspace(subscripts{k})),':') %If subscript is colon , then don't ss2in
   fid(k)=1;
   if howmany==1
    start=parens(1)+1;      finish=parens(2)-1;
   elseif howmany==2
    if k==1
     start=parens(1)+1;     finish=centercomma(1)-1;
    elseif k==2
     start=centercomma(1)+1;finish=parens(2)-1;
    end
   end
   temp1=findstr(funstr{i}(start:finish),':');
   if ~isempty(temp1)
    temp1=temp1(1);
    for m=1:length(temp1)
     [outflag,howmany3,subscripts3,centercomma3,parens3]=inwhichlast(i,temp1+start-1);
     if ~isempty(parens3)
      if parens(1)==parens3(1) %parens we are in match parens that relop is in
       fid(k)=0;
      end
      if funstr{i}(parens3(1))=='['
       [outflag,howmany4,subscripts4,centercomma4,parens4]=inwhichlast(i,parens3(1));
       if ~isempty(parens4)
        if parens(1)==parens4(1) %parens we are in match parens that relop is in
         fid(k)=0;
        end
       end       
      end
     end
    end
   end
  end
 end
end
