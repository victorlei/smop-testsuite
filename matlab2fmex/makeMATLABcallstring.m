function tempstr1=makeMATLABcallstring(howmany,subscripts,centercomma,parens,i,j);
declare_globals
tempstr1=''; %default
for k=1:howmany
 is_real=0; is_complex=0; is_integer=1; is_logical=0; is_1d=0; is_2d=0; scalar=1;
 is_2d_r=0; is_2d_c=0;
 if howmany==1
  if isinf(j)
   start=parens(1);            finish=parens(2);
   [outflag,howmany3,subscripts3,centercomma3,parens]=inwhichlast(i,parens(1));   %97
  else
   start=parens(1);           finish=parens(2);
  end
  nums=find((funstrnumbers_b{i}>=start)&(funstrnumbers_b{i}<=finish));
  words=find((funstrwords_b{i}>=start)&(funstrwords_b{i}<=finish));
 else
  if k==1
   start=parens(1);          finish=centercomma(1);
  elseif k==howmany
   start=centercomma(k-1);   finish=parens(2);
  else
   start=centercomma(k-1);   finish=centercomma(k);
  end
  nums=find((funstrnumbers_b{i}>=start)&(funstrnumbers_b{i}<=finish));
  words=find((funstrwords_b{i}>=start)&(funstrwords_b{i}<=finish));
 end
 for m=1:length(words) %Should be a main level (or in bracket in main level) word to check it
  [outflag,howmany3,subscripts3,centercomma3,parens3]=inwhichlast(i,funstrwords_b{i}(words(m)));
  if ~isempty(parens3)
   %if parens3(1)==parens(1)% | parens4(1)==parens(1)
   if parens3(1)==parens(1) | parens3(1)==1
    %howmany,subscripts,centercomma,parens,i,j,nums,words,kb
    if length(find(strcmpi(funstrwords{i}(words(m)),inoutother3)))>0 %Have a variable
     if length(find(strcmpi(funstrwords{i}(words(m)),inoutother{3})))>0 %Have a local variable
      temp1=find(strcmpi(funstrwords{i}(words(m)),inoutother{3}));
      if strcmpi(localvartype{temp1},'real')
       is_real=1; %Now we are indexed to the real numbers.
      elseif strcmpi(localvartype{temp1},'complex')
       is_complex=1;
      elseif strcmpi(localvartype{temp1},'logical')
       is_logical=1;
      end
     else
      if isreal(getfield(cw,funstrwords{i}{words(m)}))
       is_real=1; %Now we are indexed to the real numbers.
      else
       is_complex=1;
      end
     end
     [howmany2,subscripts2,centercomma2,parens2]=hassubscript(i,words(m));
     %What dim of var do we have?
     if howmany2==0 %No subscript? either 2d or scalar
      if prod(size(getfield(cw,funstrwords{i}{words(m)})))>1
       if     size(getfield(cw,funstrwords{i}{words(m)}),1)==1, is_2d_r=1;
       elseif size(getfield(cw,funstrwords{i}{words(m)}),2)==1, is_2d_c=1;
       else                                                     is_2d=1;
       end
      end
     elseif howmany2>0 %Run through subscripts, if have anything but scalar, result is 2d
      if ~is_2d
       typestr=makeMATLABcallstring(howmany2,subscripts2,centercomma2,parens2,i,words(m));
       if howmany2==1
        if any(strcmpi(typestr(1),typs{12})) %non scalar
         if     size(getfield(cw,funstrwords{i}{words(m)}),1)==1
          if any(strcmpi(typestr(1),typs{1})) %If a array indexes a row, result is matrix
           is_2d=1;
          else                               %Otherwise result is row
           is_2d_r=1;
          end
         elseif size(getfield(cw,funstrwords{i}{words(m)}),2)==1
          if any(strcmpi(typestr(1),typs{1})) %If a array indexes a column, result is matrix
           is_2d=1;
          else                               %Otherwise result is col
           is_2d_c=1;
          end
         else %Have a 2-D var with 1 subscript, must be find or some, so result is same as subscr
          if     any(strcmpi(typestr(1),typs{1})) %array indexed by array, result is array
           is_2d=1;
          elseif any(strcmpi(typestr(1),typs{9}))|any(strcmpi(typestr(1),typs{4}))
           is_2d_r=1;
          elseif any(strcmpi(typestr(1),typs{10}))%array indexed by col, result is col
           is_2d_c=1;
          end
         end
        end
       elseif howmany2==2
        if any(strcmpi(typestr(1),typs{12}))&any(strcmpi(typestr(2),typs{12})) %both non scalar
         if     size(getfield(cw,funstrwords{i}{words(m)}),1)==1, is_2d_r=1;
         elseif size(getfield(cw,funstrwords{i}{words(m)}),2)==1, is_2d_c=1;
         else %Have a 2-D array indexed by 2 vectors, result is array
          nums1=find((funstrnumbers_b{i}>=parens2(1))&(funstrnumbers_b{i}<=centercomma2(1)));
          words1=find((funstrwords_b{i}>=parens2(1))&(funstrwords_b{i}<=centercomma2(1)));
          nums2=find((funstrnumbers_b{i}>=centercomma2(1))&(funstrnumbers_b{i}<=parens2(2)));
          words2=find((funstrwords_b{i}>=centercomma2(1))&(funstrwords_b{i}<=parens2(2)));
          if (length(nums1)+length(words1))==1 %1st subscript is 1 val vector, so result is row
           is_2d_r=1;
          elseif (length(nums2)+length(words2))==1 %2nd sub is 1 val vector, result col
           is_2d_c=1;
          else %both are multivalued vectors
           is_2d=1;
          end
         end
        elseif any(strcmpi(typestr(1),typs{12})) %Only 1st sub is nonscalar, 2nd is scalar
         is_2d_c=1; %Because this will be vectorized at some point (?? should we)
        elseif any(strcmpi(typestr(2),typs{12})) %Only 2nd sub is nonscalar, 1st is scalar
         is_2d_r=1; %Because this will be vectorized at some point (?? should we)
        else %Do nothing, is scalar result
        end
       end
      end
     end
    elseif length(find(strcmpi(funstrwords{i}(words(m)),'pi')))>0
     is_real=1;
    elseif length(find(strcmpi(funstrwords{i}(words(m)),'end')))>0
    elseif length(find(strcmpi(funstrwords{i}(words(m)),{'i' 'j' 'cmplx'})))>0
     is_real=0;is_complex=1;
    elseif length(find(strcmpi(funstrwords{i}(words(m)),make_words)))>0 %make_variable
     [howmany2,subscripts2,centercomma2,parens2]=hassubscript(i,words(m));
     typestr=makeMATLABcallstring(howmany2,subscripts2,centercomma2,parens2,i,words(m));
     eval(['[temp1,temp2,temp5]=',funstrwords{i}{words(m)}(1:end-1),'_make(''',typestr,''');']);
     if any(strcmpi(temp5{2},typs{6})), is_real=1;   end
     if any(strcmpi(temp5{2},typs{3})), is_complex=1;end
     if any(strcmpi(temp5{2},typs{8})), is_logical=1;end
     if any(strcmpi(temp5{2},typs{1})), is_2d=1;     end
     if any(strcmpi(temp5{2},typs{4})), is_1d=1;     end
     if any(strcmpi(temp5{2},typs{9})), is_2d_r=1;   end
     if any(strcmpi(temp5{2},typs{10})),is_2d_c=1;   end
    elseif length(find(strcmpi(funstrwords{i}(words(m)),make_words2)))>0 
     %make_variable no f but not intrinsic
     [howmany2,subscripts2,centercomma2,parens2]=hassubscript(i,words(m));
     typestr=makeMATLABcallstring(howmany2,subscripts2,centercomma2,parens2,i,words(m));
     eval(['[temp1,temp2,temp5]=',funstrwords{i}{words(m)},'_make(''',typestr,''');']);
     if any(strcmpi(temp5{2},typs{6})), is_real=1;   end
     if any(strcmpi(temp5{2},typs{3})), is_complex=1;end
     if any(strcmpi(temp5{2},typs{8})), is_logical=1;end
     if any(strcmpi(temp5{2},typs{1})), is_2d=1;     end
     if any(strcmpi(temp5{2},typs{4})), is_1d=1;     end
     if any(strcmpi(temp5{2},typs{9})), is_2d_r=1;   end
     if any(strcmpi(temp5{2},typs{10})),is_2d_c=1;   end
    elseif any(strcmpi(funstrwords{i}(words(m)),filename_all))          %subroutine name
     is_2d=1;
    elseif any(strcmpi(funstrwords{i}(words(m)),intrinsics))
     %Intrinsics means all words taken care of in functionresultsize.m
     if ~(is_complex&is_2d)
      tempstr=functionresultsize(i,words(m));
      if any(strcmpi(typs{6},tempstr)), is_real=1;   end
      if any(strcmpi(typs{3},tempstr)), is_complex=1;end
      if any(strcmpi(typs{8},tempstr)), is_logical=1;end
      if any(strcmpi(typs{1},tempstr)), is_2d=1;     end
      if any(strcmpi(typs{4},tempstr)), is_1d=1;     end     
      if any(strcmpi(typs{9},tempstr)), is_2d_r=1;   end
      if any(strcmpi(typs{10},tempstr)),is_2d_c=1;   end     
     end
%%%      if strcmpi(funstrwords{i}{words(m)},'sum')
%%%       'intrins',funstr{i},funstrwords{i}{words(m)}
%%%       is_real
%%%       is_complex
%%%       is_integer
%%%       is_logical
%%%       is_1d
%%%       is_2d
%%%       is_2d_r
%%%       is_2d_c
%%%       scalar
%%%       tempstr
%%%       kb
%%%      end
    elseif strncmp('mxT',funstrwords{i}(words(m)),3)
     if length(funstrwords{i}{words(m)})>3
      if strcmpi(funstrwords{i}{words(m)}(1:4),'mxTR')
       is_real=1;       is_2d=1;
      end
      if strcmpi(funstrwords{i}{words(m)}(1:4),'mxTC')
       is_complex=1;    is_2d=1;
      end
      if strcmpi(funstrwords{i}{words(m)}(1:4),'mxTI')
                        is_2d=1;
      end
     end
    else
     is_real=1; is_2d=1; %scalar=0; %default if something else is there (eg another mlcall)
    end   
   end
  end
%%%      if strcmpi(funstrwords{i}{words(m)},'sum')
%%%       'intrins',funstr{i},funstrwords{i}{words(m)}
%%%       m
%%%       is_real
%%%       is_complex
%%%       is_integer
%%%       is_logical
%%%       is_1d
%%%       is_2d
%%%       is_2d_r
%%%       is_2d_c
%%%       scalar
%%%       kb
%%%      end
 end
 
 if ~(is_1d|is_2d)
  temp=findstr(funstr{i},'['); %Promote to vector if has a [] expr.
  temp1=findrights(temp,funstr{i});
  if ~isempty(temp)
   temp=temp( (temp>=start)&(temp<=finish) );
   if ~isempty(temp)
    for m=1:length(temp)
     [outflag,howmany3,subscripts3,centercomma3,parens3]=inwhichlast(i,temp(m)+1);     
     if ~isempty(parens3)
      if parens3(1)==parens(1)
       %funstr{i}(temp(m)+1:temp1(m)-1)
       tempstr2=makeMATLABcallstring(1,{funstr{i}(temp(m)+1:temp1(m)-1)},[],[temp(m)+1 temp1(m)-1],i,inf);
       if any(strcmpi(tempstr2,typs{6})), is_real=1;   end
       if any(strcmpi(tempstr2,typs{3})), is_complex=1;end
       if any(strcmpi(tempstr2,typs{8})), is_logical=1;end
%%%       'reeeeeeeeeeeeeeeee2',funstr{i},outflag,howmany3,subscripts3,centercomma3,parens3,
%%%       is_real
%%%       is_complex
%%%       is_integer
%%%       kb
       is_1d=1; break
      else
       [outflag,howmany3,subscripts3,centercomma3,parens3]=inwhichlast(i,temp(m)); %279
       if ~isempty(parens3)
        if parens3(1)==parens(1)
         %funstr{i}(temp(m)+1:temp1(m)-1)
         tempstr2=makeMATLABcallstring(1,{funstr{i}(temp(m)+1:temp1(m)-1)},[],[temp(m)+1 temp1(m)-1],i,inf);
         if any(strcmpi(tempstr2,typs{6})), is_real=1;   end
         if any(strcmpi(tempstr2,typs{3})), is_complex=1;end
         if any(strcmpi(tempstr2,typs{8})), is_logical=1;end
%%%         'reeeeeeeeeeeeeeeee',funstr{i},outflag,howmany3,subscripts3,centercomma3,parens3
%%%         is_real
%%%         is_complex
%%%         is_integer
%%%         kb
         is_1d=1; break
        end
       end
      end
     end
    end
%%%    'bracketmake',funstr{i}(start:finish),funstr{i},
%%%    is_real
%%%    is_complex
%%%    is_integer
%%%    is_logical
%%%    is_1d
%%%    is_2d
%%%    is_2d_r
%%%    is_2d_c
%%%    scalar
%%%    kb
   end
  end
  temp=findstr(funstr{i},':'); %Promote to vector if has a : expr.
  if ~isempty(temp)
   temp=temp( (temp>=start)&(temp<=finish) );
   if ~isempty(temp)
    for m=1:length(temp)
     [outflag,howmany3,subscripts3,centercomma3,parens3]=inwhichlast(i,temp(m)); %35          
     if ~isempty(parens3)
      if parens3(1)==parens(1)
       is_1d=1; break
      end
     end
    end
   end
  end
 end
%%%        if any(strcmpi('prod',funstrwords{i}))
%%%        'wqordggggggggg',funstr{i},funstrwords{i}{j},
%%%        is_real
%%%        is_complex
%%%        is_integer
%%%        is_logical
%%%        is_1d
%%%        is_2d
%%%        is_2d_r
%%%        is_2d_c
%%%        scalar
%%%        kb
%%%       end

 if ~(is_real|is_complex)
  %fid=0;parens3=[1 1];
  for m=1:length(nums) % Promote to real if has a real number at main level
   if m>1
    %funstr{i},funstr{i}(start:finish),funstr{i},kb
    if ~(funstrnumbers_b{i}(nums(m))>parens3(1) & funstrnumbers_b{i}(nums(m))<parens3(2) & ~any(funstr{i}(funstrnumbers_b{i}(nums(m-1)):funstrnumbers_b{i}(nums(m)))=='['))
     [outflag,howmany3,subscripts3,centercomma3,parens3]=inwhichlast(i,funstrnumbers_b{i}(nums(m)));
    end
   else
    [outflag,howmany3,subscripts3,centercomma3,parens3]=inwhichlast(i,funstrnumbers_b{i}(nums(m)));
   end
   if ~isempty(parens3)
%%%   if parens(1)~=1 % Don't have a main level entity
    if outflag==1 % In bracket
     [outflag,howmany4,subscripts4,centercomma4,parens4]=inwhichlast(i,parens3(1));
    else
     parens4=parens3;
    end
%%%   end
    if parens3(1)==parens(1) | parens4(1)==parens(1)
     %funstr{i},funstr{i}(start:finish),funstr{i},kb
     if length(intersect([typs{17}{:}],funstrnumbers{i}{nums(m)}))>0
      is_real=1; break
     end
    end
   end
  end
 end
%%%        if any(strcmpi('prod',funstrwords{i}))
%%%         'wqordggggggggg22222',funstr{i},funstrwords{i}{j},
%%%         is_real
%%%         is_complex
%%%         is_integer
%%%         is_logical
%%%         is_1d
%%%         is_2d
%%%         is_2d_r
%%%         is_2d_c
%%%         scalar
%%%         kb
%%%        end
        
 %Now decide what this is...
 if is_2d_r|is_2d_c, is_2d=1; end
 if is_2d, is_1d=1; scalar=1; end
 if is_1d,          scalar=1; end
 if is_integer
  tempstr1(k)=barr{3,end+1-scalar-is_1d-is_2d};
 end
 if is_real
  tempstr1(k)=barr{2,end+1-scalar-is_1d-is_2d};
 end
 if is_complex
  tempstr1(k)=barr{1,end+1-scalar-is_1d-is_2d};
 end
 %Now a test to see of this is a logical statement
 for n=1:length(loglist)
  if ~isempty(subscripts{k})
   temp3=findstrexact(loglist{n},subscripts{k});
   if ~isempty(temp3)
    temp4=findstrexact(subscripts{k},funstr{i});
    for p=1:length(temp3)
     [outflag,howmany3,subscripts3,centercomma3,parens3]=inwhichlast(i,temp4+temp3(p)-1);
     if ~isempty(parens3)
      if parens(1)==parens3(1) %parens we are in match parens that relop is in
       is_logical=1;
      end
     end
    end
   end
  end
 end
 if is_logical
  tempstr1(k)=barr{4,end+1-scalar-is_1d-is_2d};
 end
 if is_2d
  [a(k),b(k)]=find(strcmpi(tempstr1(k),barr));
  if is_2d_r, tempstr1(k)=barr2{a(k),1}; end
  if is_2d_c, tempstr1(k)=barr2{a(k),2}; end
 end
end
%%%if zzz
%%% '11111111111111111'
%%% if ~isempty(parens)
%%%  is_real
%%%  is_complex
%%%  is_integer
%%%  is_logical
%%%  is_1d
%%%  is_2d
%%%  is_2d_r
%%%  is_2d_c
%%%  scalar
%%%  funstr{i}(parens(1):parens(2))
%%% end
%%% howmany,subscripts,centercomma,parens
%%% funstr{i},
%%% tempstr1
%%% kb
%%%end
