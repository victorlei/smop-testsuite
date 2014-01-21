function [changedflag,funstr]=fixNotOperator(i,loc,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords,fortranfunwords,formats,localVar,logicalops,typeDefs,var_words)

logicalops=logicalops([1,4:10],:);
changedflag=1;
goonimag=0;

goon=1; %goon and change things with extra parentheses
temp=nextNonSpace(funstr{i},loc+4);
if ~isempty(temp)
 temp1=find(funstrwords_b{i}==temp(1));
 if ~isempty(temp1)
  temp2=find(strcmp({localVar{:,1}},funstrwords{i}{temp1(1)}));
  if ~isempty(temp2)
   if strcmp(localVar{temp2(1),3},'logical')
    funstr{i}=[funstr{i}(1:loc-1),'~',funstr{i}(loc+5:end)];
    goonimag=1;
    goon=0;
   end % if strcmp(localVar{temp2(1),
  end % if ~isempty(temp2)
 end % if ~isempty(temp1)
end % if ~isempty(temp)

%'ssssssssssss',funstr{i},kb

if goon
 %now deal with the other case
 %  what is the next logicalops on the same paren level as the .not. ?
 l_p=funstr{i}=='(';
 r_p=funstr{i}==')';
 c_p=cumsum(l_p-r_p);
 [a,b]=regexp(funstr{i},{logicalops{:,2}},'start','end');
 if any(~cellfun('isempty',a))
  for j=1:length(a)
   if ~isempty(a{j})
    if c_p(loc)==c_p(a{j}(1))
     [tempflag,dummy,delims]=changeoperator_f(i,logicalops{j,2},a{j}(1),funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords,fortranfunwords,formats,localVar,typeDefs,var_words);
     if all(delims)
      if delims(1)==temp
       %that's it, put parens here
       goonimag=1;
       funstr{i}=[funstr{i}(1:loc-1),'~',...
           funstr{i}(loc+5:delims(1)-1),...
                  '(',funstr{i}(delims(1):delims(4)),')',...
                  funstr{i}(delims(4)+1:end)];
       %'nnnnnnnnnn',funstr{i},goon,kb
      end % if delims(1)==temp
     end % if ~isempty(delims)     
     break
    end % if c_p(loc)==c_p(a{j}(1))
   end % if ~isempty(a{j})
  end % for j=1:length(a)
 end % if any(~cellfun('isempty',
end % if goon


% just for good measure
if ~goonimag
 % change things anyway
 [tempflag,dummy,delims]=changeoperator_f(i,'.not.',loc,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords,fortranfunwords,formats,localVar,typeDefs,var_words);
 %funstr{i},'-------------------',kb
 if delims(1)==0 && delims(2)==0 && delims(3)~=0 && delims(4)~=0
  funstr{i}=[funstr{i}(1:loc-1),'~',...
             funstr{i}(loc+5:delims(3)-1),...
             '(',funstr{i}(delims(3):delims(4)),')',...
             funstr{i}(delims(4)+1:end)];
 else
  funstr{i}=[funstr{i}(1:loc-1),'~',funstr{i}(loc+5:end)];
 end
 %'ssssssssssss',funstr{i},kb
end