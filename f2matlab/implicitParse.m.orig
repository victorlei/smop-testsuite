function implicit=implicitParse(implicit,i,j,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords,var_words)
 
% can't do implicit for types yet
%
% but can do stuff like:
% IMPLICIT DOUBLE PRECISION (D)
% IMPLICIT COMPLEX (S,Y), LOGICAL(1) (L,A-C)
% IMPLICIT CHARACTER*32 (T-V)
% IMPLICIT CHARACTER*2 (W)
 
 
 for jj=j:length(funstrwords{i})
  temp=strcmp(funstrwords{i}{jj},var_words);
  if any(temp)
   %OK, found a var_word, now find the next set of () with letters in it
   temp1=find(funstr{i}=='(');
   found=0;
   if ~isempty(temp1)
    for ii=1:length(temp1)
     temp2=findrights_f(temp1(ii),funstr{i});
     if temp1(ii)>funstrwords_e{i}(jj) && any(isletter(funstr{i}(temp1(ii):temp2)))
      found=temp1(ii);      break
     end % if temp1(ii)>funstrwords_e{i}(ii) && any(isletter(funstr{i}(temp1(ii):temp2)))
    end % for j=1:
    if found
     in=funstr{i}(temp1(ii):temp2);
     commas=find(in==',');
     temp3=[1,commas,length(in)];
     for k=1:length(temp3)-1
      group=in(temp3(k)+1:temp3(k+1)-1);
      if any(group=='-')
       start=group(find(isletter(group),1,'first'));
       finish=group(find(isletter(group),1,'last'));
       for kk=double(start):double(finish)
        implicit=assignImplicit(implicit,char(kk),funstrwords{i}{jj});
       end % for kk=double(start):double(finish)
      else %should be a single letter at this point
       if length(strtrim(group))==1 && isletter(strtrim(group))
        implicit=assignImplicit(implicit,strtrim(group),funstrwords{i}{jj});
       end % if length(strtrim(group))==1 && isletter(strtrim(group))
      end % if any(group=='-')
     end % for k=1:length(temp3)-1
    end % if found
   else %no ()'s after this?
    break
   end % if ~isempty(temp1)
  end % if any(temp)
 end % for i=j:length(funstrwords{i})
  
 
function implicit=assignImplicit(implicit,in,varType)
 
% var_words={'real';'complex';'integer';'logical';'character';'implicit';'intrinsic';'dimension';'common';'double';'precision';'doubleprecision';'intent';'allocatable';'pointer';'equivalence';'external';'parameter';'save';'automatic';'private';'public';'static';'optional';'volatile';'data';'type';'recursive'};
 
 if any(strcmp(varType,{'real','complex','integer','logical','character'}))
  implicit{double(in)-96,2}=varType;
 else
  implicit{double(in)-96,2}='real'; %default
 end