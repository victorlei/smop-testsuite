function out = isInteger(i,range,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords,fortranfunwords,formats,localVar)

str=funstr{i};
tl=str=='(';
tl(find(tl))=~inastring_f(str,find(tl));
tl(find(tl))=~inastring2_f(str,find(tl));
tr=str==')';
tr(find(tr))=~inastring_f(str,find(tr));
tr(find(tr))=~inastring2_f(str,find(tr));
tb=tl-tr;   tlevel=[0,cumsum(tb)];

if str(range(1))=='('
 loc=range(1)+1;
else
 loc=range(1);
end


%what words and numbers are on the same level as loc?
possW=find(funstrwords_b{i}>=range(1) & funstrwords_b{i}<=range(2) & ...
      tlevel(funstrwords_b{i})==tlevel(loc));
possN=find(funstrnumbers_b{i}>=range(1) & funstrnumbers_b{i}<=range(2) & ...
      tlevel(funstrnumbers_b{i})==tlevel(loc));

outW=false(1,length(possW));;
outN=false(1,length(possN));;

localVar{size(localVar,1)+1,1}='fix';localVar{size(localVar,1),3}='integer';
for ii=1:length(possW)
 temp=find(strcmp(funstrwords{i}{possW(ii)},{localVar{:,1}}));
 if ~isempty(temp)
  if strcmp(localVar{temp,3},'integer')
   outW(ii)=true;
  end % if strcmp(localVar{temp,
 end % if ~isempty(temp)
end

for ii=1:length(possN)
 if isempty(regexpi(funstrnumbers{i}{possN(ii)},'[\.edq]'))
  outN(ii)=true;
 end % if ~isempty(temp)
end

if all(outW) && all(outN)
 out=1;
else
 out=0;
end

%'inttttttttttt',funstr{i},funstr{i}(range(1):range(2)),out,kb