function [sublist,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,fs_good]=findendSub_f(linenum,sublist,s,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,fs_good,funwords,var_words)
%if linenum is empty, then find sublist for the entire funstr
% otherwise, return sublist entry that the linenum is in

global modUsedMods

type_words={'program';'function';'subroutine';'module';'blockdata';'end';'contains';'interface';'use'};
goon=1;
if isempty(linenum)
 sublist=cell(0,9);
 if isempty(modUsedMods),  modUsedMods=cell(0,2); end
 left=0; right=0;
 for i=1:s
%%%  if any(strcmp('end',funstrwords{i}))
%%%   'pppppppp',sublist,funstr{i},kb
%%%  end
  for j=1:length(funstrwords{i})
   temp=find(strcmpi(funstrwords{i}{j},type_words));
%%%   if any(strcmpi(funstrwords{i},'clear_cellsim_inputs'))
%%%    's2s2s2s2s2s',funstr{i},temp,j,kb
%%%   end
   if ~isempty(temp)
    funstr{i}(funstrwords_b{i}(j):funstrwords_e{i}(j))=...
        lower(funstr{i}(funstrwords_b{i}(j):funstrwords_e{i}(j)));
    if validSpot(funstr{i},funstrwords_b{i}(j))
     %if ~inastring_f(funstr{i},funstrwords_b{i}(j))&~incomment(funstr{i},funstrwords_b{i}(j))
     switch lower(type_words{temp(1)})
      case {'program';'function';'subroutine';'module';'blockdata';'interface'}
%%%       funstr{i},'gggggggggggg',kb
       goon=1; %we do in fact have a subprg beginning
       if j>1
        if strcmpi(funstrwords{i}{j-1},'end')
         goon=0;
        end
       end % if j>1
       if goon
        left=left+1;
        slLen=size(sublist,1);
        if length(funstrwords{i})>j
         sublist{slLen+1,1}=funstrwords{i}{j+1}; %subprg name
        else
         sublist{slLen+1,1}='';
        end % if length(funstrwords{i})>temp(1)
            % subscripts 
        if any(strcmp(funstrwords{i}{j},{'function','subroutine'}))
         [howmany,subscripts,centercomma,parens]=hassubscript_f(i,j+1,funstr,funstrnumbers,funstrnumbers_b,funstrnumbers_e,funstrwords,funstrwords_b,funstrwords_e,funwords);
         if howmany>0
          sublist{slLen+1,8}=strtrim(subscripts);
         end
         %'smaaaaaaaa',funstr{i},kb
        end
        ii=i; %Start this after the last line of the last subprg
        for ii=i-1:-1:1
         if length(funstrwords{ii})>0
          temp4=find(funstr{ii}=='%',1,'first');
          if isempty(temp4)
           %i=ii; 
           break
          else
           if any(funstrwords_b{ii}<temp4)
            %i=ii;
            break
           end
          end
         end % if length(funstrwords{ii})>0
         if ii==1,ii=0; break; end
        end
        sublist{slLen+1,2}=ii+1;                      %beginning line (incl. leading comments)
        sublist{slLen+1,3}=[];                        %ending line
        sublist{slLen+1,4}=type_words{temp(1)};       %subprg type
        sublist{slLen+1,5}=[];                        %if it has a contains, then line no
        sublist{slLen+1,6}=left-right-1;              %nest level (main level is nest level 0)
        sublist{slLen+1,7}='';                        %parent name
        sublist{slLen+1,9}=i;                         %actual subprg declaration
        if sublist{slLen+1,6}~=0
%%%         if strcmp('clear_cellsim_inputs',sublist{end,1})
%%%          'sssssssssssss',kb
%%%         end
         temp6=find([sublist{1:slLen+1,6}]==sublist{slLen+1,6}-1,1,'last');
         sublist{slLen+1,7}=sublist{temp6,1};
%%%         for k=slLen+1:-1:1
%%%          if sublist{k,6}==sublist{slLen+1,6}-1
%%%           sublist{slLen+1,7}=sublist{k,1};           break
%%%          end % if sublist{k,
%%%         end % for k=slLen+1:-1:1
        end % if sublist{slLen+1,
       end % if goon
       break
      case 'contains'
       sublist{size(sublist,1),5}=i;                  %if it has a contains
       break
      case 'end'
       % make sure this is not an end select or something
       goonimag=1;
       if length(funstrwords{i})>j
        if ~any(strcmpi(funstrwords{i}{j+1},type_words))
         goonimag=0;
        end
       end

%%%       if any(strcmpi(funstrwords{i},'skipping'))
%%%       sublist,goonimag,temp,funstr{i},i,j,'55555555',kb
%%%       end

       % wait! is this a variable end?        
       if (any(funstr{i}=='=') && any(validSpot(funstr{i},find(funstr{i}=='=')))) || ...
            any(strcmp(funstrwords{i},'call'))
        goonimag=0;
        funstr{i}(funstrwords_b{i}(j):funstrwords_e{i}(j))='eml';
        [s,fs_good]=updatefunstr_1line_f(funstr,fs_good,i);
        %'rerererererer',funstr(i),kb
       end
       if goonimag && goon
        right=right+1;
%%%        %right,left,{funstr{i-10:i+2}},i
%%%        %if right>20, 'jjjjjjjjjjjjj',right,left,kb, end
%close the last open subprg
        for k=size(sublist,1):-1:1
         if isempty(sublist{k,3})
          sublist{k,3}=i;         break
         end % if isempty(sublist(size(sublist,
        end % for k=size(sublist,
        break
       end
      case 'use'
       % found a module, so add it to the list if its not already there
       %  what module, function or program are we in
       for ii=size(sublist,1):-1:1
        if any(strcmp(sublist{ii,4},{'program';'function';'subroutine';'module'}))
         break
        end
       end
       temp1=find(strcmp({modUsedMods{:,1}},sublist{ii,1}));
       if isempty(temp1)
        modUsedMods{size(modUsedMods,1)+1,1}=sublist{ii,1};
        temp1=size(modUsedMods,1);
       end
       if isempty(modUsedMods{temp1,2}), modUsedMods{temp1,2}=cell(0,1); end
       % and what is the name of the used module? Put it into modUsedMods{temp1,2}{here}
       modUsedMods{temp1,2}=unique({modUsedMods{temp1,2}{:},funstrwords{i}{j+1}});
       %'s2s2s2s2s2s--------',funstr{i},temp,j,kb
     end % switch type_words{j}
         %'pppppppp1',sublist,funstr{i},kb
    end % if ~inastring_f(funstr{i},
   else
%%%     if any(strcmpi(funstrwords{i},'parse_formula'))
%%%      's2s2s2s2s2s--------',funstr{i},temp,j,kb
%%%     end     
    if ~any(strcmpi(var_words,funstrwords{i}{j})) &&...
         ~strcmpi('kind',funstrwords{i}{j}) && ...
         ~(j>1 && strcmpi(funstrwords{i}{j-1},'type'))
     break
    end
   end % if ~isempty(temp)
  end % for j=1:length(type_words)
 end % for i=1:s
end % if isempty(linenum)

%'yyyyyyyyyyyyyy',funstr,sublist,modUsedMods,kb

