function [strOut,groups]=fixDataGroups(groups);

%'00000000000',groups,kb
%find the '/'s and commas 
subGroup=cell(1,length(groups));
for ii=1:length(groups)
 if any(groups{ii}=='/')
  sla(ii,1:2)=find(groups{ii}=='/');
  [subGroup{ii}]=getTopGroupsAfterLoc(groups{ii}(sla(ii,1)+1:sla(ii,2)-1),0);
  for jj=length(subGroup{ii}):-1:1
   if any(subGroup{ii}{jj}=='*')
    astLoc=find(subGroup{ii}{jj}=='*');
    goon=1;
    %are we in a string?
    qloc=find(subGroup{ii}{jj}=='''');
    if ~isempty(qloc)
     if mod(length(qloc(qloc<astLoc(1))),2)==1
      goon=0;
     end
    end
    if goon
%%%     num=str2num(subGroup{ii}{jj}(1:astLoc-1));
%%%     if ~isempty(num)
%%%     for kk=0:num-1
%%%      subGroup{ii}{jj+kk}=subGroup{ii}{jj}(astLoc+1:end);
%%%     end
%%%%%%     subGroup{ii}{jj}=repmat([subGroup{ii}{jj}(astLoc+1:end),','],1,str2num(subGroup{ii}{jj}(1:astLoc-1)));
%%%%%%     subGroup{ii}{jj}=subGroup{ii}{jj}(1:end-1);
%%%%%%          subGroup{ii}{jj}=['repmat(',subGroup{ii}{jj}(astLoc+1:end),',1,',subGroup{ii}{jj}(1:astLoc-1),')'];
%%%     'fffffffff',subGroup,subGroup{ii}{jj},kb
%%%    end
     
     
     num=str2num(subGroup{ii}{jj}(1:astLoc-1));
     if ~isempty(num)
      oldL=length(subGroup{ii});
      for kk=1:num-1
       subGroup{ii}{oldL+kk}='';
      end
      [subGroup{ii}{jj+num:length(subGroup{ii})}]=deal(subGroup{ii}{jj+1:oldL});
      [subGroup{ii}{jj:jj+num-1}]=deal(subGroup{ii}{jj}(astLoc+1:end));
     else
      % can only be one of these! very limited...
      subGroup{ii}{jj}=['ones(1,',subGroup{ii}{jj}(1:astLoc-1),')*',subGroup{ii}{jj}(astLoc+1:end)];
     end % if ~isempty(num)
     %'000000000001212',num,subGroup{ii},subGroup,kb

    end
%%%    'fffffffff',subGroup,subGroup{ii}{jj},kb
    
   end % if any(subGroup{ii}{jj}=='*')
  end % for jj=length(subGroup{ii}):-1:1
 end % for jj=1:length(temp1) end % if any(groups{ii}=='/')
end % for ii=1:length(groups)

%']]]]]]]]]',groups,subGroup,kb

% now put them back together
for ii=1:length(groups)
 if any(groups{ii}=='/')
  groups{ii}=groups{ii}(1:sla(ii,1));
  for jj=1:length(subGroup{ii})
   groups{ii}=[groups{ii},subGroup{ii}{jj}];
   if jj~=length(subGroup{ii})
    groups{ii}=[groups{ii},','];
   else
    groups{ii}=[groups{ii},'/'];
   end
  end % for jj=1:length(subGroup{ii})
      %'ppppppppppp',groups{ii},kb
 else
  % need to grab one form the next group with /'s and reassign
  for jj=ii+1:length(groups)
   if any(groups{jj}=='/')
    groups{ii}=[groups{ii},'/',subGroup{jj}{1},'/'];
    subGroup{jj}={subGroup{jj}{2:end}};
    %'ooooooooooo',groups,subGroup,kb
    break
   end % if ~any(groups{ii}=='/')
  end % for jj=ii+1:length(groups)
 end % if ~any(groups{ii}=='/')
end % for ii=1:length(groups)

strOut='data ';
for ii=1:length(groups)
 strOut=[strOut,groups{ii}];
 if ii~=length(groups), strOut=[strOut,',']; end
end % for ii=1:length(groups)

%'tttttttttt',groups,strOut,kb
%fix the ridiculous numer* notation

%%%for ii=1:length(groups)
%%% if any(groups{ii}=='*')
