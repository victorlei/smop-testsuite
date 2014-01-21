function [localVar,originalLocaVar]=addVars(localVar,modLocalVar,usedMods,modUsedMods)

originalLocaVar=localVar;
for i=1:length(usedMods)
 [localVar{size(localVar,1)+1:size(localVar,1)+size(modLocalVar{usedMods(i),2},1),:}]=deal(modLocalVar{usedMods(i),2}{:,:});
 %does this module use others?
 temp=find(strcmp({modUsedMods{:,1}},modLocalVar{usedMods(i),1}));
 if ~isempty(temp)
  %'deeeeeeeeeee',temp,localVar,usedMods,kb
  for j=1:length(modUsedMods{temp,2})
   % where is this used mod in modLocalVar
   temp2=find(strcmp(modUsedMods{temp,2}{j},{modLocalVar{:,1}}));
   if ~isempty(temp2)
    localVar=addVars(localVar,modLocalVar,temp2,modUsedMods);
   end
  end
 end
end


[a,b]=unique({localVar{:,1}});
localVar=localVar(b,:);

%%%localVar
%%%'feeeeeee',kb
