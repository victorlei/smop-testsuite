for i=1:length(temp2)
 temp4=find(temp3==i);
 if length(temp4)>1
  temp5=changeCase(temp4);
  temp6=0;
  for j=1:length(temp5)
   temp7=length(find(temp5{j}==upper(temp5{1})));
   if temp7>temp6
    %'ttttttt222222222',kb
    temp6=temp7;
    temp2(i)=temp4(j);
   end % if temp7>temp6
  end % for j=1:length(temp5)
 end % if length(find(temp3==i))>1
end % for i=1:length(changeCase)
 
