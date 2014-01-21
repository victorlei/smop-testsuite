function implicit=implicitRules
 
 implicit=cell(26,2);
 for i=97:122
  ii=i-96;
  implicit{ii,1}=char(i);
  if i>=105 & i<=110
   implicit{ii,2}='integer';
  else
   implicit{ii,2}='real';
  end
 end
 