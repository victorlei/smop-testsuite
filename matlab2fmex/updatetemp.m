function maxTRC=updatetemp(maxTRC,topval,typestr);
if strcmp(typestr,'r')
 if topval>maxTRC(1)
  maxTRC(1)=topval;
 end
elseif strcmp(typestr,'c')
 if topval>maxTRC(2)
  maxTRC(2)=topval;
 end
elseif strcmp(typestr,'i')
 if topval>maxTRC(3)
  maxTRC(3)=topval;
 end
end
