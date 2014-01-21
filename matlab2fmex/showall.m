function showall(funstr,indented)
if nargin==0
 for i=1:size(funstr,1)
  disp(funstr{i})
 end
else
 for i=1:size(funstr,1)
  disp(['  ',funstr{i}])
 end
end
