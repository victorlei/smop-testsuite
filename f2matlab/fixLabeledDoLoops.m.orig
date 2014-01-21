% is the next thing after the do a number?
if length(funstrwords{i})>1
 if length(funstrnumbers{i})>0
  temp=find(funstrnumbers_b{i}>funstrwords_b{i}(1));
  if ~isempty(temp)
   temp=temp(1);
   if funstrnumbers_b{i}(temp)<funstrwords_b{i}(2)
    %we have a label,     %where is its line label?
    temp1=funstrnumbers{i}{temp};
    for j=fliplr(fs_good)
     if j<i, break; end
     temp2=regexp(funstr{j},'^[\d]+\s+');
     if ~isempty(temp2)
      temp3=funstrnumbers{j}{find(temp2==funstrnumbers_b{j})};
      if strcmp(temp3,temp1)
       %here is the matching label line, put an enddo after this line
       temp5=0;
       for temp4=fs_good(fs_good>j)
        if strcmp(funstrwords{temp4}{1},'end')
         temp5=temp5+1;
        else
         break
        end % if strcmp(funstrwords{temp4}{1},
       end % for temp4=fs_good(fs_good>j)
       j=j+temp5;
       [funstr{j+1:end+1}]=deal(funstr{j:end});
       %funstr.','mmmmmmmmmmm',kb
       funstr{j+1}='end';
       [funstr,funstrwords,funstrwords_b,funstrwords_e,funstrnumbers,funstrnumbers_b,funstrnumbers_e,s,fs_good]=updatefunstr_f(funstr); 
       break
      end % if strcmp(temp3,
     end % if ~isempty(temp2)
    end % for j=fliplr(fs_good)
    %remove the label from the do line
    funstr{i}=[funstr{i}(1:funstrnumbers_b{i}(temp)-1),funstr{i}(funstrnumbers_e{i}(temp)+1:end)];
   end % if funstrnumbers_b{i}(temp)<funstrwords_b{i}(2)
  end % if ~isempty(temp)
 end % if length(funstrnumbers{i})>0
end % if length(funstrwords{i})>1
%temp2,'dddddddddddddd',funstr.',kb