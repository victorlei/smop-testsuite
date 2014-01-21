function funstr=replaceword(i,j,funstr,funstrwords,funstrwords_b,funstrwords_e,repstr)
funstr{i}=[funstr{i}(1:(funstrwords_b{i}(j)-1)),repstr,funstr{i}((funstrwords_e{i}(j)+1):length(funstr{i}))];
