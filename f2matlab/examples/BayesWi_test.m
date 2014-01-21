function bayes
tepoch=0;xref=zeros(6,1);maxit=0;reject=0;

tepoch=sqrt(2.0);
xref=[sqrt(3.0),2.,3.,4.,5.,6.];
maxit=sqrt(5.0);
reject=sqrt(6.0);
fid_9=fopen('baydebug.out','w+');
%format ( e20.13'tepoch='e20.13,'tepoch='e20.13);
fprintf(1,['%20.13g','tepoch=','%20.13g','tepoch=','%20.13g' ' \n'], tepoch,tepoch,tepoch);
%format( "abc",4x,./,2(6(e20.13),./],./];
fprintf(fid_9,['abc',repmat(' ',1,4), '\n ' ,repmat([repmat(['%20.13g'] ,1,6), '\n '  '\n ' ] ,1,2), '\n '  '\n '  ' \n'], xref,xref);
fprintf(fid_9,['abc',repmat(' ',1,4), '\n ' ,repmat([repmat(['%20.13g'] ,1,6), '\n '  '\n ' ] ,1,2), '\n '  '\n ' ], xref,xref);
%format( ././,2x,"bayes filter",./,2x,['epoch time :'],e20.13,./,2x,['previous estimated state vector:'],./,2x,3e20.13,./,2x,3e20.13,./, 2x,['maximum iterations:'],i3,./, 2x,'reject if gt ',e12.5,' sigma ',./././ );
fprintf(fid_9,[ '\n '  '\n ' ,repmat(' ',1,2),'bayes filter', '\n ' ,repmat(' ',1,2),'epoch time :','%20.13g', '\n ' ,repmat(' ',1,2),'previous estimated state vector:', '\n ' ,repmat(' ',1,2),repmat('%20.13g',1,3), '\n ' ,repmat(' ',1,2),repmat('%20.13g',1,3), '\n ' ,repmat(' ',1,2),'maximum iterations:','%3g', '\n ' ,repmat(' ',1,2),'reject if gt ','%12.5g',' sigma ', '\n '  '\n '  '\n '  ' \n'], tepoch,xref,maxit,reject);

fprintf(1,'%0.15g ', tepoch);fprintf(1,'%s \n','ggggggg');
fprintf(fid_9,'%0.15g ', tepoch);fprintf(fid_9,'%s ','asd');fprintf(fid_9,'%0.15g ',xref);fprintf(fid_9,'%s ','tytyty');fprintf(fid_9,'%0.15g \n',maxit);
fprintf(fid_9,'%0.15g \n', tepoch);
fprintf(1,'%s \n','wrote it');
fclose(fid_9);
fprintf(1,'%0.15g ', tepoch);fprintf(1,'%s ','asd');fprintf(1,'%0.15g ',xref);fprintf(1,'%s ','tytyty');fprintf(1,'%0.15g \n',maxit);
fprintf(1,[ '\n '  '\n ' ,repmat(' ',1,2),'bayes filter', '\n ' ,repmat(' ',1,2),'epoch time :','%20.13g', '\n ' ,repmat(' ',1,2),'previous estimated state vector:', '\n ' ,repmat(' ',1,2),repmat('%20.13g',1,3), '\n ' ,repmat(' ',1,2),repmat('%20.13g',1,3), '\n ' ,repmat(' ',1,2),'maximum iterations:','%3g', '\n ' ,repmat(' ',1,2),'reject if gt ','%12.5g',' sigma ', '\n '  '\n '  '\n ' ], tepoch,xref,maxit,reject);

