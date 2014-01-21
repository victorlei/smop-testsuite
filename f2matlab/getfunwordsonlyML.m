function funwords=getfunwordsonlyML
funwords=cell(7,1);
funwords{1}='abs';
funwords{2}='conj';
funwords{3}='fix';
funwords{4}='size';
funwords{5}='deblank';
funwords{6}='upper';
funwords{7}='lower';
funwords{length(funwords)+1}='imag';
funwords{length(funwords)+1}='ans';

