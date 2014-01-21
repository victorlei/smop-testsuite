for i=1:length(switches)
 switch i
  case 1
  want_kb=switches(1);  % 1 ==> if keyboard mode is desired after some conversion steps
  case 2
  want_ze=switches(2);  % 1 ==> direct f2matlab to zero all array variables.
  case 3
  want_fi=switches(3);  % 1 ==> direct f2matlab to try to put fix()'s around declared integers.
  case 4
  want_smm=switches(4); % 1 ==> try to deal with shape mismatching on in/out vars, 0 ==> don't
  case 5
  want_row=switches(5); % 1 ==> 1-D fortran arrays become row vectors, if 0 then column vectors
  case 6
  want_lc=switches(6);  % 1 ==> try to preserve case on variable names
  case 7
  want_cla=switches(7); % 1 ==> deal with possible command line arguments, 0 ==> don't
  case 8
  want_pst=switches(8); % 1 ==> have local variables be persistent by default (fortran behavior), 0 => not
  case 9
  want_vai=switches(9); % 1 ==> add "varargin" to the input args on all functions (useful in some cases)
  case 10
  want_for=switches(10); % 1 ==> increment for loop vars on exit
  case 11
  want_MP=switches(11);  % 1 ==> change all the local vars in the main program to have an MP suffix 
  case 12
  want_gl=switches(12);  % 1 ==> use global statements instead of module decs in subprograms
 end % switch i
end % for i=1:length(switches)

