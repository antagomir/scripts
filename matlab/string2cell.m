function a=string2cell(mystring);
% Converts a string of whitespace separated substrings into a cell
% array of the substrings.
% Author: Jaakko Peltonen
% Added March 29, 2006
a={};
i=1; j=1;
while i <= length(mystring),
  nosubstring=1;
  while nosubstring==1,
    if isspace(mystring(i))==0, 
      nosubstring=0; 
    else 
      i=i+1; 
      if i > length(mystring), 
        nosubstring=0; 
      end; 
    end;
  end;
  if i <= length(mystring),
    j=i+1;
    while (j <= length(mystring)) && (isspace(mystring(j))==0), 
      j=j+1; 
    end;
    substring=mystring(i:j-1);
    a={a{:},substring};
    i=j;
  end;
end;

