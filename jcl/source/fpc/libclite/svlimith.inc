
type
   __vlimit_resource = (
     LIM_NORAISE := 0,
     LIM_CPU := 1,
     LIM_FSIZE := 2,
     LIM_DATA := 3,
     LIM_STACK := 4,
     LIM_CORE := 5,
     LIM_MAXRSS := 6);

Const
     VLIMIT_INFINITY = $7fffffff;

function vlimit(__resource:__vlimit_resource; __value:longint):longint;cdecl;external clib name 'vlimit';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
