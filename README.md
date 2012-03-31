
__License__: [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0.html)

__Author__: Uvarov Michael ([`freeakk@gmail.com`](mailto:freeakk@gmail.com))


6> gproc:reg({p, l, {mod, type}}).
true
7> gproc:send({p, l, {mod, type}}, {self(), x, y}).    
{<0.45.0>,x,y}
8> flush().
Shell got {<0.45.0>,x,y}

