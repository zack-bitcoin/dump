### Dump
This is a very simple harddrive database. It only stores one size of block. here is an example of using it.

```
1> ID = database003,
2> dump_sup:start_link(ID, 5, 10000, ram, "temp").
{ok,<0.37.0>}
3> V = <<3,2,1,5,6>>.
<<3,2,1,5,6>>
4> Location = dump:put(V, ID).
15992
5> V = dump:get(Location, ID).
<<3,2,1,5,6>>
6> dump:delete(Location, ID).
ok
7> dump:get(Location, ID).
<<3,2,1,5,6>>
8> Location = dump:put(<<0:40>>, ID).
15992
9> dump:get(Location, ID).
<<0,0,0,0,0>>
```

it uses the atom ```database003``` so that you can start more than one dump, and give them different names.


Creates 3 files: Name_bits.db, Name.db, and Name_rest.db.

Name_bits.db stores 1 bit for every data entry in the database. To remember if that slot has data we need, or if we can write to it.

Name_rest.db stores an integer that is the current lowest slot that is available to be written to. This is the next place that data will be written to.

Name.db stores the actual data in a binary format. all concatinated together.