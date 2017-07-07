#!/bin/bash

for q in $(cat mw1.MW tags | cut -f1 -d" " | sed "s/\.//" | sed "s///" | sort | uniq -u | xargs); do 
  grep $q mw1.sdf >> tags
done

# cat tags | sort > blah && mv blah tags

