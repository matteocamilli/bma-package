#!/bin/bash

###########################
# adaptation raw log
LOG=$1
###########################

echo 'type model RE success'

for i in {1..8}
do
  n=$(grep Logit $LOG | grep "Vars: $i" | cut -d':' -f2 | cut -d' ' -f2)
  re=$(grep Logit $LOG | grep "Vars: $i" | cut -d':' -f3 | cut -d' ' -f2)
  success=$(grep Logit $LOG | grep "Vars: $i" | cut -d':' -f4 | cut -d' ' -f2)
  type=$(yes 'Logit' | head -n $(echo "$n" | wc -w))
  model=$(yes 'Logit'$i | head -n $(echo "$n" | wc -w))

  paste -d ' ' <(echo "$type") <(echo "$model") <(echo "$re") <(echo "$success")
done

re=$(grep "BMA RE" $LOG | cut -d':' -f2 | cut -d' ' -f2)
success=$(grep "BMA RE" $LOG | cut -d':' -f3 | cut -d' ' -f2)
type=$(yes 'BMA' | head -n $(echo "$re" | wc -w))
model=$type

paste -d ' ' <(echo "$type") <(echo "$model") <(echo "$re") <(echo "$success")
