#!/bin/bash

###########################
# precision-recall raw log
LOG=$1
###########################

echo 'type model precision recall F1'

for i in {1..8}
do
  n=$(grep Logit $LOG | grep "Vars: $i" | cut -d':' -f2 | cut -d' ' -f2)
  precision=$(grep Logit $LOG | grep "Vars: $i" | cut -d':' -f2 | cut -d' ' -f5)
  recall=$(grep Logit $LOG | grep "Vars: $i" | cut -d':' -f2 | cut -d' ' -f8)
  f1=$(grep Logit $LOG | grep "Vars: $i" | cut -d':' -f2 | cut -d' ' -f11)
  type=$(yes 'Logit' | head -n $(echo "$n" | wc -w))
  model=$(yes 'Logit'$i | head -n $(echo "$n" | wc -w))

  paste -d ' ' <(echo "$type") <(echo "$model") <(echo "$precision") <(echo "$recall") <(echo "$f1")
done
