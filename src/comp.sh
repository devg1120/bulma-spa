#grep -r -n -e --ADD -e Comp01 -e comp01  -e Comp02 -e comp02 . > compdiff 
#grep -r -n -e --ADD -e Comp01 -e comp01  -e Comp02 -e comp02 -e Comp03 -e comp03 .  |less
#grep -r -n  -E  "comp[0-9][0-9]" .  |less
grep -r -n  -E -i "\-\-ADD COMPONENT|comp[0-9][0-9]"  *.elm |less
