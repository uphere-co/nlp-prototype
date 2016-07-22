#!/usr/bin/env bash

function size {
    if [ -z $1 ]; then echo 0; exit 1; fi
    if [ -f $1 ]; then
        echo $(wc -l $1|awk '{print $1}')
    else
        echo 0
    fi
    echo
}

script_path=$(dirname $0)

headers=$(find . -type f -name '*.h')

for h in $headers; do
    echo $h
    $script_path/headerguard_gen.py $h > $h.guarded
    if [ -f $h.guarded ] && [ $(( $(size $h.guarded) - $(size $h) )) -eq 5 ];then
        mv $h.guarded $h
    else
        rm $h.guarded
    fi
done
