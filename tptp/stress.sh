#/bin/bash

while true
    do
        for i in `seq 1 20`
        do
            echo $i ms
            ./Test $i +RTS -N2
            pgrep eprover
        done
    done
