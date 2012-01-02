for i in *.svg; do
inkscape -D -w 23 -f ${i} -e $(basename $i .svg)_small.png;
done
