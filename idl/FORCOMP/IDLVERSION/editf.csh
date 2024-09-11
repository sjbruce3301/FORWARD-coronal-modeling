# 
#
\rm a.jou
foreach f ( *.f ) 
    echo 'doing ' $f
    cat $f | sed  's/ZERO/0.d0/g' \
    | sed  's/TWO/2.d0/g' \
    | sed  's/ONE/1.d0/g' \
    | sed  's/THREE/3.d0/g'  \
    | sed  's/FOUR/4.d0/g'  \
    | sed  's/FLOAT/double/g'  \
    | sed  's/\.EQ\./ EQ /g'  \
    | sed  's/\.LE\./ LE /g'  \
    | sed  's/\.LT\./ LT /g'  \
    | sed  's/\.GT\./ GT /g'  \
    | sed  's/\.GE\./ GE /g'  \
    | sed  's/\.NE\./ NE /g'  \
    | sed  's/\.AND\./ and /g'  \
    | sed  's/ENDDO/ endfor /g'  \
    | sed  's/DO /FOR? /g'  \
    | sed  's/\.AND\./ and /g' > a.jou
    echo '\mv a.jou $f.new'
   \mv a.jou $f.new
# ls $f
end

#
