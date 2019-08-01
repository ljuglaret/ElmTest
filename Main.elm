module Main exposing (..)

succ x = x + 1
prec y = y - 1
neg x = -x

addition : (Int , Int) -> Int 
addition (x, y) = 
    if (x >=0 && y >=0)
    then
        if (x == 0)
        then y 
        else 
            if (y == 0)
            then x 
            else addition ((succ x), (prec y))
    else
        if (x<=0 && y <=0)
        then neg (addition(neg x, neg y))
        else 
            if (x<=0 && y > 0)
            then  neg ((neg x ) - y)
            else neg ((neg y ) - x)

mult : Int -> Int -> Int  
mult a b = 
    let 
        aux absa absb  =
            if (absa == 0)
            then 0
            else
                if (absb == 0)
                then 0
                else
                    if ((a > 0 && b >0 ) || (a < 0 && b<0))
                    then addition (absa , (aux absa (prec absb)))
                    else (addition (-absa , (aux absa (prec absb))))  
    in 
        aux (abs a ) (abs b)
    
-- a * b 
-- a + a + a + ... + a 

puissance : Int -> Int -> Int
puissance a b =    
    let 
        aux absa absb  =
            if (absb == 0)
            then 1
            else
                if (absa == 0)
                then 0
                else
               
                    if (b>0)
                    then mult a  (aux a (prec absb))
                    else 0
    in 
        aux a b

-- x^n = x*...*x