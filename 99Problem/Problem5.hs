myReverse a = if a == []
                then []
                else [last a] ++ (myReverse (take (length a - 1) a))